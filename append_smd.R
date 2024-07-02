#load packages
library(httr2)
library(tidyr)
library(dplyr)
library(purrr)
library(tidygeocoder)
library(sf)
library(geojsonsf)
library(readxl)
library(fastLink)
library(stringr)
library(reticulate) # for address parsing


vf_path <- 'path to DC voter file'

# save the api key in your .Renviron as 'an_key'
get_api_key <- function() {
  key <- Sys.getenv("an_key")
  if (identical(key, "")) {
    stop("No API key found, please supply with `api_key` argument or with an_key env var")
  }
  return(key)
}

# generic function for querying AN api
an <- function(resource = NULL, url = NULL, api_key = get_api_key()) {
  if (is.null(url)) {
    url = "https://actionnetwork.org/api/v2/"
    }
  if (is.null(resource)) {
    resource = ''
  }
  request(url) %>%
    req_headers("OSDI-API-Token" = api_key)%>%
    req_url_path_append(resource)
}

get <- function(req) {
  req <- req_perform(req) %>% 
    resp_body_json()
}

put <- function(req, data) {
  req <- req_body_json(req, data = data) %>%
    req_method('PUT') %>%
    req_perform() %>% 
    resp_body_json()
}
  
get_people <- function() {
  json  <- an(resource = 'people') %>% get()
  print(paste0('pulling action network people, page ',json$page))
  people_json <- json$`_embedded`$`osdi:people`
  people_tibble <- tibble::tibble(people = people_json)
  people <- people_tibble %>%
    hoist(people, 
          an_uri = 'identifiers',
          First_Name = 'given_name',
          Last_Name = 'family_name',
          postal_addresses = 'postal_addresses',
          .transform = list(
            postal_addresses = function(x) keep(x, function(x) x$primary == TRUE)[[1]]
          )) %>%
    hoist(postal_addresses,
          Address_Line_1 = list('address_lines', 1L),
          City = 'locality',
          state = 'region',
          Zip_Code = 'postal_code',
          country = 'country',
          an_latitude = list('location', 'latitude'),
          an_longitude = list('location', 'longitude'),
          geocode_accuracy = list('location', 'accuracy')
          ) %>%
    mutate(an_uri = str_sub(str_subset(unlist(an_uri), 'action_network:'), 16))
  json  <- an(url = json$`_links`$`next`$href) %>% get()
  while (length(json$`_embedded`$`osdi:people`)!= 0) {
    print(paste0('pulling action network people, page ',json$page))
    people_json <- json$`_embedded`$`osdi:people`
    people_tibble <- tibble::tibble(people = people_json)
    new_people <- people_tibble %>%
      hoist(people, 
            an_uri = 'identifiers',
            First_Name = 'given_name',
            Last_Name = 'family_name',
            postal_addresses = 'postal_addresses',
            .transform = list(
              postal_addresses = function(x) keep(x, function(x) x$primary == TRUE)[[1]]
            )) %>%
      hoist(postal_addresses,
            Address_Line_1 = list('address_lines', 1L),
            City = 'locality',
            state = 'region',
            Zip_Code = 'postal_code',
            country = 'country',
            an_latitude = list('location', 'latitude'),
            an_longitude = list('location', 'longitude'),
            geocode_accuracy = list('location', 'accuracy')
      ) %>%
      mutate(an_uri = str_sub(str_subset(unlist(an_uri), 'action_network:'), 16))
    people <- rbind(people, new_people)
    json  <- an(url = json$`_links`$`next`$href) %>% get()
    }
  return(people)
}

put_person <- function(an_uri, data) {
  req  <- an(resource = paste0('people/', an_uri)) %>%
    put(data = data)
}

# pull everyone from AN with fields for matching
people <- get_people()

# why are we only getting approximate level geocodes from action network? that's not good enough for an SMD
# mapbox gets better accuracy. mapbox free tier is 600/minute. # of addresses is under that for now
people %>%
  group_by(geocode_accuracy) %>%
  summarise(count = n())

people <- people %>%
  mutate(full_address = paste0(coalesce(Address_Line_1, ''), ", ", coalesce(City, ''), ", ", coalesce(state, ''), " ", coalesce(Zip_Code, '')))

people_geocoded <- people %>%
  filter(!is.na(Address_Line_1)) %>%
  geocode(full_address, method = 'mapbox', lat = mapbox_lat, long = mapbox_long, full_results = T)

people_geocoded %>% 
  group_by(properties.accuracy) %>%
  summarise(count = n())

# load SMD shapefiles
smds <- st_read("https://maps2.dcgis.dc.gov/dcgis/rest/services/DCGIS_DATA/Administrative_Other_Boundaries_WebMercator/MapServer/55/query?outFields=*&where=1%3D1&f=geojson")


# append SMDs for people with lat long
people_geocoded <- people_geocoded %>% 
  filter(!is.na(mapbox_lat)) %>%
  st_as_sf(coords=c("mapbox_long","mapbox_lat"), remove=FALSE , crs = 4326)

people_geocoded_smds <- st_join(people_geocoded, smds)

people_geocoded_smds %>% 
  group_by(is.na(ANC_ID)) %>%
  summarise(count = n())

people_geocoded_smds_skinny <- people_geocoded_smds %>%
  st_drop_geometry() %>%
  select(an_uri, an_address_smd = SMD_ID, mapbox_accuracy = properties.accuracy)

# load the voter file
vf <- read_excel(vf_path)

# easier to make vf address match AN, but might make worse matches, hmm
vf <- vf %>%
  mutate(vf_id = row_number(),
    Address_Line_1 = str_replace_all(
      paste(coalesce(as.character(Street_Number), ''), coalesce(Street_Number_Suffix, ''), coalesce(Street_Name, ''), coalesce(Street_Type, ''), coalesce(Street_Dir_Suffix, '')),
                           '\\s+', ' '))

# match everyone to voter file
matches_out <- fastLink(
  dfA = people, dfB = vf,
  varnames = c('Last_Name', 'First_Name', 'Address_Line_1', "City", "Zip_Code"),
  stringdist.match = c('Last_Name', 'First_Name', 'Address_Line_1', "City"),
  partial.match = c('First_Name', 'Last_Name', 'Address_Line_1'),
  dedupe.matches = FALSE,
  threshold.match = .75
)



vf_matches <- getMatches(dfA = people, dfB = vf, fl.out = matches_out) %>%
  group_by(an_uri) %>%
  # multiple voter file people can match to a VF record, but I only want the top ranked match for each AN record
  filter(row_number(desc(posterior)) ==  1 & posterior >= .75)


vf_matches_skinny <- vf_matches %>%
  select(an_uri, reg_smd = SMD, vf_match_probability = posterior)


# table with AN data and SMDs from different sources to compare

people_smds <- people %>%
  left_join(people_geocoded_smds_skinny, by = join_by(an_uri)) %>%
  left_join(vf_matches_skinny, by = join_by(an_uri)) %>%
  mutate(res_smd = coalesce(an_address_smd, reg_smd))

people_smds_out <- people_smds %>%
  select(!c(postal_addresses, people, full_address), an_geocode_accuracy = geocode_accuracy)

people_smds_out %>% count(is.na(an_address_smd), is.na(reg_smd), sort = TRUE)
readr::write_tsv(people_smds_out, 'smds_appended.tsv')

people_smds_out %>%
  filter(!is.na(reg_smd) & !is.na(an_address_smd) & reg_smd != an_address_smd) %>%
  count()

people_smds_out %>%
  filter(!is.na(reg_smd)) %>%
  count()

people_smds_out %>%
  filter(!is.na(an_address_smd)) %>%
  count()

people_smds_out %>%
  count()


people_smds_out %>%
  group_by(coalesce(an_address_smd, reg_smd)) %>%
  count() %>%
  arrange(desc(n))

people_smds_out %>%
  group_by(an_address_smd) %>%
  count() %>%
  arrange(desc(n))

people_smds_out %>%
  group_by(reg_smd) %>%
  count() %>%
  arrange(desc(n))

# push SMDs back to AN
post_smds <- function(an_uri, res_smd, reg_smd, ...) {
  message(paste0('adding smds for ', an_uri))
  data = list(custom_fields = list(res_smd = res_smd))
  if(!is.na(reg_smd)) {
    data$custom_fields$reg_smd = reg_smd
  }
  put_person(an_uri, data)
}

res <- people_smds_out %>%
  filter(!is.na(res_smd)) %>%
  pmap(post_smds)
                                                                                                                                                                                                                                                                                           
