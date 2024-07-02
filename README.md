# Append SMDs to Action Network

The append_smd.R script gets people from Action Network and tries to find their SMDs in two ways:
1. finding the polygon their address is within, and
2. matching them to the voter file
Then it pushes the SMD back to Action Network

# Set Up

1. Store your Action Network API key as the environmental variable an_key (`Sys.setenv(an_key = key)`), or supply it in the an() function.
2. store the API key for your geocoding service. Currently the script uses mapbox, so store the key with `Sys.setenv(MAPBOX_API_KEY = key)`. If you want to use another service, read the [tidygeocoder docs](https://jessecambon.github.io/tidygeocoder/reference/geo.html).
3. Set the path to the voter file at the top of the script. You can [request a voter file from the BOE](https://vr.dcboe.org/230363516728053).

## Future development
Should do some better address processing. It would probably result in better matches to the voter file.