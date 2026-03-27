require(httr)

headers = c(
  `Accept` = 'application/json, text/javascript, */*; q=0.01',
  `Referer` = 'https://ec.europa.eu/eurostat/cache/RCI/rcit/index70.html',
  `X-Requested-With` = 'XMLHttpRequest',
  `User-Agent` = 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/76.0.3809.100 Safari/537.36',
  `Sec-Fetch-Mode` = 'cors'
)

res <- httr::GET(url = 'https://ec.europa.eu/eurostat/cache/RCI/rcit/maps/topo/NUTS_RG_20M_2016_4326_LEVL_3.json', httr::add_headers(.headers=headers))

res <- content(res, as = "text") %>% fromJSON(flatten = TRUE) 
regions <- res[["objects"]]
regions <- regions[["NUTS_RG_20M_2016_4326"]]
regions <- regions[["geometries"]]

regions <- apply(regions,2,as.character)
write.csv(regions, "nuts_data/NUTS3_geogr_features.csv")
