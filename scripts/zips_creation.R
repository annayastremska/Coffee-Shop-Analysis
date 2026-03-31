library(tigris)
library(sf)
library(dplyr)
library(leaflet)

options(tigris_use_cache = T)
dir.create("map", showWarnings = F)

crime  <- read.csv("stage/stg_crime_data.csv")
subway <- read.csv("stage/stg_nyc_subway_locations.csv") |>
  rename(Latitude = GTFS.Latitude, Longitude = GTFS.Longitude)

nyc_zips <- zctas(state = "NY", year = 2010) |>
  st_transform(4326) |>
  select(zipcode = ZCTA5CE10, geometry)

crime_sf  <- st_as_sf(crime,  coords = c("Longitude", "Latitude"), crs = 4326)
subway_sf <- st_as_sf(subway, coords = c("Longitude", "Latitude"), crs = 4326)

crime_counts <- st_join(crime_sf, nyc_zips, left = FALSE) |>
  st_drop_geometry() |>
  count(zipcode, name = "crime_count")

subway_counts <- st_join(subway_sf, nyc_zips, left = FALSE) |>
  st_drop_geometry() |>
  count(zipcode, name = "subway_stations")

nyc_zips <- nyc_zips |>
  left_join(crime_counts,  by = "zipcode") |>
  left_join(subway_counts, by = "zipcode") |>
  mutate(
    crime_count     = coalesce(crime_count,     0L),
    subway_stations = coalesce(subway_stations, 0L)
  ) |>
  # Only exclude ZIPs with no signal at all
  filter(crime_count > 0 | subway_stations > 0)

nyc_zips <- nyc_zips |>
  mutate(
    # BUG FIX: normalize on crime_count directly, no population division
    crime_norm  = (crime_count - min(crime_count)) / (max(crime_count) - min(crime_count)),
    subway_norm = (subway_stations - min(subway_stations)) / (max(subway_stations) - min(subway_stations)),
    
    # Score: lower crime = safer (+50), more subway = better connected (+50)
    score = round((1 - crime_norm) * 50 + subway_norm * 50, 1),
    
    # BUG FIX: crime_level now based on crime_count, not crime_rate
    crime_level = case_when(
      crime_count <= quantile(crime_count, 0.33) ~ "Low",
      crime_count <= quantile(crime_count, 0.66) ~ "Medium",
      TRUE                                        ~ "High"
    )
  ) |>
  arrange(desc(score)) |>
  mutate(rank = row_number())

nyc_zips |>
  st_drop_geometry() |>
  select(rank, zipcode, crime_level, crime_count, subway_stations, score) |>
  write.csv("mart/neighborhood_scores.csv", row.names = FALSE)

pal <- colorNumeric(palette = "RdYlGn", domain = nyc_zips$score)

leaflet(nyc_zips) |>
  addProviderTiles("CartoDB.DarkMatter") |>
  addPolygons(
    fillColor   = ~pal(score),
    fillOpacity = 0.7,
    color       = "#222",
    weight      = 0.8,
    popup = ~paste0(
      "<b>ZIP ", zipcode, "</b><br>",
      "Rank: #", rank, "<br>",
      "Score: <b>", score, "</b>/100<br>",
      "Crime: <b>", crime_level, "</b> (", format(crime_count, big.mark = ","), " arrests)<br>",
      "Subway stations: <b>", subway_stations, "</b>"
    )
  ) |>
  addLegend("bottomleft", pal = pal, values = ~score,
            title = "&#9749; Location Score", opacity = 0.9) |>
  htmlwidgets::saveWidget("map/nyc_location_map.html", selfcontained = TRUE)