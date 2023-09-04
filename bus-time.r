pacman::p_load(
  "tidyverse",
  "tidytransit",
  "jsonlite",
  "lubridate",
  "hms",
  "sf",
  "ggspatial",
  "gtfstools",
  "sfnetworks",
  "ggimage",
  "imputeTS",
  "BiocManager",
  "installr",
  "devtools"
)
pacman::p_load_gh("thomasp85/gganimate")


Sys.setlocale(locale = "he_IL.UTF-8")

# Get gtfs file from here https://github.com/hasadna/open-bus-pipelines/blob/main/STRIDE_ETL_PROCESSES.md#gtfs-etl-download-upload
gtfs <- read_gtfs("israel-public-transportation.zip", encoding = "UTF-8")

gtfs$agency %>% View()
gtfs_get_route_id_by_short <- function(gtfs, name_short) {
  gtfs$routes %>% filter(route_short_name == name_short)
}
gtfs_get_route_id_by_short(gtfs, 9) %>%
  filter(agency_id == 3) %>%
  View()
routes <- bind_rows(
  gtfs_get_route_id_by_short(gtfs, 1) %>% filter(agency_id == 5),
  gtfs_get_route_id_by_short(gtfs, 25) %>% filter(agency_id == 5),
  gtfs_get_route_id_by_short(gtfs, 54) %>% filter(agency_id == 5),
  gtfs_get_route_id_by_short(gtfs, 9) %>% filter(agency_id == 3, str_detect(route_long_name, "ירושלים")),
  gtfs_get_route_id_by_short(gtfs, 19) %>% filter(agency_id == 3, str_detect(route_long_name, "הר הצופים"), str_detect(route_long_name, "עין כרם")),
  gtfs_get_route_id_by_short(gtfs, 68) %>% filter(agency_id == 3, route_id < 11000)
)
lines <- map(routes$route_id, ~ gtfs %>%
  filter_by_route_id(.x) %>%
  `$`(shapes) %>%
  arrange(shape_pt_sequence) %>%
  group_by(shape_id) %>%
  nest() %>%
  mutate(line = st_sfc(map(data, ~ .x[, 2:1] %>%
    as.matrix() %>%
    st_linestring()), crs = 4326)) %>%
  st_sf() %>%
  select(-data) %>%
  st_transform(2039)) %>% bind_rows()


df1 <- bind_cols(routes, lines)
list_of_ress <- vector("list", 100)
j <- 1
for (x in df1$route_id) {
  for (i in 0:9) {
    print(x)
    print(i)
    ress <- read_json(paste0("https://open-bus-stride-api.hasadna.org.il/siri_rides/list?siri_route__line_refs=", x, "&offset=", i * 100, "&scheduled_start_time_from=2022-03-14T00:00:00.000Z&scheduled_start_time_to=2022-03-15T00:00:00.000Z")) # nolint
    if (length(ress) == 0) {
      break
    }
    list_of_ress[[j]] <- map_df(1:length(ress), ~ ress[[.x]]) %>% mutate(route_id = x)
    j <- j + 1
  }
}
# map_df(1:length(ress),~ress[[.x]]) %>% View()
rides <- bind_rows(list_of_ress)
list_of_ress <- vector("list", nrow(rides) * 2)
j <- 1
for (x in 1:nrow(rides)) {
  for (i in 0:9) {
    print(x)
    print(i)
    ress <- read_json(paste0("https://open-bus-stride-api.hasadna.org.il/siri_vehicle_locations/list?siri_rides__ids=", rides$id[x], "&siri_routes__ids=", rides$siri_route_id[x], "&offset=", i * 100))
    if (length(ress) == 0) {
      break
    }
    list_of_ress[[j]] <- map_df(1:length(ress), ~ ress[[.x]]) %>% mutate(route_id = rides$route_id[x])
    j <- j + 1
  }
}
locs <- bind_rows(list_of_ress)
locs %>%
  group_by(route_id) %>%
  nest()
# ress <- read_json(paste0("https://open-bus-stride-api.hasadna.org.il/siri_vehicle_locations/list?siri_rides__ids=12423&siri_routes__ids=1257"))
# map_df(1:length(ress),~ress[[.x]]) %>% st_as_sf(coords = c("lon","lat"),crs = 4326) %>% mapview::mapView()
# res <- map_df(df1$route_id,function(x){
#   map_df(0:50, function(y){
#     Sys.sleep(1)
#     print(x)
#     print(y)
#     res2 <- read_json(paste0("https://open-bus-stride-api.hasadna.org.il/siri_vehicle_locations/list?siri_routes__line_ref=",x,"&offset=",1+y*100,"&recorded_at_time_from=2022-03-14T00:00:00.000Z&recorded_at_time_to=2022-03-15T00:00:00.000Z"))
#     df2 <- map_df(1:length(res2),~res2[[.x]])
#     print(df2)
#     df2
#   })
# })
locs %>%
  left_join(df1 %>% mutate(route_id = as.integer(route_id)), by = c("siri_route__line_ref" = "route_id")) %>%
  mutate(recorded_at_time = as_datetime(recorded_at_time, tz = "Israel")) %>%
  ggplot(aes(x = recorded_at_time)) +
  geom_histogram() +
  facet_wrap(~ paste(route_short_name, route_long_name))
data <- locs %>%
  group_by(siri_route__line_ref) %>%
  nest() %>%
  mutate(siri_route__line_ref = as.character(siri_route__line_ref))
df2 <- df1 %>%
  left_join(data, by = c("route_id" = "siri_route__line_ref"))
stops <- map_df(df2$route_id, ~ gtfs %>%
  filter_by_route_id(.x) %>%
  `$`(stops) %>%
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) %>%
  st_transform(2039) %>%
  mutate(route_id = .x)) %>%
  group_by(route_id) %>%
  nest()
df3 <- df2 %>%
  left_join(stops, by = c("route_id"))
df4 <- df3 %>%
  as_tibble() %>%
  st_sf() %>%
  mutate(net = map2(line, data.x, function(x, y) {
    blnd <- y %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
      st_transform(2039)
    line <- x %>% st_sfc(crs = 2039)
    blnd50 <- st_intersection(blnd, st_buffer(line, 50))
    line %>%
      as_sfnetwork(directed = T) %>%
      st_network_blend(blnd50) %>%
      arrange(tidygraph::node_topo_order()) %>%
      {
        csts <- (.) %>%
          st_network_cost(from = 1) %>%
          t()
        (.) %>% mutate(csts = csts[, 1])
      } %>%
      as_tibble()
  }))
p <- df4 %>%
  select(net) %>%
  slice(1) %>%
  unnest(net) %>%
  mutate(
    siri_ride__scheduled_start_time = as_datetime(siri_ride__scheduled_start_time, tz = "Israel"),
    tim = as_hms(as_datetime(recorded_at_time, tz = "Israel")),
    tim = round_hms(tim, 60)
  ) %>%
  filter(!hour(as_datetime(recorded_at_time, tz = "Israel")) %in% c(0, 23)) %>%
  select(siri_ride__id, tim, siri_ride__scheduled_start_time, csts) %>%
  st_drop_geometry() %>%
  group_by(siri_ride__id) %>%
  arrange(tim) %>%
  mutate(tim1 = (tim - min(tim)) %>% as.numeric() %>% `/`(60)) %>%
  filter(!is.na(tim1)) %>%
  complete(nesting(siri_ride__id), tim1 = full_seq(tim1, 1)) %>%
  fill(tim) %>%
  fill(siri_ride__scheduled_start_time) %>%
  mutate(
    tim = as_hms(min(tim) + tim1 * 60),
    csts = na_interpolation(csts),
    bus = "./bus.png"
  ) %>%
  ggplot(aes(y = siri_ride__scheduled_start_time, x = csts, group = siri_ride__id)) +
  geom_image(aes(image = bus)) +
  ggtitle("{closest_state}") +
  ylab("זמן יציאה") +
  xlab("מרחק מהמוצא") +
  # annotate("text", x= stpsn$costs, label = stpsn$stop_name,y= mtim, angle = 90,size = 3)+
  gganimate::transition_states(tim)
gganimate::animate(p, fps = 60, duration = 40)
