#   Case Study: How Does a Bike-Share Navigate Speedy Success?

#   Install required R packages

install.packages("rmarkdown")
install.packages("tidyverse")
install.packages("mice")
install.packages("naniar")
install.packages("janitor")
install.packages("skimr")
install.packages("stringdist")
install.packages("lubridate")
install.packages("geosphere")
install.packages("fedmatch")
install.packages("leaflet")
install.packages("s2")

#     Update this section progressively

#   Load libraries

library(rmarkdown)
library(tidyverse)
library(mice)
library(naniar)
library(janitor)
library(skimr)
library(stringdist)
library(lubridate)
library(geosphere)
library(fedmatch)
library(leaflet)
library(s2)
library(scales)

#     Update this section progressively

#   Read valid station names

stations_v00 <- as_tibble(read_csv("~/Documents/Case_Study_2022_001/csv_files/Divvy_Bicycle_Stations_All.csv", show_col_types = FALSE))

#   Check structure

str(stations_v00)

#   Clean station names

stations_v01 <- stations_v00 %>% 
  select(ID:`Station Name`,
         Latitude:Longitude) %>% 
  clean_names() %>% 
  mutate(valid_id = as.character(id),
         station = clean_strings(string = stations_v00$`Station Name`)) %>% 
  select(valid_id:station,
         latitude:longitude) %>% 
  mutate(station = str_replace_all(station, coll(" "), "_"),
         .keep = "unused") %>% 
  rename(valid_longitude = longitude,
         valid_latitude = latitude) %>% 
  relocate(valid_id, .before = valid_latitude) %>% 
  mutate(valid_longitude = round(valid_longitude, 5),
         valid_latitude = round(valid_latitude, 5)) %>% 
  arrange(station)

#   Inspect distinct values per var and list findings - check R Markdown report

stations_v01 <- stations_v01 %>% filter(valid_id != "13") %>% 
  filter(station != "public_rack_laflin_st_and51st_st")

#   Check for NAs

any_na(stations_v01) 

#   No NAs - Hold stations data

#   Read Trips data

aug_22 <- read_csv("~/Documents/Case_Study_2022_001/csv_files/202208-divvy-tripdata.csv", show_col_types = FALSE)  
jul_22 <- read_csv("~/Documents/Case_Study_2022_001/csv_files/202207-divvy-tripdata.csv", show_col_types = FALSE)  
jun_22 <- read_csv("~/Documents/Case_Study_2022_001/csv_files/202206-divvy-tripdata.csv", show_col_types = FALSE)  
may_22 <- read_csv("~/Documents/Case_Study_2022_001/csv_files/202205-divvy-tripdata.csv", show_col_types = FALSE)  
apr_22 <- read_csv("~/Documents/Case_Study_2022_001/csv_files/202204-divvy-tripdata.csv", show_col_types = FALSE)  
mar_22 <- read_csv("~/Documents/Case_Study_2022_001/csv_files/202203-divvy-tripdata.csv", show_col_types = FALSE)  
feb_22 <- read_csv("~/Documents/Case_Study_2022_001/csv_files/202202-divvy-tripdata.csv", show_col_types = FALSE)  
jan_22 <- read_csv("~/Documents/Case_Study_2022_001/csv_files/202201-divvy-tripdata.csv", show_col_types = FALSE)  
dec_21 <- read_csv("~/Documents/Case_Study_2022_001/csv_files/202112-divvy-tripdata.csv", show_col_types = FALSE)  
nov_21 <- read_csv("~/Documents/Case_Study_2022_001/csv_files/202111-divvy-tripdata.csv", show_col_types = FALSE)  
oct_21 <- read_csv("~/Documents/Case_Study_2022_001/csv_files/202110-divvy-tripdata.csv", show_col_types = FALSE)  
sep_21 <- read_csv("~/Documents/Case_Study_2022_001/csv_files/202109-divvy-tripdata.csv", show_col_types = FALSE)  

#   Check structure and compatibility

#View(compare_df_cols(aug_22, jul_22, jun_22, may_22, apr_22, mar_22, feb_22, jan_22, dec_21, nov_21, oct_21, sep_21))

#   Combine rows

last_12m_v00 <- bind_rows(aug_22, jul_22, jun_22, may_22, apr_22, mar_22, feb_22, jan_22, dec_21, nov_21, oct_21, sep_21)

#   Clear environment and free unused memory

rm(aug_22, jul_22, jun_22, may_22, apr_22, mar_22, feb_22, jan_22, dec_21, nov_21, oct_21, sep_21)
gc()

#   Apply similar cleaning steps as applied to station names

last_12m_v01 <- last_12m_v00 %>% 
  mutate(rideable_type = clean_strings(rideable_type),
         start_station_name = clean_strings(start_station_name),
         start_station_id = clean_strings(start_station_id),
         end_station_name = clean_strings(end_station_name),
         end_station_id = clean_strings(end_station_id),
         member_casual = clean_strings(member_casual),
         .keep = "unused") %>% 
  mutate(rideable_type = str_replace_all(rideable_type, coll(" "), "_"),
         start_station_name = str_replace_all(start_station_name, coll(" "), "_"),
         start_station_id = str_replace_all(start_station_id, coll(" "), "_"),
         end_station_name = str_replace_all(end_station_name, coll(" "), "_"),
         end_station_id = str_replace_all(end_station_id, coll(" "), "_")) %>% 
  select(everything()) %>%   
  rename(user_type = member_casual,
         bike_type = rideable_type,
         start_station = start_station_name,
         start_id = start_station_id,
         end_station = end_station_name,
         end_id = end_station_id,
         started = started_at,
         ended = ended_at) %>% 
  relocate(user_type, .before = bike_type) %>% 
  relocate(started:ended, .after = end_lng) %>% 
  relocate(end_id, .after = end_station) %>% 
  mutate(start_lat = round(start_lat, 5),
         start_lng = round(start_lng, 5),
         end_lat = round(end_lat, 5),
         end_lng = round(end_lng, 5),
         .keep = "unused") %>% 
  arrange(start_station)

#   Inspect distinct values per var and list findings

#   Clear environment and free up unused memory

rm(last_12m_v00)
gc()

#   Create separate tibbles to list unique station names from non-NA data

start_st_v00 <- last_12m_v01 %>% select (start_station:start_id, start_lat:start_lng) %>% 
  distinct(start_station, .keep_all = TRUE) %>% na.omit() %>% 
  arrange(start_station)

end___st_v00 <- last_12m_v01 %>% select (end_station:end_id, end_lat:end_lng) %>% 
  distinct(end_station, .keep_all = TRUE) %>% na.omit() %>% 
  arrange(end_station)

#   Combine both into a table of all station names 

all_stat_v00 <- bind_rows(start_st_v00, end___st_v00, .id = NULL) %>% 
  select(start_station:start_lng) %>% 
  rename(station = start_station,
         latitude = start_lat,
         longitude = start_lng, 
         id = start_id) %>% na.omit() %>% 
  distinct(station, .keep_all = TRUE)

#   Check for duplicates

dupe_sta_v01 <- all_stat_v00 %>% get_dupes(station) 

#     No duplicates

#   Inspect all stations and list findings - check R Markdown report

#View(all_stat_v00)

#   First iteration of left join all_stat_v00 and stations_v02 by station 

all_stat_v01 <- left_join(all_stat_v00, stations_v02, by = "station", keep = TRUE)

#   Compare

#   Rearrange columns for comparison

all_stat_v02 <- all_stat_v01 %>% rename(station = station.x,
                                        valid_station = station.y) %>% 
  relocate(valid_station, .before = id) %>% 
  relocate(valid_id, .after = id) %>% 
  relocate(valid_latitude, .after = latitude) %>% 
  relocate(valid_longitude, .after = longitude) 

#   Compare station names

#     TRUE if names match

all_stat_v02$validated_station <- all_stat_v02$station %in% all_stat_v02$valid_station

all_stat_v03 <- all_stat_v02 %>% relocate(validated_station, .before = id) %>% arrange(validated_station) 

table(str_count(all_stat_v03$validated_station, "FALSE")) 

#     FALSE, 399 names did not match

#   Separate into groups: Valid and Invalid

#   Validated station names

validsta_v00 <- all_stat_v03 %>% select(station,
                                        validated_station,
                                        id,
                                        latitude,
                                        longitude) %>% 
  filter(validated_station == "TRUE") 

#   Not exact matches or invalid

invalsta_v00 <- all_stat_v03 %>% select(station,
                                        validated_station,
                                        id,
                                        latitude,
                                        longitude)  %>% 
  filter(validated_station == "FALSE") 

#     399 cases were not exact matches
#     Some station names in data have close matches with valid station names
#     Use fuzzy string matching and the Levenshtein Algorithm 

wrongsta_v00 <- as_vector(invalsta_v00$station)
validsta_v01 <- stations_v02$station
amatch(wrongsta_v00, validsta_v01, maxDist = 10)

#     Returned the position in the "valid_names" vector that best matches the seemingly wrong name 
#     Visualize close matches

closemtc_v00 <- stations_v02[amatch(wrongsta_v00, validsta_v01, maxDist = 10),]
closemtc_v00$wrong_name <- wrongsta_v00
closemtc_v00 <- closemtc_v00 %>% relocate(wrong_name)

#     Manual checks led to the conclusion that further cleaning of station names in both tables 
#     may increase the number of exact matches
#     Some names with id having 18 or 19 digit chars end with "public_rack_" or "pubic_rack" 
#     Some wrong station names in all stations could match if prefixes and suffixes were removed
#     Noticed the same applies to station names ending in _temp, _charging, _warehouse
#     Remove these from names

#   Remove prefixes and suffixes in station names from both tables

stations_v03 <- stations_v02 %>% 
  mutate(station = str_remove_all(station, coll("public_rack_"))) %>% 
  mutate(station = str_remove_all(station, coll("pubic_rack_"))) %>% 
  mutate(station = str_remove_all(station, coll("_temp"))) %>% 
  mutate(station = str_remove_all(station, coll("_charging"))) %>% 
  mutate(station = str_remove_all(station, coll("_warehouse")))

all_stat_v04 <- all_stat_v03 %>% select(station,
                                        id,
                                        latitude,
                                        longitude) %>% 
  mutate(station = str_remove_all(station, coll("public_rack_"))) %>% 
  mutate(station = str_remove_all(station, coll("pubic_rack_"))) %>% 
  mutate(station = str_remove_all(station, coll("_temp"))) %>% 
  mutate(station = str_remove_all(station, coll("_charging"))) %>% 
  mutate(station = str_remove_all(station, coll("_warehouse")))

#   Check for duplicates

dupe_sta_v02 <- stations_v03 %>% get_dupes(station)

#     52 entries, 26 stations with same name but different ids

#   To avoid duplication issues, use distinct names

stations_v04 <- stations_v03 %>% distinct(station, .keep_all = TRUE)

#   Check whether changes improved the number of exact matches

#   Iteration 2 of left join by station name

all_stat_v05 <- left_join(all_stat_v04, stations_v04, 
                              by = "station", 
                              keep = TRUE)

all_stat_v05 %>% filter(is.na(station.y))

#     Only 34 not found. Improved from 399

#   Compare whether station names match

#   Rearrange columns, name to name, id to id, lon-lat to lon-lat

all_stat_v06 <- all_stat_v05 %>% rename(station = station.x, valid_station = station.y) %>% 
  relocate(valid_station, .after = station) %>% 
  relocate(valid_id, .after = id) %>% 
  relocate(valid_latitude, .after = latitude) 

#   Compare variables with the %in% operator

#     TRUE if values from all_stat match valid names

all_stat_v06$validated_station <- all_stat_v06$station %in% all_stat_v06$valid_station
all_stat_v07 <- all_stat_v06 %>% relocate(validated_station, .before = id)
  
#   Check results

table(str_count(all_stat_v07$validated_station, "FALSE")) 

#     Only 34 names do not match (Aha moment!)

#   Filter all_stations by validated_station = "FALSE"

#   Sort alphabetically by station

invalsta_v01 <- all_stat_v07 %>% select(station, validated_station, id, latitude, longitude) %>% 
  filter(validated_station == "FALSE")

#     Number of exact matches increased considerably
#     Try Levenshtein again
#     Compare invalid names to valid station names again

#   Manual checks 

#     Found stations with typos that could be repaired
#     Stations that could be classified as "cyclistic_service" (charging, warehouse, mobile repair)
#     Searched online for stations and found out some were "out_of_service"

#   Apply corrections

all_stat_v08 <- all_stat_v07 %>%  
#     typos
  mutate(station = str_replace_all(station, coll("63rd_and_western_ave_north_corner"), "63rd_and_western_ave_n")) %>% 
  mutate(station = str_replace_all(station, coll("63rd_and_western_ave_south_corner"), "63rd_and_western_ave_s")) %>% 
  mutate(station = str_replace_all(station, coll("ashland_ave_and_45th_st_midblock_south"), "ashland_ave_and_45th_st_s")) %>% 
  mutate(station = str_replace_all(station, coll("austin_ave_and_wellington_ave_midblock"), "austin_ave_and_wellington_ave")) %>% 
  mutate(station = str_replace_all(station, coll("ewing_ave_and_96th_st"), "ewing_ave_and_96th_st_n")) %>% 
  mutate(station = str_replace_all(station, coll("kedvale_ave_and_63rd_st"), "kedvale_ave_and_63rd_st_e")) %>% 
  mutate(station = str_replace_all(station, coll("kedzie_and_103rd_st_west"), "kedzie_and_103rd_st_w")) %>% 
  mutate(station = str_replace_all(station, coll("kedzie_ave_and_61st_pl"), "kedzie_ave_and_61st_pl_e")) %>% 
  mutate(station = str_replace_all(station, coll("kedzie_ave_andamp_62nd_pl"), "kedzie_ave_and_62nd_pl")) %>% 
  mutate(station = str_replace_all(station, coll("keeler_ave_and_madison_st"), "keeler_ave_and_madison_st_s")) %>% 
  mutate(station = str_replace_all(station, coll("kenneth_ave_and_63rd_st"), "kenneth_ave_and_63rd_st_e")) %>% 
  mutate(station = str_replace_all(station, coll("mt_greenwood_library_north"), "mt_greenwood_library_n")) %>% 
  mutate(station = str_replace_all(station, coll("n_shore_channel_trail_and_argyle_ave"), "n_shore_channel_trail_and_argyle_st")) %>% 
  mutate(station = str_replace_all(station, coll("prairie_ave_and_47th_st_midblock"), "prairie_ave_and_47th_st")) %>% 
  mutate(station = str_replace_all(station, coll("talman_ave_and_51st_st_midblock"), "talman_ave_and_51st_st")) %>% 
  mutate(station = str_replace_all(station, coll("whippie_st_and_26th_st"), "whipple_st_and_26th_st")) %>% 
  mutate(station = str_replace_all(station, coll("woodlawn_ave_and_63rd_st_ne"), "woodlawn_ave_and_63rd_st_n")) %>% 
  mutate(station = str_replace_all(station, coll("woodlawn_ave_and_63rd_st_se"), "woodlawn_ave_and_63rd_st_s")) %>% 
#     cyclistic service
  mutate(station = str_replace_all(station, coll("base_2132_w_hubbard"), "cyclistic_service")) %>% 
  mutate(station = str_replace_all(station, coll("base_2132_w_hubbard_warehouse"), "cyclistic_service")) %>% 
  mutate(station = str_replace_all(station, coll("divvy_cassette_repair_mobile_station"), "cyclistic_service")) %>% 
  mutate(station = str_replace_all(station, coll("divvy_valet_oakwood_beach"), "cyclistic_service")) %>% 
  mutate(station = str_replace_all(station, coll("hastings_wh_2"), "cyclistic_service")) %>% 
  mutate(station = str_replace_all(station, coll("pawel_bialowas_test_pbsc_station"), "cyclistic_service")) %>% 
  mutate(station = str_replace_all(station, coll("throop_hastings_mobile_station"), "cyclistic_service")) %>% 
  mutate(station = str_replace_all(station, coll("west_chi_watson"), "cyclistic_service")) %>% 
  mutate(station = str_replace_all(station, coll("westchi"), "cyclistic_service")) %>% 
#     out of service
  mutate(station = str_replace_all(station, coll("dusable_lake_shore_dr_and_ohio_st"), "out_of_service")) %>% 
  mutate(station = str_replace_all(station, coll("lamon_ave_and_archer_ave"), "out_of_service")) %>% 
  mutate(station = str_replace_all(station, coll("morgan_st_and_lake_st"), "out_of_service")) %>% 
  mutate(station = str_replace_all(station, coll("newhastings"), "out_of_service")) %>% 
  mutate(station = str_replace_all(station, coll("whipple_st_and_irving_park_rd"), "out_of_service"))

#   Use names in all_stat_v08 to repair other variables: ids, longitudes, and latitudes

all_stat_v09 <- all_stat_v08 %>% select(station, latitude, longitude, id)
all_stat_v10 <- left_join(all_stat_v09, stations_v04, by = "station", keep = TRUE)
all_stat_v10$validated_name <- all_stat_v10$station.x %in% all_stat_v10$station.y
table(str_count(all_stat_v10$validated_name, "FALSE")) 

#     14 names do not match

outliers_v00 <- all_stat_v10 %>% filter(validated_name == FALSE) %>% select(station.x:id) %>% 
  rename(station = station.x,
         valid_latitude = latitude,
         valid_longitude = longitude,
         valid_id = id) %>% 
  mutate(valid_id = station) %>%  
  relocate(valid_id, .after = station)

#   Add outliers to stations_v04 

stations_v05 <- rbind(stations_v04, outliers_v00)

#   Use stations_v05 to clean last_12m_v01

#   Use the corresponding valid id, and valid lon-lat data obtained from the city of chicago website

#   Repair start station names in last_12m_v01

last_12m_v02 <- last_12m_v01 %>% rename(
  station = start_station) %>%  
  mutate(station = str_remove_all(station, coll("public_rack_"))) %>% 
  mutate(station = str_remove_all(station, coll("pubic_rack_"))) %>% 
  mutate(station = str_remove_all(station, coll("_temp"))) %>% 
  mutate(station = str_remove_all(station, coll("_charging"))) %>% 
  mutate(station = str_remove_all(station, coll("_warehouse"))) %>% 
#   Typos
  mutate(station = str_replace_all(station, coll("63rd_and_western_ave_north_corner"), "63rd_and_western_ave_n")) %>% 
  mutate(station = str_replace_all(station, coll("63rd_and_western_ave_south_corner"), "63rd_and_western_ave_s")) %>% 
  mutate(station = str_replace_all(station, coll("ashland_ave_and_45th_st_midblock_south"), "ashland_ave_and_45th_st_s")) %>% 
  mutate(station = str_replace_all(station, coll("austin_ave_and_wellington_ave_midblock"), "austin_ave_and_wellington_ave")) %>% 
  mutate(station = str_replace_all(station, coll("ewing_ave_and_96th_st"), "ewing_ave_and_96th_st_n")) %>% 
  mutate(station = str_replace_all(station, coll("kedvale_ave_and_63rd_st"), "kedvale_ave_and_63rd_st_e")) %>% 
  mutate(station = str_replace_all(station, coll("kedzie_and_103rd_st_west"), "kedzie_and_103rd_st_w")) %>% 
  mutate(station = str_replace_all(station, coll("kedzie_ave_and_61st_pl"), "kedzie_ave_and_61st_pl_e")) %>% 
  mutate(station = str_replace_all(station, coll("kedzie_ave_andamp_62nd_pl"), "kedzie_ave_and_62nd_pl")) %>% 
  mutate(station = str_replace_all(station, coll("keeler_ave_and_madison_st"), "keeler_ave_and_madison_st_s")) %>% 
  mutate(station = str_replace_all(station, coll("kenneth_ave_and_63rd_st"), "kenneth_ave_and_63rd_st_e")) %>% 
  mutate(station = str_replace_all(station, coll("mt_greenwood_library_north"), "mt_greenwood_library_n")) %>% 
  mutate(station = str_replace_all(station, coll("n_shore_channel_trail_and_argyle_ave"), "n_shore_channel_trail_and_argyle_st")) %>% 
  mutate(station = str_replace_all(station, coll("prairie_ave_and_47th_st_midblock"), "prairie_ave_and_47th_st")) %>% 
  mutate(station = str_replace_all(station, coll("talman_ave_and_51st_st_midblock"), "talman_ave_and_51st_st")) %>% 
  mutate(station = str_replace_all(station, coll("whippie_st_and_26th_st"), "whipple_st_and_26th_st")) %>% 
  mutate(station = str_replace_all(station, coll("woodlawn_ave_and_63rd_st_ne"), "woodlawn_ave_and_63rd_st_n")) %>% 
  mutate(station = str_replace_all(station, coll("woodlawn_ave_and_63rd_st_se"), "woodlawn_ave_and_63rd_st_s")) %>% 
#   Cyclistic Service
  mutate(station = str_replace_all(station, coll("base_2132_w_hubbard"), "cyclistic_service")) %>% 
  mutate(station = str_replace_all(station, coll("base_2132_w_hubbard_warehouse"), "cyclistic_service")) %>% 
  mutate(station = str_replace_all(station, coll("divvy_cassette_repair_mobile_station"), "cyclistic_service")) %>% 
  mutate(station = str_replace_all(station, coll("divvy_valet_oakwood_beach"), "cyclistic_service")) %>% 
  mutate(station = str_replace_all(station, coll("hastings_wh_2"), "cyclistic_service")) %>% 
  mutate(station = str_replace_all(station, coll("pawel_bialowas_test_pbsc_station"), "cyclistic_service")) %>% 
  mutate(station = str_replace_all(station, coll("throop_hastings_mobile_station"), "cyclistic_service")) %>% 
  mutate(station = str_replace_all(station, coll("west_chi_watson"), "cyclistic_service")) %>% 
  mutate(station = str_replace_all(station, coll("westchi"), "cyclistic_service")) %>% 
#   Out of Service
  mutate(station = str_replace_all(station, coll("dusable_lake_shore_dr_and_ohio_st"), "out_of_service")) %>% 
  mutate(station = str_replace_all(station, coll("lamon_ave_and_archer_ave"), "out_of_service")) %>% 
  mutate(station = str_replace_all(station, coll("morgan_st_and_lake_st"), "out_of_service")) %>% 
  mutate(station = str_replace_all(station, coll("newhastings"), "out_of_service")) %>% 
  mutate(station = str_replace_all(station, coll("whipple_st_and_irving_park_rd"), "out_of_service"))

#   Clear environment and free unused memory

rm(last_12m_v01)
gc()

#   Repair end station names in last_12m_v02

last_12m_v03 <- last_12m_v02 %>% rename(
  start_station = station) %>% rename(
    station = end_station) %>%  
  mutate(station = str_remove_all(station, coll("public_rack_"))) %>% 
  mutate(station = str_remove_all(station, coll("pubic_rack_"))) %>% 
  mutate(station = str_remove_all(station, coll("_temp"))) %>% 
  mutate(station = str_remove_all(station, coll("_charging"))) %>% 
  mutate(station = str_remove_all(station, coll("_warehouse"))) %>% 
#   Typos
  mutate(station = str_replace_all(station, coll("63rd_and_western_ave_north_corner"), "63rd_and_western_ave_n")) %>% 
  mutate(station = str_replace_all(station, coll("63rd_and_western_ave_south_corner"), "63rd_and_western_ave_s")) %>% 
  mutate(station = str_replace_all(station, coll("ashland_ave_and_45th_st_midblock_south"), "ashland_ave_and_45th_st_s")) %>% 
  mutate(station = str_replace_all(station, coll("austin_ave_and_wellington_ave_midblock"), "austin_ave_and_wellington_ave")) %>% 
  mutate(station = str_replace_all(station, coll("ewing_ave_and_96th_st"), "ewing_ave_and_96th_st_n")) %>% 
  mutate(station = str_replace_all(station, coll("kedvale_ave_and_63rd_st"), "kedvale_ave_and_63rd_st_e")) %>% 
  mutate(station = str_replace_all(station, coll("kedzie_and_103rd_st_west"), "kedzie_and_103rd_st_w")) %>% 
  mutate(station = str_replace_all(station, coll("kedzie_ave_and_61st_pl"), "kedzie_ave_and_61st_pl_e")) %>% 
  mutate(station = str_replace_all(station, coll("kedzie_ave_andamp_62nd_pl"), "kedzie_ave_and_62nd_pl")) %>% 
  mutate(station = str_replace_all(station, coll("keeler_ave_and_madison_st"), "keeler_ave_and_madison_st_s")) %>% 
  mutate(station = str_replace_all(station, coll("kenneth_ave_and_63rd_st"), "kenneth_ave_and_63rd_st_e")) %>% 
  mutate(station = str_replace_all(station, coll("mt_greenwood_library_north"), "mt_greenwood_library_n")) %>% 
  mutate(station = str_replace_all(station, coll("n_shore_channel_trail_and_argyle_ave"), "n_shore_channel_trail_and_argyle_st")) %>% 
  mutate(station = str_replace_all(station, coll("prairie_ave_and_47th_st_midblock"), "prairie_ave_and_47th_st")) %>% 
  mutate(station = str_replace_all(station, coll("talman_ave_and_51st_st_midblock"), "talman_ave_and_51st_st")) %>% 
  mutate(station = str_replace_all(station, coll("whippie_st_and_26th_st"), "whipple_st_and_26th_st")) %>% 
  mutate(station = str_replace_all(station, coll("woodlawn_ave_and_63rd_st_ne"), "woodlawn_ave_and_63rd_st_n")) %>% 
  mutate(station = str_replace_all(station, coll("woodlawn_ave_and_63rd_st_se"), "woodlawn_ave_and_63rd_st_s")) %>% 
#   Cyclistic service
  mutate(station = str_replace_all(station, coll("base_2132_w_hubbard"), "cyclistic_service")) %>% 
  mutate(station = str_replace_all(station, coll("base_2132_w_hubbard_warehouse"), "cyclistic_service")) %>% 
  mutate(station = str_replace_all(station, coll("divvy_cassette_repair_mobile_station"), "cyclistic_service")) %>% 
  mutate(station = str_replace_all(station, coll("divvy_valet_oakwood_beach"), "cyclistic_service")) %>% 
  mutate(station = str_replace_all(station, coll("hastings_wh_2"), "cyclistic_service")) %>% 
  mutate(station = str_replace_all(station, coll("pawel_bialowas_test_pbsc_station"), "cyclistic_service")) %>% 
  mutate(station = str_replace_all(station, coll("throop_hastings_mobile_station"), "cyclistic_service")) %>% 
  mutate(station = str_replace_all(station, coll("west_chi_watson"), "cyclistic_service")) %>% 
  mutate(station = str_replace_all(station, coll("westchi"), "cyclistic_service")) %>% 
#   Out of service
  mutate(station = str_replace_all(station, coll("dusable_lake_shore_dr_and_ohio_st"), "out_of_service")) %>% 
  mutate(station = str_replace_all(station, coll("lamon_ave_and_archer_ave"), "out_of_service")) %>% 
  mutate(station = str_replace_all(station, coll("morgan_st_and_lake_st"), "out_of_service")) %>% 
  mutate(station = str_replace_all(station, coll("newhastings"), "out_of_service")) %>% 
  mutate(station = str_replace_all(station, coll("whipple_st_and_irving_park_rd"), "out_of_service")) %>% 
  rename(end_station = station) 

#   Clear environment and free unused memory

rm(last_12m_v02)
gc()

#   Analyse Missing values

#     Are there missing values?
any_na(last_12m_v03) 
#       TRUE
#     How many?
n_miss(last_12m_v03)
#       3,672,788
#     Percent of missing data? 
prop_miss(last_12m_v03)
#       4.8%
#     Where are the NAs concentrated?
miss_var_summary(last_12m_v03)
#       start_station, start_id end_station, end_id, end_lng, end_lat
#     Check distribution of missing data with an UpSet plot
gg_miss_upset(last_12m_v03, 
              nsets = 6, 
              nintersects = 6,
              matrix.color = "#619CFF",
              main.bar.color = "#619CFF",
              sets.bar.color = "#F8766D")

#       missing values found in the following variables:
#         end_station
#         end_id
#         start_station
#         start_id
#         end_lat
#         end_lng

#       Upset plot shows six (6) different intersections of missing values
na_cases_v00 <- last_12m_v03 %>% select(everything()) %>% filter(!complete.cases(.))
#         Case 1: start_id, start_station, end_station, end_id
na_case1_v00 <- na_cases_v00 %>% filter(is.na(start_id)) %>% filter(is.na(end_id))
#         Case 2: end_station, end_id
na_case2_v00 <- na_cases_v00 %>% filter(!is.na(start_station)) %>% filter(!is.na(end_lng))
#         Case 3: start_id, start_station
na_case3_v00 <- na_cases_v00 %>% filter(is.na(start_id)) %>% filter(!is.na(end_id))
#         Case 4: end_station, end_id, end_lng, end_lat
na_case4_v00 <- na_cases_v00 %>% filter(is.na(end_lng))
#         Case 5: start_station
na_case5_v00 <- na_cases_v00 %>% filter(is.na(start_station)) %>% filter(!is.na(start_id)) %>% 
  filter(!is.na(end_station))
#         Case 6: start_station, end_station, end_id,
na_case6_v00 <- na_cases_v00 %>% filter(is.na(start_station)) %>% filter(!is.na(start_id)) %>% 
  filter(is.na(end_station))

#     Could the missing values be identified using other variables, other observations? Missing At Random (MAR)

#     Case 1: Probably, from location data and valid station names
#     Case 2: Probably, from location data and valid station names
#     Case 3: Probably, from location data and valid station names
#     Case 4: No, because end_lng is missing
#     Case 5: Probably, from location data and valid station names
#     Case 6: Probably, from location data and valid station names 

#     Filter out cyclistic service, out of service, and unidentified Linder Ave & Archer Ave

last_12m_v04 <- last_12m_v03 %>% filter(!start_station == "cyclistic_service") %>% 
  filter(!end_station == "cyclistic_service") %>% filter(!start_station == "out_of_service") %>% 
  filter(!end_station == "out_of_service") %>% filter(!start_station == "linder_ave_and_archer_ave") %>% 
  filter(!end_station == "linder_ave_and_archer_ave")

#     last_12m_v04 is a partially clean dataset of complete cases

#     Filter complete cases

complete_v00 <- last_12m_v03 %>% select(ride_id, start_station:end_lng) %>% filter(complete.cases(.))

#     Filter distinct location data
#     Sort by start_lng

complete_v01 <- complete_v00 %>% distinct(start_lat, start_lng, .keep_all = TRUE) %>% arrange(start_lng)

#     Unique pairs of location data for start (s) and end(e) stations

unq_sloc_v00 <- complete_v00 %>% select(ride_id, start_station, start_id, start_lat, start_lng) %>% 
  distinct(start_lat, start_lng, .keep_all = TRUE)

unq_eloc_v00 <- complete_v00 %>% select(ride_id, end_station, end_id, end_lat, end_lng) %>% 
  distinct(end_lat, end_lng, .keep_all = TRUE) %>% 
  rename(start_station = end_station,
         start_id = end_id,
         start_lng = end_lng,
         start_lat = end_lat)

#     Combine to one table

unq_rbnd_v00 <- rbind(unq_sloc_v00, unq_eloc_v00) %>% 
  rename(station = start_station,
         id = start_id,
         lng = start_lng,
         lat = start_lat) %>% arrange(lat, lng)

#     Group Cases 1:3, 5:6 and distinct them by location data

ca_12356_v00 <- bind_rows(na_case1_v00, na_case2_v00, na_case3_v00, na_case5_v00, na_case6_v00) %>% 
  select(ride_id:bike_type, start_lat:ended) %>% 
  rename(lat = start_lat, lng = start_lng) %>% distinct(lat, lng, .keep_all = TRUE)

ca_12356_v01 <- left_join(ca_12356_v00, unq_rbnd_v00, by = c("lat", "lng"))

#     has NAs

ca_12356_v02 <- ca_12356_v01 %>% select(ride_id.x:ended, station:id) %>% 
  rename(ride_id = ride_id.x,
         start_lat = lat,
         start_lng = lng,
         start_station = station,
         start_id = id) %>% 
  distinct(ride_id, .keep_all = TRUE)

ca_12356_v03 <- ca_12356_v02 %>% rename(lat = end_lat, lng = end_lng)

ca_12356_v04 <- left_join(ca_12356_v03, unq_rbnd_v00, by = c("lat", "lng"))

ca_12356_v05 <- ca_12356_v04 %>% select(ride_id.x:start_id, station:id) %>% 
  rename(ride_id = ride_id.x,
         end_lng = lng,
         end_lat = lat,
         end_station = station,
         end_id = id) %>% distinct(ride_id, .keep_all = TRUE) %>% 
  relocate(start_station:end_id, .before = start_lat)

#     Continue analysis of missing data

last_12m_v05 <- bind_rows(last_12m_v04, ca_12356_v05) 

unrpaird_v00 <- anti_join(last_12m_v03, last_12m_v05, by = "ride_id")

gg_miss_upset(unrpaird_v00, 
              nsets = 6, 
              nintersects = 6,
              matrix.color = "#619CFF",
              main.bar.color = "#619CFF",
              sets.bar.color = "#F8766D")

#     Cases without end_lng left out (these seem to be stolen/damaged units)

unrpaird_v01 <- unrpaird_v00  %>% filter(!is.na(end_lng))

unrpaird_v02 <- unrpaird_v01 %>% select(ride_id:bike_type, start_lat:ended) %>% 
  rename(lat = start_lat, lng = start_lng)

stations_v06 <- stations_v05 %>% rename(lat = valid_latitude,
                                        lng = valid_longitude)

unrpaird_v03 <- left_join(unrpaird_v02, stations_v06, by = c("lat", "lng"))

unrpaird_v04 <- unrpaird_v03 %>% select(everything()) %>% rename(start_lat = lat,
                                                                 start_lng = lng,
                                                                 start_station = station,
                                                                 start_id = valid_id)

unrpaird_v05 <- unrpaird_v04 %>% rename(lat = end_lat, lng = end_lng)

unrpaird_v06 <- left_join(unrpaird_v05, stations_v06, by = c("lat", "lng"))

unrpaird_v07 <- unrpaird_v06 %>% select(everything()) %>% rename(end_lat = lat,
                                                                 end_lng = lng, 
                                                                 end_station = station,
                                                                 end_id = valid_id) %>% 
  relocate(start_station:end_id, .before = start_lat) %>% na.omit()

last_12m_v06 <- bind_rows(last_12m_v05, unrpaird_v07)

unrpaird_v08 <- anti_join(last_12m_v03, last_12m_v06, by="ride_id")

unrpaird_v09 <- unrpaird_v08 %>% filter(!is.na(end_lng))

#     Use the S2 package to identify a close match by location

unrpaird_v10 <- unrpaird_v09 %>% select(ride_id:bike_type, start_lat:start_lng) %>% arrange(start_lat)

stations_v07 <- stations_v06 %>% arrange(lat)

stations_v07$closest <- 1:nrow(stations_v07)

set1_s2_v00 <- s2_lnglat(unrpaird_v10$start_lng, unrpaird_v10$start_lat)

set2_s2_v00 <- s2_lnglat(stations_v07$lng, stations_v07$lat)

unrpaird_v10$closest <- s2_closest_feature(set1_s2_v00, set2_s2_v00)

unrpaird_v11 <- left_join(unrpaird_v10, stations_v07, by = "closest")

unrpaird_v12 <- unrpaird_v11 %>% select(ride_id:bike_type, station:lng) %>% 
  rename(start_station = station,
         start_id = valid_id,
         start_lat = lat,
         start_lng = lng)

unrpaird_v13 <- unrpaird_v09 %>% select(ride_id, end_lat:ended) %>% arrange(end_lat)

set1_s2_v01 <- s2_lnglat(unrpaird_v13$end_lng, unrpaird_v13$end_lat)

unrpaird_v13$closest <- s2_closest_feature(set1_s2_v01, set2_s2_v00)

unrpaird_v14 <- left_join(unrpaird_v13, stations_v07, by = "closest")

unrpaird_v15 <- unrpaird_v14 %>% select(ride_id, started:ended, station:lng) %>% 
  rename(end_station = station,
         end_id = valid_id,
         end_lat = lat,
         end_lng = lng)

unrpaird_v16 <- left_join(unrpaird_v12, unrpaird_v15, by = "ride_id")

unrpaird_v17 <- unrpaird_v16 %>% filter(!start_station == "cyclistic_service") %>% 
  filter(!end_station == "cyclistic_service") %>% filter(!start_station == "out_of_service") %>% 
  filter(!end_station == "out_of_service") %>% filter(!start_station == "linder_ave_and_archer_ave") %>% 
  filter(!end_station == "linder_ave_and_archer_ave")

unrpaird_v18 <- unrpaird_v17 %>% relocate(end_station:end_lng, .before = start_lat) %>%  
  relocate(start_lat:start_lng, .before = end_lat)

last_12m_v07 <- bind_rows(last_12m_v06, unrpaird_v18)

#     last_12m_v07 is clean without missing values

#   Data Manipulation

last_12m_v08 <- last_12m_v07 %>% 
  mutate(start_date = date(last_12m_v07$started),
    start_month = month(last_12m_v07$started, label = FALSE),
    start_day = wday(last_12m_v07$started, label = FALSE),
    season = case_when(
      start_month %in% 1:2 ~ "Winter",
      start_month %in% 3:5 ~ "Spring",
      start_month %in% 6:8 ~ "Summer",
      start_month %in% 9:11 ~ "Fall",
      start_month %in% 12 ~ "Winter",
      ),
    month = case_when(
      start_month %in% 1 ~ "Jan",
      start_month %in% 2 ~ "Feb",
      start_month %in% 3 ~ "Mar",
      start_month %in% 4 ~ "Apr",
      start_month %in% 5 ~ "May",
      start_month %in% 6 ~ "Jun",
      start_month %in% 7 ~ "Jul",
      start_month %in% 8 ~ "Aug",
      start_month %in% 9 ~ "Sep",
      start_month %in% 10 ~ "Oct",
      start_month %in% 11 ~ "Nov",
      start_month %in% 12 ~ "Dec",
    ),
    weekday = case_when(
      start_day %in% 1 ~ "Sun",
      start_day %in% 2 ~ "Mon",
      start_day %in% 3 ~ "Tue",
      start_day %in% 4 ~ "Wed",
      start_day %in% 5 ~ "Thu",
      start_day %in% 6 ~ "Fri",
      start_day %in% 7 ~ "Sat"
      ),
    duration = difftime(last_12m_v07$ended, last_12m_v07$started, units = "mins"),
    usage = case_when(
      duration < 1 ~ "invalid",
      duration >= 1 &
      duration <= 1440 ~ "single_day",
      duration > 1440 ~ "multiday"
      )
  )

#   Check date-time manipulation

table(str_count(last_12m_v08$usage))

#       110,435 invalid records, 
#       5,742,806 records as single_day
#       445 multiday, 

#   Continue reshaping the table

#   Group by user_type and bike_type
#   Select variables relevant to the analysis
#   Ignore station IDs and vars used for calculations
#   Filter by rideable_type: only classic and electric
#   Filter by usage: single_day
#   Remove unused tibbles and clear environment

last_12m_v09 <- last_12m_v08 %>% 
  group_by(user_type, bike_type) %>% select(user_type:start_station, end_station, start_lat:end_lng, season:usage) %>% 
  filter(., grepl('single_day', usage)) %>% na.omit() %>% arrange(season, month, weekday)

last_12m_v10 <- last_12m_v09 %>% mutate(bike_type = str_remove_all(bike_type, coll("_bike"))) %>% 
  mutate(start_station = str_replace_all(start_station, coll("_"), " ")) %>% 
  mutate(end_station = str_replace_all(end_station, coll("_"), " ")) %>%            
  mutate(user_type = str_to_title(user_type)) %>% 
  mutate(bike_type = str_to_title(bike_type)) %>% 
  mutate(start_station = str_to_title(start_station)) %>% 
  mutate(end_station = str_to_title(end_station))

any_na(last_12m_v10)

#   Check output

members_v00 <- last_12m_v10 %>% filter(user_type == "Member")
casuals_v00 <- last_12m_v10 %>% filter(user_type == "Casual")

#   Descriptive analysis on duration

#   Compare members and casual users

table(last_12m_v10$user_type)
#   Casual 2,336,422
#   Member 3,261,883

table(members_v00$bike_type)
#   Classic 1,833,041
#   Electric 1,428,842

table(casuals_v00$bike_type)
#   Classic 1,012,191
#   Docked 203,021
#   Electric 1,121,210

mean(members_v00$duration)
mean(casuals_v00$duration)
#   12.78 mins
#   23.54 mins

aggregate(last_12m_v10$duration ~ last_12m_v10$user_type + last_12m_v10$season, FUN = mean)
aggregate(last_12m_v10$duration ~ last_12m_v10$user_type + last_12m_v10$month, FUN = mean)
aggregate(last_12m_v10$duration ~ last_12m_v10$user_type + last_12m_v10$weekday, FUN = mean)

#   Bike type preference

ggplot(data = last_12m_v10)+
  geom_bar(mapping = aes(x = user_type, fill = bike_type))+ 
  labs(x="",
       y="",
       title="Bike Preference",
       caption="")+ 
  scale_y_continuous(labels = label_number(suffix = "  K", scale = 1e-3))+ 
  scale_fill_manual(name = "",
                    labels = c("Classic", "Docked", "Electric"),
                    values = c("#619CFF", "61BEFF", "#F8766D"))

#   Duration

ggplot(data = last_12m_v10)+
  geom_bar(mapping = aes(x = as.numeric(duration), fill = user_type))+ 
  labs(x="Mins",
       y="",
       title="Trip Duration",
       caption="")+
  scale_x_continuous(limits = c(1, 45))+
  scale_y_continuous(labels = label_number(suffix = "  K", scale = 1e-3))+ 
  scale_fill_manual(name = "",
                    labels = c("Casual", "Member"),
                    values = c("#619CFF", "#F8766D"))

#   Seasonal

last_12m_v10 %>% group_by(user_type, season) %>% summarise(n=n())

last_12m_v10$season <- factor(last_12m_v10$season, levels = c("Winter", "Spring", "Summer", "Fall"))

ggplot(data = last_12m_v10)+
  geom_bar(mapping = aes(x = season, fill = user_type))+ 
  labs(x="",
       y="",
       title="Seasonal Ridership",
       caption="")+ 
  scale_y_continuous(labels = label_number(suffix = "  K", scale = 1e-3))+ 
  scale_fill_manual(name = "",
                    labels = c("Casual", "Member"),
                    values = c("#619CFF", "#F8766D"))

#   Monthly

last_12m_v10$month <- factor(last_12m_v10$month, levels = c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr", 
                                                              "May", "Jun", "Jul", "Aug", "Sep", "Oct"))

ggplot(data = last_12m_v10)+
  geom_bar(mapping = aes(x = month, fill = user_type))+ 
  labs(x="",
       y="",
       title="Monthly Ridership",
       caption="")+ 
  scale_y_continuous(labels = label_number(suffix = "  K", scale = 1e-3))+ 
  scale_fill_manual(name = "",
                    labels = c("Casual", "Member"),
                    values = c("#619CFF", "#F8766D"))

#   Weekly

last_12m_v10 %>% group_by(user_type, weekday) %>% summarise(n=n())

last_12m_v10$weekday <- factor(last_12m_v10$weekday, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

ggplot(data = last_12m_v10)+
  geom_bar(mapping = aes(x = weekday, fill = user_type))+ 
  labs(x="",
       y="",
       title="Weekly Ridership",
       caption="")+ 
  scale_y_continuous(labels = label_number(suffix = "  K", scale = 1e-3))+ 
  scale_fill_manual(name = "",
                    labels = c("Casual", "Member"),
                    values = c("#619CFF", "#F8766D"))

#   Cyclistic Stations Map

b <- cbind(stations_v07$lng, stations_v07$lat)

Chicago_map1 <- leaflet(data = b) %>% setView(lng = -87.62791, lat = 41.88250, zoom = 12) %>% 
  addTiles() %>% 
  addMarkers(clusterOptions = markerClusterOptions())
Chicago_map1

#   Find Top 25 most popular start stations among casual users in Fall Weekends

pop_stns_v00 <- last_12m_v10 %>% 
  group_by(user_type, season, weekday, start_station) %>% 
  summarise(n = n()) %>% arrange(desc(n)) %>% 
  filter(., grepl('Casual', user_type)) %>% 
  filter(., grepl('Fall', season)) %>% 
  filter(., weekday == 'Sat' | weekday == 'Sun') %>% 
  head(n = 25L)

#   Find Top 25 most popular end stations among casual users in Fall Weekends

pop_stne_v00 <- last_12m_v10 %>% 
  group_by(user_type, season, weekday, end_station) %>% 
  summarise(n = n()) %>% arrange(desc(n)) %>% 
  filter(., grepl('Casual', user_type)) %>% 
  filter(., grepl('Fall', season)) %>% 
  filter(., weekday == 'Sat' | weekday == 'Sun') %>% 
  head(n = 25L)

#   Combine and Map Top 10

pop_stns_v01 <- pop_stns_v00 %>% mutate(station = start_station)
pop_stne_v01 <- pop_stne_v00 %>% mutate(station = end_station)
pop_stnf_v00 <- rbind(pop_stns_v01, pop_stne_v01)
pop_stnf_v01 <- pop_stnf_v00 %>% select(n, station)

stations_v08 <- stations_v07 %>% 
  mutate(station = str_replace_all(station, coll("_"), " ")) %>% 
  mutate(station = str_to_title(station))
stations_v08 <- stations_v08 %>% select(station, lat, lng)

pop_stnf_v02 <- left_join(pop_stnf_v01, stations_v08) %>% arrange(desc(n)) %>% 
  distinct(station, .keep_all = TRUE) %>% 
  arrange(station)

pop_stnf_v03 <- pop_stnf_v02 %>% ungroup() %>% select(n:lng) %>% distinct(station, .keep_all = TRUE) %>% 
  arrange(desc(n)) %>% head(n=10L)

fall <- cbind(pop_stnf_v03$lng, pop_stnf_v03$lat)

Chicago_map2 <- leaflet(data = fall) %>% setView(lng = -87.62791, lat = 41.88250, zoom = 12) %>% 
  addTiles() %>% addMarkers()
Chicago_map2

#   Find Top 25 most popular start stations among casual users in Summer Weekends

pop_stns_v02 <- last_12m_v10 %>% 
  group_by(user_type, season, weekday, start_station) %>% 
  summarise(n = n()) %>% arrange(desc(n)) %>% 
  filter(., grepl('Casual', user_type)) %>%
  filter(., grepl('Summer', season)) %>% 
  filter(., weekday == 'Sat' | weekday == 'Sun') %>% 
  head(., n=25L)

#   Find Top 25 most popular end stations among casual users in Summer Weeekends

pop_stne_v02 <- last_12m_v10 %>% 
  group_by(user_type, season, weekday, end_station) %>% 
  summarise(n = n()) %>% arrange(desc(n)) %>% 
  filter(., grepl('Casual', user_type)) %>% 
  filter(., grepl('Summer', season)) %>% 
  filter(., weekday == 'Sat' | weekday == 'Sun') %>% 
  head(., n=25L)

#   Combine and Map Top 10

pop_stns_v03 <- pop_stns_v02 %>% mutate(station = start_station)
pop_stne_v03 <- pop_stne_v02 %>% mutate(station = end_station)
pop_stnf_v04 <- rbind(pop_stns_v03, pop_stne_v03)
pop_stnf_v05 <- pop_stnf_v04 %>% select(n, station)

pop_stnf_v06 <- left_join(pop_stnf_v05, stations_v08) %>% arrange(desc(n)) %>% distinct(station, .keep_all = TRUE)

pop_stnf_v07 <- pop_stnf_v06 %>% ungroup() %>% select(n:lng) %>% distinct(station, .keep_all = TRUE) %>% 
  arrange(desc(n)) %>% head(n=10L)

summer <- cbind(pop_stnf_v07$lng, pop_stnf_v07$lat)

Chicago_map3 <- leaflet(data = summer) %>% setView(lng = -87.62791, lat = 41.88250, zoom = 12) %>% 
  addTiles() %>% addMarkers()
Chicago_map3

# Find Top 25 most popular start stations among casual users in Winter Weeks

pop_stns_v04 <- last_12m_v10 %>% 
  group_by(user_type, season, weekday, start_station) %>% 
  summarise(n = n()) %>% arrange(desc(n)) %>% 
  filter(., grepl('Casual', user_type)) %>%
  filter(., grepl('Winter', season)) %>% 
  filter(., weekday %in% c("Mon","Tue","Wed","Thu","Fri")) %>% 
  head(., n=25L)

# Find Top 25 most popular end stations among casual users in Winter Weeks

pop_stne_v04 <- last_12m_v10 %>% 
  group_by(user_type, season, weekday, end_station) %>% 
  summarise(n = n()) %>% arrange(desc(n)) %>% 
  filter(., grepl('Casual', user_type)) %>% 
  filter(., grepl('Winter', season)) %>% 
  filter(., weekday %in% c("Mon","Tue","Wed","Thu","Fri")) %>%
  head(., n=25L)

#   Combine and Map Top 10

pop_stns_v05 <- pop_stns_v04 %>% mutate(station = start_station)
pop_stne_v05 <- pop_stne_v04 %>% mutate(station = end_station)
pop_stnf_v06 <- rbind(pop_stns_v05, pop_stne_v05)
pop_stnf_v07 <- pop_stnf_v06 %>% select(n, station)

pop_stnf_v08 <- left_join(pop_stnf_v07, stations_v08) %>% arrange(desc(n)) %>% distinct(station, .keep_all = TRUE)

pop_stnf_v09 <- pop_stnf_v08 %>% ungroup() %>% select(n:lng) %>% distinct(station, .keep_all = TRUE) %>% 
  arrange(desc(n)) %>% head(n=10L)

winter <- cbind(pop_stnf_v09$lng, pop_stnf_v09$lat)

Chicago_map4 <- leaflet(data = winter) %>% setView(lng = -87.62791, lat = 41.88250, zoom = 12) %>% 
  addTiles() %>% addMarkers()
Chicago_map4

# Find Top 25 most popular start stations among casual users in Spring Weeks

pop_stns_v06 <- last_12m_v10 %>% 
  group_by(user_type, season, weekday, start_station) %>% 
  summarise(n = n()) %>% arrange(desc(n)) %>% 
  filter(., grepl('Casual', user_type)) %>%
  filter(., grepl('Spring', season)) %>% 
  filter(., weekday %in% c("Mon","Tue","Wed","Thu","Fri")) %>% 
  head(., n=25L)

# Find Top 25 most popular end stations among casual users in Spring Weeks

pop_stne_v06 <- last_12m_v10 %>% 
  group_by(user_type, season, weekday, end_station) %>% 
  summarise(n = n()) %>% arrange(desc(n)) %>% 
  filter(., grepl('Casual', user_type)) %>% 
  filter(., grepl('Spring', season)) %>% 
  filter(., weekday %in% c("Mon","Tue","Wed","Thu","Fri")) %>%
  head(., n=25L)

#   Combine and Map Top 10

pop_stns_v07 <- pop_stns_v06 %>% mutate(station = start_station)
pop_stne_v07 <- pop_stne_v06 %>% mutate(station = end_station)
pop_stnf_v10 <- rbind(pop_stns_v07, pop_stne_v07)
pop_stnf_v11 <- pop_stnf_v10 %>% select(n, station)

pop_stnf_v12 <- left_join(pop_stnf_v11, stations_v08) %>% arrange(desc(n)) %>% distinct(station, .keep_all = TRUE)

pop_stnf_v13 <- pop_stnf_v12 %>% ungroup() %>% select(n:lng) %>% distinct(station, .keep_all = TRUE) %>% 
  arrange(desc(n)) %>% head(n=10L)

spring <- cbind(pop_stnf_v13$lng, pop_stnf_v13$lat)

Chicago_map5 <- leaflet(data = spring) %>% setView(lng = -87.62791, lat = 41.88250, zoom = 12) %>% 
  addTiles() %>% addMarkers()
Chicago_map5

#   Clear environment and free up unused memory

rm(all_stat_v01, all_stat_v02, all_stat_v03, all_stat_v04, all_stat_v05,
   all_stat_v06, all_stat_v07, all_stat_v08, all_stat_v09, all_stat_v10, b,
   ca_12356_v00, ca_12356_v01, ca_12356_v02, ca_12356_v03, ca_12356_v04,
   ca_12356_v05, closemtc_v00, complete_v00, complete_v01, dupe_sta_v01, dupe_sta_v02,
   end___st_v00, invalsta_v00, invalsta_v01, last_12m_v03, last_12m_v04, 
   last_12m_v05, last_12m_v06, na_case1_v00, na_case2_v00, na_case3_v00,
   na_case4_v00, na_case5_v00, na_case6_v00, na_cases_v00, outliers_v00, pop_stne_v00,
   pop_stns_v00, set1_s2_v00, set1_s2_v01, set2_s2_v00, start_st_v00, stations_v01, stations_v02,
   stations_v03, stations_v04, stations_v06, stations_v07,
   unq_eloc_v00, unq_sloc_v00, unq_rbnd_v00, unrpaird_v00, unrpaird_v01, unrpaird_v02, unrpaird_v03,
   unrpaird_v04, unrpaird_v05, unrpaird_v06, unrpaird_v07, unrpaird_v08, unrpaird_v09,
   unrpaird_v10, unrpaird_v11, unrpaird_v12, unrpaird_v13, unrpaird_v14, unrpaird_v15,
   unrpaird_v16, unrpaird_v17, unrpaird_v18, validsta_v00, validsta_v01, wrongsta_v00)
gc()
