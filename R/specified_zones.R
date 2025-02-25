data <- read_zancoord(dir = "data/cherryshrimp2024/coord.csv/",
                      file="LightDarktestCS_final-20241128T133023-XY_data.csv") %>%
        transform_xy()
xy <- data
arena.df %>% write_csv(., "arena.dim.csv")
start <- 50 # assay start time
end <- 480 # assay end time
file <- "test.csv"
#zone.df %>% write_csv(., "zone.dim.csv")

#Need the four zones per arena
zone.df <- read_csv("zone.dim.csv")

#Filter to retain only runtime after 50 (after acclimatisation).
#Zantiks csv starts from 51
rel.xy <- left_join(xy, arena.df) %>%  mutate(rel.X=X-xmin,
                                                  rel.Y=Y-ymin)

#Actually need to estimate the time and count? before binning
zoned.xy <- rel.xy %>%
  rowwise() %>%
  mutate(Zone={matching_zone <- zone.df %>%
    filter(rel.X >= xmin & rel.X <= xmax & rel.Y >=ymin & rel.Y<= ymax) %>%
    pull(Zone)  # Extract the matching zone

  if(length(matching_zone) > 0) matching_zone else NA_character_
  }) %>%
  filter(!is.na(Zone))


#Could do it in the transformed state,
#Create variable type, and do counts, time and distance.

zoned.xy$TIME_BIN <-cut(zoned.xy$RUNTIME,
                        breaks=seq(min(zoned.xy$RUNTIME),
                                  max(zoned.xy$RUNTIME), by=1), right=FALSE)

distance <- zoned.xy %>%
  mutate(TIME_BIN=as.numeric(gsub("\\)", "", gsub("^.*,", "",
                                                  as.character(TIME_BIN))))) %>%
  group_by(arena) %>% arrange(TIME_BIN) %>%
  mutate(x.dis=abs(rel.X-lead(rel.X)),
          y.dis=abs(rel.Y-lead(rel.Y))) %>%
  group_by(arena, TIME_BIN, Zone, file.timestamp) %>%
  summarize(dis=sqrt(max(x.dis)^2 + max(y.dis)^2),
         type="D") %>%
  filter(!is.na(dis)) %>%
  pivot_wider(names_from = Zone, values_from = dis) %>%
  mutate(across(contains("Z"), ~ replace_na(., 0)))


time <- zoned.xy %>%
  group_by(arena, Zone, TIME_BIN, file.timestamp) %>%
  summarize(time=max(RUNTIME)-min(RUNTIME)) %>%
  mutate(TIME_BIN=as.numeric(gsub("\\)", "", gsub("^.*,", "",
                                                  as.character(TIME_BIN)))),
         type="T") %>%
  filter(!is.na(Zone)) %>%
  pivot_wider(names_from = Zone, values_from = time) %>%
  mutate(across(contains("Z"), ~ replace_na(., 0)))

#Fix this section to have 1 for each time in time bin the zone is entered
count <- zoned.xy %>%
      group_by(arena) %>%
      mutate(count=0) %>%
      mutate(TIME_BIN=as.numeric(gsub("\\)", "", gsub("^.*,", "",
                                                  as.character(TIME_BIN)))),
         type="C") %>%
  filter(!is.na(Zone)) %>%
  pivot_wider(names_from = Zone, values_from = count) %>%
  mutate(across(contains("Z"), ~ replace_na(., 0))) %>%
  dplyr::select(TIME_BIN, arena, type, file.timestamp, contains("Z"))

count <- time %>%
                mutate(across(contains("Z"), ~ ifelse(. != 0, 1, 0)),
                      type = "C") %>%
                filter(TIME_BIN > start & TIME_BIN <= end) %>%
                group_by(arena)

#Output data should look like zantiks file
#TIME TIME_BIN arena, type, file.timestamp, file.date, file, Zones

test <- bind_rows(distance, time, count) %>%
  filter(TIME_BIN > start & TIME_BIN <= end) %>%
  arrange(TIME_BIN) %>%
  ungroup() %>%
  mutate(TIME=TIME_BIN,
         TIME_BIN=round(TIME_BIN-start, 0),
         file.date=ymd(substr(file.timestamp, 1, 8)),
         file=file) %>%
  filter(TIME_BIN !=0) %>%
  dplyr::select(TIME, TIME_BIN, arena, type,
                file.timestamp, file.date, file, contains("Z"))

