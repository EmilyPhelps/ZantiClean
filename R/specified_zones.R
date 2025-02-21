#arena.df %>% write_csv(., "arena.dim.csv")

#zone.df %>% write_csv(., "zone.dim.csv")

#Need the four zones per arena
zone.df <- read_csv("zone.dim.csv")

#Filter to retain only runtime after 50 (after acclimatisation).
#Zantiks csv starts from 51

xy$TIME_BIN <-cut(xy$RUNTIME, breaks=seq(min(xy$RUNTIME), max(xy$RUNTIME), by=1), right=FALSE)

bin.xy <- xy %>% filter(RUNTIME >= 51) %>%
  group_by(arena, file.timestamp, TIME_BIN) %>%
  summarise(X=mean(X, na.rm=TRUE),
            Y=mean(Y, na.rm=TRUE)) %>%
  mutate(TIME_BIN=as.numeric(gsub("\\)", "", gsub("^.*,", "", as.character(TIME_BIN)))))

rel.xy <- left_join(xy, arena.df) %>%  mutate(rel.X=X-xmin,
                                                  rel.Y=Y-ymin)

#Actually need to estimate the time and count? before binning
zoned.xy <- rel.xy %>%
  rowwise() %>%
  mutate(Zone={matching_zone <- zone.df %>%
    filter(rel.X >= xmin & rel.X <= xmax & rel.Y >=ymin & rel.Y<= ymax) %>%
    pull(Zone)  # Extract the matching zone

  if(length(matching_zone) > 0) matching_zone else NA_character_
  })

