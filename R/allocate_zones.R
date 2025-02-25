## Function to reallocate zones within data
#' is_point_in_zone
#'
#' This function is used to determine whether a point is within a zone or not.
#' @param x X coordinate of to be checked
#' @param y Y coordinate to be checked
#' @param xmin Lower X boundary of the zone
#' @param xmax Upper X boundary of the zone
#' @param ymin Lower Y boundary of the zone
#' @param ymax Upper Y boundry of the Zone
is_point_in_zone <- function(x, y, xmin, xmax, ymin, ymax) {
  x >= xmin & x <= xmax & y >= ymin & y <= ymax
}
#' allocate_zones
#'
#' This function is used to change the zone allocation and calculate the distance, time and counts data for the new zones
#'
#' @param XY Transformed coord data
#' @param zones A dataframe with the number of zones and the coordinates of the zones.
#'              e.g. zone, xmin, xmax, ymin, ymax
#'              Zone name must have a Z in it. e.g. Z1, Z2, Z3 or Z_inner, Z_outer
#'              Also the zones cannot overlap. Make them 0.1 apart at least.
#' @param arena A dataframe with the coordinates of the arenas.
#'              e.g. arena, xmin, xmax, ymin, ymax
#' @param start The start time of the assay, after acclimatization (in seconds)
#' @param end The end time of the assay (in seconds)
#' @param file The name of the file to be included. If not included output will
#'             have NA in this column.
#' @param nested Logical vector, is Zone1 within Zone2. Only applicable for two Zones currently.
#' @return An object containing a data frame reflective of the transformed csv
#' @export
allocate_zones <- function(XY, zones, arena, start, end, file, nested){
  if(missing(nested)){
    nested <- FALSE
  }

  if(missing(file)){
    file <- NA
  }
  rel.xy <- left_join(xy, arena) %>%
                mutate(rel.X=X-xmin, rel.Y=Y-ymin)

  if(nested == FALSE){
    zoned.xy <- rel.xy %>%
      rowwise() %>%
      mutate(Zone={matching_zone <- zones %>%
        filter(rel.X >= xmin & rel.X <= xmax & rel.Y >=ymin & rel.Y<= ymax) %>%
        pull(Zone)  # Extract the matching zone

      if(length(matching_zone) > 0) matching_zone else NA_character_
      }) %>%
        filter(!is.na(Zone))
    }
  else {
    inner <- zones %>% filter(Zone =="Z_inner")
    outer <- zones %>% filter(Zone == "Z_outer")

    zoned.xy <- rel.xy %>% rowwise() %>%
      mutate(Zone = case_when(
        is_point_in_zone(rel.X, rel.Y, inner$xmin, inner$xmax, inner$ymin, inner$ymax) ~ "Z_inner",
        is_point_in_zone(rel.X, rel.Y, outer$xmin, outer$xmax, outer$ymin, outer$ymax) ~ "Z_outer",
        TRUE ~ "none"
      ))
  }

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

  output <- bind_rows(distance, time, count) %>%
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

  return(output)
  }
