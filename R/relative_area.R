## Functions to estimate relative area from Zantiks data
#' walk_dist()
#'
#' This function estimates random swims (or walks if you have legs). This can then be used
#' to generate relative area. It works within the estimate_RA() fucntion
#'
#' @param data A dataframe containing behavioural data from summary_behaviour()
#' @param start (optional) Starting position for the swim in x,y format
#' @param xlim (optional) The width of the tank used for the OFT
#' @param ylim (optional) The length of the tank used for the OFT
#' @param stepsize A dataframe containing steps estimated from real data.
#' @return An data frame containing x,y coordinates for a random swim.
#' @export 
walk_dist <- function(tracklength = 1132,
                      xlim = c(0, 360),
                      ylim = c(0, 270),
                      start = c(180, 67.5),  # can be randomized externally
                      stepsize = df_step_dist_cm) {
  
  # Preallocate storage
  max_steps <- ceiling(tracklength) * 2  # conservative buffer
  mat_walk <- matrix(NA_real_, ncol = 2, nrow = max_steps)
  mat_index <- 1
  
  # Starting coordinates
  x <- start[1]
  y <- start[2]
  dist_travel <- 0
  
  while (dist_travel <= tracklength) {
    repeat {
      move <- dplyr::sample_n(stepsize, 1)
      x_dir <- sample(c(-1, 1), 1)
      y_dir <- sample(c(-1, 1), 1)
      
      xi <- move$x_move * x_dir
      yi <- move$y_move * y_dir
      
      newx <- x + xi
      newy <- y + yi
      
      if (newx > xlim[1] && newx < xlim[2] &&
          newy > ylim[1] && newy < ylim[2]) break
    }
    
    # Save current position
    mat_walk[mat_index, ] <- c(x, y)
    mat_index <- mat_index + 1
    
    # Intermediate points for long steps
    if (move$step_length > 1) {
      n_breaks <- ceiling(move$step_length)
      for (j in 1:(n_breaks - 1)) {
        tmpx <- x + (j / n_breaks) * xi
        tmpy <- y + (j / n_breaks) * yi
        mat_walk[mat_index, ] <- c(tmpx, tmpy)
        mat_index <- mat_index + 1
      }
    }
    
    # Update location and step_length
    x <- newx
    y <- newy
    dist_travel <- dist_travel + move$step_length
  }
  
  # Trim excess rows
  mat_walk <- mat_walk[1:(mat_index - 1), , drop = FALSE]
  return(mat_walk)
}
#' estimate_RA()
#'
#' This function uses a random sample of xy OFT data to generate random swims to estimate
#' the expected area covered for a given tracklength. If you use this variable please cite: 
#' Houslay et al. 2022, https://doi.org/10.7554/eLife.67126
#'
#' @param dir A directory containing the xy data you wish to sample form 
#' @param data A dataframe containing behavioral data from summary_behaviour()
#' @param arena.df A small dataframe containing the coordinates of the arenas within the Zantiks tank/enclosure.
#'         This should include xmin, xmax, ymin, ymax for each arena.
#' @return The original dataframe with the addition of relative area.
#' @export 
#Calc Relative Area
estimate_RA <- function(dir, data, arena.df){
  #Sample from raw files directory and read these in
  sampled <- list.files(dir) %>% sample(10,replace = FALSE)
  
  xy.list <- lapply(sampled, function(file){
                    read_zancoord(dir, file) %>%
                    transform_xy(.)})
  
  names(xy.list) <- sampled
  
  xy.df <- bind_rows(
    Map(function(file, df) {
      df$filename <- file
      df
    }, names(xy.list), xy.list)
  )
  
  #Calculate step size for sampled data
  step_num <- xy.df %>% group_by(filename) %>% summarize(n=n())
  
  step_coords <- matrix(numeric(length = nrow(xy.df)), 
                        ncol = 3, nrow = nrow(xy.df))
  
  step_coords_df <- xy.df %>%
    group_by(filename) %>%
    mutate(
      x_move = X - lag(X),
      y_move = Y - lag(Y),
      step_length = sqrt(x_move^2 + y_move^2)
    ) %>%
    ungroup() %>% 
    dplyr::select(arena,file.timestamp, x_move, y_move, step_length) %>%
    dplyr::rename(X=x_move, Y=y_move)
    
  area.df <-calc_area(step_coords_df, arena.df)
  
  #Standardize by arena and remove any duplicates ingoring direction
  stand_step_df <- left_join(step_coords_df, arena.df) %>%
    mutate(x_move=abs(X-xmin),
           y_move=abs(Y-ymin)) %>%
    dplyr::select(!c(xmin, xmax, ymin, ymax)) %>%
    distinct(x_move, y_move,
             .keep_all = TRUE) |> 
    arrange(step_length) %>% 
    filter(!is.na(step_length))
  
  #Generate track lengths based on range of all data
  minTL <- quantile(data$track_length, 0.05)
  maxTL <- quantile(data$track_length, 0.95)
  some_tracklen <- rep(seq(from = minTL,
                           to = maxTL,
                           length.out = 100), each = 5)
  
  some_num_walks <- length(some_tracklen)
  samp_area <- numeric(some_num_walks)
  
  #Standardization brings everything to be like a arena 1 (or whole arena if no divisions)
  startx <- unique(arena.df$xmax - arena.df$xmin)
  starty <- unique(arena.df$ymax - arena.df$ymin)
  
  if (length(startx) != 1) stop("Error: startx must be length 1.")
  if (length(starty) != 1) stop("Error: starty must be length 1.")
  
  #Do random swims (or walks if you have legs) and estimate area
  results_df <- map(some_tracklen, function(tlen) {
    start_pos <- c(sample(0:360, 1), sample(0:67.5, 1))
    
    walk <- walk_dist(
      start = start_pos,
      tracklength = tlen,
      stepsize = stand_step_df
    ) %>%
      as_tibble() %>%
      rename(X = V1, Y = V2)
    
    area_result <- calc_area(walk)
    
    area_value <- area_result[[1]][1]
    
    tibble(track_length = tlen,
           area = area_value)
  }) %>% bind_rows()
  
  #Plot a polynomial model to this data and predict for our real values and then calculate RA
  lm_sim <- lm(area ~ poly(track_length,4), data = results_df) 
  
  pred <- tibble(track_length = data$track_length)
  pred$fit <- predict(lm_sim, newdata = data, se.fit = FALSE)
  
  output <- tibble(data, predicted=pred$fit) %>%
    mutate(relative.area=area-predicted) %>% 
    dplyr::select(!predicted)
  
  return(output)}
