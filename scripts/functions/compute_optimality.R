
# function to compute GDD optimality

compute_optimality <- function(gdd, ncores = 2){
  
  nyr <- nlyr(gdd)/365
  i <- rep(1:nyr, each = 365)
  gdd_tot <- tapp(gdd, i, fun=max) # calculate total GDD accumulated over the season

  
  .gdd_tot <- wrap(gdd_tot) # make serializable raster
  .gdd <- wrap(gdd) # make serializable raster
  
  # compute Rsquared within two nested loops (one is parallelized)
  plan(multisession, workers = ncores)
  rsq <- future_lapply(1:365, function(d){
    gdd_tot <- rast(.gdd_tot)
    gdd <- rast(.gdd)
    rsq_d <- subset(gdd_tot, 1) # make a copy (in which we will save R2 values)
    ind <- 1:nrow(as.data.frame(gdd_tot))
    gdd_tot_df <- as.data.frame(gdd_tot)
    gdd_d_df <- as.data.frame(subset(gdd, which(time(gdd)==d)))
    test <- lapply(ind, function(i){
      y <- as.numeric(gdd_tot_df[i,])
      x <- as.numeric(gdd_d_df [i,])
      m <- lm(y ~ x)
      s <- summary(m)
      s$r.squared
    })
    val <- values(rsq_d)
    val[!is.na(val)] <- unlist(test)
    values(rsq_d) <- val
    wrap(rsq_d)
  }, future.seed=TRUE)
  plan(sequential);gc()
  rsq <- rast(lapply(rsq, rast))
  
  # compute remaining GDD
  gdd_tot_rep <- rep(gdd_tot, each = 365)
  gdd_rem <- gdd_tot_rep - gdd
  i <- rep(1:365, nyr)
  gdd_rem <- tapp(gdd_rem, i, fun=mean)
  
  # transform rasters to a data.frame (more convenient to plot results)
  ind <- which(!is.na(values(subset(gdd_rem,1))))
  data_plot <- lapply(ind, function(i){
    data.frame(id = i, doy = 1:365, env_pred = t(extract(rsq, i)), growth_pot = t(extract(gdd_rem, i)))
  })
  data_plot <- as.data.frame(do.call(rbind, data_plot))
  
  # compute optimality (euclidean distance from perfect point)
  min_opt <- sqrt(((0 - 1)^2 + (0 - 1)^2))
  data_plot <- data_plot %>%
    group_by(id) %>%
    mutate(max_pot = max(growth_pot),
           opt = min_opt-sqrt(((growth_pot/max_pot) - 1)^2 + (env_pred - 1)^2))
  
  return(data_plot)
}