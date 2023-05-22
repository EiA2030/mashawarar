rank.aggregate <- function(years = NULL,
                            jobs = 1,
                            path.to.ex = NULL){
  for (year in years) {
    t <- data.table::fread(paste0(path.to.ex, "/", year, "/dssat_aggregate_", year, ".csv"))
    if(tail(t$pweek, n = 1) != 52){
      t$pweek <- t$pweek + 1
    }
    colnames(t) <- c(paste0("pdate_", year), "pyear", "pmon", "pweek", "pdoy", "var", paste0("yield_", year), "x", "y")
    t <- t[,c(8,9,1,4,6,7)]
    # out <- merge(out, t[,c(8,9,1,4,6,7)], by = c("x", "y", "pweek"))
    # t <- t[, c("x", "y", "var", "pweek", "pdate", "yield"), with=FALSE]
    if(year == years[1]){
      out <- t
    }
    else {
      out <- merge(out, t, by = c("x", "y", "pweek", "var"), all = TRUE)
    }
  }
  
  avg.date <- c()
  upper.date <- c()
  lower.date <- c()
  avg.yld <- c()
  upper.yld <- c()
  lower.yld <- c()
  cv.yld <- c()
  for (row in 1:nrow(out)){
    years <- as.numeric(gsub("yield_", "", colnames(out)[grepl("yield_", colnames(out))]))
    weights <- exp(-0.1*(years[length(years)]-years)) # As per: https://stackoverflow.com/a/37238415
    # Calculate optimal planting dates
    dates <- lapply(out[row, colnames(out)[grepl("pdate_", colnames(out))], with=FALSE], function(i) as.Date(as.character(i), format = "%Y%m%d"))
    dates <- format(as.Date(as.numeric(dates), origin ="1970-01-01"), format = "%j")
    pdate <- format(as.Date(round(weighted.mean(as.numeric(dates), weights, na.rm = TRUE), digits = 0), origin ="1969-12-31"), format = "%d %B")
    ci.date <- confidence_interval(as.numeric(dates), 0.9)
    avg.date <- c(avg.date, pdate)
    upper.date <- c(upper.date, format(as.Date(ci.date[[2]], origin ="1971-01-01"), format = "%d %B"))
    lower.date <- c(lower.date, format(as.Date(ci.date[[1]], origin ="1971-01-01"), format = "%d %B"))
    # Calculate metrics for yield
    ylds <- lapply(out[row, colnames(out)[grepl("yield_", colnames(out))], with=FALSE], function(i) as.numeric(i))
    yld <- weighted.mean(as.numeric(ylds), weights, na.rm = TRUE)
    ci.yld <- confidence_interval(as.numeric(ylds), 0.9)
    avg.yld <- c(avg.yld, yld)
    upper.yld <- c(upper.yld, ci.yld[[2]])
    lower.yld <- c(lower.yld, ci.yld[[1]])
    cv <- sqrt(Hmisc::wtd.var(as.numeric(ylds), weights))/yld
    cv.yld <- c(cv.yld, cv)
  }
  safe.pdate <- data.frame("pdate_lower90" = lower.date, "pdate_mean" = avg.date, "pdate_upper90" = upper.date)
  safe.yield <- data.frame("yield_lower90" = lower.yld, "yield_mean" = avg.yld, "yield_upper90" = upper.yld, "yield_cv" = cv.yld)
  out <- cbind(out[,1:4],safe.pdate)
  out <- cbind(out,safe.yield)
  
  ##  Rank (top 3) optimal variety and date by yield
  # initialize output table
  nn <- data.frame("X" = NA, "Y" = NA, "var" = NA, "pdate" = NA, "rank" = NA)
  # Find top 3 combinations
  for (rank in 1:3) {
    res <- by(out, list(out$x, out$y), function(x)
      c(
        X = x$x[rank],
        Y = x$y[rank],
        var = x$var[rev(order(x$yield_mean))][rank],
        pdate = x$pdate_mean[rev(order(x$yield_mean))][rank],
        rank = rank
      )
    )
    n <- as.data.frame(do.call(rbind, res))
    nn <- rbind(nn, n)
  }
  # Sort output by location
  nn <- nn[with(nn, order(X, Y)), ]
  nn$E <- ifelse(nn$X > 0, "E", "W")
  nn$N <- ifelse(nn$Y > 0, "N", "S")
  nn$lookup_key <- paste0(nn$E,nn$X,nn$N,nn$Y)
  nn <- nn[,c(8,3,4,5)]
  # Write output
  write.table(nn, paste0("/home/jovyan/saa-use-case/data/outputs/", "v", format(Sys.Date(), "%Y%m%d"), ".csv"), sep = ",", row.names = FALSE)
  # # Remove DSSAT Aggregates
  # for (year in years) {
  #   file.remove(paste0(path.to.ex, "/", year, "/dssat_aggregate_", year, ".csv"))
  # }
}

# Generate a upper & lower CI (taken from: https://stackoverflow.com/questions/48612153/how-to-calculate-confidence-intervals-for-a-vector) 
confidence_interval <- function(vector, interval) {
  # Mean of sample
  # vec_mean <- mean(vector)
  weights <- exp(-0.1*(length(vector)-seq_along(vector)))
  vec_mean <- weighted.mean(vector, weights, na.rm = TRUE)
  # Standard deviation of sample
  # vec_sd <- sd(vector)
  vec_sd <- sqrt(Hmisc::wtd.var(vector, weights))
  # Sample size
  n <- length(vector)
  # Error according to t distribution
  error <- qt((interval + 1)/2, df = n - 1) * vec_sd / sqrt(n)
  # Confidence interval as a vector
  result <- c("lower" = vec_mean - error, "upper" = vec_mean + error)
  return(result)
}
