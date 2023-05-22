dssat.aggregate <- function(years = NULL,
                            jobs = 1,
                            path.to.ex = NULL){
  vars <- as.data.frame(DSSAT::read_cul("/home/jovyan/saa-use-case/data/inputs/dssat/MZCER047.CUL"))[["VAR#"]]
  require(doParallel)
  require(foreach)
  # Set number of parallel workers
  cls <- parallel::makePSOCKcluster(jobs)
  doParallel::registerDoParallel(cls)
  # Extract yearly crop growths from DSSAT
  for (year in years) {
    dirs <- list.dirs(paste(path.to.ex, year, sep = "/"), full.names = FALSE, recursive = FALSE)
    out <-
      foreach::foreach(v = seq_along(vars), .export = ".GlobalEnv", .inorder = TRUE) %:%
      foreach::foreach(d = seq_along(dirs), .export = ".GlobalEnv", .inorder = TRUE, .packages = c("tidyverse", "DSSAT")) %dopar% {
        vv <- vars[v]
        f <- paste0(path.to.ex, "/", year, "/", dirs[d], "/EX", vv, ".OUT")
        if (file.exists(f)){
          s <- as.data.frame(DSSAT::read_output(f))
          pdate <- s[,"PDAT"]
          pdate <- as.data.frame(format(pdate, format = '%Y%m%d'))
          colnames(pdate) <- "pdate"
          pdate$pyear <- format(as.Date(pdate$pdate, "%Y%m%d"), "%Y")
          pdate$pmon <- format(as.Date(pdate$pdate, "%Y%m%d"), "%m")
          pdate$pweek <- format(as.Date(pdate$pdate, "%Y%m%d"), "%W")
          pdate$pdoy <- format(as.Date(pdate$pdate, "%Y%m%d"), "%j")
          pdate$var <- as.character(vv)
          pdate$yield <- s[, "HWAM"]
          loc <- read.table(paste0(path.to.ex, "/", year, "/", dirs[d], '/WHTE', formatC(width = 4, as.integer((d-1)), flag = "0"), '.WTH'), skip = 4, nrow = 1)
          pdate$x <- as.numeric(loc[3])
          pdate$y <- as.numeric(loc[2])
          pdate
        }
      }
    gc()
    out <- do.call("rbind", unlist(out, recursive = FALSE))
    write.table(out, paste0(path.to.ex, "/", year, "/dssat_aggregate_", year, ".csv"), sep = ",", row.names = FALSE)
  }
  stopCluster(cls)
}
