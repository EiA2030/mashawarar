# 1. Read inputs and check validity
  # 1.1 Catch arguments from bash
args <- commandArgs()
aoi <- as.character(args[6])
start.Date <- as.Date(args[7])
end.Date <- as.Date(args[8])
workers <- as.integer(args[9])
  # 1.2 Check validity of inputs and necessary files
    # Check the spatial inputs (either file or a valid state in Nigeria)
if(grepl(paste0("v", format(Sys.Date(), "%Y%m%d"), ".csv"), aoi)){
  if(!file.exists(paste0("/home/jovyan/saa-use-case/data/inputs/user/", "v", format(Sys.Date(), "%Y%m%d"), ".csv"))){
    stop("Please, ensure that a valid file with coordinates (Longitude and latitude columns, in .csv) is located in the user folder.")
  }
} else {
  if(!(aoi %in% c("Kano", "Kaduna"))){
    stop("Please, use an accepted state in Nigeria for the DST")
  }
}
    # Check if dates are correct and valid
if(as.Date(start.Date) < as.Date("1990-01-01") | as.Date(start.Date) > as.Date(paste0(as.integer(format(Sys.Date(), "%Y"))-1, "-12-31"))){
  stop("Start date is not accepted. Please, make sure is after or on 1st Jan 1990 and not grater than Dec 31st of last year. Also ensure is the correct date format (YYYY-MM-DD)")
}
if(as.Date(end.Date) < as.Date("1990-01-01") | as.Date(end.Date) > as.Date(paste0(as.integer(format(Sys.Date(), "%Y"))-1, "-12-31")) | as.Date(end.Date) < as.Date(start.Date)){
  stop("End date is not accepted. Please, make sure is after or on 1st Jan 1990, not grater than Dec 31st of last year and after the *Start Date*. Also ensure is the correct date format (YYYY-MM-DD)")
}
     # Check that the number of jobs is appropriate
if(workers > 12){stop("Number of parallel workers not accepted. Please use a number lower than 12")}
    # Check that the necessary .MZX template file is there
if(!file.exists(paste0("/home/jovyan/saa-use-case/data/inputs/dssat/xfiles/v", as.character(format(Sys.Date(), "%Y%m%d")), ".MZX"))) stop("Please, add an .MZX template")
# Check that the necessary .CUL file with the varieties is there
if(!file.exists(paste0("/home/jovyan/saa-use-case/data/inputs/dssat/culfiles/v", as.character(format(Sys.Date(), "%Y%m%d")), ".CUL"))) stop("Please, add an .CUL template")
# # 2. Install requirements
# packs <- data.frame("package" = c("tidyverse", "lubridate", "DSSAT", "doParallel", "foreach"),
#                     "version" = c("1.3.2", "1.9.2", "0.0.6", "1.0.17", "1.5.2"))
# ipacks <- installed.packages()
# for (p in 1:nrow(packs)){
#   pack <- packs[p,1]
#   ver <- packs[p,2]
#   print(ipacks[ipacks[,"Package"] == pack, "Version"])
#   # if(pack %in% ipacks[,"Package"] == FALSE){
#   #   install_version(pack, ver, repos = "https://cloud.r-project.org/")
#   # }
#   # else if(ver != ipacks[ipacks[,"Package"] == pack, "Version"]){
#   #   utils::remove.packages(pack)
#   #   install_version(pack, ver, repos = "https://cloud.r-project.org/")
#   # }
# }
# 3. Source Functions
funcs <- list.files("/home/jovyan/saa-use-case/functions",
                        recursive = TRUE, full.names = TRUE, include.dirs = FALSE,
                        pattern = paste0(c("02", "03", "04"), collapse = '|'))
for (func in funcs) {
  source(func)
}
# 4. Start DST
  # 4.1 Define the AOI depending on the input by the user
if(grepl(paste0("v", format(Sys.Date(), "%Y%m%d"), ".csv"), aoi)){
  gps <- read.csv(paste0("/home/jovyan/saa-use-case/data/inputs/user/", "v", format(Sys.Date(), "%Y%m%d"), ".csv"))
} else {
  gps <- define.regions(iso = "NGA", level = 1, resolution = 0.05)
  gps <- gps[gps$NAME_1 == aoi,]
}
  # 4.2 Define the list of years to simulate
years <- unique(format(seq(as.Date(as.character(start.Date)), as.Date(as.character(end.Date)), by = "day"), "%Y"))
  # 4.3 Create the intermediate directory for DSSAT files
dssat.paths <- paste0("/home/jovyan/saa-use-case/data/intermediate/dssat/v", as.character(format(Sys.Date(), "%Y%m%d")), "/", years)
for (dssat.path in dssat.paths) {
  dir.create(dssat.path, recursive = TRUE)
}
  # 4.4 DSSAT
  # 4.4.0 Remove all pre-existing varieties. Replace with ours
CUL <- DSSAT::read_cul("/home/jovyan/saa-use-case/data/inputs/dssat/MZCER047.CUL")
DSSAT::write_cul(CUL, file_name = "/opt/DSSAT/Genotype/MZCER047.CUL")
# Start simulations
for (year in years) {
  dssat.intermediate <- paste0("/home/jovyan/saa-use-case/data/intermediate/dssat/v", as.character(format(Sys.Date(), "%Y%m%d")), "/", year)
    # 4.4.1 Execute DSSAT + ETL
  dssat.extdata(coords = gps,
                sdate = paste0(year, "-01-01"),
                edate = paste0(year, "-12-31"),
                jobs = workers,
                path.to.ex = dssat.intermediate)
    # 4.4.2 Define experiment file
  dssat.Xdata(coords = gps,
              sdate = paste0(year, "-01-01"),
              edate = paste0(year, "-12-31"),
              jobs = workers,
              path.to.ex = dssat.intermediate)
    # 4.4.3 Execute DSSAT
  dssat.execute(coords = gps,
                sdate = paste0(year, "-01-01"),
                edate = paste0(year, "-12-31"),
                jobs = workers,
                path.to.ex = dssat.intermediate)
}
  # 4.5 Aggregation
    # 4.5.1 Aggregate DSSAT outputs by year
dssat.aggregate(years = years,
                jobs = workers,
                path.to.ex = paste0("/home/jovyan/saa-use-case/data/intermediate/dssat/v", as.character(format(Sys.Date(), "%Y%m%d"))))
    # 4.5.2 Ranking the final results
rank.aggregate(years = years,
               jobs = workers,
               path.to.ex = paste0("/home/jovyan/saa-use-case/data/intermediate/dssat/v", as.character(format(Sys.Date(), "%Y%m%d"))))