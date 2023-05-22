dssat.Xdata <- function(coords = NULL,
                        sdate, edate,
                        jobs = 1,
                        path.to.ex = NULL){
  require(doParallel)
  require(foreach)
  # Set number of parallel workers
  cls <- parallel::makePSOCKcluster(jobs)
  doParallel::registerDoParallel(cls)
  #Set working directory to experment directory
  setwd(path.to.ex)
  # Process soil & weather
  foreach::foreach(pnt=seq_along(coords[,1]), .export = ls(globalenv()), .inorder = TRUE, .packages = c("tidyverse", "lubridate")) %dopar% {
    ############################################## MODIFICATION OF EXPERIMENTAL FILES ############################################
    setwd(paste(path.to.ex,paste0('EXTE', formatC(width = 4, (as.integer(pnt)-1), flag = "0")), sep = "/"))
    x = coords[pnt,1]
    y = coords[pnt,2]
    # Write the .MZX file for the location
    v <- as.data.frame(DSSAT::read_cul("/home/jovyan/saa-use-case/data/inputs/dssat/MZCER047.CUL"))
    v <- data.frame("C" = 1:length(v[[1]]),
                    "CR" = "MZ",
                    "INGENO" = v[[1]],
                    "CNAME" = v[[2]])
    #Read in template FileX
    filex <- DSSAT::read_filex("/home/jovyan/saa-use-case/data/inputs/dssat/ABWF2002.MZX")
    # Define Authorship
    filex$GENERAL$PEOPLE <- "SAA Use Case"
    filex$GENERAL$ADDRESS <- "Excellence in Agronomy Initiative - CGIAR"
    # Define location
    filex$GENERAL$SITE <- paste(paste0("X=", x), paste0("Y=", y), sep = ", ")
    # Define Simulation Controls
    filex$`SIMULATION CONTROLS`$SDATE <- format(as.Date(sdate, "%Y-%m-%d"), "%y%j")
    filex$`SIMULATION CONTROLS`$RSEED <- as.integer(runif(1, 2000, 2500))
    filex$`SIMULATION CONTROLS`$IRRIG <- "N"
    filex$`SIMULATION CONTROLS`$FERTI <- "N"
    filex$`SIMULATION CONTROLS`$RESID <- "N"
    # Define planting date
    pdates <- data.frame(seq(1,length(seq(as.Date(sdate, "%Y-%m-%d") + 1, as.Date(edate, "%Y-%m-%d") - 1, by = "1 week")), by = 1),
                         format(seq(as.Date(sdate, "%Y-%m-%d") + 1, as.Date(edate, "%Y-%m-%d") - 1, by = "1 week"), "%y%j"),
                         -99,5.3,5.3,"S","R",75,-99,3,-99,-99,-99,-99,-99,
                         paste0("PD", seq(1:length(seq(as.Date(sdate, "%Y-%m-%d") + 1, as.Date(edate, "%Y-%m-%d") - 1, by = "1 week")))))
    colnames(pdates) <- colnames(filex$`PLANTING DETAILS`)
    filex$`PLANTING DETAILS` <- pdates
    # Define irrigation
    filex$`IRRIGATION AND WATER MANAGEMENT`$IDEP <- 0
    filex$`IRRIGATION AND WATER MANAGEMENT`$ITHR <- 0
    filex$`IRRIGATION AND WATER MANAGEMENT`$IEPT <- 0
    filex$`IRRIGATION AND WATER MANAGEMENT`$IAMT <- 0
    filex$`IRRIGATION AND WATER MANAGEMENT`$IDATE <- format(as.Date(sdate, "%Y-%m-%d"), "%y%j")
    # Define fertilizer amounts
    fert <- data.frame(1, format(as.Date(sdate, "%Y-%m-%d") + 1, "%y%j"), "FE013", "AP004", 0, 0, 0, 0, 0, 0, "NA", "NA")
    colnames(fert) <- colnames(filex$`FERTILIZERS (INORGANIC)`)
    filex$`FERTILIZERS (INORGANIC)` <- fert
    # Define Automatic management options
    filex$`SIMULATION CONTROLS`$PFRST <- format(as.Date(sdate, "%Y-%m-%d"), "%y%j")
    filex$`SIMULATION CONTROLS`$PLAST <- format(as.Date(edate, "%Y-%m-%d"), "%y%j")
    filex$`SIMULATION CONTROLS`$PH2OL <- 0
    filex$`SIMULATION CONTROLS`$PH2OU <- 100
    filex$`SIMULATION CONTROLS`$PH2OD <- 0
    filex$`SIMULATION CONTROLS`$PSTMX <- 50
    filex$`SIMULATION CONTROLS`$PSTMN <- 0
    filex$`SIMULATION CONTROLS`$IMDEP <- 0
    filex$`SIMULATION CONTROLS`$ITHRL <- 0
    filex$`SIMULATION CONTROLS`$ITHRU <- 100
    filex$`SIMULATION CONTROLS`$IRAMT <- 0
    filex$`SIMULATION CONTROLS`$IREFF <- 1
    filex$`SIMULATION CONTROLS`$NMDEP <- 0
    filex$`SIMULATION CONTROLS`$NMTHR <- 0
    filex$`SIMULATION CONTROLS`$NAMNT <- 0
    filex$`SIMULATION CONTROLS`$RIPCN <- 0
    filex$`SIMULATION CONTROLS`$RTIME <- 1
    filex$`SIMULATION CONTROLS`$RIDEP <- 0
    filex$`SIMULATION CONTROLS`$HFRST <- format(as.Date(sdate, "%Y-%m-%d"), "%y%j")
    filex$`SIMULATION CONTROLS`$HLAST <- format(as.Date(edate, "%Y-%m-%d"), "%y%j")
    # Add SOIL and WTH references to FileX
    filex$FIELDS$WSTA<-paste0("WHTE", formatC(width = 4, as.integer((pnt-1)), flag = "0"))
    # filex$FIELDS$ID_SOIL<-paste0('ISDA', formatC(width = 6, as.integer((pnt-1)), flag = "0"))
    filex$FIELDS$ID_SOIL<-DSSAT::read_sol("SOIL.SOL")[,"PEDON"][[1]]
    # Prepare the treatment levels of FileX
    for (var in v$C) {
      # Define cultivars
      filex$CULTIVARS <- v[var,]
      filex$CULTIVARS$C <- 1
      # Define Treatments
      treat <- data.frame(seq_along(seq(1,length(seq(as.Date(sdate, "%Y-%m-%d") + 1, as.Date(edate, "%Y-%m-%d") - 1, by = "1 week")), by = 1)),
                          1, 1, 0,
                          # Treatment name
                          paste0(rep(v$INGENO[var], each = length(seq(as.Date(sdate, "%Y-%m-%d") + 1, as.Date(edate, "%Y-%m-%d") - 1, by = "1 week"))),
                                 "_PD_", seq(1:length(seq(as.Date(sdate, "%Y-%m-%d") + 1, as.Date(edate, "%Y-%m-%d") - 1, by = "1 week")))),
                          # Cultivar selection
                          # rep(v$C, each=length(seq(as.Date(sdate, "%Y-%m-%d") + 1, as.Date(edate, "%Y-%m-%d") - 1, by = "1 week"))),
                          1,
                          1, 0, 1,
                          seq(1,length(seq(as.Date(sdate, "%Y-%m-%d") + 1, as.Date(edate, "%Y-%m-%d") - 1, by = "1 week")), by = 1),
                          0, 1, 0, 0, 0, 0, 0, 1)
      colnames(treat) <- colnames(filex$`TREATMENTS                        -------------FACTOR LEVELS------------`)
      filex$`TREATMENTS                        -------------FACTOR LEVELS------------` <- treat
      DSSAT::write_filex(filex, paste0("EX",v$INGENO[var],'.MZX'))
    }
    setwd(path.to.ex)
    gc()
  }
  stopCluster(cls)
}