get.isda <- function(X = NULL, Y = NULL){
  dep <- c("top"="0..20cm","bottom"="20..50cm")
  pars <- c("clay"="clay_tot_psa","sand"="sand_tot_psa","silt"="silt_tot_psa", "texture"="texture.class",
            "fcc"="fcc","bedrock_cm"="bdr","bulk_density"="db_od","ph"="ph_h2o", "stone_content"="log.wpg2",
            "carbon_total"="log.c_tot","carbon_organic"="log.oc","nitrogen_total"="log.n_tot_ncs",
            "phosphorous_extractable"="log.p_mehlich3","potassium_extractable"="log.k_mehlich3",
            "zinc_extractable"="log.zn_mehlich3","magnesium_extractable"="log.mg_mehlich3","calcium_extractable"="log.ca_mehlich3",
            "aluminium_extractable"="log.al_mehlich3","iron_extractable"="log.fe_mehlich3","sulphur_extractable"="log.s_mehlich3",
            "cec"="log.ecec.f")
  chem <- c("carbon_total"="log.c_tot","carbon_organic"="log.oc",
            "phosphorous_extractable"="log.p_mehlich3","potassium_extractable"="log.k_mehlich3",
            "zinc_extractable"="log.zn_mehlich3","magnesium_extractable"="log.mg_mehlich3","calcium_extractable"="log.ca_mehlich3",
            "aluminium_extractable"="log.al_mehlich3","iron_extractable"="log.fe_mehlich3","sulphur_extractable"="log.s_mehlich3",
            "cec"="log.ecec.f")
  url <- "/home/jovyan/common_data/isda/raw/"
  outl <- list()
  soil <- NULL
  labs <- c("iso", "X", "Y", "depth", "lyr_center")
  n <- 1
  for (par in pars) {
    out <- NULL
    var <- names(pars[pars == par])
    for (d in dep) {
      d <- ifelse(par == "bdr" | par == "fcc", "0..200cm", d)
      lyr <- paste("sol",par,"m_30m",d,"2001..2017_v0.13_wgs84.tif",sep = "_")
      tif.cog <- paste0(url,lyr)
      data <- suppressWarnings(terra::rast(tif.cog))
      q <- terra::extract(data, data.frame(x = X, y = Y), xy = TRUE)
      val <- q[,2]
      if (par %in% c(chem, "log.wpg2")){val <- expm1(val / 10)}
      else if (par == "db_od"){val <- val / 100}
      else if (par == "log.n_tot_ncs"){val <- expm1(val / 100)}
      else if (par == "ph_h2o"){val <- val / 10}
      else if (par == "texture.class"){val <- as.character(factor(val, levels = c(1:12), labels = c("Clay", "Silty Clay", "Sandy Clay", "Clay Loam", "Silty Clay Loam", "Sandy Clay Loam", "Loam", "Silt Loam", "Sandy Loam", "Silt", "Loamy Sand", "Sand")))}
      # Needs to be added
      else if (par == "fcc"){
        fcc.atts <- read.table("/home/jovyan/saa-use-case/data/inputs/main/soil/isda/fcc_attributes.tab", sep = "\t", header = T)
        fcc.atts$Class <- lengths(regmatches(fcc.atts$Description, gregexpr(",", fcc.atts$Description))) + 1
        fcc.atts$Class <- ifelse(fcc.atts$Description == "No constraints", 0, fcc.atts$Class)
        fcc.atts$SLPF <- (fcc.atts$Class  - max(fcc.atts$Class))/(min(fcc.atts$Class) - max(fcc.atts$Class))
        data <- data %% 3000
        data <- terra::classify(data, cbind(fcc.atts$Value, fcc.atts$SLPF))
        q <- terra::extract(data, data.frame(x = X, y = Y), xy = TRUE)
        val <- q[,2]
      }
      else val <- val
      loc <- paste0("https://nominatim.openstreetmap.org/reverse?lat=",Y,"&lon=",X,"&format=json")
      loc <- jsonlite::fromJSON(loc)
      country <- toupper(loc$address$country_code)
      val <- cbind("ISO" = country, "X" = X, "Y" = Y, "depth" = ifelse(d == "0..20cm", as.integer(20), as.integer(50)), "lyr_center" = ifelse(d == "0..20cm", as.integer(10), as.integer(20+15)), val)
      colnames(val)[ncol(val)] <- var
      out <- rbind(out, val)
    }
    if(n == 1){
      soil <- cbind(soil, out)
    }
    else {
      soil <- cbind(soil, out[,6])
    }
    labs <- c(labs, var)
    n <- n + 1
  }
  soil <- as.data.frame(soil)
  colnames(soil) <- labs
  
  soil[soil=="NaN"]<-NA
  
  return(soil)
}

isda2dssat <- function(isda = NULL){
  # Add pedo-transfer values
  # Drained upper limit (cm3 cm3)
  isda$DUL <- {
    clay <- as.numeric(isda$clay) * 1e-2
    sand <- as.numeric(isda$sand) * 1e-2
    om <- (as.numeric(isda$carbon_organic) * 1e-2) * 2
    ans0 <- -0.251 * sand + 0.195 * clay + 0.011 * om + 0.006 * (sand * om) - 0.027 * (clay * om) + 0.452 * (sand * clay) + 0.299
    ans <- ans0 + (1.283 * ans0^2 - 0.374 * ans0 - 0.015)
    ans
  }
  # Drained upper limit saturated (cm3 cm3)
  DUL_S <- {
    clay <- as.numeric(isda$clay) * 1e-2
    sand <- as.numeric(isda$sand) * 1e-2
    om <- (as.numeric(isda$carbon_organic) * 1e-2) * 2
    ans0 <- 0.278 * sand + clay * 0.034 + om * 0.022 + -0.018 * sand * om - 0.027 * clay * om + -0.584 * sand * clay + 0.078
    ans <- ans0 + (0.636 * ans0 - 0.107)
    ans
  }
  # Lower limit of plant extractable soil water (cm3 cm3)
  isda$LL15 <- {
    clay <- as.numeric(isda$clay) * 1e-2
    sand <- as.numeric(isda$sand) * 1e-2
    om <- (as.numeric(isda$carbon_organic) * 1e-2) * 2
    ans0 <- -0.024 * sand + 0.487 * clay + 0.006 * om + 0.005 * sand * om + 0.013 *clay * om + 0.068 *sand * clay +  0.031
    ans <- ans0 + (0.14 * ans0 - 0.02)
    ans
  }
  # Saturated upper limit (cm3 cm3)
  isda$SAT <- {
    sand <- sand * 1e-2
    ans <- isda$DUL + DUL_S - 0.097 * sand + 0.043
    ans
  }
  # Saturated hydraulic conductivity (cm h1)
  B <- (log(1500) - log(33))/(log(isda$DUL) - log(isda$LL15))
  Lambda <- 1/B
  isda$SKS <- (1930 * (isda$SAT - isda$DUL)^(3 - Lambda)) * 100
  isda$SSS <- round(as.numeric(isda$SKS), digits = 1)
  # Albedo (unitless)
  isda$SALB <- ifelse(isda$texture %in% c("Clay", "Silty Clay", "Silty Clay Loam", "Silt Loam"), 0.12,
                      ifelse(isda$texture %in% c("Sandy Clay", "Clay Loam", "Sandy Clay Loam", "Loam", "Sandy Loam", "Silt"), 0.13,
                             ifelse(isda$texture %in% c("Silty Loam"), 0.14,
                                    ifelse(isda$texture %in% c("Loamy Sand"), 0.16,
                                           ifelse(isda$texture %in% c("Sand"), 0.19, NA)))))
  isda$SRGF <- 1*exp(-0.02 * as.numeric(isda$lyr_center))
  return(isda)
}
