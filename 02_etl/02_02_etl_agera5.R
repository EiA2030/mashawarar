agera5 <- function(startDate, endDate, coordPoints = NULL){
  dates <- seq.Date(as.Date(startDate, format = "%Y-%m-%d"), as.Date(endDate, format = "%Y-%m-%d"), by = "day")
  year <- unique(format(dates, "%Y"))
  wind <- terra::rast()
  temp <- terra::rast()
  tmin <- terra::rast()
  tmax <- terra::rast()
  rhum <- terra::rast()
  srad <- terra::rast()
  files <- list.files(paste0("/home/jovyan/saa-use-case/data/inputs/main/weather/",
                             c("RelativeHumidity", "SolarRadiation", "Temperature", "TemperatureMax", "TemperatureMean", "TemperatureMin", "WindSpeed")),
                      full.names = T,
                      pattern = paste0(year, collapse = '|'))
  for (file in files) {
    if (grepl("WindSpeed", file, fixed = TRUE)){terra::add(wind) <- terra::rast(file)}
    else if (grepl("TemperatureMean", file, fixed = TRUE)){terra::add(temp) <- terra::rast(file)}
    else if (grepl("TemperatureMin", file, fixed = TRUE)){terra::add(tmin) <- terra::rast(file)}
    else if (grepl("TemperatureMax", file, fixed = TRUE)){terra::add(tmax) <- terra::rast(file)}
    else if (grepl("RelativeHumidity", file, fixed = TRUE)){terra::add(rhum) <- terra::rast(file)}
    else if (grepl("SolarRadiation", file, fixed = TRUE)){terra::add(srad) <- terra::rast(file)}
  }
  names(wind) <- as.character(format(as.Date(terra::time(wind)), "%Y%m%d"))
  names(temp) <- as.character(format(as.Date(terra::time(temp)), "%Y%m%d"))
  names(tmin) <- as.character(format(as.Date(terra::time(tmin)), "%Y%m%d"))
  names(tmax) <- as.character(format(as.Date(terra::time(tmax)), "%Y%m%d"))
  names(rhum) <- as.character(format(as.Date(terra::time(rhum)), "%Y%m%d"))
  names(srad) <- as.character(format(as.Date(terra::time(srad)), "%Y%m%d"))
  wind <- wind[[as.character(format(dates, format = "%Y%m%d"))]]
  temp <- temp[[as.character(format(dates, format = "%Y%m%d"))]]
  tmin <- tmin[[as.character(format(dates, format = "%Y%m%d"))]]
  tmax <- tmax[[as.character(format(dates, format = "%Y%m%d"))]]
  rhum <- rhum[[as.character(format(dates, format = "%Y%m%d"))]]
  srad <- srad[[as.character(format(dates, format = "%Y%m%d"))]]
  w <- data.frame()
  for (pnt in seq(1:nrow(coordPoints))){
    lon <- coordPoints[pnt, 1]
    lat <- coordPoints[pnt, 2]
    z.wind <- terra::extract(wind,data.frame(lon,lat))
    z.temp <- terra::extract(temp,data.frame(lon,lat))
    z.tmin <- terra::extract(tmin,data.frame(lon,lat))
    z.tmax <- terra::extract(tmax,data.frame(lon,lat))
    z.rhum <- terra::extract(rhum,data.frame(lon,lat))
    z.srad <- terra::extract(srad,data.frame(lon,lat))
    out <- data.frame("dates" = dates)
    out$X <- lon
    out$Y <- lat
    out$WIND <- as.vector(t(z.wind[2:length(z.wind)]))
    out$TEMP <- as.vector(t(z.temp[2:length(z.temp)]))
    out$TMIN <- as.vector(t(z.tmin[2:length(z.tmin)]))
    out$TMAX <- as.vector(t(z.tmax[2:length(z.tmax)]))
    out$RELH <- as.vector(t(z.rhum[2:length(z.rhum)]))
    out$RADN <- as.vector(t(z.srad[2:length(z.srad)]))
    out <- data.frame("X" = out$X, "Y" = out$Y, "dates" = out$dates,
                      "year" = format(as.Date(out$dates), format = "%Y"),
                      "month" = format(as.Date(out$dates), format = "%m"),
                      "day" = format(as.Date(out$dates), format = "%d"),
                      "WIND" = out$WIND,
                      "TEMP" = out$TEMP-273,
                      "TMIN" = out$TMIN-273,
                      "TMAX" = out$TMAX-273,
                      "RHUM" = out$RELH,
                      "SRAD" = out$RADN*1e-6)
    w <- rbind(w, out)
  }
  return(w)
}
