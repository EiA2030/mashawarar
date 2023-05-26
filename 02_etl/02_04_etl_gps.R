define.regions <- function(iso = NULL, level = NULL, resolution = 0.05){
  if(is.null(iso)){return("Please provide an ISO code")}
  else if(is.null(level)){return("Please indicate a level")}
  else {
    pol <- sf::st_as_sf(sf::st_read(paste0("/home/jovyan/saa-use-case/data/inputs/main/administrative/gadm36_", iso, ".gpkg"), layer = paste("gadm36", iso, as.character(level), sep = "_"), quiet = TRUE))
    grd <- sf::st_join(sf::st_centroid(sf::st_as_sf(sf::st_make_grid(sf::st_bbox(pol), what="polygons", cellsize = resolution))), pol, join = sf::st_intersects)
    grd <- grd[unlist(sf::st_intersects(pol, grd)), ]
    regions <- paste0("NAME_", seq(0, level, 1))
    df <- data.frame(sf::st_coordinates(sf::st_centroid(sf::st_geometry(grd))), data.frame(grd)[,colnames(grd) %in% regions])
    colnames(df) <- c("X", "Y", regions)
    return(df)
  }
}