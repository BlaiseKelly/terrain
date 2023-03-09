library(dplyr)
library(httr)
library(sf)
library(raster)
library(ggplot2)
library(raster)
library(sf)
library(leaflet)
library(osmdata)
library(mapview)
library(htmlwidgets)
library(ggmap)
library(viridis)
library(stringr)

select <- dplyr::select

## define an area
osm_bb <- st_bbox(c(xmin = 4.80, xmax = 4.9, ymax = 52.36, ymin = 52.32), crs = st_crs(4326))
mod_area <- st_as_sf(st_as_sfc(osm_bb))

##download landuse from osm
x <- opq(bbox = osm_bb) %>%
  add_osm_feature(key = c('landuse')) %>%
  osmdata_sf()

##extract polygons
landuse <- x$osm_polygons
landuse <- select(landuse, osm_id, landuse)

landuse_u <- data.frame(unique(landuse$landuse))
landuse <- st_intersection(landuse, mod_area)

url <- parse_url("https://geodata.nationaalgeoregister.nl/ahn3/wfs")
url$query <- list(service = "WFS",
                  version = "2.0.0",
                  request = "GetFeature",
                  typename = "ahn3:ahn3_bladindex",
                  BBOX = paste(st_bbox(st_transform(mod_area, 28992)), collapse = ","),
                  outputFormat = "application/json")
request <- build_url(url)

ahn3_blads<- st_read(request) %>% 
  st_transform(4326)

ahn3_in <- ahn3_blads %>% 
  mutate(bladnr = toupper(bladnr)) %>% 
  st_transform(28992)

dir.create("inputs/dat/terrain/LIDAR_05", recursive = TRUE)
dir.create("inputs/dat/terrain/LIDAR_5", recursive = TRUE)

ahn3_in <- c('25dn2', '25bz2', '25ez1', '25gn1')

for(bn in ahn3_in){
  tryCatch({

    bn <- toupper(bn)

    DSM <- paste0("https://download.pdok.nl/rws/ahn3/v1_0/05m_dsm/R_", bn, ".ZIP")
    DTM <- paste0("https://download.pdok.nl/rws/ahn3/v1_0/05m_dtm/M_", bn, ".ZIP")

    download.file(DTM, dest="inputs/dat/terrain/DTM.zip", mode="wb")
    download.file(DSM, dest="inputs/dat/terrain/DSM.zip", mode="wb")
    unzip ("inputs/dat/terrain/DTM.zip", exdir = "inputs/dat/terrain/LIDAR_05") ##unzip the file
    unzip ("inputs/dat/terrain/DSM.zip", exdir = "inputs/dat/terrain/LIDAR_05")

  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

  print(paste0(bn))
  flush.console()

}

DTMs <- list.files(path ="inputs/dat/terrain/LIDAR_5/",pattern = "M5_",full.names = TRUE)
DSMs <- list.files(path ="inputs/dat/terrain/LIDAR_5",pattern = "R5_",full.names = TRUE)
r_files <- list()
for (bn in ahn3_in){
  
  bn <- toupper(bn)
  
  t <- raster(DTMs[grepl(bn, DTMs)])
  s <- raster(DSMs[grepl(bn, DSMs)])
  
  s_rast <- s-t
  
  ## resolution to average over
  Z0_res <- 300
  
  ma_rdnew <- st_as_sf(st_as_sfc(st_bbox(s_rast)))
  
  L <- as.numeric(round(sqrt(st_area(ma_rdnew))/Z0_res))
  
 

##create gridded area
split_area <- st_make_grid(ma_rdnew, square = T, n = c(L, L)) %>% # the grid, covering bounding box
  st_sf()

## loop through squares to sum up Z0 for each

for (s in 1:NROW(split_area)){
  df <- split_area[s,]
  r_cropped <- crop(s_rast, df)
  Z0 <- sd(r_cropped@data@values, na.rm = TRUE)*0.1
  df$Z0 <- Z0
  nam <- as.character(paste0(bn,s))
  r_files[[nam]] <- df
  print(nam)
}

}

sumz <- do.call(rbind, r_files)

plot(sumz)

mapview(sumz)

# plot color schemes
pal_elev <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(sumz$Z0),
                         na.color = "transparent")
pal_slope <- colorNumeric('YlOrRd', values(slope),
                          na.color = "transparent")

# render map
leaflet() %>% 
  addProviderTiles('CartoDB.Positron') %>% 
  addRasterImage(elev, colors = pal_elev, opacity = 0.8,group='elevation') %>%
  addRasterImage(slope, colors = pal_slope, opacity = 0.8,group='slope') %>%
  addMarkers(lon,lat,popup='Turbine Location') %>%
  addLegend(pal = pal_elev, values = values(elev),title = "Elevation",position='topright') %>%
  addLegend(pal = pal_slope, values = values(slope),title = "Slope",position='bottomright') %>%
  addLayersControl(
    baseGroups = c("Esri.WorldTopoMap", "Esri.WorldImagery","Esri.WorldShadedRelief"),
    overlayGroups = c("elevation", "slope"),
    options = layersControlOptions(collapsed = FALSE),
    position = 'topleft') %>% 
  hideGroup("slope")








ma_rdnew <- st_transform(mod_area, 28992)
all_rz <- list()
for (r in DSMs){
  
  r_in <- crop(raster(r), ma_rdnew)
  
  #r2p <- data.frame(rasterToPoints(r_in))
  
  nam <- str_sub(r, 29, -5)
  
  all_rz[[nam]] <- r_in
  
  print(nam)
  
}

#all_pz <- do.call(raster,all_rz)

## merge doesn't like names
names(all_rz) <- NULL

DSM_rast <- do.call(merge, all_rz)

DSM_rast <- raster('inputs/dat/terrain/LIDAR_5/R5_25DN2.TIF')
DTM_rast <- raster('inputs/dat/terrain/LIDAR_5/M5_25DN2.TIF')

s_rast <- DSM_rast-DTM_rast
plot(DTM_rast)
DSM_mean <- mean(s_rast@data@values, na.rm = TRUE)*0.1



#calculate slope
slope=terrain(s_rast,opt='slope',unit = 'degrees')

##approximate Z0
Z0=terrain(s_rast,opt='TRI')/30
Z0_mean <- mean(Z0@data@values, na.rm = TRUE)

Z0_100 <- aggregate(Z0, fact = 10)
p1 <- plot(Z0_100)

tot_Z0 <- mean(Z0@data@values, na.rm = TRUE)



mapview(Z0)













mapview(DSM_rast)

DTM_rast <- Reduce(function(x,y)mosaic(x,y, tolerance=1, fun=mean), lapply(DTMs, raster))

for(bn in ahn3_in){
  tryCatch({
    
    bn <- toupper(bn)
    
    DSM <- paste0("https://download.pdok.nl/rws/ahn3/v1_0/5m_dsm/R5_", bn, ".ZIP")
    DTM <- paste0("https://download.pdok.nl/rws/ahn3/v1_0/5m_dtm/M5_", bn, ".ZIP")
    dir.create("inputs/dat/terrain/", recursive = TRUE)
    download.file(DTM, dest="inputs/dat/terrain/DTM.zip", mode="wb")
    download.file(DSM, dest="inputs/dat/terrain/DSM.zip", mode="wb") 
    unzip ("inputs/dat/terrain/DTM.zip", exdir = "inputs/dat/terrain/LIDAR_5") ##unzip the file
    unzip ("inputs/dat/terrain/DSM.zip", exdir = "inputs/dat/terrain/LIDAR_5")
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  print(paste0(bn))
  flush.console()
  
}

DTMs <- paste0("inputs/dat/terrain/LIDAR_5/M5_", KBNs)
DSMs <- paste0("inputs/dat/terrain/LIDAR_5/R5_", KBNs, ".TIF")
all_rz <- list()
for (r in DTMs){
  
  r_in <- raster(r)
  
  df_r <- rasterToPoints(r_in)
  
  all_rz[[r]] <- df_r
  
  print(r)
  
}

one_rast_df <- do.call(rbind, all_rz)

DTM_rast <- rasterFromXYZ(one_rast_df)

DSM_rast <- Reduce(function(x,y)mosaic(x,y, tolerance=1, fun=mean), lapply(DSMs, raster))


DTM_rast <- Reduce(function(x,y)mosaic(x,y, tolerance=1, fun=mean), lapply(DTMs, raster))
ma_rdnew <- st_transform(mod_area, 28992)
DSM_crop <- crop(DSM_rast, ma_rdnew)
#DTM_crop <- crop(DTM_rast, ma_rdnew)
mapview(DSM_crop)
writeRaster(DSM_crop, "input/dat/terrain/DSM_5m.TIF",format = 'GTiff',overwrite = TRUE)
writeRaster(DTM_crop, "input/dat/terrain/DTM_5m.TIF",format = 'GTiff',overwrite = TRUE)




