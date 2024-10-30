library(tidyverse)
library(terra)
library(sf)
library(MapRs)

img_list <- "Data/Sentinel2/" %>% 
  list.files(recursive = T, full.names = T ,pattern = ".SAFE", include.dirs = T) %>% 
  as_tibble() %>% 
  rename(path = "value") %>% 
  dplyr::filter(str_detect(path,"20210906") | str_detect(path, "20210814"))

msk <- read_sf("Data/shp/mask_seagrasses.shp")




NDVI <- function(img){
  a <- ((img$B08)-(img$B04))/((img$B08)+(img$B04))
  return(c(indice = "NDVI", raster = a))
}

IRslope <- function(img){
  a <- ((img$B08)-(img$B05))/((img$B08)+(img$B05))
  return(c(indice = "IRslope", raster = a*5))
}

Darkening1 <- function(img){
  a <- (((img$B08)-(img$B05))/137)*(1/img$B03)
  return(c(indice = "Darkening1", raster = a))
}

Green_red <- function(img){
  a <- ((img$B03)-(img$B04))/((img$B03)+(img$B04))
  return(c(indice = "Green_red", raster = a))
}


Slope_Green_red <- function(img){
  a <- (((img$B08)-(img$B05))/137) * 1/(((img$B03)-(img$B04))/((img$B03)+(img$B04)))
  return(c(indice = "Slope_Green_red", raster = a))
}

Slope_Green_red2 <- function(img){
  a <- (((img$B08)-(img$B05))/((img$B08)+(img$B05)) )* 1/(((img$B03)-(img$B04))/((img$B03)+(img$B04)))
  return(c(indice = "Slope_Green_red2", raster = a))
}

Slope_Green_red3 <- function(img){
  a <- (((img$B03)-(img$B04))/((img$B03)+(img$B04))) * 1/(0.2-(img$B05))
  return(c(indice = "Slope_Green_red3", raster = a))
}

Slope_Green_red4 <- function(img){
  a <- 1.1* (((img$B03)-(img$B04))/((img$B03)+(img$B04))) * 1/(0.1-(img$B05))
  return(c(indice = "Slope_Green_red4", raster = a))
}

Slope_Green_red5 <- function(img){
  a <- (((img$B03)-(img$B04))/((img$B03)+(img$B04))) * 1/(0.15-(img$B05))
  return(c(indice = "Slope_Green_red5", raster = a))
}



compare_to_interSDI <- function(img){
  a <- ((((img$B05)-(img$B04))-((img$B03)-(img$B02)))/(((img$B05)+(img$B04))+((img$B03)+(img$B02))))
  return(c(indice = "SDI", raster = a))
}

aSDI <- function(img){
  a <- (((((img$B05)-(img$B04))+((img$B03)-(img$B02)))/3)/(((img$B05)+(img$B04))+((img$B03)+(img$B02))))
  return(c(indice = "aSDI", raster = a))
}

NDSI <- function(img){
  a <- ((img$B05)-(((img$B04)+(img$B03)+(img$B02))/3))/((img$B05)+(((img$B04)+(img$B03)+(img$B02))/3))
  return(c(indice = "NDSI", raster = a))
}

DSDI <- function(img){
  a <- ((img$B08)-(img$B05))/((img$B03+(img$B04)))
  return(c(indice = "DSDI", raster = a))
}

SDI_2 <- function(img){
  a <- (img$B03+ ((740-560)/(842-560))*(img$B08-img$B03)) - img$B06
  return(c(indice = "SDI_2", raster = a))
}


indices <- c(SDI_2)


for(i in 1:nrow(img_list)){
  
    img <- Read_S2(img_list$path[i]) %>%
      terra::crop(msk, mask = T)

  
  # img_std <- img %>% as.data.frame(xy = T) %>% 
  #   pivot_longer(-c(x,y), names_to = "bands",values_to = "values") %>% 
  #   group_by(x,y) %>% 
  #   mutate(std = (max(values)-values)/(max(values)-min(values))) %>% 
  #   dplyr::select(-values) %>% 
  #   pivot_wider(names_from = "bands",values_from = std) %>% 
  #   rast(type ="xyz")
  
  names(img) <- c("B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", "B08A", "B09", 
                   "B11", "B12")
  # names(img_std) <- c("B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", "B08A", "B09", 
  #                 "B11", "B12")
  
  for(ii in 1:length(indices)){
    fn <- indices[[ii]]
    
    a <- fn(img)
    
    output <- paste0("Output/test_indices/",gsub(".*/","",img_list$path[i]) %>% gsub(".SAFE","",.),"/",a$indice,".tif")
    
    if(!dir.exists(paste0("Output/test_indices/",gsub(".*/","",img_list$path[i]) %>% gsub(".SAFE","",.)))){
      dir.create((paste0("Output/test_indices/",gsub(".*/","",img_list$path[i]) %>% gsub(".SAFE","",.))),recursive = T)
    }
    
    writeRaster(a$raster,output,overwrite = T)
    
  }
  # 
  # for(ii in 1:length(indices)){
  #   fn <- indices[[ii]]
  #   
  #   a <- fn(img_std)
  #   
  #   output <- paste0("Output/test_indices/",gsub(".*/","",img_list$path[i]) %>% gsub(".SAFE","",.),"/",a$indice,"_std.tif")
  #   
  #   if(!dir.exists(paste0("Output/test_indices/",gsub(".*/","",img_list$path[i]) %>% gsub(".SAFE","",.)))){
  #     dir.create((paste0("Output/test_indices/",gsub(".*/","",img_list$path[i]) %>% gsub(".SAFE","",.))),recursive = T)
  #   }
  #   
  #   writeRaster(a$raster,output,overwrite = T)
  #   
  # }
}


