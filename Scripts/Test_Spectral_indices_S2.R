library(tidyverse)
library(terra)
library(sf)
library(MapRs)

img_list <- "Data/Sentinel2/" %>% 
  list.files(recursive = T, full.names = T ,pattern = ".SAFE", include.dirs = T) %>% 
  as_tibble() %>% 
  rename(path = "value") %>% 
  dplyr::filter(str_detect(path,"20210906") | str_detect(path,"20240919"))

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
  a <- (((img$B03)-(img$B04))/((img$B03)+(img$B04))) * 1/(0.2-((img$B05)/10000))
  return(c(indice = "Slope_Green_red3", raster = a))
}



NDVI_IR <- function(img){
  a <- (((img$B05)-(img$B04))/((img$B05)+(img$B04)))
  return(c(indice = "NDVI_IR", raster = a))
}


indices <- c(NDVI, IRslope, Darkening1, Green_red,Slope_Green_red,Slope_Green_red2,NDVI_IR,Slope_Green_red3)


for(i in 1:nrow(img_list)){
  
  if(str_detect(img_list$path[i],"20210906")){
    img <- Read_S2(img_list$path[i]) %>% 
      terra::crop(msk, mask = T) 
  }else{
    img <- Read_S2(img_list$path[i]) 
  }
  
  
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


