library(tidyverse)
library(sf)
library(leaflet)
library(Utilities.Package)

df <- list.files("Data/MeteoFrance/RAW/",pattern = "HOR_departement_85_*", full.names = TRUE) %>% 
  map(read.delim,
      sep = ";",
      .progress = TRUE) %>%
  list_rbind() %>% 
  dplyr::filter(!is.na(`T`))



# T : Temperature (°C)
# TD : Dewpoint (°C)
# U : Humidity (%)
# FF : wind speed (m/s)
df_processed <- df %>% 
  dplyr::select(AAAAMMJJHH,NOM_USUEL,LAT,LON,`T`,TD, U, FF) %>% 
  mutate(date = substr(AAAAMMJJHH,1,8)) %>% 
  group_by(date,NOM_USUEL) %>% 
  reframe( LAT = unique(LAT),
           LON = unique(LON),
           `T` = mean(`T`, na.rm = T),
           TD = mean(TD, na.rm = T),
           U = mean(U, na.rm = T),
           FF = mean(FF, na.rm = T))


maps <- df_processed %>% 
  group_by(NOM_USUEL) %>% 
  reframe(LAT = unique(LAT),
          LON = unique(LON),
          n =n ()) %>% 
  st_as_sf(coords=c("LON","LAT") )  %>% 
  st_set_crs("EPSG:4326")
    

leaflet() %>% 
  addTiles() %>% 
  addProviderTiles(provider = providers$Esri.WorldImagery) %>% 
  leaflet::addMarkers(data = maps) %>% 
  addLabelOnlyMarkers(data = maps,
                      label = ~NOM_USUEL,
                      labelOptions = labelOptions(
                        noHide = TRUE,  # Labels are always visible
                        direction = 'top',
                        textOnly = TRUE,
                        style = list(
                          "color" = "black",          # Text color
                          "background-color" = "white",  # Background color
                          "font-size" = "14px",        # Font size
                          "padding" = "5px"            # Padding
                        )
                      )
  ) %>% 
  clearBounds()


### DF selected site

df_selected <- df_processed %>% 
  dplyr::filter(NOM_USUEL == "NOIRMOUTIER EN") %>% 
  dplyr::filter(!is.na(U)) %>% 
  rename(Date_chr = "date",
         StationName = "NOM_USUEL",
         Temperature = "T",
         Dewpoint = "TD",
         Humidity = "U",
         WindSpeed = "FF") %>% 
  mutate(Date = as.Date(Date_chr,format = "%Y%m%d")) %>% 
  dplyr::filter(Date > as.Date("2001-01-01"))

write.csv(df_selected,"WeatherData_Jim.csv", row.names = F)


df_selected %>% 
  ggplot(aes(x = Date, y = Humidity))+
  geom_line()+
  theme_Bede()

df_selected

