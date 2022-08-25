

pacman::p_load(raster, rgdal, rgeos, tidyverse, broom, readxl, ggplot2)



shp <- shapefile('meanTemp/mean_temp.shp')


#covid <- read.csv('datos_covid_deptos.csv', sep = ";")


shp@data$NOM_MUNICI <- iconv(shp@data$NOM_MUNICI, from = 'UTF-8', to = 'latin1')



tdy <- broom::tidy(shp, 'ID_ESPACIA')
dfm <- tibble(id_espacia = shp@data$ID_ESPACIA, mpio = shp@data$NOM_MUNICI, temp = shp@data$MEAN)
dfm <- inner_join(tdy, dfm, by = c('id' = 'id_espacia'))

#covid[[2]] <- toupper(covid[[2]])
#covid[6, 2] <- "BOGOTÁ, D.C."
#covid[26, 2] <- "QUINDIO"

#data <- merge(dfm, covid, by.x="dpto", by.y="depto_dane")

#data <- tibble(id_dpto = data$id_dpto, dpto = data$dpto, casos = data$CONFIRMADOS)
  

#d1 <- inner_join(tdy, data, by = c('id' = 'id_dpto'))


pnm <- raster::getData('GADM', country = 'PAN', level = 0)
ecu <- raster::getData('GADM', country = 'ECU', level = 0)
ven <- raster::getData('GADM', country = 'VEN', level = 0)
per <- raster::getData('GADM', country = 'PER', level = 0)
bra <- raster::getData('GADM', country = 'BRA', level = 0)


pnm <- broom::tidy(pnm, 'NAME_0')
ecu <- broom::tidy(ecu, 'NAME_0')
ven <- broom::tidy(ven, 'NAME_0')
per <- broom::tidy(per, 'NAME_0')
bra <- broom::tidy(bra, 'NAME_0')


# Hagamos el mapa


mapa <- ggplot()+
  geom_polygon(data = dfm, aes(x = lat, y = lat, group = id, fill = temp), colour = 'grey')+
  scale_fill_distiller(palette = 'Spectral')+
  geom_polygon(data = pnm, aes(x = long, y = lat, group = group), fill = NA, colour = 'grey')+
  geom_polygon(data = ecu, aes(x = long, y = lat, group = group), fill = NA, colour = 'grey')+
  geom_polygon(data = ven, aes(x = long, y = lat, group = group), fill = NA, colour = 'grey')+
  geom_polygon(data = per, aes(x = long, y = lat, group = group), fill = NA, colour = 'grey')+
  geom_polygon(data = bra, aes(x = long, y = lat, group = group), fill = NA, colour = 'grey')+
  coord_equal(xlim = c(-80, -66), ylim = c(-4.22, 13))+
  xlab('Longitud')+
  ylab('Latitud')+
  labs(fill = 'Temperatura (C)')+
  theme(legend.justification = c(0,0),
        legend.position = c(0.005, 0.005),
        legend.background = element_rect(fill = alpha('white', 1), colour = alpha('white', 0.4)))

ggsave(plot = mapa, filename = 'mapaTemp.png', units = 'in', width = 6, height = 7.5, dpi = 300)

