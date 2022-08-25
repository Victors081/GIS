



pacman::p_load(sf, dplyr, ggplot2)



muni <- read_sf("CAPAS/shp/COLOMBIA/MUNICIPIO/MGN_MPIO_POLITICO.shp")


nombres <- c("BUENAVENTURA","CALI","CANDELARIA","JAMUNDÍ","PALMIRA","YUMBO")

cercanos <- c("VALLE DEL CAUCA","CAUCA")

valle <- muni %>% filter(DPTO_CNMBR %in% cercanos)

ciudades <- valle %>% filter(MPIO_CNMBR %in% nombres)




align_legend <- function(p, hjust = 0.5)
{
  # extract legend
  g <- cowplot::plot_to_gtable(p)
  grobs <- g$grobs
  legend_index <- which(sapply(grobs, function(x) x$name) == "guide-box")
  legend <- grobs[[legend_index]]
  
  # extract guides table
  guides_index <- which(sapply(legend$grobs, function(x) x$name) == "layout")
  
  # there can be multiple guides within one legend box  
  for (gi in guides_index) {
    guides <- legend$grobs[[gi]]
    
    # add extra column for spacing
    # guides$width[5] is the extra spacing from the end of the legend text
    # to the end of the legend title. If we instead distribute it by `hjust:(1-hjust)` on
    # both sides, we get an aligned legend
    spacing <- guides$width[5]
    guides <- gtable::gtable_add_cols(guides, hjust*spacing, 1)
    guides$widths[6] <- (1-hjust)*spacing
    title_index <- guides$layout$name == "title"
    guides$layout$l[title_index] <- 2
    
    # reconstruct guides and write back
    legend$grobs[[gi]] <- guides
  }
  
  # reconstruct legend and write back
  g$grobs[[legend_index]] <- legend
  g
}

library(cowplot)


extrafont::fonts()

extrafont::fonttable()

extrafont::loadfonts(device="win")



d <-
  ggplot()+  geom_sf(data = muni, fill = "white", colour = "black") + 
  geom_sf(data = ciudades,  aes(fill = MPIO_CNMBR), colour = "black")+
  geom_sf_label(data = ciudades,aes(label = MPIO_CNMBR),size=10,
                family="Impact")+
  coord_sf(xlim=c(-77.9,-76.0),ylim=c(3.0,4.2),crs = st_crs(4326))+
  theme_void(base_family = "Iosevka", base_size = 48) +
  labs(x = NULL, y = NULL,
       title = "",
       subtitle = "",
       caption = "Observatorio de Politicas Públicas ICESI | Dirección de análisis y métodos")+
  guides(fill = "none")  +
  theme(legend.position = c(0.9, 0.8), legend.direction = "vertical",
        plot.background = element_rect(fill = "white", color = NA), 
        panel.background = element_rect(fill = "#51d1f6", color = NA),
        legend.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", colour = NA),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        text =  element_text(color = "black"),
        title =  element_text(color = "black"),
        plot.caption = element_text(size = 28, face = "italic"),
        plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 32),
        legend.text = element_text(size = 29),
        legend.title = element_text(size = 32),
        legend.title.align=0.5
  )








j <- ggdraw(d) +
  draw_label("@crisalida081", 
             fontfamily = "Monotype Corsiva", x = 0.58, y = 0.4,hjust = 0,color = "#808080",size = 22)


ggsave("GRAFICOS/CALI Y OTROS2.png", plot = j, dpi = 320, width = 85, height = 70, units = "cm")



