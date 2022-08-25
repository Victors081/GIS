



pacman::p_load(sf, dplyr, ggplot2, writexl, readxl,ggspatial)



muni <- read_sf("CAPAS/shp/COLOMBIA/MUNICIPIO/MGN_MPIO_POLITICO.shp")


nombres <- c("BUENAVENTURA","CALI","CANDELARIA","JAMUNDÍ","PALMIRA","YUMBO")



sort(valle$MPIO_CNMBR)






valle <- muni %>% filter(DPTO_CNMBR == "VALLE DEL CAUCA")


class(valle)

x <- valle %>% select(MPIO_CNMBR,MPIO_CDPMP)


write_xlsx(x,"MUNICIPIOS/municipios valle.xlsx")




# proceso en excel


municipios <- read_excel("MUNICIPIOS/municipios valle.xlsx")



municipios <- municipios %>% select(MPIO_CDPMP, REGION) %>% left_join(valle, by="MPIO_CDPMP")


municipios <- municipios %>% st_sf(sf_column_name = 'geometry')



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


extrafont::font_import()

extrafont::fonts()

extrafont::fonttable()

extrafont::loadfonts(device="win")





  d <-
  ggplot()+  geom_sf(data = muni, fill = "white", colour = "black") + 
  geom_sf(data = municipios,  aes(fill = REGION), colour = "black")+
  coord_sf(xlim=c(-78.2,-75.6),ylim=c(3.0,5.1),crs = st_crs(4326))+
  theme_void(base_family = "serif", base_size = 48) +
  labs(x = NULL, y = NULL,
       title = "Regiones del Valle del Cauca \n",
       caption = "Observatorio de Politicas Públicas ICESI | Dirección de análisis y métodos")+
    guides(fill = guide_legend(override.aes = list(size = 12),title="")) +
  theme(legend.position = c(0.1, 0.8), legend.direction = "vertical",
        plot.background = element_rect(fill = "white", color = NA), 
        panel.background = element_rect(fill = "#51d1f6", color = NA),
        #legend.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", colour = NA),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        text =  element_text(color = "black"),
        title =  element_text(color = "black"),
        plot.caption = element_text(size = 28, face = "italic"),
        plot.title = element_text(size = 39),
        plot.subtitle = element_text(size = 32),
        legend.text = element_text(size = 29, family = "serif"),
        legend.title = element_text(size = 29),
        legend.title.align=0.5,
        legend.margin = margin(1,25,5,15, unit="mm"),
        legend.background = element_rect(fill = "white",color = "red"),  ### SEE HERE ###
        legend.box.background = element_rect(color = "black")
        
  )+
    ggspatial::annotation_scale(
      location = "bl",
      bar_cols = c("grey60", "white"),
      text_family = "ArcherPro Book"
    ) +
    ggspatial::annotation_north_arrow(
      location = "bl", which_north = "true",
      pad_x = unit(1.4, "in"), pad_y = unit(1, "in"),
      style = ggspatial::north_arrow_nautical(
        fill = c("grey40", "white"),
        line_col = "grey20",
        text_family = "ArcherPro Book"
      ),height = unit(10, "cm"),
      width = unit(10, "cm")
    )



  




j <- ggdraw(align_legend(d)) +
  draw_label("@crisalida081", 
             fontfamily = "Monotype Corsiva", x = 0.58, y = 0.4,hjust = 0,color = "#808080",size = 22)


ggsave("GRAFICOS/REGIONES VALLE.png", plot = j, dpi = 320, width = 85, height = 70, units = "cm")


