


pacman::p_load(readxl, dplyr, ggplot2, sf)

# DATOS


datos <- read_excel("SEGURIDAD BARRIO/Barrio seguro.xlsx")

# CAPAS


COMUNAS <- read_sf("CAPAS/shp/comunas/Comunas.shp")

BARRIOS <- read_sf("CAPAS/shp/barrios/Barrios.shp", options = "ENCODING=WINDOWS-1252")

BARRIOS <- st_as_sf(BARRIOS, crs = 4326)

CORREGIMIENTOS <- read_sf("CAPAS/shp/corregimientos/Corregimientos.shp", options = "ENCODING=WINDOWS-1252")

ZONA_EXP <- read_sf("CAPAS/shp/suelo expansion/Division_suelo_expansion.shp", options = "ENCODING=WINDOWS-1252")

#VIAS <- read_sf("CAPAS/VIAS/Jerarquizacion_vial.shp", options = "ENCODING=WINDOWS-1252")




SEGURO <- left_join(COMUNAS, datos, by = c("comuna" = "Comuna"))




# Cramos cortes y discretizamos los valores
br <- c(0, 40, 50, 60,70, 80,100) /100

SEGURO$barrio_noseguro1 <- cut(SEGURO$barrio_noseguro,
                                                 breaks = br,
                                                 dig.lab = 5)

# Creamos etiquetas personalizadas - e.g. (0k-10k]
labs <-c(0, 40, 50, 60,70, 80,100)
labs_plot <- paste0("(", labs[1:6], "%-", labs[2:7], "%]")




# FUENTES


#install.packages("extrafont")


#library(extrafont)

library(remotes)

#remotes::install_version("Rttf2pt1", version = "1.3.8")

extrafont::font_import()

# This tries to autodetect the directory containing the TrueType fonts.
# If it fails on your system, please let me know.

# Vector of font family names
extrafont::fonts()

# Show entire table
extrafont::fonttable()

# Register fonts for Windows bitmap output
extrafont::loadfonts(device="win")

pacman::p_load(ggthemes, ggplot2)


fontTable = extrafont::fonttable()

fontTable$Face = with(fontTable, ifelse(Bold & Italic, "bold.italic", 
                                        ifelse(Bold, "bold",
                                               ifelse(Italic, "italic", "plain"))))
fontTable$Face = factor(fontTable$Face, levels = c("plain","bold","italic","bold.italic"), ordered = TRUE)
fontTable$FamilyName = factor(fontTable$FamilyName, levels = rev(sort(unique(fontTable$FamilyName))), ordered = TRUE)

p = ggplot(fontTable) +
  geom_text(aes(x=Face, y=FamilyName, label=FullName, family=FamilyName, fontface=Face)) +
  labs(title="Windows Fonts in R", x=NULL, y=NULL) +
  theme_tufte() +
  theme(axis.ticks = element_blank(),
        axis.text=element_text(size=12, colour="gray40", family='Arial'),
        axis.text.x=element_text(face=c("plain","bold","italic","bold.italic")),
        plot.title=element_text(size=16, family='Arial'))

ggsave("font_ggplot_map.png", width = 10, height = 33)






















# MAPA


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

pal <- hcl.colors(6, "reds", rev = TRUE, alpha = 0.7)



d <-
  ggplot() +  geom_sf(data = SEGURO,  aes(fill = barrio_noseguro1), color = "black")+
  geom_sf_label(data = SEGURO,aes(label = comuna),size=10,
               family="Impact")+
  coord_sf(xlim=c(-76.6,-76.43),ylim=c(3.33,3.5),crs = st_crs(4326))+
  theme_void(base_family = "Iosevka", base_size = 48) +
  labs(x = NULL, y = NULL,
       title = "Santiago de Cali: ",
       subtitle = "Nivel de inseguridad por Comuna",
       caption = "Observatorio de Politicas Públicas Icesi | © CaliBRANDO")+
  # Paleta personalizada
  scale_fill_manual(values = pal,
                    drop = FALSE,
                    na.value = "grey80",
                    label = labs_plot,
                    # Legend
                    guide = guide_legend(direction = "horizontal",
                                         nrow = 1,
                                         label.position = "bottom")) +
  guides(fill = guide_legend(override.aes = list(size = 12),title="Nivel")) +
  theme(legend.position = c(0.9, 0.8), legend.direction = "vertical",
        plot.background = element_rect(fill = "white", color = NA), 
        panel.background = element_rect(fill = "white", color = NA),
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


#plot.background = element_rect(fill = "#212121", color = NA) GRAY SHADE






j <- ggdraw(align_legend(d)) +
  draw_label("@crisalida081", 
             fontfamily = "Monotype Corsiva", x = 0.58, y = 0.4,hjust = 0,color = "#808080",size = 22)


ggsave("Barrio no seguro.png", plot = j, dpi = 320, width = 85, height = 70, units = "cm")

