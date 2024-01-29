library(dplyr)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(sf)
library(assertthat)
library(purrr)
library(igraph)
library(ggraph)
library(ggmap)
library(ggrepel)
library(ggpubr)
library(viridis)

setwd("C:/Users/33677/Documents/NEW_WORK/Congrès/ECE11 2023/data")
stylo <- read.table("stylophoran material.txt", sep="\t", row.names = 1, h=T, stringsAsFactors = TRUE)


## Google search ####
google.scholar <- read.table("Google_search.txt", sep="\t", row.names = 1, h=T, stringsAsFactors = TRUE)
jpeg(filename = "Google_search.jpeg", width=12, height=6, units="cm", res=300, pointsize = 8)
par()
barplot(t(google.scholar), beside = TRUE, col=viridis(3),
        ylim=c(0,700), names.arg = rownames(google.scholar),
        ylab="Number of publications", xlab="Time",
        main="Evolution of the use of terms (Google Scholar)",
        legend.text = TRUE, args.legend = list(bty="n", x="topleft", ncol=1))
dev.off()


## Mapping ####
  ## Prepare the layout
maptheme <-
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(legend.position = "bottom",
        legend.title=element_text(size = 8),
        legend.text=element_text(size = 8),
        legend.key.height= unit(0.5, 'cm'),
        legend.key.width= unit(0.5, 'cm')
  ) +
  theme(panel.grid = element_blank()) +
  theme(panel.background = element_blank()) +
  theme(plot.margin = unit(c(0, 0, 0.5, 0), 'cm'))
country_shapes <- geom_polygon(data = map_data('world'),
                               aes(x = long, y = lat, group = group),
                               fill = "white", color = "grey80",
                               linewidth = 0.15)
mapcoords <- ggplot2::coord_fixed(xlim = c(-150, 180), ylim = c(-55, 80))


  ## location and origin of holotypes
stylo.holotype.origin <- as.data.frame(table(stylo$origin, dnn = list("geounit")), responseName = "Nbr_of_holotypes")
stylo.holotype.collection <- as.data.frame(table(stylo$collection, dnn = list("geounit")), responseName = "Nbr_of_holotypes")

  ## Get mean location of world countries
world.data <- ggplot2::map_data("world") %>% dplyr::select("long","lat","region") %>% split(., .$region, drop=TRUE)
world.data.mean.list <- lapply(world.data, function(data){
  data %>% dplyr::select(-region) %>% droplevels() %>% geosphere::geomean(.)
})
world.data.mean <- as.data.frame(do.call(rbind, world.data.mean.list),
                                 row.names = names(world.data.mean.list))

  ## Get edges between origin and location of the holotypes
edgelist <- as.data.frame(table(stylo %>% select(origin, collection)), responseName = "link", stringsAsFactors = F) %>%
  filter(., !link==0) %>%
  .[!.$origin==.$collection,] %>%
  arrange(origin)

  ## Prepare the network
g <- igraph::graph_from_data_frame(edgelist, directed = TRUE)

nodes <- data.frame(lon=world.data.mean[V(g)$name,"x"],
                    lat=world.data.mean[V(g)$name,"y"],
                    country=V(g)$name)

edgelist_for_plot <- edgelist %>%
  dplyr::inner_join(nodes, by = c('origin' = 'country')) %>%
  dplyr::rename(x = lon, y = lat) %>%
  dplyr::inner_join(nodes, by = c('collection' = 'country')) %>%
  dplyr::rename(xend = lon, yend = lat)
assertthat::assert_that(nrow(edgelist_for_plot) == nrow(edgelist))

country.origin <- stylo.holotype.origin %>% 
  #dplyr::mutate(geounit=dplyr::recode(geounit, "United Kingdom"="UK", "United States of America"="USA")) %>%
  dplyr::inner_join(., ggplot2::map_data(map="world"), by=c("geounit"="region"))

country.collection <- stylo.holotype.collection %>% 
  #dplyr::mutate(geounit=dplyr::recode(geounit, "United Kingdom"="UK", "United States of America"="USA")) %>%
  dplyr::inner_join(., ggplot2::map_data(map="world"), by=c("geounit"="region"))

  ## Plot the map and network
ggholotypes <- 
ggplot(nodes) + country_shapes +
  geom_polygon(data = country.origin,
               aes(x = long, y = lat, group = group, alpha = Nbr_of_holotypes),
               fill = "goldenrod", color = "black", size = 0.1)+
  scale_alpha(range=c(0.1,1), breaks=c(1,5,10,20,30), name="Origined holotypes")+
  geom_curve(data = edgelist_for_plot,
             aes(x = x, y = y, xend = xend, yend = yend, color = link),
             size = 1, curvature = 0.1,
             arrow=arrow(angle=30, length = unit(0.1, "inches"), type = "open")
             )+
  scale_color_gradient(low="lightskyblue", high="darkblue", name="Moved holotypes")+
  geom_point(aes(x = lon, y = lat), 
             shape = 20, size = 2, color = 'black') +
  geom_text_repel(aes(x = lon, y = lat, label = country),
                  size = 2, color = "grey30", fontface = "bold") +
  mapcoords + maptheme

ggsave(file="holotypes.jpeg", plot=ggholotypes, width=16, height=9, units="cm")


  ## Time-dependant plot
svg(filename = "barplot local.svg", width=7, height=1.75, pointsize = 8)
hist(stylo$year, right=FALSE,
     xlab="", ylab="Number of described holotypes", main="",
     breaks=seq(1850,2025,25), xlim=range(1850,2050), ylim=range(0,60), labels = TRUE)
dev.off()

stylo <- stylo%>%mutate(year_interval = case_when((year<1925 ~ "before 1925"), 
                                                  (year>=1925&year<1975 ~ "1925-1974"), 
                                                  (year>=1975&year<2000 ~ "1975-1999"),
                                                  (year>=2000&year<2025 ~ "since 2000")))
## Local co-authoring ####
stylo.national.data <- 
  stylo %>% dplyr::select(national.authors, year_interval, origin) %>% unique() %>% split(., .$year_interval)
plot.national <- function(data){
  plot.oritime <- 
    ggplot() + country_shapes + maptheme + mapcoords+
    geom_polygon(
      data = ggplot2::map_data(map="world", region = data$origin),
      aes(x = long, y = lat, group = group),
      fill = "goldenrod", color = "black", size = 0.1)+
    labs(title = data$year_interval)+
    theme(plot.title = element_text(size=6))
  
  national.data <- as.data.frame(table(data$national.authors))
  pie.nat <-
    ggplot(data=national.data, 
           aes(x="", y=Freq, fill=Var1)) +
    geom_bar(stat="identity", width=1) +
    geom_text(aes(label = Var1), size=1,
              position = position_stack(vjust = 0.5)) +
    coord_polar("y", start=0) +
    theme_nothing()
  
  plot.oritime + patchwork::inset_element(pie.nat, 
                                          left = 0.03, bottom = -0.1, right = 0.23, top = .7,
                                          align_to = "full")
}
# plot.national(stylo.national.data$`1925-1974`)
plot.natoritime <- lapply(stylo.national.data, plot.national)
plot.natoritime.list <- ggpubr::ggarrange(plotlist = plot.natoritime, ncol = 1, nrow = 4)
ggsave(filename = "plot natoritime.svg", plot = plot.natoritime.list, 
       device = "svg", height = 13, units = "cm")

national.data.1 <- as.data.frame(table(national.data$`before 1925`$national.authors))
# Basic piechart
pie.1 <- 
  ggplot(data=national.data.1, 
       aes(x="", y=Freq, fill=Var1)) +
  geom_bar(stat="identity", width=1) +
  geom_text(aes(label = Var1),
            position = position_stack(vjust = 0.5)) +
  coord_polar("y", start=0) +
  theme_nothing()
pie.1

plot.1850 + patchwork::inset_element(pie.1, left = 0, bottom = -0.1, right = 0.25, top = .7,
                                     align_to = "full")