rm(list=ls())
# Load required libraries
library(sf)
library(ggplot2)
library(scatterpie)
library(rnaturalearth)
library(dplyr)


lags <- read_csv("./data/africa_lags.csv") %>% drop_na()
lags <- reshape2::dcast(country+iso_a3 ~ whichlag, value.var= "pct", data=lags)

africa <- ne_countries(continent = "Africa", returnclass = "sf") %>%
  mutate(geometry_centroid = st_centroid(geometry),
           X = as.numeric(st_coordinates(geometry_centroid)[, 1]),
           Y = as.numeric(st_coordinates(geometry_centroid)[, 2]))

lags_sp <- as.data.frame(africa %>% select(iso_a3, X, Y ))[1:3]
africa_lags <- left_join(lags_sp, lags, by="iso_a3")

newcolors = c("#ffcba4", "#ca8af2", "#84d7bb")

ggplot() +
  geom_sf(data = africa, fill = NA, color = "black") +  
  geom_scatterpie(data=africa_lags,
    aes(x = X, y = Y),
    cols = c("yield", "yield_lag", "yield_lead"),
    color = NA,
    alpha = 0.7,
    pie_scale = 1  
  ) +  scale_fill_manual(values = newcolors) +
  theme_void() 
ggsave("./plots/africa_lags_piechart.pdf", width=5.7, height=6.3)

#####################

# Aggregate geometries by subregion
africa_subregion <- ne_countries(continent = "Africa", returnclass = "sf") %>%  select(iso_a3, subregion, geometry) %>% distinct() %>% 
  group_by(subregion) %>%  mutate(geometry_reg = st_union(geometry)) %>% 
  mutate(centroid = st_centroid(geometry_reg),
    X = st_coordinates(centroid)[, 1],
    Y = st_coordinates(centroid)[, 2])

lags_sp_reg <- as.data.frame(africa_subregion %>% select(iso_a3, subregion, X, Y ))[1:4]
africa_lags_reg <- left_join(lags_sp_reg, lags, by="iso_a3") %>% group_by(subregion, X, Y) %>%
  summarise(yield = mean(yield, na.rm=T), 
            yield_lag = mean(yield_lag, na.rm=T),
            yield_lead = mean(yield_lead, na.rm=T))

ggplot() +
  geom_sf(data = africa, aes(fill=subregion), alpha=0.5, color = NA) +  
  scale_fill_manual(values= c("gray0", "gray20", "gray40", "gray65", "gray85"))+
  ggnewscale::new_scale_fill()  + 
  geom_scatterpie(data=africa_lags_reg,
                  aes(x = X, y = Y),
                  cols = c("yield", "yield_lag", "yield_lead"),
                  color = NA,
                  pie_scale = 4  
  ) +  scale_fill_manual(values=newcolors)+
  theme_void() 

ggsave("./plots/africa_regions_piechart.pdf", width=5.7, height=6.3)
