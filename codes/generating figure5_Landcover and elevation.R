library(sf)
library(dplyr)
library(mapview)
library(viridisLite)
library(ggplot2)
library(tidyr)


###########################################################
## Figure: Proportion of major land cover and elevation  ##
###########################################################

### Data reading

# SIKA DEER
deer_1978 <- st_read("./data/animal/sikadeer/sikadeer_1978.shp") %>% data.frame(.)
deer_2003 <- st_read("./data/animal/sikadeer/sikadeer_2003_new.shp") %>% data.frame(.)
deer_2014 <- st_read("./data/animal/sikadeer/sikadeer_2014_new.shp") %>% data.frame(.)


# WILD DEER
boar_1978 <- st_read("./data/animal/wildboar/wildboar_1978.shp") %>% data.frame(.)
boar_2003 <- st_read("./data/animal/wildboar/wildboar_2003_new.shp") %>% data.frame(.)
boar_2014 <- st_read("./data/animal/wildboar/wildboar_2014_new.shp") %>% data.frame(.)


# ASIATIC BLACK BEAR
blackbear_1978 <- st_read("./data/animal/blackbear/blackbear_1978.shp") %>% data.frame(.)
blackbear_2003 <- st_read("./data/animal/blackbear/blackbear_2003_new.shp") %>% data.frame(.)
blackbear_2017 <- st_read("./data/animal/blackbear/blackbear_2017_new.shp") %>% data.frame(.)


# JAPANESE SEROW
serow_1978 <- st_read("./data/animal/serow/serow_1978.shp") %>% data.frame(.)
serow_2003 <- st_read("./data/animal/serow/serow_2003_new.shp") %>% data.frame(.)
serow_2017 <- st_read("./data/animal/serow/serow_2017_new.shp") %>% data.frame(.)


# JAPANESE MACAQUE
macaque_1978 <- st_read("./data/animal/macaque/macaque_1978.shp") %>% data.frame(.)
macaque_2003 <- st_read("./data/animal/macaque/macaque_2003_new.shp") %>% data.frame(.)
macaque_2017 <- st_read("./data/animal/macaque/macaque_2017_new.shp") %>% data.frame(.)


# BROWN BEAR
brownbear_1978 <- st_read("./data/animal/brownbear/brownbear_1978.shp") %>% data.frame(.)
brownbear_2003 <- st_read("./data/animal/brownbear/brownbear_2003_new.shp") %>% data.frame(.)
brownbear_2017 <- st_read("./data/animal/brownbear/brownbear_2017_new.shp") %>% data.frame(.)


### Data merging and Land cover and elevation combining
all_species_1978 <- rbind(deer_1978, boar_1978, blackbear_1978, serow_1978, macaque_1978, brownbear_1978)
all_species_2003 <- rbind(deer_2003, boar_2003, blackbear_2003, serow_2003, macaque_2003, brownbear_2003)
all_species_2010s <- rbind(deer_2014, boar_2014, blackbear_2017, serow_2017, macaque_2017, brownbear_2017)


all_species_1978 <- all_species_1978 %>%
  left_join(., landuse76, by = join_by(mesh_code)) %>%
  left_join(., topo, by = join_by(mesh_code))

all_species_2003 <- all_species_2003 %>%
  left_join(., landuse06, by = join_by(mesh_code)) %>%
  left_join(., topo, by = join_by(mesh_code))

all_species_2010s <- all_species_2010s %>%
  left_join(., landuse14, by = join_by(mesh_code)) %>%
  left_join(., topo, by = join_by(mesh_code)) 


all_species_periods <- rbind(all_species_1978, all_species_2003, all_species_2010s) %>%
  rename(Forest = FO, Agriculture = AG, Development = HU, Elevation = ELEmean)


all_species_periods_avg <- all_species_periods %>%
  pivot_longer(cols = c(Agriculture, Forest, Development, Elevation),
               names_to = "type",
               values_to = "value")


### Average calculation for land use type and elevation, and Figure generating
all_species_periods_avg <- all_species_periods_avg %>%
  group_by(period, type, animals) %>%
  summarise(mean_value = mean(value, na.rm = TRUE)) %>%
  ungroup()
all_species_periods_avg <- all_species_periods_avg %>% mutate(period = as.numeric(period))


#Label, color, and shape
all_species_periods_avg$animals <-
  factor(all_species_periods_avg$animals,
         levels = c("sikadeer", "serow", "wildboar", "macaque", "blackbear", "brownbear"),
         labels = c("sika deer", "Japanese serow", "wild boar", "Japanese macaque",  "Asiatic black bear", "brown bear"))

colors <- c("Forest" = "#006b4e", "Agriculture" = "#e69f00", "Development" = "#BBBBBB")
shapes <- c("Asiatic black bear" = 15, "brown bear" = 0, "Japanese serow" = 8,
            "wild boar" = 16, "sika deer" = 17, "Japanese macaque" = 9)
filter(ratio_clas != 'less_half')


#Figure generating: Land cover
ggplot(all_species_periods_avg %>% filter(type != "Elevation"),
       aes(x = period, y = mean_value * 100, color = type, shape = animals, linetype = animals)) +
  geom_vline(xintercept=1978, size = 0.5, alpha = 0.1) +
  geom_vline(xintercept=2003, size = 0.5, alpha = 0.1) +
  geom_vline(xintercept=2014, size = 0.5, alpha = 0.1) +
  geom_vline(xintercept=2017, size = 0.5, alpha = 0.1) +
  geom_vline(xintercept=1978, size = 5, color = "#004496", alpha = 0.1) +
  geom_vline(xintercept=2003, size = 5, color = "#46C06E", alpha = 0.1) +
  geom_vline(xintercept=2015.5, size = 10, color = "#FDE725", alpha = 0.1) + # To create line between 2014 and 2017
  geom_line(aes(group = interaction(type, animals)), size = 1, alpha = 0.4) +
  geom_point(size = 3, alpha = 0.6) +
  scale_color_manual(values = colors, breaks = c("Forest", "Agriculture", "Development")) +
  scale_shape_manual(values = shapes) +
  scale_x_continuous(breaks = c(1978, 2003, 2014, 2017)) +
  theme_minimal() +
  labs(x = "Years", y = "Mean percentage of cover area in the grid cells") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA), 
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        legend.position = "none") + 
  theme(axis.text = element_text(size = 14),
        axis.title.y = element_text(size = 18, margin = margin(r = 10)),
        axis.title.x = element_text(size = 18)) + guides(color = guide_legend(nrow = 1))

ggsave(file = "figure4_land cover.png", dpi = 300, width = 4.5, height = 9)


#Figure generating: Elevation
ggplot(all_species_topo %>% filter(type == "Elevation"),
       aes(x = period, y = ELE_mean, shape = animals, linetype = animals)) +
  geom_vline(xintercept=1978, size = 0.5, alpha = 0.1) +
  geom_vline(xintercept=2003, size = 0.5, alpha = 0.1) +
  geom_vline(xintercept=2014, size = 0.5, alpha = 0.1) +
  geom_vline(xintercept=2017, size = 0.5, alpha = 0.1) +
  geom_vline(xintercept=1978, size = 5, color = "#004496", alpha = 0.1) +
  geom_vline(xintercept=2003, size = 5, color = "#46C06E", alpha = 0.1) +
  geom_vline(xintercept=2015.5, size = 10, color = "#FDE725", alpha = 0.1) +
  geom_line(aes(group = animals), size = 1, alpha = 0.4) +
  geom_point(size = 3, alpha = 0.6) +
  scale_shape_manual(values = shapes) +
  scale_x_continuous(breaks = c(1978, 2003, 2014, 2017)) +
  theme_minimal() +
  labs(x = "Years", y = NULL) +
  scale_y_continuous(sec.axis = dup_axis(name = "Mean elevation (m) in the grid cells")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y.left = element_blank(),
        axis.ticks.y.left = element_blank(),
        axis.title.y.left = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA), 
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        legend.position = "none") + 
  theme(axis.text = element_text(size = 14),
        axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y.right = element_text(size = 18, margin = margin(l = 10))) +
  guides(shape = FALSE) + guides(linetype = FALSE) + guides(color = guide_legend(nrow = 1))


ggsave(file = "figure4_elevation.png", dpi = 300, width = 4.5, height = 9)

