library(sf)
library(dplyr)
library(mapview)
library(viridisLite)
library(ggplot2)
library(tidyr)


###########################################################
#########  Figure: Number of occupied cells    ############
###########################################################

### SIKA DEER
# Data reading
deer_1978 <- st_read("./data/animal/sikadeer/sikadeer_1978.shp") %>% data.frame(.)
deer_2003 <- st_read("./data/animal/sikadeer/sikadeer_2003.shp") %>% data.frame(.)
deer_2014 <- st_read("./data/animal/sikadeer/sikadeer_2014.shp") %>% data.frame(.)

sikadeer_merged <- rbind(deer_1978, deer_2003, deer_2014) %>% mutate(animals = "sikadeer")

sikadeer_count <- sikadeer_merged %>% data.frame() %>%
  group_by(period) %>%
  summarise(count = n()) %>%
  ungroup() %>% mutate(animals = "sikadeer")

rm(deer_1978, deer_2003, deer_2014, sikadeer_merged)


### WILD BOAR
# Data reading
boar_1978 <- st_read("./data/animal/wildboar/wildboar_1978.shp") %>% data.frame(.) 
boar_2003 <- st_read("./data/animal/wildboar/wildboar_2003.shp") %>% data.frame(.)
boar_2014 <- st_read("./data/animal/wildboar/wildboar_2014.shp") %>% data.frame(.)

wildboar_merged <- rbind(boar_1978, boar_2003, boar_2014) %>% mutate(animals = "wildboar")

wildboar_count <- wildboar_merged %>% data.frame() %>%
  group_by(period) %>%
  summarise(count = n()) %>%
  ungroup() %>% mutate(animals = "wildboar")

rm(boar_1978, boar_2003, boar_2014, wildboar_merged)


### ASIATIC BLACK BEAR
# Data reading
blackbear_1978 <- st_read("./data/animal/blackbear/blackbear_1978.shp") %>% data.frame(.) 
blackbear_2003 <- st_read("./data/animal/blackbear/blackbear_2003.shp") %>% data.frame(.)
blackbear_2017 <- st_read("./data/animal/blackbear/blackbear_2017.shp") %>% data.frame(.)

blackbear_merged <- rbind(blackbear_1978, blackbear_2003, blackbear_2017) %>% mutate(animals = "blackbear")

blackbear_count <- blackbear_merged %>% data.frame() %>%
  group_by(period) %>%
  summarise(count = n()) %>%
  ungroup() %>% mutate(animals = "blackbear")

rm(blackbear_1978, blackbear_2003, blackbear_2017, blackbear_merged)


### JAPANESE SEROW
# Data reading
serow_1978 <- st_read("./data/animal/serow/serow_1978.shp") %>% data.frame(.) 
serow_2003 <- st_read("./data/animal/serow/serow_2003.shp") %>% data.frame(.)
serow_2017 <- st_read("./data/animal/serow/serow_2017.shp") %>% data.frame(.)

serow_merged <- rbind(serow_1978, serow_2003, serow_2017) %>% mutate(animals = "serow")

serow_count <- serow_merged %>% data.frame() %>%
  group_by(period) %>%
  summarise(count = n()) %>%
  ungroup() %>% mutate(animals = "serow")

rm(serow_1978, serow_2003, serow_2017, serow_merged)


### JAPANESE MACAQUE
# Data reading
macaque_1978 <- st_read("./data/animal/macaque/macaque_1978.shp") %>% data.frame(.) 
macaque_2003 <- st_read("./data/animal/macaque/macaque_2003.shp") %>% data.frame(.)
macaque_2017 <- st_read("./data/animal/macaque/macaque_2017.shp") %>% data.frame(.)

macaque_merged <- rbind(macaque_1978, macaque_2003, macaque_2017) %>% mutate(animals = "macaque")

macaque_count <- macaque_merged %>% data.frame() %>%
  group_by(period) %>%
  summarise(count = n()) %>%
  ungroup() %>% mutate(animals = "macaque")

rm(macaque_1978, macaque_2003, macaque_2017, macaque_merged)


### BROWN BEAR
# Data reading
brownbear_1978 <- st_read("./data/animal/brownbear/brownbear_1978.shp") %>% data.frame(.) 
brownbear_2003 <- st_read("./data/animal/brownbear/brownbear_2003.shp") %>% data.frame(.)
brownbear_2017 <- st_read("./data/animal/brownbear/brownbear_2017.shp") %>% data.frame(.)

brownbear_merged <- rbind(brownbear_1978, brownbear_2003, brownbear_2017) %>% mutate(animals = "brownbear")

brownbear_count <- brownbear_merged %>% data.frame() %>%
  group_by(period) %>%
  summarise(count = n()) %>%
  ungroup() %>% mutate(animals = "brownbear")

rm(brownbear_1978, brownbear_2003, brownbear_2017, brownbear_merged)



### Data merging and Figure generating
merged <- rbind(sikadeer_count, wildboar_count, blackbear_count, serow_count, macaque_count, brownbear_count) %>%
  mutate(period = as.numeric(period))

merged$animals <-
  factor(merged$animals,
         levels = c("sikadeer", "serow", "wildboar", "macaque", "blackbear", "brownbear"),
         labels = c("sika deer", "Japanese serow", "wild boar", "Japanese macaque",  "Asiatic black bear", "brown bear"))

shapes <- c("Asiatic black bear" = 15, "brown bear" = 0, "Japanese serow" = 8,
            "wild boar" = 16, "sika deer" = 17, "Japanese macaque" = 9)


ggplot(sikadeer_count, aes(x = period, y = count, linetype = animals)) +
  geom_vline(xintercept=1978, size = 0.5, alpha = 0.1) +
  geom_vline(xintercept=2003, size = 0.5, alpha = 0.1) +
  geom_vline(xintercept=2014, size = 0.5, alpha = 0.1) +
  geom_vline(xintercept=2017, size = 0.5, alpha = 0.1) +
  geom_vline(xintercept=1978, size = 5, color = "#004496", alpha = 0.1) +
  geom_vline(xintercept=2003, size = 5, color = "#46C06E", alpha = 0.1) +
  geom_vline(xintercept=2015.5, size = 14, color = "#FDE725", alpha = 0.1) + # To create line between 2014 and 2017
  geom_line(aes(group = animals), size = 0.7) +
  geom_point(size = 3) +
  scale_shape_manual(values = shapes) +
  scale_x_continuous(breaks = c(1978, 2003, 2014, 2017)) +
  theme_minimal() +
  labs(x = "Years", y = "Number of occupied grid cells") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA), 
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        legend.position = "none") + 
  theme(axis.text = element_text(size = 14),
        axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y.right = element_text(size = 18, margin = margin(l = 10))) + guides(color = guide_legend(nrow = 1))


ggsave(file = "figure3_number of occupied cells.png", dpi = 300, width = 6, height = 9)
