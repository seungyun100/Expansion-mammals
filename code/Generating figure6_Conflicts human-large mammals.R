library(reshape2)
library(ggplot2)
library(dplyr)
library(gtable)
library(grid)

###########################################################
##### Figure: Example human-large mammals Conflicts  ######
###########################################################

#Panel 1#
##### Example of Crop damage 

#Data reading
crop_damage <- read.csv("./data/society_impact/crop_damage.csv") %>% mutate(Amount = Amount *0.001) # Amount convert to Million Yen

### Crop damage data of sika deer 
crop_damage_deer <- crop_damage %>% filter(Species == "deer")

# Data by Prefecture
df_deer_Hokkaido <- subset(crop_damage_deer, Prefecture == "Hokkaido")
df_deer_Iwate <- subset(crop_damage_deer, Prefecture == "Iwate")

# Generate figure for Crop damage of sika deer
p1 <- ggplot(df_deer_Iwate, aes(x = Years, y = Amount)) +
  geom_line(color = "#E8C615", size = 1.5) +
  ggtitle("Amount of crop damage\n") +
  labs(x=NULL,y=NULL) +
  scale_x_continuous(breaks= c(2003, 2005, 2010, 2014), expand = c(0, 0), limits = c(2002.7, 2014.3)) +
  scale_y_continuous(breaks= c(0, 20, 40, 60), expand = c(0, 0), limits = c(0,61.09)) +
  theme(
    panel.background = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_line(color = "gray50", size = 0.5),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 22),
    axis.text.y = element_text(colour="#E8C615", size = 14),
    axis.text.x = element_text(size = 14),
    axis.ticks = element_line(colour = 'gray50'),
    axis.ticks.length = unit(.25, "cm"),
    axis.ticks.x = element_line(colour = "black"),
    axis.ticks.y = element_blank()    )
p1

p2 <- ggplot(df_deer_Hokkaido, aes(x = Years, y = Amount)) + 
  geom_line(colour = "#00A4E6", size = 1.5) +  
  ggtitle("Amount of crop damage\n") +
  labs(x=NULL,y=NULL) +
  scale_x_continuous(breaks= c(2003, 2005, 2010, 2014), expand = c(0, 0), limits = c(2002.7, 2014.3)) +
  scale_y_continuous(breaks= c(0, 200, 400, 600), expand = c(0, 0), limits = c(0,610.9)) +
  theme(
    panel.background = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.y = element_text(colour="#00A4E6", size=14),
    axis.text.x = element_text(size = 14),
    axis.ticks = element_line(colour = 'gray50'),
    axis.ticks.length = unit(.25, "cm"),
    axis.ticks.x = element_line(colour = "black"),
    axis.ticks.y = element_blank())
p2


# make gtable objects from ggplot objects
# gtable object shows how grobs are put together to form a ggplot
g1 <- ggplot_gtable(ggplot_build(p1))
g2 <- ggplot_gtable(ggplot_build(p2))

# get the location of the panel of p1 
# so that the panel of p2 is positioned correctly on top of it
pp <- c(subset(g1$layout, name == "panel", se = t:r))

# superimpose p2 (the panel) on p1
g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                     pp$l, pp$b, pp$l)

# extract the y-axis of p2
ia <- which(g2$layout$name == "axis-l")
ga <- g2$grobs[[ia]]
ax <- ga$children[[2]]

# flip it horizontally
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)

# add the flipped y-axis to the right
g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)

unit_label <- textGrob(
  "(million JPY)", 
  x = unit(1, "npc") - unit(6, "mm"),      
  y = unit(1, "npc") - unit(10, "mm"),    
  just = c("right", "top"),
  gp = gpar(fontsize = 14, col = "gray40")
)

combined <- grobTree(
  g,            
  unit_label   
)

grid.newpage()
png("crop_damage_sikadeer.png", width = 4, height = 5, units = "in", res = 300)
grid.draw(combined)
dev.off()

################################################################################


#Panel 2#
##### Crop damage data of wild boar
crop_damage_boar <- crop_damage %>% filter(Species == "boar")

# Data by Prefecture
df_boar_Chiba <- subset(crop_damage_boar, Prefecture == "Chiba")
df_boar_Tochigi <- subset(crop_damage_boar, Prefecture == "Tochigi")

# Generate figure for Crop damage of wild boar
p1 <- ggplot(df_boar_Chiba, aes(x = Years, y = Amount)) +
  geom_line(color = "#349292", size = 1.5) +
  ggtitle("Amount of crop damage\n") +
  labs(x=NULL,y=NULL) +
  scale_x_continuous(breaks= c(2003, 2005, 2010, 2014), expand = c(0, 0), limits = c(2002.7, 2014.3)) +
  scale_y_continuous(breaks= c(0, 5, 10, 15, 20), expand = c(0, 0), limits = c(0,21.09)) +
  theme(
    panel.background = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_line(color = "gray50", size = 0.5),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 22),
    axis.text.y = element_text(colour="#349292", size = 14),
    axis.text.x = element_text(size = 14),
    axis.ticks = element_line(colour = 'gray50'),
    axis.ticks.length = unit(.25, "cm"),
    axis.ticks.x = element_line(colour = "black"),
    axis.ticks.y = element_blank()    )
p1

p2 <- ggplot(df_boar_Tochigi, aes(x = Years, y = Amount)) + 
  geom_line(colour = "#8B2180", size = 1.5) +  
  ggtitle("Amount of crop damage\n") +
  labs(x=NULL,y=NULL) +
  scale_x_continuous(breaks= c(2003, 2005, 2010, 2014), expand = c(0, 0), limits = c(2002.7, 2014.3)) +
  scale_y_continuous(breaks= c(0, 5, 10, 15, 20), expand = c(0, 0), limits = c(0,21.9)) +
  theme(
    panel.background = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.y = element_text(colour="#8B2180", size=14),
    axis.text.x = element_text(size = 14),
    axis.ticks = element_line(colour = 'gray50'),
    axis.ticks.length = unit(.25, "cm"),
    axis.ticks.x = element_line(colour = "black"),
    axis.ticks.y = element_blank())
p2


# make gtable objects from ggplot objects
# gtable object shows how grobs are put together to form a ggplot
g1 <- ggplot_gtable(ggplot_build(p1))
g2 <- ggplot_gtable(ggplot_build(p2))

# get the location of the panel of p1 
# so that the panel of p2 is positioned correctly on top of it
pp <- c(subset(g1$layout, name == "panel", se = t:r))

# superimpose p2 (the panel) on p1
g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                     pp$l, pp$b, pp$l)

# extract the y-axis of p2
ia <- which(g2$layout$name == "axis-l")
ga <- g2$grobs[[ia]]
ax <- ga$children[[2]]

# flip it horizontally
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)

# add the flipped y-axis to the right
g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)

unit_label <- textGrob(
  "(million JPY)", 
  x = unit(1, "npc") - unit(6, "mm"),      
  y = unit(1, "npc") - unit(10, "mm"),    
  just = c("right", "top"),
  gp = gpar(fontsize = 14, col = "gray40")
)

combined <- grobTree(
  g,            
  unit_label  
)

grid.newpage()
png("crop_damage_wildboar.png", width = 4, height = 5, units = "in", res = 300)
grid.draw(combined)
dev.off()

################################################################################

#Panel 3#
##### Example of vehicle collision of Sika deer
car_collision <- read.csv("./data/society_impact/sikadeer_car_collision.csv") %>% mutate(type = "car")
train_collision <- read.csv("./data/society_impact/sikadeer_train_collision.csv") %>% mutate(type = "train")
collisions <- rbind(car_collision, train_collision)

# Generate figure for vehicle collision of Sika deer
p <- ggplot(collisions, aes(x = years, y = numbers, group = type)) +
  geom_col(data = filter(collisions, type == "train"),
           aes(fill = type), width = 0.9, alpha = 0.5) +
  geom_line(data = filter(collisions, type == "car"),
            aes(color = type), size = 2) +
  scale_fill_manual(values = c("train" = "#4BD0FF")) +
  scale_color_manual(values = c("car" = "blue")) +
  labs(title = "Number of collision", x = NULL, y = NULL) +
  scale_x_continuous(breaks = c(1987, 1990, 2000, 2010, 2014), expand = c(0, 0), limits = c(1986, 2015)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0,2505)) +
  theme_bw(base_size = 13) +
  theme(
    panel.border = element_blank(),
    panel.background = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_line(color = "gray50", size = 0.5),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 22),
    axis.text.y = element_text(colour="black", size = 12, margin = margin(r = 0)),
    axis.text.x = element_text(size = 12),
    axis.ticks = element_line(colour = 'gray50'),
    axis.ticks.length = unit(.25, "cm"),
    axis.ticks.x = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    legend.position = c(0.03, 0.99),
    legend.justification = c("left", "top"),
    legend.title = element_blank(),
    legend.key.height = unit(0.3, "cm"),
    legend.spacing.y = unit(0.1, "cm"),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.background = element_rect(fill = "transparent", color = NA)
  )
p

ggsave("number_of_collision.png", plot = p, width = 4, height = 5, dpi = 300)

################################################################################

#Panel 4#
##### Human injuries caused by Asiatic black bears
injuries <- read.csv("./data/society_impact/blackbear_damage.csv")

p <- ggplot(injuries, aes(x = years, y = numbers)) +
  geom_line(size = 1.5) +
  labs(title = "Number of human injury\n", x = NULL, y = NULL) +
  scale_x_continuous(breaks = c(1980, 1990, 2000, 2010, 2017), expand = c(0, 0), limits = c(1979, 2018)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0,151)) +
  theme_bw(base_size = 13) +
  theme(
    panel.border = element_blank(),
    panel.background = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_line(color = "gray50", size = 0.5),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 22),
    axis.text.y = element_text(colour="black", size = 14),
    axis.text.x = element_text(size = 14),
    axis.ticks = element_line(colour = 'gray50'),
    axis.ticks.length = unit(.25, "cm"),
    axis.ticks.x = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    legend.position = c(0.03, 0.99),
    legend.justification = c("left", "top"),
    legend.title = element_blank(),
    legend.key.height = unit(0.3, "cm"),
    legend.spacing.y = unit(0.1, "cm"),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.background = element_rect(fill = "transparent", color = NA)
  )
p

ggsave("number_of_injury.png", plot = p, width = 4, height = 5, dpi = 300)
