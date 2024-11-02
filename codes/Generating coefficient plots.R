
library(dplyr)
library(ggplot2)

###########################################################
##### Extract coefficients from the occupation models #####
###########################################################


### Coefficients of occupation models for Sika deer in Honshu, Kyushu, and Shikoku (hks)

Result_sikadeer_hks_p1p2_fit <- as.data.frame(cbind(summary(sikadeer_hks_p1p2_fit)$coef,
                                                    confint(sikadeer_hks_p1p2_fit))) %>%
  rename(., "p_value"="Pr(>|z|)") %>% 
  mutate(species = "sikadeerhks", period = "p1_p2", 
         significance = ifelse(p_value <= 0.05, 'sig', 'insig'), variables = row.names(.)) %>%
  rename(., "low95" = "2.5 %", "upp95" = "97.5 %") %>%
  mutate(variables = case_when(
    variables == "scale(AG)" ~ "Area of agricultural land",
    variables == "scale(adjacent_counts)" ~ "Number of adjacent cells occupied",
    variables == "scale(TRImean)" ~ "Terrain ruggedness index",
    variables == "scale(aband)" ~ "Area of abandoned agricultural land",
    variables == "scale(snow)" ~ "Maximum snow depth",
    variables == "scale(NL2003)" ~ "Artificial light at night",
    TRUE ~ variables )) %>%
  mutate(variables = factor(variables, levels = c("Number of adjacent cells occupied",
                                                  "Terrain ruggedness index",
                                                  "Maximum snow depth",
                                                  "Area of abandoned agricultural land",
                                                  "Area of agricultural land",
                                                  "Artificial light at night")))

Result_sikadeer_hks_p2p3_fit <- as.data.frame(cbind(summary(sikadeer_hks_p2p3_fit)$coef,
                                                    confint(sikadeer_hks_p2p3_fit))) %>%
  rename(., "p_value"="Pr(>|z|)") %>% 
  mutate(species = "sikadeerhks", period = "p2_p3", 
         significance = ifelse(p_value <= 0.05, 'sig', 'insig'), variables = row.names(.)) %>%
  rename(., "low95" = "2.5 %", "upp95" = "97.5 %") %>%
  mutate(variables = case_when(
    variables == "scale(AG)" ~ "Area of agricultural land",
    variables == "scale(adjacent_counts)" ~ "Number of adjacent cells occupied",
    variables == "scale(TRImean)" ~ "Terrain ruggedness index",
    variables == "scale(aband)" ~ "Area of abandoned agricultural land",
    variables == "scale(snow)" ~ "Maximum snow depth",
    variables == "scale(NL2014)" ~ "Artificial light at night",
    TRUE ~ variables )) %>%
  mutate(variables = factor(variables, levels = c("Number of adjacent cells occupied",
                                                  "Terrain ruggedness index",
                                                  "Maximum snow depth",
                                                  "Area of abandoned agricultural land",
                                                  "Area of agricultural land",
                                                  "Artificial light at night")))



### Coefficients of occupation models for Sika deer in Hokkaido (hok)

Result_sikadeer_hok_p1p2_fit <- as.data.frame(cbind(summary(sikadeer_hok_p1p2_fit)$coef,
                                                    confint(sikadeer_hok_p1p2_fit))) %>%
  rename(., "p_value"="Pr(>|z|)") %>% 
  mutate(species = "sikadeerhok", period = "p1_p2", 
         significance = ifelse(p_value <= 0.05, 'sig', 'insig'), variables = row.names(.)) %>%
  rename(., "low95" = "2.5 %", "upp95" = "97.5 %") %>%
  mutate(variables = case_when(
    variables == "scale(AG)" ~ "Area of agricultural land",
    variables == "scale(adjacent_counts)" ~ "Number of adjacent cells occupied",
    variables == "scale(TRImean)" ~ "Terrain ruggedness index",
    variables == "scale(aband)" ~ "Area of abandoned agricultural land",
    variables == "scale(snow)" ~ "Maximum snow depth",
    variables == "scale(NL2003)" ~ "Artificial light at night",
    TRUE ~ variables )) %>%
  mutate(variables = factor(variables, levels = c("Number of adjacent cells occupied",
                                                  "Terrain ruggedness index",
                                                  "Maximum snow depth",
                                                  "Area of abandoned agricultural land",
                                                  "Area of agricultural land",
                                                  "Artificial light at night")))

Result_sikadeer_hok_p2p3_fit <- as.data.frame(cbind(summary(sikadeer_hok_p2p3_fit)$coef,
                                                    confint(sikadeer_hok_p2p3_fit))) %>%
  rename(., "p_value"="Pr(>|z|)") %>% 
  mutate(species = "sikadeerhok", period = "p2_p3", 
         significance = ifelse(p_value <= 0.05, 'sig', 'insig'), variables = row.names(.)) %>%
  rename(., "low95" = "2.5 %", "upp95" = "97.5 %") %>%
  mutate(variables = case_when(
    variables == "scale(AG)" ~ "Area of agricultural land",
    variables == "scale(adjacent_counts)" ~ "Number of adjacent cells occupied",
    variables == "scale(TRImean)" ~ "Terrain ruggedness index",
    variables == "scale(aband)" ~ "Area of abandoned agricultural land",
    variables == "scale(snow)" ~ "Maximum snow depth",
    variables == "scale(NL2014)" ~ "Artificial light at night",
    TRUE ~ variables )) %>%
  mutate(variables = factor(variables, levels = c("Number of adjacent cells occupied",
                                                  "Terrain ruggedness index",
                                                  "Maximum snow depth",
                                                  "Area of abandoned agricultural land",
                                                  "Area of agricultural land",
                                                  "Artificial light at night")))



### Coefficients of occupation models for wild boar

Result_wildboarp1p2_fit <- as.data.frame(cbind(summary(wildboarp1p2_fit)$coef,
                                               confint(wildboarp1p2_fit))) %>%
  rename(., "p_value"="Pr(>|z|)") %>% 
  mutate(species = "wildboar", period = "p1_p2", 
         significance = ifelse(p_value <= 0.05, 'sig', 'insig'), variables = row.names(.)) %>%
  rename(., "low95" = "2.5 %", "upp95" = "97.5 %") %>%
  mutate(variables = case_when(
    variables == "scale(AG)" ~ "Area of agricultural land",
    variables == "scale(adjacent_counts)" ~ "Number of adjacent cells occupied",
    variables == "scale(TRImean)" ~ "Terrain ruggedness index",
    variables == "scale(aband)" ~ "Area of abandoned agricultural land",
    variables == "scale(snow)" ~ "Maximum snow depth",
    variables == "scale(NL2003)" ~ "Artificial light at night",
    TRUE ~ variables )) %>%
  mutate(variables = factor(variables, levels = c("Number of adjacent cells occupied",
                                                  "Terrain ruggedness index",
                                                  "Maximum snow depth",
                                                  "Area of abandoned agricultural land",
                                                  "Area of agricultural land",
                                                  "Artificial light at night")))

Result_wildboarp2p3_fit <- as.data.frame(cbind(summary(wildboarp2p3_fit)$coef,
                                               confint(wildboarp2p3_fit))) %>%
  rename(., "p_value"="Pr(>|z|)") %>% 
  mutate(species = "wildboar", period = "p2_p3", 
         significance = ifelse(p_value <= 0.05, 'sig', 'insig'), variables = row.names(.)) %>%
  rename(., "low95" = "2.5 %", "upp95" = "97.5 %") %>%
  mutate(variables = case_when(
    variables == "scale(AG)" ~ "Area of agricultural land",
    variables == "scale(adjacent_counts)" ~ "Number of adjacent cells occupied",
    variables == "scale(TRImean)" ~ "Terrain ruggedness index",
    variables == "scale(aband)" ~ "Area of abandoned agricultural land",
    variables == "scale(snow)" ~ "Maximum snow depth",
    variables == "scale(NL2014)" ~ "Artificial light at night",
    TRUE ~ variables )) %>%
  mutate(variables = factor(variables, levels = c("Number of adjacent cells occupied",
                                                  "Terrain ruggedness index",
                                                  "Maximum snow depth",
                                                  "Area of abandoned agricultural land",
                                                  "Area of agricultural land",
                                                  "Artificial light at night")))



### Coefficients of occupation models for Asiatic black bear

Result_blackbearp1p2_fit <- as.data.frame(cbind(summary(blackbearp1p2_fit)$coef,
                                                confint(blackbearp1p2_fit))) %>%
  rename(., "p_value"="Pr(>|z|)") %>% 
  mutate(species = "black bear", period = "p1_p2", 
         significance = ifelse(p_value <= 0.05, 'sig', 'insig'), variables = row.names(.)) %>%
  rename(., "low95" = "2.5 %", "upp95" = "97.5 %") %>%
  mutate(variables = case_when(
    variables == "scale(AG)" ~ "Area of agricultural land",
    variables == "scale(adjacent_counts)" ~ "Number of adjacent cells occupied",
    variables == "scale(TRImean)" ~ "Terrain ruggedness index",
    variables == "scale(aband)" ~ "Area of abandoned agricultural land",
    variables == "scale(snow)" ~ "Maximum snow depth",
    variables == "scale(NL2003)" ~ "Artificial light at night",
    TRUE ~ variables )) %>%
  mutate(variables = factor(variables, levels = c("Number of adjacent cells occupied",
                                                  "Terrain ruggedness index",
                                                  "Maximum snow depth",
                                                  "Area of abandoned agricultural land",
                                                  "Area of agricultural land",
                                                  "Artificial light at night")))

Result_blackbearp2p3_fit <- as.data.frame(cbind(summary(blackbearp2p3_fit)$coef,
                                                confint(blackbearp2p3_fit))) %>%
  rename(., "p_value"="Pr(>|z|)") %>% 
  mutate(species = "black bear", period = "p2_p3", 
         significance = ifelse(p_value <= 0.05, 'sig', 'insig'), variables = row.names(.)) %>%
  rename(., "low95" = "2.5 %", "upp95" = "97.5 %") %>%
  mutate(variables = case_when(
    variables == "scale(AG)" ~ "Area of agricultural land",
    variables == "scale(adjacent_counts)" ~ "Number of adjacent cells occupied",
    variables == "scale(TRImean)" ~ "Terrain ruggedness index",
    variables == "scale(aband)" ~ "Area of abandoned agricultural land",
    variables == "scale(snow)" ~ "Maximum snow depth",
    variables == "scale(NL2017)" ~ "Artificial light at night",
    TRUE ~ variables )) %>%
  mutate(variables = factor(variables, levels = c("Number of adjacent cells occupied",
                                                  "Terrain ruggedness index",
                                                  "Maximum snow depth",
                                                  "Area of abandoned agricultural land",
                                                  "Area of agricultural land",
                                                  "Artificial light at night")))



### Coefficients of occupation models for Japanese serow

Result_serowp1p2_fit <- as.data.frame(cbind(summary(serowp1p2_fit)$coef,
                                            confint(serowp1p2_fit))) %>%
  rename(., "p_value"="Pr(>|z|)") %>% 
  mutate(species = "serow", period = "p1_p2", 
         significance = ifelse(p_value <= 0.05, 'sig', 'insig'), variables = row.names(.)) %>%
  rename(., "low95" = "2.5 %", "upp95" = "97.5 %") %>%
  mutate(variables = case_when(
    variables == "scale(AG)" ~ "Area of agricultural land",
    variables == "scale(adjacent_counts)" ~ "Number of adjacent cells occupied",
    variables == "scale(TRImean)" ~ "Terrain ruggedness index",
    variables == "scale(aband)" ~ "Area of abandoned agricultural land",
    variables == "scale(snow)" ~ "Maximum snow depth",
    variables == "scale(NL2003)" ~ "Artificial light at night",
    TRUE ~ variables )) %>%
  mutate(variables = factor(variables, levels = c("Number of adjacent cells occupied",
                                                  "Terrain ruggedness index",
                                                  "Maximum snow depth",
                                                  "Area of abandoned agricultural land",
                                                  "Area of agricultural land",
                                                  "Artificial light at night")))

Result_serowp2p3_fit <- as.data.frame(cbind(summary(serowp2p3_fit)$coef,
                                            confint(serowp2p3_fit))) %>%
  rename(., "p_value"="Pr(>|z|)") %>% 
  mutate(species = "serow", period = "p2_p3", 
         significance = ifelse(p_value <= 0.05, 'sig', 'insig'), variables = row.names(.)) %>%
  rename(., "low95" = "2.5 %", "upp95" = "97.5 %") %>%
  mutate(variables = case_when(
    variables == "scale(AG)" ~ "Area of agricultural land",
    variables == "scale(adjacent_counts)" ~ "Number of adjacent cells occupied",
    variables == "scale(TRImean)" ~ "Terrain ruggedness index",
    variables == "scale(aband)" ~ "Area of abandoned agricultural land",
    variables == "scale(snow)" ~ "Maximum snow depth",
    variables == "scale(NL2017)" ~ "Artificial light at night",
    TRUE ~ variables )) %>%
  mutate(variables = factor(variables, levels = c("Number of adjacent cells occupied",
                                                  "Terrain ruggedness index",
                                                  "Maximum snow depth",
                                                  "Area of abandoned agricultural land",
                                                  "Area of agricultural land",
                                                  "Artificial light at night")))



### Coefficients of occupation models for Japanese macaque

Result_macaquep1p2_fit <- as.data.frame(cbind(summary(macaquep1p2_fit)$coef,
                                              confint(macaquep1p2_fit))) %>%
  rename(., "p_value"="Pr(>|z|)") %>% 
  mutate(species = "macaque", period = "p1_p2", 
         significance = ifelse(p_value <= 0.05, 'sig', 'insig'), variables = row.names(.)) %>%
  rename(., "low95" = "2.5 %", "upp95" = "97.5 %") %>%
  mutate(variables = case_when(
    variables == "scale(AG)" ~ "Area of agricultural land",
    variables == "scale(adjacent_counts)" ~ "Number of adjacent cells occupied",
    variables == "scale(TRImean)" ~ "Terrain ruggedness index",
    variables == "scale(aband)" ~ "Area of abandoned agricultural land",
    variables == "scale(snow)" ~ "Maximum snow depth",
    variables == "scale(NL2003)" ~ "Artificial light at night",
    TRUE ~ variables )) %>%
  mutate(variables = factor(variables, levels = c("Number of adjacent cells occupied",
                                                  "Terrain ruggedness index",
                                                  "Maximum snow depth",
                                                  "Area of abandoned agricultural land",
                                                  "Area of agricultural land",
                                                  "Artificial light at night")))

Result_macaquep2p3_fit <- as.data.frame(cbind(summary(macaquep2p3_fit)$coef,
                                              confint(macaquep2p3_fit))) %>%
  rename(., "p_value"="Pr(>|z|)") %>% 
  mutate(species = "macaque", period = "p2_p3", 
         significance = ifelse(p_value <= 0.05, 'sig', 'insig'), variables = row.names(.)) %>%
  rename(., "low95" = "2.5 %", "upp95" = "97.5 %") %>%
  mutate(variables = case_when(
    variables == "scale(AG)" ~ "Area of agricultural land",
    variables == "scale(adjacent_counts)" ~ "Number of adjacent cells occupied",
    variables == "scale(TRImean)" ~ "Terrain ruggedness index",
    variables == "scale(aband)" ~ "Area of abandoned agricultural land",
    variables == "scale(snow)" ~ "Maximum snow depth",
    variables == "scale(NL2017)" ~ "Artificial light at night",
    TRUE ~ variables )) %>%
  mutate(variables = factor(variables, levels = c("Number of adjacent cells occupied",
                                                  "Terrain ruggedness index",
                                                  "Maximum snow depth",
                                                  "Area of abandoned agricultural land",
                                                  "Area of agricultural land",
                                                  "Artificial light at night")))



### Coefficients of occupation models for Brown bear

Result_brownbearp1p2_fit <- as.data.frame(cbind(summary(brownbearp1p2_fit)$coef,
                                                confint(brownbearp1p2_fit))) %>%
  rename(., "p_value"="Pr(>|z|)") %>% 
  mutate(species = "brown bear", period = "p1_p2", 
         significance = ifelse(p_value <= 0.05, 'sig', 'insig'), variables = row.names(.)) %>%
  rename(., "low95" = "2.5 %", "upp95" = "97.5 %") %>%
  mutate(variables = case_when(
    variables == "scale(AG)" ~ "Area of agricultural land",
    variables == "scale(adjacent_counts)" ~ "Number of adjacent cells occupied",
    variables == "scale(TRImean)" ~ "Terrain ruggedness index",
    variables == "scale(aband)" ~ "Area of abandoned agricultural land",
    variables == "scale(snow)" ~ "Maximum snow depth",
    variables == "scale(NL2003)" ~ "Artificial light at night",
    TRUE ~ variables )) %>%
  mutate(variables = factor(variables, levels = c("Number of adjacent cells occupied",
                                                  "Terrain ruggedness index",
                                                  "Maximum snow depth",
                                                  "Area of abandoned agricultural land",
                                                  "Area of agricultural land",
                                                  "Artificial light at night")))

Result_brownbearp2p3_fit <- as.data.frame(cbind(summary(brownbearp2p3_fit)$coef,
                                                confint(brownbearp2p3_fit))) %>%
  rename(., "p_value"="Pr(>|z|)") %>% 
  mutate(species = "brown bear", period = "p2_p3", 
         significance = ifelse(p_value <= 0.05, 'sig', 'insig'), variables = row.names(.)) %>%
  rename(., "low95" = "2.5 %", "upp95" = "97.5 %") %>%
  mutate(variables = case_when(
    variables == "scale(AG)" ~ "Area of agricultural land",
    variables == "scale(adjacent_counts)" ~ "Number of adjacent cells occupied",
    variables == "scale(TRImean)" ~ "Terrain ruggedness index",
    variables == "scale(aband)" ~ "Area of abandoned agricultural land",
    variables == "scale(snow)" ~ "Maximum snow depth",
    variables == "scale(NL2017)" ~ "Artificial light at night",
    TRUE ~ variables )) %>%
  mutate(variables = factor(variables, levels = c("Number of adjacent cells occupied",
                                                  "Terrain ruggedness index",
                                                  "Maximum snow depth",
                                                  "Area of abandoned agricultural land",
                                                  "Area of agricultural land",
                                                  "Artificial light at night")))


###########################################################################
##### Generates plot, showing coefficients from the occupation models #####
###########################################################################

# Coefficient results merging
result_merged <- rbind(Result_blackbearp1p2_fit, Result_blackbearp2p3_fit,
                              Result_brownbearp1p2_fit, Result_brownbearp2p3_fit,
                              Result_serowp1p2_fit, Result_serowp2p3_fit,
                              Result_macaquep1p2_fit, Result_macaquep2p3_fit,
                              Result_wildboarp1p2_fit, Result_wildboarp2p3_fit,
                              Result_sikadeer_hks_p1p2_fit, Result_sikadeer_hks_p2p3_fit,
                              Result_sikadeer_hok_p1p2_fit, Result_sikadeer_hok_p2p3_fit) %>%
  mutate(species = factor(species, levels = c("sikadeerhks", "wildboar",
                                              "black bear", "serow",
                                              "macaque", "sikadeerhok", "brown bear"))) %>%
  mutate(significance = factor(significance, levels = c("sig", "insig")))


# generate plot
ggplot(data=result_merged, aes(x = coef, y=variables, col = period, shape=significance)) +
  geom_rect(aes(xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf), fill = "gray68", alpha = 0.01, show.legend = F) +
  geom_rect(aes(xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "honeydew1", alpha = 0.03, show.legend = F) +
  geom_errorbar(aes(xmin=low95, xmax=upp95), width=.1, alpha = 0.5, position = position_dodge(width = -0.3)) +
  geom_point(size=3, position = position_dodge(width = -0.3)) +
  geom_vline(xintercept = 0, color = "black") +
  theme_bw() + 
  scale_color_manual(values = c("#443A83FF", "#AADC32FF"), name = "Period", labels = c("1978-2003", "2003-2010's")) +
  scale_shape_manual(values=c(16, 1)) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none", legend.box = "vertical") + 
  labs(x = "Coefficient", y = 'Variables') + facet_grid(. ~ species) +
  theme(strip.background = element_blank(),strip.text.x = element_blank())

ggsave(file = "coefficients_plot.svg", dpi = 300, width = 11, height = 6.5)
