
library(ggplot2)
library(dplyr)
library(broom)
library(purrr)
library(stringr)

###########################################################
##### Extract coefficients from the occupation models #####
###########################################################

models <- list(
  deer_hks_p1p2_fit,
  deer_hks_p2p3_fit,
  deer_hok_p1p2_fit,
  deer_hok_p2p3_fit,
  boar_p1p2_fit,
  boar_p2p3_fit,
  blackbear_p1p2_fit,
  blackbear_p2p3_fit,
  serow_p1p2_fit,
  serow_p2p3_fit,
  macaque_p1p2_fit,
  macaque_p2p3_fit,
  brownbear_p1p2_fit,
  brownbear_p2p3_fit)

model_names <- c("deer_hks_p1p2", "deer_hks_p2p3",
                 "deer_hok_p1p2", "deer_hok_p2p3",
                 "boar_p1p2", "boar_p2p3",
                 "blackbear_p1p2", "blackbear_p2p3", 
                 "serow_p1p2", "serow_p2p3",
                 "macaque_p1p2", "macaque_p2p3",
                 "brownbear_p1p2", "brownbear_p2p3")


# Extract results
results <- map2_df(models, model_names, function(model, name) {
  tidy_df <- broom::tidy(model, conf.int = TRUE)  
  
  tidy_df <- tidy_df %>%
    mutate(
      model = name,
      period = case_when(
        grepl("p1p2", name) ~ "p1p2", 
        grepl("p2p3", name) ~ "p2p3",
        TRUE ~ NA_character_         
      ),
      term = case_when(
        grepl("snow", term) ~ "Snow",
        TRUE ~ term
      ),
      species = str_extract(name, "deer_hks|deer_hok|boar|blackbear|serow|macaque|brownbear"),
      shape = ifelse(p.value < 0.05, 'significant', 'insignificant') 
    ) %>%
    mutate(
      species = factor(species, levels = c("deer_hks", "boar", "blackbear", "serow", "macaque", "deer_hok", "brownbear")) 
    )
  
  return(tidy_df)
})

# variables name
results_ <- results %>%
  mutate(term = case_when(
    term == "scale(AG)" ~ "Area of agricultural land",
    term == "scale(adjacent_counts)" ~ "Number of adjacent cells occupied",
    term == "scale(TRImean)" ~ "Terrain ruggedness index",
    term == "scale(aband)" ~ "Area of abandoned agricultural land",
    term == "scale(snow)" ~ "Average maximum snow depth",
    term == "scale(NL)" ~ "Artificial light at night",
    TRUE ~ term )) %>%
  mutate(term = factor(term, levels = c("Number of adjacent cells occupied",
                                                  "Average maximum snow depth",
                                                  "Terrain ruggedness index",
                                                  "Area of abandoned agricultural land",
                                                  "Area of agricultural land",
                                                  "Artificial light at night"))) %>%
  mutate(period = factor(period, levels = c(
                                        "p2p3",
                                        "p1p2"))) %>% 
  mutate(species = factor(species, levels = c("deer_hks", "boar", "blackbear", "serow", "macaque", 
                                              "deer_hok", "brownbear")))

### making plot
ggplot(results_, aes(x = estimate, y = term, color = period, shape = as.factor(shape))) +
  theme_bw() +
  geom_rect(aes(xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf), fill = "gray68", alpha = 0.01, show.legend = F) +
  geom_rect(aes(xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "honeydew1", alpha = 0.03, show.legend = F) +
  geom_vline(xintercept = 0, size = 1, color = "white") +
  geom_vline(xintercept = 0, size = 1, color = "darkgray", linetype = "dashed") +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.6, alpha = 0.5, position = position_dodge(width = 0.5)) +
  geom_point(size = 2, position = position_dodge(width = 0.5)) +
  scale_shape_manual(values = c("significant" = 16, "insignificant" = 1)) +  # 매뉴얼로 점 모양 정의
  facet_grid( ~ species) +
  labs(shape = "Significance") + # 범례 제목 설정
  scale_color_manual(values = c("#AADC32FF", "#443A83FF"), name = "Period",
                     labels = c("2003-2010's", "1978-2003")) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom", legend.box = "vertical") + 
  labs(x = "Coefficient", y = 'Variables') + facet_grid(. ~ species) +
  #  theme(strip.background = element_blank(),strip.text.x = element_blank()) +
  theme(strip.text.x = element_blank()) +
  guides(shape = "none") +
  theme(
    strip.text = element_text(color = "black"),
    strip.background = element_rect(fill = "white", color = "black"),
    panel.border = element_rect(color = "black", fill = NA)
  )

ggsave(file = "coefficients_plot.svg", dpi = 300, width = 11, height = 6.5)
