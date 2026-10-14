#----------------------------------------------------------#
#
#
#     BABE 4MO controls herbivory CANOPY  vs. UNDERSTORY
#
#         Analyses of END OF THE SEASON herbivory
#   
#                   Katerina Sam   2026
#
#----------------------------------------------------------#
library(dplyr)
library(tidyr)
library(readr)  
library(glmmTMB)

# Load summarized data 
dataset_sum <- read_csv("data/output/twig_summary_start_end_difference.csv") 
summary(dataset_sum)

# Inspect unique twigs
table(dataset_sum$TwigCode)[table(dataset_sum$TwigCode) > 1]

# record the factors
custom_order <- c("LAK", "TOM", "BUB", "KAK", "DRO", "EUC")  # Replace with your desired order
dataset_sum$Site <- factor(dataset_sum$Site, levels = custom_order)
dataset_sum$Site <-as.factor(dataset_sum$Site)         # 6 sites as a factor
dataset_sum$Strata <-as.factor(dataset_sum$Strata)     # 2 factors, canopy vs. understory
dataset_sum$PatrolIndiv <-as.factor(dataset_sum$PatrolIndiv)   #PatrolIndiv AB = year 1, CD = year 2
dataset_sum$PlantSpeciesAnal <-as.factor(dataset_sum$PlantSpeciesAnal)  # latin name of the species
dataset_sum$IndividualCode <-as.factor(dataset_sum$IndividualCode)     #  E.g. BUB_SP1_1_CD - includes the nested design Site_Pl.species_Individual_Year
dataset_sum$TwigCode <-as.factor(dataset_sum$TwigCode) #  E.g. TwigCode each individual had three of these twigs
summary(dataset_sum)

dataset_sum <- dataset_sum %>%
  mutate(Climate = case_when(
    Site %in% c("LAK", "TOM", "EUC") ~ "Temperate",
    Site %in% c("BUB", "KAK", "DRO") ~ "Tropical",
    TRUE ~ NA_character_
  ))
dataset_sum$Climate <-as.factor(dataset_sum$Climate)  
summary(dataset_sum)

# Add latitude data
Sites <- data.frame(Site = c("TOM", "LAK", "BUB", "KAK", "DRO", "EUC"),
                    Lat = c(42.68, 51.2, 21.6, -5.13, -16.1, -33.62))
dataset_sum <- dataset_sum %>%
  left_join(Sites, by = "Site")
summary(dataset_sum)

#----------------------------------------------------------#
# 2.1 Models for the end of the season herbivory damage after the  4MO  -----
#----------------------------------------------------------#
# start building models   # not sure about the random factor  
# Individual code e.g. EUC_SP5_2_CD has Site_SpeciesID_Individuum_Year
# HerbProp_End - Proportion of the leaf area left at the endo of the season
# i.e. there are 3 values per each individual tree in the data

glm.herbEND.null <-glmmTMB(HerbProp_End +0.0001 ~ 1 +
                          + (1|PlantSpeciesAnal) + (1|IndividualCode), 
                        family = beta_family(), 
                        data = dataset_sum,
                        na.action = "na.exclude")
glm.herbEND.latitude <-glmmTMB(HerbProp_End +0.0001 ~ poly(Lat,2) +
                              + (1|PlantSpeciesAnal) + (1|IndividualCode) , 
                            family = beta_family(),
                            data = dataset_sum,
                            na.action = "na.exclude")
glm.herbEND.strata <-glmmTMB(HerbProp_End +0.0001 ~ Strata +
                            + (1|PlantSpeciesAnal) + (1|IndividualCode), 
                          family = beta_family(), 
                          data = dataset_sum,
                          na.action = "na.exclude")
glm.herbEND.add <-glmmTMB(HerbProp_End +0.0001 ~ Strata + poly(Lat,2) +
                         + (1|PlantSpeciesAnal) + (1|IndividualCode), 
                       family = beta_family(), 
                       data = dataset_sum,
                       na.action = "na.exclude")
glm.herbEND.int <-glmmTMB(HerbProp_End +0.0001 ~ Strata * poly(Lat,2) +
                         + (1|PlantSpeciesAnal) + (1|IndividualCode), 
                       family = beta_family(), 
                       data = dataset_sum,
                       na.action = "na.exclude")
glm.herbEND.climate <-glmmTMB(HerbProp_End +0.0001 ~ Climate +
                             + (1|PlantSpeciesAnal) + (1|IndividualCode), 
                           family = beta_family(), 
                           data = dataset_sum,
                           na.action = "na.exclude")
glm.herbEND.climateadd <-glmmTMB(HerbProp_End +0.0001 ~ Climate + Strata + 
                                + (1|PlantSpeciesAnal) + (1|IndividualCode), 
                              family = beta_family(), 
                              data = dataset_sum,
                              na.action = "na.exclude")
glm.herbEND.climateInt <-glmmTMB(HerbProp_End +0.0001 ~ Climate * Strata + 
                                + (1|PlantSpeciesAnal) + (1|IndividualCode), 
                              family = beta_family(), 
                              data = dataset_sum,
                              na.action = "na.exclude")

# compare the models
library(bbmle)
AICctab(glm.herbEND.null, glm.herbEND.latitude, glm.herbEND.strata, glm.herbEND.add, glm.herbEND.int, glm.herbEND.climate, glm.herbEND.climateadd, glm.herbEND.climateInt)
AIC(glm.herbEND.null, glm.herbEND.latitude, glm.herbEND.strata, glm.herbEND.add, glm.herbEND.int, glm.herbEND.climate, glm.herbEND.climateadd, glm.herbEND.climateInt)
library(MuMIn)
model.sel(glm.herbEND.null, glm.herbEND.latitude, glm.herbEND.strata, glm.herbEND.add, glm.herbEND.int, glm.herbEND.climate, glm.herbEND.climateadd, glm.herbEND.climateInt)
summary(glm.herb.int) # exploratory only


# “What are the estimated marginal means (or trends) implied by the model?” 
# Emmeans better for the hypothesis testing - “Herbivory differed between strata across latitude”
library(emmeans)
emm <- emmeans(glm.herbEND.climateInt,
               ~ Climate * Strata,
               at = list(Lat = seq(-40, 55, length.out = 500)),
               type = "response")
emm_df <- as.data.frame(emm)
summary (emm)

site_lats <- c(42.68, 51.2, 21.6, -5.13, -16.1, -33.62)
emm_sites <- emmeans(
  glm.herbEND.climateInt,
  ~ Climate * Strata,
  at = list(Lat = site_lats),
  type = "response"
)
summary(emm_sites)

#----------------------------------------------------------#
# 2.2 Figure from model draw  with emmeans  -----
#----------------------------------------------------------#
# ---------------------------
# 0) Setup
# ---------------------------
library(tidyverse)
library(emmeans)

# graphical properties definition for upcoming graphs
theme_set(theme_classic())
PDF_width <-  10
PDF_height <-  6
pad <- 0.3
dodge_width <- 0.05

# ---------------------------
# 1) Define site positions
# ---------------------------
site_positions <- tibble(
  Site = c("LAK", "TOM", "BUB", "KAK", "DRO", "EUC"),
  x_pos = 1:6,
  Climate = c("Temperate", "Temperate", "Tropical",
              "Tropical", "Tropical", "Temperate")
)

# ---------------------------
# 2) Clean dataset & join site positions
# ---------------------------
dataset_sum_clean <- dataset_sum %>%
  dplyr::select(Site, Strata, HerbProp_End, IndividualCode, PlantSpeciesAnal) %>%
  left_join(site_positions, by = "Site") %>%
  mutate(
    Strata  = factor(Strata, levels = c("Understory", "Canopy")),
    Climate = factor(Climate, levels = c("Temperate", "Tropical"))
  )

# ---------------------------
# 3) Pull Climate × Strata emmeans
# ---------------------------
clim_strata_means <- glm_herbClimate_emmeans$emmeans %>%
  as_tibble() %>%
  mutate(
    Climate = factor(Climate, levels = c("Temperate","Tropical")),
    Strata = factor(Strata, levels = c("Understory","Canopy"))
  )

# ---------------------------
# 4) Build segment data (ribbons/lines) per Climate × Strata
# ---------------------------
segments_spec <- tribble(
  ~Strata,        ~Climate,     ~sites,
  "Canopy",       "Temperate",  c("LAK","TOM"),
  "Canopy",       "Temperate",  c("EUC"),
  "Canopy",       "Tropical",   c("BUB","KAK","DRO"),
  "Understory",   "Temperate",  c("LAK","TOM"),
  "Understory",   "Temperate",  c("EUC"),
  "Understory",   "Tropical",   c("BUB","KAK","DRO")
)

segments_df <- segments_spec %>%
  rowwise() %>%
  mutate(
    xs   = list(sort(site_positions$x_pos[match(sites, site_positions$Site)])),
    xmin = min(xs) - pad,
    xmax = max(xs) + pad
  ) %>%
  ungroup() %>%
  left_join(
    clim_strata_means %>%
      dplyr::select(Climate, Strata, response, asymp.LCL, asymp.UCL),
    by = c("Climate", "Strata")
  ) %>%
  rowwise() %>%
  mutate(
    x_seq = list(seq(xmin, xmax, length.out = 50)),
    y     = response   * 100,
    ymin = asymp.LCL * 100,
    ymax = asymp.UCL * 100
  ) %>%
  ungroup() %>%
  mutate(segment_id = row_number()) %>%
  unnest(x_seq)

# ---------------------------
# 5) Build points positions (mean points per Climate × Strata)
# ---------------------------
climate_strata_points <- clim_strata_means %>%
  rowwise() %>%
  mutate(
    # store the numeric vector as a list-column
    climate_sites = list(site_positions$x_pos[site_positions$Climate == Climate]),
    x_center = mean(climate_sites[[1]]) + ifelse(Strata=="Understory",-dodge_width,dodge_width),
    y = response * 100
  ) %>%
  ungroup()

# ---------------------------
# 6) Define colours / linetypes
# ---------------------------
custom_strata_cols <- c(
  "Understory" = "#E69F00",  # muted amber
  "Canopy"     = "deepskyblue3"   # soft sky blue
)

# IN CHANGE scale_fill_manual(values = c("#42adc7", "#ffb902"))
# IN CHANGE scale_color_manual(values = c("#42adc7", "#ffb902"))

strata_linetypes <- c("Understory" = "solid", "Canopy" = "solid")

# ---------------------------
# 7) Plot
# ---------------------------
ggplot() +
  # Raw points
  geom_point(
    data = dataset_sum_clean,
    aes(
      x = x_pos + ifelse(Strata=="Canopy",-dodge_width,dodge_width),
      y = HerbProp_End*100,
      color = Strata,
      shape = Strata, 
    ),
    alpha = 0.3, size = 1.5,
    position = position_jitter(width = 0.05),
    show.legend = FALSE
  ) +
  
  ylim(0,40) +
  
  # CI ribbons
  geom_ribbon(
    data = segments_df,
    aes(x = x_seq, ymin = ymin, ymax = ymax,
        fill = Strata, group = segment_id),
    alpha = 0.2
  ) +
  
  # Mean lines (one line per segment)
  geom_line(
    data = segments_df %>%
      group_by(segment_id, Strata, Climate) %>%
      summarise(x = x_seq, y = rep(y[1], length(x_seq)), .groups = "drop") %>%
      unnest(c(x, y)),
    aes(x = x, y = y, colour = Strata, linetype = Strata,
        group = segment_id),
    size = 1.5
  ) +
  
  # Scales
  scale_shape_manual(values = c("Understory" = 16, "Canopy" = 16)) +
  scale_fill_manual(values = custom_strata_cols) +
  scale_linetype_manual(values = strata_linetypes) +
  scale_colour_manual(values = custom_strata_cols) +  # match line colors to fills
  
  scale_x_continuous(
    breaks = site_positions$x_pos,
    labels = site_positions$Site
  ) +
  
  labs(
    x = "Sites (ordered by latitude)",
    y = expression(atop("Standing herbivory damage",
                        "four months into the growing season (%)"))
  ) +
  
  theme(
    text = element_text(size = 18),
    legend.position = "none",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    plot.margin = margin(20,20,20,20)
  ) +
  
  geom_vline(xintercept = c(2.5, 5.5),
             linetype = "dashed",
             color = "grey50",
             linewidth = 1.2) +
  
  annotate(
    "text",
    x = c(4),           # positions BETWEEN or OVER factor groups
    y = 40,
    label = c("Tropical"),
    vjust = 1.3,
    fontface = "bold",
    size = 5
  ) +

  annotate(
    "text",
     x = c(1.5, 6.1),           # positions BETWEEN or OVER factor groups
     y = 40,
      label = c("Temperate", "Temperate"),
     vjust = 1.3,
     fontface = "bold",
    size = 5
    )






