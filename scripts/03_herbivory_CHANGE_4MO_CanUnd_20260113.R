#----------------------------------------------------------#
#
#
#     BABE 4MO controls herbivory CANOPY  vs. UNDERSTORY
#
#            Analyses of CHANGE in herbivory
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

#  rename column Diff to something more sensible
colnames(dataset_sum)[colnames(dataset_sum) == 'Diff'] <- 'TwigHerbPropDiff'

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
# 2.1 Models for the differnce in herbivory damage over the 4MO  -----
#----------------------------------------------------------#
# start building models   # not sure about the random factor  
# Individual code e.g. EUC_SP5_2_CD has Site_SpeciesID_Individuum_Year
# TwigHerbPropDiff - change between the starting herbivory and end herbivory per twig,
# i.e. there are 3 values per each individual tree in the data

glm.herb.null <-glmmTMB(TwigHerbPropDiff +0.0001 ~ 1 +
                          + (1|PlantSpeciesAnal) + (1|IndividualCode), 
                        family = beta_family(), 
                        data = dataset_sum,
                        na.action = "na.exclude")
glm.herb.latitude <-glmmTMB(TwigHerbPropDiff +0.0001 ~ poly(Lat,2) +
                          + (1|PlantSpeciesAnal) + (1|IndividualCode) , 
                        family = beta_family(),
                        data = dataset_sum,
                        na.action = "na.exclude")
glm.herb.strata <-glmmTMB(TwigHerbPropDiff +0.0001 ~ Strata +
                          + (1|PlantSpeciesAnal) + (1|IndividualCode), 
                          family = beta_family(), 
                        data = dataset_sum,
                        na.action = "na.exclude")
glm.herb.add <-glmmTMB(TwigHerbPropDiff +0.0001 ~ Strata + poly(Lat,2) +
                            + (1|PlantSpeciesAnal) + (1|IndividualCode), 
                       family = beta_family(), 
                          data = dataset_sum,
                          na.action = "na.exclude")
glm.herb.int <-glmmTMB(TwigHerbPropDiff +0.0001 ~ Strata * poly(Lat,2) +
                         + (1|PlantSpeciesAnal) + (1|IndividualCode), 
                       family = beta_family(), 
                       data = dataset_sum,
                       na.action = "na.exclude")
glm.herb.climate <-glmmTMB(TwigHerbPropDiff +0.0001 ~ Climate +
                         + (1|PlantSpeciesAnal) + (1|IndividualCode), 
                       family = beta_family(), 
                       data = dataset_sum,
                       na.action = "na.exclude")
glm.herb.climateadd <-glmmTMB(TwigHerbPropDiff +0.0001 ~ Climate + Strata + 
                            + (1|PlantSpeciesAnal) + (1|IndividualCode), 
                          family = beta_family(), 
                          data = dataset_sum,
                          na.action = "na.exclude")
glm.herb.climateInt <-glmmTMB(TwigHerbPropDiff +0.0001 ~ Climate * Strata + 
                                + (1|PlantSpeciesAnal) + (1|IndividualCode), 
                              family = beta_family(), 
                              data = dataset_sum,
                              na.action = "na.exclude")

# compare the models
library(bbmle)
AICctab(glm.herb.null, glm.herb.latitude, glm.herb.strata, glm.herb.add, glm.herb.int, glm.herb.climate, glm.herb.climateadd, glm.herb.climateInt)
AIC(glm.herb.null, glm.herb.latitude, glm.herb.strata, glm.herb.add, glm.herb.int, glm.herb.climate, glm.herb.climateadd, glm.herb.climateInt)
library(MuMIn)
model.sel(glm.herb.null, glm.herb.latitude, glm.herb.strata, glm.herb.add, glm.herb.int, glm.herb.climate, glm.herb.climateadd, glm.herb.climateInt)
summary(glm.herb.int) # exploratory only

## Predict the values - Given my fitted model, what is the expected response at these exact covariate values?
newData <- data.frame(Lat = rep(seq(from = -40, to = 55, length.out = 500),2),
                      Strata = rep(c("Understory", "Canopy"), each = 500))

library(merTools)
newData$Herbivory <- predict(glm.herb.int, newdata = newData, se.fit = TRUE, re.form = NA, type = "response")
pred <- predict(glm.herb.int,
                newdata = newData,
                type = "response",
                se.fit = TRUE,
                re.form = NA)

newData$Herbivory <- pred$fit
newData$HerbivoryLwr <- plogis(qlogis(pred$fit) - 1.96 * pred$se.fit)
newData$HerbivoryUpr <- plogis(qlogis(pred$fit) + 1.96 * pred$se.fit)
summary(newData)
summary(newData$HerbivoryUpr - newData$HerbivoryLwr)

newData %>% 
  as_tibble() %>% 
  write_csv("data/output/prediction_herbivory.csv")

# “What are the estimated marginal means (or trends) implied by the model?” 
# Emmeans better for the hypothesis testing - “Herbivory differed between strata across latitude”
library(emmeans)
emm <- emmeans(glm.herb.int,
               ~ Lat * Strata,
               at = list(Lat = seq(-40, 55, length.out = 500)),
               type = "response")
emm_df <- as.data.frame(emm)
summary (emm)

site_lats <- c(42.68, 51.2, 21.6, -5.13, -16.1, -33.62)
emm_sites <- emmeans(
  glm.herb.int,
  ~ Lat * Strata,
  at = list(Lat = site_lats),
  type = "response"
)
summary(emm_sites)

#----------------------------------------------------------#
# 2.2 Figure from model draw  with predicted values -----
#----------------------------------------------------------#
colnames(newData)
# graphical properties definition for upcoming graphs
theme_set(theme_classic())
PDF_width <-  10
PDF_height <-  6

library(ggplot2)
model_plot_01 <- ggplot() +
  
  # Add the confidence interval ribbon
  geom_ribbon(
    data = newData,
    aes(
      x = Lat,
      ymin = HerbivoryLwr *100,
      ymax = HerbivoryUpr *100,
      fill = Strata, 
      group = Strata),
    alpha = 0.3) +
  
  scale_x_reverse() +
  
  # Add the fitted line
  geom_line(
    data = newData,
    aes(
      x = Lat,
      y = Herbivory *100,
      col = Strata),
    size = 2) +
  
  # Add jittered points
  geom_point(
    data = dataset_sum,
    aes(
      x = Lat,
      y = TwigHerbPropDiff*100,
      col = Strata,
      fill = Strata),
    size = 2, alpha = 0.3, 
    position = position_jitterdodge(
      dodge.width = 3,
      jitter.width = 3)) +
  
  # Add labels and style
  labs(
    x = "Latitude",
    y = expression(paste("Change in herbivory damage"))) +
  scale_fill_manual(values = c("#42adc7", "#ffb902")) +
  scale_color_manual(values = c("#42adc7", "#ffb902")) +
  
  theme(
    text = element_text(size = 14),
    legend.position = "right",
    axis.line = element_line(colour = "black", linewidth = 0.5, linetype = "solid"),
    axis.ticks = element_line(colour = "black", linewidth = 0.5, linetype = "solid"))
model_plot_01
# I think it plots the ribbons and the CIs but they are so small that they are not visible?

# save pdf
ggsave(
  "figures/Figure_1_HerbivoryDiff_Predicted.pdf",
  model_plot_01,
  width = PDF_width,
  height = PDF_height,
  units = "in")

#----------------------------------------------------------#
# 2.3 Figure from model draw  with emmeans  -----
#----------------------------------------------------------#
p2 <- emm %>%
  as_tibble() %>%
  ggplot(aes(
    x = Lat,
    y = response * 100,
    color = Strata,
    group = Strata, 
  )) +
  
  # Add jittered observed points, colored by Strata
  geom_point(
    data = dataset_sum,
    aes(x = Lat, y = TwigHerbPropDiff * 100, color = Strata),
    alpha = 0.2,
    position = position_jitterdodge(jitter.width = 3, dodge.width = 3)
  ) +
  
  ylim(0,40) +
  
  # Add error bars from emmeans 
  geom_ribbon(
    aes(
      ymin = asymp.LCL * 100,
      ymax = asymp.UCL * 100,
      fill = Strata
    ),
    alpha = 0.2,
    colour = NA
  ) + 
  
  scale_fill_manual(values = c(
    "Canopy"    = "#42adc7",
    "Understory" = "#E69F00"
  )) + 
  
  # Add estimated means
  geom_point(
    shape = 16,
    size = 1,
    position = position_dodge(width = 0.5)
  ) +
  
  scale_x_reverse() +
  
  # Labels
  labs(
    x = "Latitude",
    y = expression(atop("Change in herbivory damage (in %)")),
    color = "Strata"
  ) +
  
  # Set the colors for Canopy and Understory
  scale_color_manual(values = c("Canopy" = "#42adc7", "Understory" = "#E69F00")) +
  
  # Theme adjustments
  theme(
    text = element_text(size = 18),
    legend.position = "none",
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    axis.title.y = element_text(margin = margin(r = 5), hjust = 0.5),
    axis.title.x = element_text(hjust = 0.5),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  
  geom_vline(xintercept = c(23, -23),
             linetype = "dashed",
             color = "grey50",
             linewidth = 1.2) +
  
  annotate(
    "text",
    x = c(0),           # positions BETWEEN or OVER factor groups
    y = 40,
    label = c("Tropical"),
    vjust = 1.3,
    fontface = "bold",
    size = 5
  ) +
  
  annotate(
    "text",
    x = c(40, -35),           # positions BETWEEN or OVER factor groups
    y = 40,
    label = c("Temperate", "Temperate"),
    vjust = 1.3,
    fontface = "bold",
    size = 5
  )
p2

# save pdf
ggsave(
  "figures/Figure_1b_HerbivoryDiff_Emmeans.pdf",
  p2,
  width = PDF_width,
  height = PDF_height,
  units = "in")

library(ggpubr)
ggarrange(p1, p2, 
          labels = c("A)", "B)"),
          ncol = 2, nrow = 1)
