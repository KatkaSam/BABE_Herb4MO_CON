library(dplyr)

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


dat_EUC_sp <- dataset_sum %>%
  filter(
    Site == "EUC",
    PlantSpeciesAnal == "Eucalyptus_tereticornis",
    !is.na(HerbProp_End)
  )

table(dat_EUC_sp$Strata)

dat_EUC_sp %>%
  group_by(Strata) %>%
  summarise(
    n = n(),
    mean = mean(HerbProp_End),
    median = median(HerbProp_End),
    sd = sd(HerbProp_End)
  )

boxplot(
  HerbProp_End ~ Strata,
  data = dat_EUC_sp,
  ylab = "Proportion herbivory (end)",
  xlab = "Forest stratum"
)

wilcox.test(
  HerbProp_End ~ Strata,
  data = dat_EUC_sp,
  exact = FALSE
)

t.test(
  HerbProp_End ~ Strata,
  data = dat_EUC_sp
)


# Load summarized data 
dataset_sum <- read_csv("data/output/twig_summary_start_end_difference.csv") 
summary(dataset_sum)

dat_EUC <- dataset_sum %>%
  filter(
    Site == "EUC",
    !is.na(HerbProp_End))
summary(dat_EUC)

dat_EUC %>%
  group_by(PlantSpeciesAnal) %>%
  summarise(
    n = n(),
    mean = mean(HerbProp_End),
    median = median(HerbProp_End),
    sd = sd(HerbProp_End)
  )

