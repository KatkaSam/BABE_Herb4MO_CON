#----------------------------------------------------------#
#
#
#               BABE 4MO controls herbivory
#
#               Data survey and preparations
#   
#                  Katerina Sam   2025
#
#----------------------------------------------------------#
#----------------------------------------------------------#
# 1. Exploratory tables and summaries  -----
#----------------------------------------------------------#

# Load libraries
library(dplyr)
library(tidyr)
library(readr)  # for reading CSV

# Load raw data
dataset_raw <- read_csv("data/input/data_clean_raw_20250821.csv") 
# the first steps are just for summarizing of the data, not for analysis

# STEP 2: Add PatrolGroup column
dataset_summary <- dataset_raw %>%
  mutate(
    PatrolGroup = paste(Strata, Patrol, sep = "_"),
    Species_num = as.numeric(gsub("[^0-9]", "", Species))  # extract numeric part of Species
  )

# STEP 3: Group and summarise
summary_df <- dataset_summary %>%
  group_by(Site, Species, Species_num, PatrolGroup) %>%
  summarise(PlantSpeciesAnal = paste(unique(PlantSpeciesAnal), collapse = ", "), .groups = "drop")

# STEP 4: Pivot wider
wide_df <- summary_df %>%
  arrange(Site, Species_num) %>%  # ensure numeric order by Species_num
  select(-Species_num) %>%        # drop helper column after sorting
  pivot_wider(
    names_from = PatrolGroup,
    values_from = PlantSpeciesAnal,
    values_fill = list(PlantSpeciesAnal = "")
  )


# OPTIONAL: Save to file
write_csv(wide_df, "data/output/summary_by_site_species_patrol.csv")

# Compare C and D columns as well
comparison <- wide_df %>%
  rowwise() %>%
  mutate(
    Understory_A_match = (`Understory_A` == `Understory_B`),
    Understory_C_match = (`Understory_C` == `Understory_D`),
    Canopy_A_match = if ("Canopy_A" %in% names(wide_df)) (`Canopy_A` == `Canopy_B`) else NA,
    Canopy_C_match = if ("Canopy_C" %in% names(wide_df)) (`Canopy_C` == `Canopy_D`) else NA
  ) %>%
  ungroup()

# View mismatches in any patrol pair
mismatches <- comparison %>%
  filter(!Understory_A_match | !Understory_C_match |
           (!is.na(Canopy_A_match) & !Canopy_A_match) |
           (!is.na(Canopy_C_match) & !Canopy_C_match))

# Print mismatching rows
print(mismatches)

# Count mismatches
cat("Understory A vs B mismatches:", sum(!comparison$Understory_A_match), "\n")
cat("Understory C vs D mismatches:", sum(!comparison$Understory_C_match), "\n")
cat("Canopy A vs B mismatches:", sum(!comparison$Canopy_A_match, na.rm = TRUE), "\n")
cat("Canopy C vs D mismatches:", sum(!comparison$Canopy_C_match, na.rm = TRUE), "\n")


# Collapse matching patrol columns into unified strata columns
wide_lumped <- wide_df %>%
  mutate(
    Understory_AB = Understory_A,  # A and B are the same
    Understory_CD = Understory_C,  # C and D are the same
    Canopy_AB     = if ("Canopy_A" %in% names(.)) Canopy_A else NA,
    Canopy_CD     = if ("Canopy_C" %in% names(.)) Canopy_C else NA
  ) %>%
  select(Site, Species, Understory_AB, Understory_CD, Canopy_AB, Canopy_CD)
write_csv(wide_lumped, "data/output/summary_by_site_species_strata_collapsed.csv")
# these steps were just for plotting of the plant species into a summary table

# here starts the preparation of the data for the analyses
# Summarize per TwigCode
summary(dataset_raw)
df_summary <- dataset_raw %>%
  group_by(TwigCode, Site, Species, Strata, Individual, Treatment, Experiment, 
           PatrolIndiv, Twig, Patrol2, PlantSpeciesReal, PlantSpeciesAnal, IndividualCode) %>%
  summarise(
    Area.x = mean(Area.x, na.rm = TRUE),
    Area.y = mean(Area.y, na.rm = TRUE),
    Herbivory = mean(Herbivory, na.rm = TRUE),
    HerbProp = mean(HerbProp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Reshape so Start/End values appear in separate columns
  pivot_wider(
    names_from = Patrol2,
    values_from = c(Area.x, Area.y, Herbivory, HerbProp),
    names_sep = "_"
  )

# View result
print(df_summary)
summary(df_summary)

# Save to CSV
write_csv(df_summary, "data/output/twig_summary_start_end.csv")

# Add difference column
df_summary <- df_summary %>%
  mutate(Diff = HerbProp_End - HerbProp_Start)
summary(df_summary)

# Set negative and NA Diff values to 0
df_summary$Diff[is.na(df_summary$Diff) | df_summary$Diff < 0] <- 0
summary(df_summary)

# Save to CSV
write_csv(df_summary, "data/output/twig_summary_start_end_difference.csv")

