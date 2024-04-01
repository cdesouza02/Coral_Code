library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(vegan)
library(lme4)

head(PAN.BDT_ColonyData_copy)


# Print the original column names
print(names(PAN.BDT_ColonyData_copy))

# Change the names of columns 092022_condition and 102023_condition
names(PAN.BDT_ColonyData_copy)[14:16] <- c("Sep_2022", "X092022_Percentage", "Oct_2023")

# Print the updated column names
print(names(PAN.BDT_ColonyData_copy))

#making a data frame that isolates transect and condition
combined_colony_data <- PAN.BDT_ColonyData_copy %>%
  select(Transect, Sep_2022, Oct_2023) %>%
  pivot_longer(cols = c(Sep_2022, Oct_2023))
combined_colony_data

# reordering so sep_2022 is before oct_2023
combined_colony_data$name <- factor(combined_colony_data$name, levels = c("Sep_2022", "Oct_2023"))

# Comparing before and after health in all three transects
ggplot(combined_colony_data) +
  aes(x = name, fill = value) +
  geom_bar() +
  scale_fill_brewer(palette = "PRGn", 
                    direction = 1) +
  labs(x = "Date Sampled", y = "# of Colonies", title = "Bleaching in Three Reefs Over Time", fill = "Condition") +
  theme_minimal() +
  facet_wrap(vars(Transect))

# reordering so sep_2022 is before oct_2023
species_colony_data$name <- factor(species_colony_data$name, levels = c("Sep_2022", "Oct_2023"))

#making a data frame that isolates transect and condition
species_colony_data <- PAN.BDT_ColonyData_copy %>%
  select(Species, Sep_2022, Oct_2023) %>%
  pivot_longer(cols = c(Sep_2022, Oct_2023))
species_colony_data

# Comparing before and after health of all species
library(ggplot2)
ggplot(species_colony_data) +
  aes(x = name, fill = value) +
  geom_bar() +
  scale_fill_brewer(palette = "PRGn", 
                    direction = 1) +
  labs(x = "Date Sampled", y = "# of Colonies", title = "Bleaching of Coral Species Over Time", fill = "Condition") +
  theme_minimal() +
  theme(legend.position = "bottom", text = element_text(size=10)) +
  facet_wrap(vars(Species), ncol = 10L)

#isolating T3 
T3_species <- PAN.BDT_ColonyData_copy[PAN.BDT_ColonyData_copy$Transect == "Crawl Cay",] %>%
  select(Species, Sep_2022, Oct_2023) %>%
  pivot_longer(cols = c(Sep_2022, Oct_2023))
T3_species

# reordering so sep_2022 is before oct_2023
T3_species$name <- factor(T3_species$name, levels = c("Sep_2022", "Oct_2023"))

# making a bar graph with just T3 species
ggplot(T3_species) +
  aes(x = name, fill = value) +
  geom_bar() +
  scale_fill_brewer(palette = "PRGn", direction = 1) +
  labs(x = "Date Sampled", y = "# of Colonies", title = "Bleaching in Crawl Cay Over Time", fill = "Condition") +
  theme_minimal() +
  facet_wrap(vars(Species), ncol = 10L)

#removing species only found in T3

species_in_all_sites <- PAN.BDT_ColonyData_copy %>%
  filter(Species %in% c("CNAT", "MCAV", "PAST", "SSID", "ORBI")) %>%
  select(Species, Sep_2022, Oct_2023) %>%
  pivot_longer(cols = c(Sep_2022, Oct_2023))
species_in_all_sites

# reordering so sep_2022 is before oct_2023
species_in_all_sites$name <- factor(species_in_all_sites$name, levels = c("Sep_2022", "Oct_2023"))

# making a bar graph with species found in all sites
ggplot(species_in_all_sites) +
  aes(x = name, fill = value) +
  geom_bar() +
  scale_fill_brewer(palette = "PRGn", direction = 1) +
  labs(x = "Date Sampled", y = "# of Colonies", title = "Effect of Bleaching on Common Coral Species", fill = "Condition") +
  theme_minimal() +
  facet_wrap(vars(Species), ncol = 10L)

#comparing transect when we take out the ones only in T3
transects_with_common_species <- PAN.BDT_ColonyData_copy %>%
  filter(Species %in% c("CNAT", "MCAV", "PAST", "SSID", "ORBI")) %>%
  select(Transect, Sep_2022, Oct_2023) %>%
  pivot_longer(cols = c(Sep_2022, Oct_2023))
transects_with_common_species

# reordering so sep_2022 is before oct_2023
transects_with_common_species$name <- factor(transects_with_common_species$name, levels = c("Sep_2022", "Oct_2023"))

# bar graph comparing three transects again but only when they have common species
ggplot(transects_with_common_species) +
  aes(x = name, fill = value) +
  geom_bar() +
  scale_fill_brewer(palette = "PRGn", 
                    direction = 1) +
  labs(x = "Date Sampled", y = "# of Colonies", title = "Bleaching in Three Reefs Over Time", fill = "Condition") +
  theme_minimal() +
  facet_wrap(vars(Transect))

#isolating T1 and T2
T1_T2_species <- PAN.BDT_ColonyData_copy %>%
  filter(Transect %in% c("STRI Reef", "Juan Point Reef")) %>%
  select(Species, Sep_2022, Oct_2023) %>%
  pivot_longer(cols = c(Sep_2022, Oct_2023))
T1_T2_species

# reordering so sep_2022 is before oct_2023
T1_T2_species$name <- factor(T1_T2_species$name, levels = c("Sep_2022", "Oct_2023"))

# making a bar graph with just T1 and T2 species
ggplot(T1_T2_species) +
  aes(x = name, fill = value) +
  geom_bar() +
  scale_fill_manual(
    values = c(`CLB ` = "#740387",
               CLP = "#81BB85",
               Healthy = "#068438")
  ) +
  labs(x = "Date Sampled", y = "# of Colonies", title = "Bleaching in STRI Reef and Juan Point Reef", fill = "Condition") +
  theme_minimal() +
  facet_wrap(vars(Species), ncol = 10L)

