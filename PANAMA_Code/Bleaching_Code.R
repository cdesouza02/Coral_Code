library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(vegan)

#making a data fram that isolates transect and condition
combined_colony_data <- PAN.BDT_ColonyData_copy %>%
  select(Transect, X092022_Condition, X102023_Condition) %>%
  pivot_longer(cols = c(X092022_Condition, X102023_Condition))
combined_colony_data

# Comparing before and after health in all three transects
ggplot(combined_colony_data) +
 aes(x = name, fill = value) +
 geom_bar() +
 scale_fill_brewer(palette = "PRGn", 
 direction = 1) +
  labs(x = "Date Sampled", y = "Number of Colonies", fill = "Health Status") +
 theme_minimal() +
  facet_wrap(vars(Transect))

# Comparing before and after health of all species
library(ggplot2)
ggplot(species_colony_data) +
 aes(x = name, fill = value) +
 geom_bar() +
 scale_fill_brewer(palette = "PRGn", 
 direction = 1) +
 labs(x = "Date Sampled", y = "Number of Colonies", fill = "Health Status") +
 theme_minimal() +
 theme(legend.position = "bottom", text = element_text(size=6)) +
 facet_wrap(vars(Species), ncol = 10L)


#isolating T3 species
T3_species <- PAN.BDT_ColonyData_copy[PAN.BDT_ColonyData_copy$Transect == "Crawl Cay",] %>%
 select(Species, X092022_Condition, X102023_Condition) %>%
  pivot_longer(cols = c(X092022_Condition, X102023_Condition))
T3_species

# making a bar graph with just T3 species
ggplot(T3_species) +
  aes(x = name, fill = value) +
  geom_bar() +
  scale_fill_brewer(palette = "PRGn", direction = 1) +
  labs(
    x = "Date Sampled",
    y = "Number of Colonies",
    fill = "Health Status"
  ) +
  theme_minimal() +
  facet_wrap(vars(Species), ncol = 10L)

#removing species only found in T3

species_in_all_sites <- PAN.BDT_ColonyData_copy %>%
  filter(Species %in% c("CNAT", "MCAV", "PAST", "SSID", "ORBI")) %>%
  select(Species, X092022_Condition, X102023_Condition) %>%
  pivot_longer(cols = c(X092022_Condition, X102023_Condition))
species_in_all_sites

# making a bar graph with species found in all sites
ggplot(species_in_all_sites) +
  aes(x = name, fill = value) +
  geom_bar() +
  scale_fill_brewer(palette = "PRGn", direction = 1) +
  labs(
    x = "Date Sampled",
    y = "Number of Colonies",
    fill = "Health Status "
  ) +
  theme_minimal() +
  facet_wrap(vars(Species), ncol = 10L)

#comparing transect when we take out the ones only in T3
transects_with_common_species <- PAN.BDT_ColonyData_copy %>%
  filter(Species %in% c("CNAT", "MCAV", "PAST", "SSID", "ORBI")) %>%
  select(Transect, X092022_Condition, X102023_Condition) %>%
  pivot_longer(cols = c(X092022_Condition, X102023_Condition))
transects_with_common_species

# bar graph comparing three transects again but only when they have common species
ggplot(transects_with_common_species) +
  aes(x = name, fill = value) +
  geom_bar() +
  scale_fill_brewer(palette = "PRGn", 
                    direction = 1) +
  labs(x = "Date Sampled", y = "Number of Colonies", fill = "Health Status") +
  theme_minimal() +
  facet_wrap(vars(Transect))

#isolating T1 and T2
T1_T2_species <- PAN.BDT_ColonyData_copy %>%
  filter(Transect %in% c("STRI Reef", "Juan Point Reef")) %>%
  select(Transect, X092022_Condition, X102023_Condition) %>%
  pivot_longer(cols = c(X092022_Condition, X102023_Condition))
T1_T2_species


# making a bar graph with just T1 and T2 species
ggplot(T1_T2_species) +
  aes(x = name, fill = value) +
  geom_bar() +
  scale_fill_manual(
    values = c(`CLB ` = "#740387",
               CLP = "#81BB85",
               Healthy = "#068438")
  ) +
  labs(
    x = "Date Sampled",
    y = "Number of Colonies",
    fill = "Health Status"
  ) +
  theme_minimal() +
  facet_wrap(vars(Transect))
