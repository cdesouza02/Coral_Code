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

#making a data frame that isolates transect and condition
species_colony_data <- PAN.BDT_ColonyData_copy %>%
  select(Species, Sep_2022, Oct_2023) %>%
  pivot_longer(cols = c(Sep_2022, Oct_2023))
species_colony_data

# reordering so sep_2022 is before oct_2023
species_colony_data$name <- factor(species_colony_data$name, levels = c("Sep_2022", "Oct_2023"))

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
  filter(Species %in% c("CNAT", "MCAV", "SSID", "ORBI")) %>%
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
  filter(Species %in% c("CNAT", "MCAV", "SSID", "ORBI")) %>%
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

# extract 102023_condition and add to a new column 1= color loss
color_loss <- PAN.BDT_ColonyData_copy %>%
  mutate(Color_Loss = ifelse(
    trimws(Oct_2023) %in% c("CLP", "CLB", "CLB;CLP"),
    1, 0
  ))
View(color_loss)

# Assuming color_loss is your data frame
color_loss_percentage <- mean(color_loss$Color_Loss == 1) * 100
color_loss_percentage

# identifying how many corals in each transect are color loss if color loss in 2023=1

#filter for bleaching in Crawl Cay 2023
Crawl_Cay_filter <- color_loss %>%
  filter(Transect == "Crawl Cay")
crawl_percentage <- mean(Crawl_Cay_filter$Color_Loss == 1) * 100
print(crawl_percentage)
#filter for bleaching in STRI 2023
STRI_filter <- color_loss %>%
  filter(Transect == "STRI Reef")
STRI_percentage <- mean(STRI_filter$Color_Loss == 1) * 100
print(STRI_percentage)
#filter for bleaching in juan 2023
juan_filter <- color_loss %>%
  filter(Transect == "Juan Point Reef")
juan_percentage <- mean(juan_filter$Color_Loss == 1) * 100
print(juan_percentage)

# identifying percentage of bleaching in each species of corals if color loss = 1
#CNAT
CNAT_filter <- color_loss %>%
  filter(Species == "CNAT")
CNAT_percentage <- mean(CNAT_filter$Color_Loss == 1) * 100
print(CNAT_percentage)
#MCAV
MCAV_filter <- color_loss %>%
  filter(Species == "MCAV")
MCAV_percentage <- mean(MCAV_filter$Color_Loss == 1) * 100
print(MCAV_percentage)
#ORBI
ORBI_filter <- color_loss %>%
  filter(Species == "ORBI")
ORBI_percentage <- mean(ORBI_filter$Color_Loss == 1) * 100
print(ORBI_percentage)
#PSTR
PSTR_filter <- color_loss %>%
  filter(Species == "PSTR")
PSTR_percentage <- mean(PSTR_filter$Color_Loss == 1) * 100
print(PSTR_percentage)
#SSID
SSID_filter <- color_loss %>%
  filter(Species == "SSID")
SSID_percentage <- mean(SSID_filter$Color_Loss == 1) * 100
print(SSID_percentage)

# renaming the Transect percentages
cat("Percent of colonies in Crawl Cay that lost color in 2023:", crawl_percentage)
cat("Percent of colonies in Juan Point that lost color in 2023:", juan_percentage)
cat("Percent of colonies in the STRI Reef that lost color in 2023:", STRI_percentage)

# renaming the species percentages 
cat("Percent of CNAT colonies lost color in 2023:", CNAT_percentage)
cat("Percent of MCAV colonies lost color in 2023:", MCAV_percentage)
cat("Percent of ORBI colonies lost color in 2023:", ORBI_percentage)
cat("Percent of PSTR colonies lost color in 2023:", PSTR_percentage)
cat("Percent of SSID colonies lost color in 2023:", SSID_percentage)

# New data frame focusing on the percentage bleached in T1 T2 species
T1_T2_boxplot <- color_loss %>%
  filter(Transect %in% c("STRI Reef", "Juan Point Reef")) %>%
  select(Species, X102023_Percentage) %>%
  pivot_longer(cols = X102023_Percentage, names_to = "T1_T2") %>%
  mutate(value = as.numeric(gsub("%", "", value))) %>%
  na.omit()
# Display the modified data
T1_T2_boxplot
# box plot displaying percentage of color loss in species in STRI and Juan
ggplot(T1_T2_boxplot) +
  aes(x = "", y = value, fill = Species) +
  geom_boxplot() +
  scale_fill_hue(direction = 1) +
  labs(
    x = "Coral Colonies",
    y = "Percentage Color Loss",
    title = "Percentage of Color Loss in STRI Reef and Juan Point Reef",
    fill = "Species"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# new data fram focusing on the percentage belached in T3 
T3_boxplot <- color_loss[color_loss$Transect == "Crawl Cay",] %>%
  select(Species, X102023_Percentage) %>%
  pivot_longer(cols = X102023_Percentage, names_to = "T3") %>%
  mutate(value = as.numeric(gsub("%", "", value))) %>%
  na.omit()
T3_boxplot
# box plot displaying percentage of color loss in species in Crawl Cay
ggplot(T3_boxplot) +
  aes(x = "", y = value, fill = Species) +
  geom_boxplot() +
  scale_fill_hue(direction = 1) +
  labs(
    x = "Coral Colonies",
    y = "Percentage Color Loss",
    title = "Percentage of Color Loss in Crawl Cay",
    fill = "Species"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


# new data fram focusing on the percentage belached in all 
all_boxplot <- color_loss %>%
  select(Species, X102023_Percentage) %>%
  pivot_longer(cols = X102023_Percentage) %>%
  mutate(value = as.numeric(gsub("%", "", value))) %>%
  na.omit()
all_boxplot
# box plot displaying percentage of color loss in Corals by Species
ggplot(all_boxplot) +
  aes(x = "", y = value, fill = Species) +
  geom_boxplot() +
  scale_fill_hue(direction = 1) +
  labs(
    x = "Coral Colonies",
    y = "Percentage Color Loss",
    title = "Percentage of Color Loss in Corals by Species",
    fill = "Species"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# new data fram focusing on the percentage belached in T1 
T1_boxplot <- color_loss[color_loss$Transect == "STRI",] %>%
  select(Species, X102023_Percentage) %>%
  pivot_longer(cols = X102023_Percentage, names_to = "T1") %>%
  mutate(value = as.numeric(gsub("%", "", value))) %>%
  na.omit()
T1_boxplot

# box plot displaying percentage of color loss in species in STRI Reef
ggplot(T1_boxplot) +
  aes(x = "", y = value, fill = Species) +
  geom_boxplot() +
  scale_fill_hue(direction = 1) +
  labs( x = "Coral Colonies", y = "Percentage Color Loss", title = "Percentage of Color Loss in STRI Reef", fill = "Species"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


 # new data fram focusing on the percentage belached in T2 
T2_boxplot <- color_loss[color_loss$Transect == "Juan Point Reef",] %>%
select(Species, X102023_Percentage) %>%
pivot_longer(cols = X102023_Percentage, names_to = "T2") %>%
mutate(value = as.numeric(gsub("%", "", value))) %>%
na.omit()
T2_boxplot

# box plot displaying percentage of color loss in species in Juan Point
ggplot(T2_boxplot) +
aes(x = "", y = value, fill = Species) +
geom_boxplot() +
scale_fill_hue(direction = 1) +
labs( x = "Coral Colonies", y = "Percentage Color Loss", title = "Percentage of Color Loss in Juan Point", fill = "Species"
) +
theme_minimal() +
theme(legend.position = "bottom")

#Finding significance between species
 speciesmod <- glmer(Color_Loss ~ Species+
                        (1|Transect),
                      data=color_loss,family="poisson")
summary(speciesmod)
anova(speciesmod)
nullmod <- glmer(Color_Loss ~ 1+
                     (1|Transect),
                   data=color_loss,family="poisson")
lr_test=anova(nullmod, speciesmod, test="Chisq")
lr_test
library(lsmeans)
lsmeans(speciesmod, pairwise~Species, contrasts="tukey")
install.packages(emmeans)
library(emmeans)
emmeans(speciesmod, pairwise~Species, contrasts="tukey")

#finding significance between transects
common_species <- color_loss %>%
  filter(Species %in% c("CNAT", "MCAV", "SSID", "ORBI")) %>%
  select(Transect, Species, Sep_2022, Oct_2023, Color_Loss)
View(common_species)
transectmod <- glmer(Color_Loss ~ Transect + (1|Species), data = common_species, family = "poisson")
summary(transectmod)
anova(transectmod)
# Fit the null model to the same dataset as transectmod
null_transectmod <- glmer(Color_Loss ~ 1 + (1|Transect), data = color_loss, family = "poisson")
# Perform likelihood ratio test
lr_test <- anova(transectmod, test = "Chisq")
lr_test

library(emmeans)
emmeans(transectmod, pairwise~Transect, contrasts="tukey")

