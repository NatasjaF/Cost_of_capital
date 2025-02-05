# =============================================================================================================================
# === Load Libraries === 
# =============================================================================================================================
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(plotly)
library(readxl)
library(dplyr)
library(scales)
library(ggpattern)  

# =============================================================================================================================
# === Data Import and Preprocessing ===
# =============================================================================================================================

Elec_GCAM_WA <- read_csv("../Elec_price_final_electricity_demand.csv")

# Load region mapping from external file (CSV recommended)
load_region_mapping <- function() {
  region_mapping <- fread("../GCAM_region_mapping.csv")
  return(region_mapping)
}

# Preprocess data
preprocess_data <- function(data, region_mapping) {
  data_long <- pivot_longer(data, cols = starts_with("20"), names_to = "year", values_to = "value")
  data_long <- left_join(data_long, region_mapping, by = "Region")
  return(data_long)
}

# Load region mapping
region_mapping <- load_region_mapping()

# Preprocess data
Elec_GCAM_WA <- preprocess_data(Elec_GCAM_WA, region_mapping)

# Preprocess the data
Elec_GCAM_WA_long <- Elec_GCAM_WA %>%
  mutate(
    Variable = case_when(
      Variable == "electricity_price" ~ "electricity_price",
      Variable == "final_energy" ~ "final_energy",
      TRUE ~ Variable
    )
  ) %>%
  pivot_wider(
    names_from = "Variable",
    values_from = "value"
  ) 

# =============================================================================================================================
# === Figure 3a Weighted average electricity price ===
# =============================================================================================================================

#Calculate the weighted average electricity price
Elec_GCAM_WA_long <- Elec_GCAM_WA_long %>%
  filter(!is.na(final_energy) & !is.na(electricity_price)) %>% 
  group_by(Scenario, Grouped_Region, year) %>%
  summarise(
    Weighted_Avg_Electricity_Price = sum(electricity_price * final_energy) / sum(final_energy),
    .groups = "drop" 
  )


Elec_GCAM_WAV <- Elec_GCAM_WA_long

# Calculate the relative change compared to baseline
Elec_GCAM_WAV_NDC <- Elec_GCAM_WAV %>%
  group_by(year, Grouped_Region) %>%
  mutate(
    `change` = ifelse(
      Scenario != "NDC_EI_CoC",
      case_when(
        Weighted_Avg_Electricity_Price * Weighted_Avg_Electricity_Price[Scenario == "NDC_EI_CoC"] > 0 ~ ((Weighted_Avg_Electricity_Price - Weighted_Avg_Electricity_Price[Scenario == "NDC_EI_CoC"]) / abs(Weighted_Avg_Electricity_Price[Scenario == "NDC_EI_CoC"])) * 100,
        Weighted_Avg_Electricity_Price > 0 & Weighted_Avg_Electricity_Price[Scenario == "NDC_EI_CoC"] < 0 ~ ((Weighted_Avg_Electricity_Price - Weighted_Avg_Electricity_Price[Scenario == "NDC_EI_CoC"]) / abs(Weighted_Avg_Electricity_Price[Scenario == "NDC_EI_CoC"])) * 100, # Handles change from negative to positive
        Weighted_Avg_Electricity_Price < 0 & Weighted_Avg_Electricity_Price[Scenario == "NDC_EI_CoC"] > 0 ~ ((Weighted_Avg_Electricity_Price - Weighted_Avg_Electricity_Price[Scenario == "NDC_EI_CoC"]) / abs(Weighted_Avg_Electricity_Price[Scenario == "NDC_EI_CoC"])) * 100, # Handles change from positive to negative
        TRUE ~ NA_real_  # Handle cases where emissions are zero or undefined
      ),
      0  # Baseline scenario itself has 0% change
    )
  ) %>%
  ungroup()


#Select scenarios with NDC baseline
selected_scenarios <- c("Survey_CoC",  "Fossil_CoC", "NonFossil_CoC", "Fragmented_CoC")

#Filter scenarios & years
Elec_GCAM_WAV_NDC <- Elec_GCAM_WAV_NDC %>%
  filter(Scenario %in% selected_scenarios) %>%
  filter(year %in% c(2030, 2050))


# Define color palette for regions
region_colors <- c(
  "Africa" = "#8DD3C7",
  "China" = "#377eb8",
  "Latin America" = "#fb9a99",
  "India" = "#FDB462",
  "RoA" = "#b2df8a",
  "S_E_Asia" = "#e78ac3",
  "West" = "#984ea3"
)

# Define scenario order
scenario_order <- c("Survey_CoC", "Fossil_CoC", "NonFossil_CoC", "Fragmented_CoC")

Elec_GCAM_WAV_NDC <- Elec_GCAM_WAV_NDC %>%
  mutate(Scenario = factor(Scenario, levels = scenario_order))

# Plot
Elec_price_plot <- ggplot(Elec_GCAM_WAV_NDC, aes(x = Grouped_Region, y = change, fill = Grouped_Region)) +
  geom_bar(stat = "identity", position = "dodge", colour = "grey20") +
  facet_grid(year ~ Scenario, scales = "free_y",
             labeller = labeller(
               Scenario = c("Survey_CoC" = "Sv", 
                            "Fossil_CoC" = "Sv-F", 
                            "NonFossil_CoC" = "Sv-NF",
                            "Fragmented_CoC" = "Frag"))) +
  labs(x = NULL, 
       y = expression("Difference from baseline in electricity price [%]"),  
       fill = "Region") + 
  theme(panel.background = element_blank(),  
        panel.border = element_rect(color = "black", fill = NA),  
        panel.grid.major = element_line(color = "grey", linetype = "dotted", size = 0.05),  
        panel.grid.minor = element_blank(),  
        axis.line = element_line(color = "black"),  
        axis.text.x = element_blank(),  
        axis.text.y = element_text(color = "black", size = 10),  
        axis.title.y = element_text(angle = 90, hjust = 1, color = "black", size = 10, face = "bold"), 
        axis.title.x = element_blank(),
        legend.title = element_text(size = 7, face = "bold"),  
        legend.text = element_text(size = 7),  
        legend.position = "bottom",  
        legend.justification = "center",  
        legend.box = "horizontal",  
        plot.title = element_text(hjust = 0.5, size = 18),
        strip.background = element_rect(fill = "grey90"),  
        strip.text = element_text(size = 9)) +  
  scale_fill_manual(values = region_colors) + 
  scale_y_continuous(limits = c(-5, 20), breaks = seq(-5, 20, by = 5)) +  
  geom_hline(yintercept = 0, color = "grey10", size = 0.7) +  
  guides(fill = guide_legend(nrow = 1))  

Elec_price_plot

# =============================================================================================================================
# === Figure 3b Investments in electricity supply ===
# =============================================================================================================================

CoC <- read_excel("../Results_all_IAMC.xlsx")

CoC_long <- gather(CoC, year, value, 6:15, factor_key=TRUE)

CoC_long <- preprocess_data(CoC, region_mapping)


selected_variables <- c("Investment|Energy Supply|Electricity|Biomass|w/ CCS",
                        "Investment|Energy Supply|Electricity|Biomass|w/o CCS",
                        "Investment|Energy Supply|Electricity|Coal|w/ CCS",
                        "Investment|Energy Supply|Electricity|Coal|w/o CCS", 
                        "Investment|Energy Supply|Electricity|Gas|w/ CCS",
                        "Investment|Energy Supply|Electricity|Gas|w/o CCS",
                        "Investment|Energy Supply|Electricity|Nuclear",
                        "Investment|Energy Supply|Electricity|Oil|w/ CCS",
                        "Investment|Energy Supply|Electricity|Oil|w/o CCS",
                        "Investment|Energy Supply|Electricity|Solar",
                        "Investment|Energy Supply|Electricity|Wind",
                        "Investment|Energy Supply|Electricity|Transmission and Distribution")


filtered_CoC_long <- CoC_long %>%
  filter(Variable %in% selected_variables)

Inv_summaries <- filtered_CoC_long %>%
  group_by(Grouped_Region, Scenario, Model, year, Variable) %>%
  summarize(total_value = sum(value, na.rm = TRUE), .groups = 'drop')

#Determine the absolute difference from baseline
NDC_inv <- Inv_summaries %>%
  filter(Scenario == "NDC_EI_baseWACC") %>%
  select(Model, Grouped_Region, Variable, year, total_value) %>%
  rename(reference_value = total_value)

inv_summaries_NDC <- Inv_summaries %>%
  left_join(NDC_inv, by = c("Model", "Grouped_Region", "Variable", "year")) %>%
  mutate(abs_diff = total_value - reference_value)

selected_scenarios <- c("Survey_NDC",  "Fossil_NDC", "NonFossil_NDC", "Fragmented_NDC")

# Filter scenarios, regions, years
inv_summaries_NDC <- inv_summaries_NDC  %>%
  filter(Grouped_Region != "World") %>%
  filter(Scenario %in% selected_scenarios) %>%
  filter(year %in% c(2030, 2050))

inv_summaries_NDC <- inv_summaries_NDC %>%
  mutate(Scenario = factor(Scenario, levels = c("Survey_NDC", "Fossil_NDC", "NonFossil_NDC", "Fragmented_NDC")))

# Add scenario labels
scenario_labels <- c(
  "Survey_NDC" = "Sv",
  "Fossil_NDC" = "Sv-F",
  "NonFossil_NDC" = "Sv-NF",
  "Fragmented_NDC" = "Frag"
)

inv_summaries_NDC <- inv_summaries_NDC %>%
  mutate(Scenario_label = scenario_labels[Scenario])


# Rename technologies
technology_mapping <- c(
  "Investment|Energy Supply|Electricity|Biomass|w/ CCS" = "Biomass w/ CCS",
  "Investment|Energy Supply|Electricity|Biomass|w/o CCS" = "Biomass w/o CCS",
  "Investment|Energy Supply|Electricity|Coal|w/ CCS" = "Coal w/ CCS",
  "Investment|Energy Supply|Electricity|Coal|w/o CCS" = "Coal w/o CCS",
  "Investment|Energy Supply|Electricity|Gas|w/ CCS" = "Gas w/ CCS",
  "Investment|Energy Supply|Electricity|Gas|w/o CCS" = "Gas w/o CCS",
  "Investment|Energy Supply|Electricity|Nuclear" = "Nuclear",
  "Investment|Energy Supply|Electricity|Oil|w/ CCS" = "Oil w/ CCS",
  "Investment|Energy Supply|Electricity|Oil|w/o CCS" = "Oil w/o CCS",
  "Investment|Energy Supply|Electricity|Solar" = "Solar",
  "Investment|Energy Supply|Electricity|Wind" = "Wind",
  "Investment|Energy Supply|Electricity|Transmission and Distribution" = "T&D"
)

inv_summaries_NDC <- inv_summaries_NDC %>%
  mutate(Technology = technology_mapping[Variable])

tech_colors <- c(
  "Oil w/o CCS" = "#9931CC",
  "Oil w/ CCS" = "#9931CC",
  "Gas w/o CCS" = "#01BEFE",
  "Gas w/ CCS" = "#01BEFE",
  "Coal w/o CCS" = "black",
  "Coal w/ CCS" = "black",
  "Biomass w/o CCS" = "#FFBE46",
  "Biomass w/ CCS" = "#FFBE46",
  "Wind" = "#006401",
  "Solar" = "#FFFF01",
  "Nuclear" = "#FF4D81",
  "T&D" = "#A6DD52"
)

technology_order <- c(
  "Oil w/o CCS", "Oil w/ CCS", "Gas w/o CCS", "Gas w/ CCS",
  "Coal w/o CCS", "Coal w/ CCS", "Biomass w/o CCS", "Biomass w/ CCS",
  "Wind", "Solar", "Nuclear", "T&D"
)

inv_summaries_NDC <- inv_summaries_NDC %>%
  mutate(Technology = factor(Technology, levels = technology_order))

inv_summaries_NDC_solid <- inv_summaries_NDC %>%
  filter(Technology %in% c("Oil w/o CCS", "Gas w/o CCS", "Coal w/o CCS", "Biomass w/o CCS", "Wind", "Solar", "Nuclear", "T&D"))

inv_summaries_NDC_stripe <- inv_summaries_NDC %>%
  filter(Technology %in% c("Oil w/ CCS", "Gas w/ CCS", "Coal w/ CCS", "Biomass w/ CCS"))

scenario_order <- c("Sv", "Sv-F", "Sv-NF", "Frag")

inv_summaries_NDC_solid <- inv_summaries_NDC_solid %>%
  mutate(Scenario_label = factor(Scenario_label, levels = scenario_order))

inv_summaries_NDC_stripe <- inv_summaries_NDC_stripe %>%
  mutate(Scenario_label = factor(Scenario_label, levels = scenario_order))

# Plot
Investments_plot <- ggplot() +
  geom_bar(
    data = inv_summaries_NDC_solid,
    aes(x = Scenario_label, y = abs_diff, fill = Technology),
    stat = "identity",
    position = "stack"
  ) +
  geom_hline(yintercept = 0, color = "black", size = 0.5) +
  geom_bar_pattern(
    data = inv_summaries_NDC_stripe,
    aes(x = Scenario_label, y = abs_diff, fill = Technology),
    stat = "identity",
    position = "stack",
    pattern = "stripe",
    pattern_fill = "white",
    pattern_colour = 'white',
    pattern_angle = 45,
    pattern_density = 0.2,
    pattern_spacing = 0.08,
    pattern_key_scale_factor = 0.5
  ) +
  facet_grid(year ~ Grouped_Region, scales = "free_x", space = "free_x") + 
  scale_fill_manual(values = tech_colors ) +
  coord_cartesian(ylim = c(-20, 30)) + # 
  scale_fill_manual(values = tech_colors, breaks = technology_order) + 
  scale_y_continuous(
    breaks = seq(-20, 30, by = 10),
  ) +  
  guides(pattern = "none") +
  theme_minimal() +
  labs(
    x = "",
    y = "Difference from baseline in investment [billion US$2010/yr]",
    fill = "Technology"
  ) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1, size = 9, face = "bold"),
    axis.text.y = element_text(size = 10, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(hjust = 1, size = 15, margin = margin(r = 10)),
    legend.text = element_text(size = 10),
    axis.ticks = element_line(color = "black"), 
    legend.title = element_text(size = 12, face = "bold"),
    strip.text.x = element_text(size = 15), 
    strip.text.y = element_text(size = 15), 
    strip.background.x = element_rect(fill = "grey90", color = NA), 
    strip.background.y = element_rect(fill = "grey90", color = NA), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    panel.border = element_rect(color = "black", fill = NA, size = 1), 
    panel.spacing.x = unit(0.2, "lines"), 
    panel.spacing.y = unit(0.5, "lines")  
  )

Investments_plot
