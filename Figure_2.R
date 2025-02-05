# =============================================================================================================================
# === Load Libraries === 
# =============================================================================================================================
library(ggplot2)
library(tidyverse)
library(readxl)
library(dplyr)
library(scales)
library(ggpattern)  

# =============================================================================================================================
# === Data Import and Preprocessing ===
# =============================================================================================================================

CoC <- read_excel("../Results_all_IAMC.csv")

CoC_long <- gather(CoC, year, value, 6:15, factor_key=TRUE)

# Load region mapping
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
CoC_long <- preprocess_data(CoC, region_mapping)

# =============================================================================================================================
# === Figure 2a: CO2 emissions from energy and industrial processes === 
# =============================================================================================================================

#Filter emissions
filtered_CoC_long <- CoC_long %>%
  filter(Variable == "Emissions|CO2|Energy and Industrial Processes") 

summarized_CoC_long <- filtered_CoC_long %>%
  group_by(Model, Scenario, year, Grouped_Region) %>%
  summarise(sum_values = sum(value, na.rm = TRUE))


# Calculate the relative change compared to baseline

#NDC: relative share 
Emissions_share_NDC <- summarized_CoC_long %>%
  group_by(year, Grouped_Region) %>%
  mutate(
    `change` = ifelse(
      Scenario != "NDC_EI_baseWACC",
      case_when(
        sum_values * sum_values[Scenario == "NDC_EI_baseWACC"] > 0 ~ ((sum_values - sum_values[Scenario == "NDC_EI_baseWACC"]) / abs(sum_values[Scenario == "NDC_EI_baseWACC"])) * 100,
        sum_values > 0 & sum_values[Scenario == "NDC_EI_baseWACC"] < 0 ~ ((sum_values - sum_values[Scenario == "NDC_EI_baseWACC"]) / abs(sum_values[Scenario == "NDC_EI_baseWACC"])) * 100, # Handles change from negative to positive
        sum_values < 0 & sum_values[Scenario == "NDC_EI_baseWACC"] > 0 ~ ((sum_values - sum_values[Scenario == "NDC_EI_baseWACC"]) / abs(sum_values[Scenario == "NDC_EI_baseWACC"])) * 100, # Handles change from positive to negative
        TRUE ~ NA_real_  # Handle cases where emissions are zero or undefined
      ),
      0  # Baseline scenario itself has 0% change
    )
  ) %>%
  ungroup()

Emissions_share <- Emissions_share_NDC %>%
  filter(
    year %in% seq(2020, 2050, by = 5),  
    Scenario %in% c(
      "Fossil_NDC", "Fragmented_NDC", "NonFossil_NDC",
      "Survey_NDC", "Survey_NDC_Infl"
    ) 
  )


selected_scenarios <- c("Survey_NDC",  "Fossil_NDC", "NonFossil_NDC", "Fragmented_NDC")

Emissions_share <- Emissions_share %>%
  filter(Scenario %in% selected_scenarios)

scenario_order <- c("Survey_NDC", "Fossil_NDC", "NonFossil_NDC", "Fragmented_NDC")
Emissions_share$Scenario <- factor(Emissions_share$Scenario, levels = scenario_order)
Emissions_share$year <- as.numeric(as.character(Emissions_share$year))

Emissions_share <- Emissions_share %>% 
  filter(year %in% c(2030, 2050)) 

region_colors <- c(
  "Africa" = "#8DD3C7",
  "China" = "#377eb8",
  "Latin America" = "#fb9a99",
  "India" = "#FDB462",
  "RoA" = "#b2df8a",
  "S_E_Asia" = "#e78ac3",
  "West" = "#984ea3",
  "World" = "grey50"
)


# Create the plot
Emissions_plot <- ggplot(Emissions_share, aes(x = Grouped_Region, y = change, fill = Grouped_Region)) +
  geom_bar(stat = "identity", position = "dodge", colour = "grey20") +
  facet_grid(year ~ Scenario, scales = "free_y",
             labeller = labeller(
               Scenario = c("Survey_NDC" = "Sv", 
                            "Fossil_NDC" = "Sv-F", 
                            "NonFossil_NDC" = "Sv-NF",
                            "Fragmented_NDC" = "Frag"))) +
  labs(x = NULL,  
       y = expression("Difference from baseline in CO"[2]*" emissions [%]"), 
       fill = "Region") + 
  theme(panel.background = element_blank(),  
        panel.border = element_rect(color = "black", fill = NA),  
        panel.grid.major = element_line(color = "grey", linetype = "dotted", size = 0.05),  
        panel.grid.minor = element_blank(),  
        axis.line = element_line(color = "black"),  
        axis.text.x = element_blank(),  
        axis.text.y = element_text(color = "black", size = 14),  
        axis.title.y = element_text(angle = 90, hjust = 1, color = "black", size = 14, face = "bold"), 
        axis.title.x = element_blank(),
        legend.title = element_text(size = 11, face = "bold"),  
        legend.text = element_text(size = 12),  
        legend.position = "bottom",  
        legend.justification = "center",  
        legend.box = "horizontal",  
        plot.title = element_text(hjust = 0.5, size = 18),
        strip.background = element_rect(fill = "grey90"),  
        strip.text = element_text(size = 14)) +  
  scale_fill_manual(values = region_colors) + # Custom colors
  scale_y_continuous(limits = c(-20, 15), breaks = seq(-20, 15, by = 10)) +  
  geom_hline(yintercept = 0, color = "grey10", size = 0.7) +  
  guides(fill = guide_legend(nrow = 1))  

Emissions_plot

# =============================================================================================================================
# === Figure 2c: Secondary electricity production === 
# =============================================================================================================================

selected_variables <- c("Secondary Energy|Electricity|Biomass",
                        "Secondary Energy|Electricity|Coal",
                        "Secondary Energy|Electricity|Gas",
                        "Secondary Energy|Electricity|Nuclear", 
                        "Secondary Energy|Electricity|Oil",
                        "Secondary Energy|Electricity|Solar",
                        "Secondary Energy|Electricity|Wind")

filtered_CoC_long <- CoC_long %>%
  filter(Variable %in% selected_variables)

sec_summaries <- filtered_CoC_long %>%
  group_by(Grouped_Region, Scenario, Model, year, Variable) %>%
  summarize(total_value = sum(value, na.rm = TRUE), .groups = 'drop')

# Multiply the values in sec_summaries with 277.778 to give TWh instead of EJ
sec_summaries <- sec_summaries %>%
  mutate(total_value_TWh = total_value * 277.778)

# Group RES & Fossil Fuels
sec_summaries_grouped <- sec_summaries %>%
  group_by(Model, year, Scenario, Grouped_Region) %>%
  summarize(
    Total_Fossil_Fuels = sum(total_value_TWh[Variable %in% c("Secondary Energy|Electricity|Coal", 
                                                             "Secondary Energy|Electricity|Gas", 
                                                             "Secondary Energy|Electricity|Oil")], na.rm = TRUE),
    Total_RES = sum(total_value_TWh[Variable %in% c("Secondary Energy|Electricity|Solar", 
                                                    "Secondary Energy|Electricity|Wind")], na.rm = TRUE),
    Biomass = sum(total_value_TWh[Variable == "Secondary Energy|Electricity|Biomass"], na.rm = TRUE),
    Nuclear = sum(total_value_TWh[Variable == "Secondary Energy|Electricity|Nuclear"], na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(Total_Fossil_Fuels, Total_RES, Biomass, Nuclear), 
               names_to = "group_var", values_to = "summed_value") %>%
  ungroup()

sec_summaries_grouped <-sec_summaries_grouped %>%
  mutate(group_var = case_when(
    group_var %in% c("Total_Fossil_Fuels")  ~ "Fossil fuels",  
    group_var %in% c("Total_RES") ~ "Wind & solar",  
    TRUE ~ as.character(group_var) 
  ))


#Determine the absolute difference from baseline
NDC_sec <- sec_summaries_grouped %>%
  filter(Scenario == "NDC_EI_baseWACC") %>%
  select(Model, Grouped_Region, group_var, year, summed_value) %>%
  rename(reference_value = summed_value)

sec_summaries_NDC <- sec_summaries_grouped %>%
  left_join(NDC_sec, by = c("Model", "Grouped_Region", "group_var", "year")) %>%
  mutate(abs_diff = summed_value - reference_value)

selected_scenarios <- c("Survey_NDC",  "Fossil_NDC", "NonFossil_NDC", "Fragmented_NDC")

# Filter scenarios, regions, years
sec_summaries_NDC <- sec_summaries_NDC  %>%
  filter(Grouped_Region != "World") %>%
  filter(Scenario %in% selected_scenarios) %>%
  filter(year %in% c(2025, 2030, 2035, 2040, 2045, 2050))

Tech_colours <- c(
  "Fossil fuels" = "black",
  "Wind & solar" = "#00CCCD",
  "Biomass" = "#FFB743",
  "Nuclear" = "#FF4D81"
)

sec_summaries_NDC <- sec_summaries_NDC %>%
  mutate(Scenario = factor(Scenario, levels = c("Survey_NDC", "Fossil_NDC", "NonFossil_NDC", "Fragmented_NDC")))

# Create the plot
Secondary_electricity_plot <- ggplot(sec_summaries_NDC, aes(x = year, y = abs_diff, fill = group_var, group = interaction(group_var, Scenario, Grouped_Region))) +
  geom_area(alpha = 1, position = "stack") + 
  geom_hline(yintercept = 0, color = "black", size = 0.5) + 
  facet_grid(Scenario ~ Grouped_Region,
             labeller = labeller(
               Scenario = c("Survey_NDC" = "Sv", 
                            "Fossil_NDC" = "Sv-F", 
                            "NonFossil_NDC" = "Sv-NF",
                            "Fragmented_NDC" = "Frag"))) +
  scale_fill_manual(values = Tech_colours) + 
  scale_x_discrete(breaks = c(2030, 2040, 2050)) + 
  theme_minimal() +
  labs(
    x = "Year",
    y = "Difference from baseline in electricity generation (TWh)",
    fill = "Technology"
  ) +
  theme(
    strip.text = element_text(size = 11), 
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 12), 
    legend.position = "bottom", 
    legend.text = element_text(size = 12), 
    panel.grid = element_blank(), 
    panel.border = element_rect(color = "black", fill = NA, size = 1), 
    axis.line = element_line(color = "black"), 
    axis.text.y = element_text(color = "black", size = 12),  
    axis.title.y = element_text(angle = 90, hjust = 1, color = "black", size = 14),
    axis.ticks = element_line(color = "black"), 
    strip.background = element_rect(fill = "grey90", color = NA) 
  )


Secondary_electricity_plot

# =============================================================================================================================
# === Figure 2d: Cumulative capacity additions === 
# =============================================================================================================================

selected_variables <- c("Capacity Additions|Electricity|Biomass|w/ CCS",
                        "Capacity Additions|Electricity|Biomass|w/o CCS",
                        "Capacity Additions|Electricity|Coal|w/ CCS",
                        "Capacity Additions|Electricity|Coal|w/o CCS", 
                        "Capacity Additions|Electricity|Gas|w/ CCS",
                        "Capacity Additions|Electricity|Gas|w/o CCS",
                        "Capacity Additions|Electricity|Nuclear",
                        "Capacity Additions|Electricity|Oil|w/ CCS",
                        "Capacity Additions|Electricity|Oil|w/o CCS",
                        "Capacity Additions|Electricity|Solar",
                        "Capacity Additions|Electricity|Wind")


filtered_CoC_long <- CoC_long %>%
  filter(Variable %in% selected_variables)  %>%
  filter(year %in% c(2025,2030,2035,2040,2045,2050))

# Calculate cumulative capacity additions by Scenario, Grouped_Region, and Variable
cumulative_capacity_additions <- filtered_CoC_long %>%
  group_by(Scenario, Grouped_Region, Variable) %>%
  summarize(cumulative_value = sum(value, na.rm = TRUE), .groups = 'drop')

# Define baseline scenarios mapping
baseline_mapping <- list(
  NDC_Scenarios = c("Survey_NDC_Infl", "Survey_NDC", "Fossil_NDC", "NonFossil_NDC", "Fragmented_NDC"),
  Windfall_CP_Scenario = c("Windfall_CP")
)

reference_NDC <- cumulative_capacity_additions %>%
  filter(Scenario == "NDC_EI_baseWACC")

reference_CP <- cumulative_capacity_additions %>%
  filter(Scenario == "CP_EI_baseWACC")

filtered_cumulative <- cumulative_capacity_additions %>%
  filter(!Scenario %in% c("NDC_EI_baseWACC", "CP_EI_baseWACC"))

cumulative_diff <- filtered_cumulative %>%
  mutate(baseline_scenario = case_when(
    Scenario %in% baseline_mapping$NDC_Scenarios ~ "NDC_EI_baseWACC",
    Scenario %in% baseline_mapping$Windfall_CP_Scenario ~ "CP_EI_baseWACC"
  )) %>%
  left_join(reference_NDC, by = c("Grouped_Region", "Variable"), suffix = c("", "_ref")) %>%
  mutate(reference_value = ifelse(baseline_scenario == "CP_EI_baseWACC",
                                  reference_CP$cumulative_value[match(Grouped_Region, reference_CP$Grouped_Region)],
                                  cumulative_value_ref),
         abs_diff = (cumulative_value - reference_value)) %>%
  select(Scenario, Grouped_Region, Variable, cumulative_value, reference_value, abs_diff)


selected_scenarios <- c("Survey_NDC", "Fossil_NDC", "NonFossil_NDC", "Fragmented_NDC")

cumulative_diff_filtered <- cumulative_diff %>%
  filter(Scenario %in% selected_scenarios) %>%
  filter(!is.na(Variable)) %>%
  filter(Grouped_Region != "World")

technology_mapping <- c(
  "Capacity Additions|Electricity|Biomass|w/ CCS" = "Biomass w/ CCS",
  "Capacity Additions|Electricity|Biomass|w/o CCS" = "Biomass w/o CCS",
  "Capacity Additions|Electricity|Coal|w/ CCS" = "Coal w/ CCS",
  "Capacity Additions|Electricity|Coal|w/o CCS" = "Coal w/o CCS",
  "Capacity Additions|Electricity|Gas|w/ CCS" = "Gas w/ CCS",
  "Capacity Additions|Electricity|Gas|w/o CCS" = "Gas w/o CCS",
  "Capacity Additions|Electricity|Nuclear" = "Nuclear",
  "Capacity Additions|Electricity|Oil|w/ CCS" = "Oil w/ CCS",
  "Capacity Additions|Electricity|Oil|w/o CCS" = "Oil w/o CCS",
  "Capacity Additions|Electricity|Solar" = "Solar",
  "Capacity Additions|Electricity|Wind" = "Wind"
)

cumulative_diff_filtered <- cumulative_diff_filtered %>%
  mutate(Technology = technology_mapping[Variable])

# Add scenario labels 
scenario_labels <- c(
  "Survey_NDC" = "Sv",
  "Fossil_NDC" = "Sv-F",
  "NonFossil_NDC" = "Sv-NF",
  "Fragmented_NDC" = "Frag"
)

cumulative_diff_filtered <- cumulative_diff_filtered %>%
  mutate(Scenario_label = scenario_labels[Scenario])

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
  "Nuclear" = "#FF4D81"
)


technology_order <- c(
  "Oil w/o CCS", "Oil w/ CCS", "Gas w/o CCS", "Gas w/ CCS",
  "Coal w/o CCS", "Coal w/ CCS", "Biomass w/o CCS", "Biomass w/ CCS",
  "Wind", "Solar", "Nuclear"
)

cumulative_diff_filtered <- cumulative_diff_filtered %>%
  mutate(Technology = factor(Technology, levels = technology_order))

cumulative_diff_filtered_solid <- cumulative_diff_filtered %>%
  filter(Technology %in% c("Oil w/o CCS", "Gas w/o CCS", "Coal w/o CCS", "Biomass w/o CCS", "Wind", "Solar", "Nuclear"))

cumulative_diff_filtered_stripe <- cumulative_diff_filtered %>%
  filter(Technology %in% c("Oil w/ CCS", "Gas w/ CCS", "Coal w/ CCS", "Biomass w/ CCS"))

# Create plot
Capacity_additions_plot <- ggplot() +
  geom_bar(
    data = cumulative_diff_filtered_solid,
    aes(x = Scenario_label, y = abs_diff, fill = Technology),
    stat = "identity",
    position = "stack"
  ) +
  geom_bar_pattern(
    data = cumulative_diff_filtered_stripe,
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
  facet_wrap(~ Grouped_Region, scales = "free_y", nrow = 4, ncol = 2) +
  geom_hline(yintercept = 0, color = "black", size = 0.5) + 
  scale_fill_manual(values = tech_colors, breaks = technology_order) + 
  scale_y_continuous(limits = c(-30,30)) + 
  guides(pattern = "none") +
  theme_minimal() +
  labs(
    x = "",
    y = "Difference from baseline in cumulative capacity additions (GW/yr)",
    fill = "Technology"
  ) +
  theme(
    strip.text = element_text(size = 11), 
    legend.position = "bottom", 
    panel.grid = element_blank(), 
    panel.border = element_rect(color = "black", fill = NA, size = 1), 
    axis.line = element_line(color = "black"), 
    axis.ticks = element_line(color = "black"), 
    axis.text.y = element_text(color = "black", size = 12),
    axis.text.x = element_text(color = "black", size = 12),
    axis.title.y = element_text(angle = 90, hjust = 1, color = "black", size = 12),
    strip.background = element_rect(fill = "grey90", color = NA) 
  )

Capacity_additions_plot