# =============================================================================================================================
# === Load Libraries === 
# =============================================================================================================================
library(tidyverse)  
library(readxl)     
library(plotly)     
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
# === Figure 4a  Co2 emissions from energy and industrial processes === 
# =============================================================================================================================

#Filter emissions
filtered_CoC_long <- CoC_long %>%
  filter(Variable == "Emissions|CO2|Energy and Industrial Processes") 

#Sum emissions across grouped regions
Emissions <- filtered_CoC_long %>%
  group_by(Model, Scenario, year, Grouped_Region) %>%
  summarise(sum_values = sum(value, na.rm = TRUE))


# Calculate the relative change compared to baseline
#CP: relative share 

Emissions_share_CP <- Emissions %>%
  group_by(year, Grouped_Region) %>%
  mutate(`change` = ifelse(
    Scenario == "Windfall_CP",
    case_when(
      sum_values * sum_values[Scenario == "CP_EI_baseWACC"] > 0 ~ ((sum_values - sum_values[Scenario == "CP_EI_baseWACC"]) / abs(sum_values[Scenario == "CP_EI_baseWACC"])) * 100,
      sum_values > 0 & sum_values[Scenario == "CP_EI_baseWACC"] < 0 ~ ((sum_values - sum_values[Scenario == "CP_EI_baseWACC"]) / abs(sum_values[Scenario == "CP_EI_baseWACC"])) * 100, # Handles change from negative to positive
      sum_values < 0 & sum_values[Scenario == "CP_EI_baseWACC"] > 0 ~ ((sum_values - sum_values[Scenario == "CP_EI_baseWACC"]) / abs(sum_values[Scenario == "CP_EI_baseWACC"])) * 100, # Handles change from positive to negative
      TRUE ~ NA_real_  # Handle cases where emissions are zero or undefined
    ),
    0  # Baseline scenario itself has 0% change
  )
  ) %>%
  ungroup()

# Filter for specified years and scenario
filtered_emissions_cp <- Emissions_share_CP %>%
  filter(
    year %in% seq(2020, 2050, by = 5),  
    Scenario %in% c(
      "Windfall_CP"
    )  
  )

Emissions_share <- filtered_emissions_cp

selected_scenarios <- c("Windfall_CP")

#Filter scenarios
Emissions_share <- Emissions_share %>%
  filter(Scenario %in% selected_scenarios)

Emissions_share$year <- as.numeric(as.character(Emissions_share$year))

Emissions_share <- Emissions_share %>% 
  filter(year %in% c(2030, 2035)) 

# Define custom color palette
region_colors <- c(
  "Africa" = "#8DD3C7",
  "China" = "#377eb8",
  "Latin America" = "#fb9a99",
  "India" = "#FDB462",
  "RoA" = "#b2df8a",
  "S_E_Asia" = "#e78ac3",
  "West" = "#984ea3",
  "World" ="grey50"
)

# Plot
Emissions_windfall_plot <- ggplot(Emissions_share, aes(x = Grouped_Region, y = change, fill = Grouped_Region)) +
  geom_bar(stat = "identity", position = "dodge", colour = "grey20") +  
  facet_wrap(~ year, ncol = 2, scales = "free_y") + 
  labs(x = NULL,  
       y = expression("Difference from baseline in CO"[2]*" emissions [%]"),  
       fill = "Region") + 
  theme(
    panel.background = element_blank(),  
    panel.border = element_rect(color = "black", fill = NA),  
    panel.grid.major = element_line(color = "grey", linetype = "dotted", size = 0.05),  
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),  
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.text.y = element_text(color = "black", size =9),  
    axis.title.y = element_text(angle = 90, hjust = 1, color = "black", size = 10), 
    axis.title.x = element_blank(),
    legend.title = element_text(size = 8, face = "bold"),  
    legend.text = element_text(size = 8),  
    legend.position = "bottom",  
    legend.justification = "center",  
    legend.box = "horizontal",  
    plot.title = element_text(hjust = 0.5, size = 18),
    strip.background = element_rect(fill = "grey90"),  
    strip.text = element_text(size = 10)
  ) +
  scale_fill_manual(values = region_colors) +  
  scale_y_continuous(
    limits = c(-3, 3),
    breaks = seq(-3, 3, by = 1),
  ) +
  geom_hline(yintercept = 0, color = "grey10", size = 0.7) + 
  guides(fill = guide_legend(nrow = 1))  

Emissions_windfall_plot

# =============================================================================================================================
# === Figure 4d Investments in electricity supply ===
# =============================================================================================================================

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
CP_inv <- Inv_summaries %>%
  filter(Scenario == "CP_EI_baseWACC") %>%
  select(Model, Grouped_Region, Variable, year, total_value) %>%
  rename(reference_value = total_value)

inv_summaries_CP <- Inv_summaries %>%
  left_join(CP_inv, by = c("Model", "Grouped_Region", "Variable", "year")) %>%
  mutate(abs_diff = total_value - reference_value)

selected_scenarios <- c("Windfall_CP")

# Filter scenarios, regions, years
inv_summaries_CP <- inv_summaries_CP  %>%
  filter(Grouped_Region != "World") %>%
  filter(Scenario %in% selected_scenarios) %>%
  filter(year %in% c(2030, 2035))


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

inv_summaries_CP <- inv_summaries_CP %>%
  mutate(Technology = technology_mapping[Variable])

# Define custom colors for the technologies
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

inv_summaries_CP <- inv_summaries_CP %>%
  mutate(Technology = factor(Technology, levels = technology_order))

inv_summaries_CP_solid <- inv_summaries_CP %>%
  filter(Technology %in% c("Oil w/o CCS", "Gas w/o CCS", "Coal w/o CCS", "Biomass w/o CCS", "Wind", "Solar", "Nuclear", "T&D"))

inv_summaries_CP_stripe <- inv_summaries_CP %>%
  filter(Technology %in% c("Oil w/ CCS", "Gas w/ CCS", "Coal w/ CCS", "Biomass w/ CCS"))


# Plot
Investments_windfall_plot <- ggplot() +
  geom_bar(
    data = inv_summaries_CP_solid,
    aes(x = Grouped_Region, y = abs_diff, fill = Technology),
    stat = "identity",
    position = "stack",
    colour = "black",
    width = 0.6 # Narrower bars
  ) +
  geom_hline(yintercept = 0, color = "grey10", size = 0.5) +
  geom_bar_pattern(
    data = inv_summaries_CP_stripe,
    aes(x = Grouped_Region, y = abs_diff, fill = Technology),
    stat = "identity",
    position = "stack",
    pattern = "stripe",
    pattern_fill = "white",
    pattern_colour = 'white',
    pattern_angle = 45,
    pattern_density = 0.2,
    pattern_spacing = 0.08,
    pattern_key_scale_factor = 0.5,
    width = 0.6
  ) +
  facet_wrap(~ year, nrow = 1) + 
  scale_fill_manual(values = tech_colors, breaks = technology_order) + 
  coord_cartesian(ylim = c(-10, 8)) + 
  scale_y_continuous(breaks = seq(-10, 8, by = 2)) + 
  theme_minimal() +
  labs(
    x = "", # Add x-axis label for regions
    y = "Difference from baseline in investment\n[billion US$2010/yr]",
    fill = "Technology"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9), 
    axis.ticks.x = element_line(color = "black"), 
    axis.text.y = element_text(hjust = 1, size = 9, color = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(angle = 90, hjust = 1, color = "black", size = 10),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9, face = "bold"),
    strip.text.x = element_text(size = 10), 
    strip.background.x = element_rect(fill = "grey90", color = NA), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    panel.border = element_rect(color = "black", fill = NA, size = 1), 
    panel.spacing.x = unit(0.5, "lines"), 
    panel.spacing.y = unit(1, "lines")  
  )

Investments_windfall_plot

# =============================================================================================================================
# === Figure 4b Weighted average electricity price === 
# =============================================================================================================================

Elec_GCAM_WA <- read_csv("../Elec_price_final_electricity_demand.csv")

# Calculate the relative change compared to baseline
percent_change <- Elec_GCAM_WA %>%
  filter(Scenario %in% c("CP_EI_CoC", "Windfall_CP")) %>%  
  pivot_wider(names_from = Scenario, values_from = Weighted_Avg_Electricity_Price) %>%  
  mutate(
    Percent_Change = ((Windfall_CP - CP_EI_CoC) / CP_EI_CoC) * 100  
  ) %>%
  select(Grouped_Region, Year, Percent_Change)  

# Ensure data completeness and interpolate missing values
percent_change <- percent_change %>%
  filter(Year %in% c(2030, 2035))

# Define custom color palette
region_colors <- c(
  "Africa" = "#8DD3C7",
  "China" = "#377eb8",
  "Latin America" = "#fb9a99",
  "India" = "#FDB462",
  "RoA" = "#b2df8a",
  "S_E_Asia" = "#e78ac3",
  "West" = "#984ea3"
)


# Plot
Elec_price__windfall_plot <- ggplot(percent_change, aes(x = Grouped_Region, y = Percent_Change, fill = Grouped_Region)) +
  geom_bar(stat = "identity", position = "dodge", colour = "grey20") +  
  facet_wrap(~ Year, ncol = 2, scales = "free_y") +  
  labs(
    x = NULL,  # Remove x-axis label
    y = "Difference from baseline in electricity price [%]",  
    fill = "Region"
  ) +
  theme(
    panel.background = element_blank(),  
    panel.border = element_rect(color = "black", fill = NA),  
    panel.grid.major = element_line(color = "grey", linetype = "dotted", size = 0.05),  
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),  
    axis.text.x = element_blank(),  
    axis.text.y = element_text(color = "black", size =9),  
    axis.title.y = element_text(angle = 90, hjust = 1, color = "black", size = 10), 
    axis.title.x = element_blank(),
    legend.title = element_text(size = 9, face = "bold"),  
    legend.text = element_text(size = 8),  
    legend.position = "bottom",  
    legend.justification = "center",  
    legend.box = "horizontal",  
    plot.title = element_text(hjust = 0.5, size = 18),
    strip.background = element_rect(fill = "grey90"),  
    strip.text = element_text(size = 10)
  ) +
  scale_fill_manual(values = region_colors) +  
  scale_y_continuous(
    limits = c(-6, 8),
    breaks = seq(-6, 8, by = 2),
  ) +
  geom_hline(yintercept = 0, color = "grey10", size = 0.7) +  
  guides(fill = guide_legend(nrow = 1))  

Elec_price__windfall_plot


# =============================================================================================================================
# === Figure 4c Final energy demand === 
# =============================================================================================================================

selected_variables <- c("Final Energy|Industry|Electricity",
                        "Final Energy|Residential and Commercial|Electricity",
                        "Final Energy|Transportation|Electricity")


filtered_CoC_long <- CoC_long %>%
  filter(Variable %in% selected_variables)


final_energy_summaries <- filtered_CoC_long %>%
  group_by(Grouped_Region, Scenario, Model, year, Variable) %>%
  summarize(total_value = sum(value, na.rm = TRUE), .groups = 'drop')


# Determine the absolute difference from baseline
CP_final_energy <- final_energy_summaries %>%
  filter(Scenario == "CP_EI_baseWACC") %>%
  select(Model, Grouped_Region, Variable, year, total_value) %>%
  rename(reference_value = total_value)

final_energy_summaries_CP <- final_energy_summaries %>%
  left_join(CP_final_energy, by = c("Model", "Grouped_Region", "Variable", "year")) %>%
  mutate(abs_diff = total_value - reference_value)

selected_scenarios <- c("Windfall_CP")

# Filter scenarios, regions, years
final_energy_summaries_CP <- final_energy_summaries_CP  %>%
  filter(Grouped_Region != "World") %>%
  filter(Scenario %in% selected_scenarios) %>%
  filter(year %in% c(2030, 2035))

# Rename technologies
technology_mapping <- c(
  "Final Energy|Industry|Electricity" = "Industry",
  "Final Energy|Residential and Commercial|Electricity" = "Buildings",
  "Final Energy|Transportation|Electricity" = "Transport"
)

final_energy_summaries_CP <- final_energy_summaries_CP %>%
  mutate(Technology = technology_mapping[Variable])


tech_colors <- c(
  "Industry" = "#6F2494",    # Purple
  "Buildings" = "#FF2966",  # Pink
  "Transport" = "#01A0D9"   # Blue
)

# Create the plot
Final_energy_windfall_plot <- ggplot(final_energy_summaries_CP, aes(x = Grouped_Region, y = abs_diff, fill = Technology)) +
  geom_bar(stat = "identity", position = "stack", colour = "grey20") +  
  facet_wrap(~ year, ncol = 2, scales = "free_y") +  # Facet by Year
  labs(
    x = NULL,  # Remove x-axis label
    y = expression("Difference in final energy consumption [EJ/yr]"),  
    fill = "End use"  # Legend title
  ) + 
  theme(
    panel.background = element_blank(),  
    panel.border = element_rect(color = "black", fill = NA),  
    panel.grid.major = element_line(color = "grey", linetype = "dotted", size = 0.05),  
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),  
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9, color = "black"),  
    axis.text.y = element_text(color = "black", size = 9),  
    axis.title.y = element_text(hjust = 1, face = "bold", color = "black", size = 10), 
    axis.title.x = element_blank(),
    legend.title = element_text(size = 9, face = "bold"),  
    legend.text = element_text(size = 8),  
    legend.position = "bottom",  
    legend.justification = "center",  
    legend.box = "horizontal",  
    plot.title = element_text(hjust = 0.5, size = 18),
    strip.background = element_rect(fill = "grey90"),  
    strip.text = element_text(size = 10)
  ) +
  scale_fill_manual(values = tech_colors) +  # Apply custom colors
  scale_y_continuous(limits = c(-1, 0.5), breaks = seq(-0.8, 0.4, by = 0.2) 
  ) +
  geom_hline(yintercept = 0, color = "grey10", size = 0.7) +  
  guides(fill = guide_legend(nrow = 1))  

Final_energy_windfall_plot