# =============================================================================================================================
# === Load Libraries === 
# =============================================================================================================================

library(ggplot2)
library(readxl)
library(dplyr)
library(grid)

# =============================================================================================================================
# === Data Import and Preprocessing ===
# =============================================================================================================================

CoC <- read_excel("../Results_all_IAMC.csv")

CoC_long <- gather(CoC, year, value, 6:15, factor_key=TRUE)

# Load region mapping
load_region_mapping <- function() {
  region_mapping <- fread("../region_mapping.csv")
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
# === Figure S1: Regional CO2 emission trajectories  === 
# =============================================================================================================================

#Filter emissions

selected_variables <- c("Emissions|CO2|AFOLU",
                        "Emissions|CO2|Energy",
                        "Emissions|CO2|Industrial Processes",
                        "Emissions|CO2|Other")

Emissions_filtered <- CoC_long %>%
  filter(Scenario %in% c("Survey_NDC", "NDC_EI_baseWACC"),
         Variable %in% selected_variables,
         Grouped_Region != "World",
         year %in% c(2020, 2025, 2030, 2035, 2040, 2045, 2050)) %>%
  mutate(year = as.numeric(as.character(year))) %>%
  arrange(Grouped_Region, Variable, Scenario, year)  # Sort data

Emissions_filtered <- Emissions_filtered %>%
  group_by(Model, Scenario, Variable, year, Grouped_Region) %>%
  summarise(sum_values = sum(value, na.rm = TRUE))

write_xlsx(Emissions_filtered , "Emissions_filtered.xlsx")

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

scenario_labels <- c("Survey_NDC" = "Sv", "NDC_EI_baseWACC" = "B-NDC")
scenario_colors <- c("Survey_NDC" = "#8BA0CD", "NDC_EI_baseWACC" = "#FDED5D")

variable_labels <- c("Emissions|CO2|AFOLU" = "AFOLU", 
                     "Emissions|CO2|Energy" = "Energy", 
                     "Emissions|CO2|Industrial Processes" = "Industrial",
                     "Emissions|CO2|Other" = "Other")

# Create the plot
Emissions_trajectory <- ggplot(Emissions_filtered, aes(x = year, y = sum_values, 
                                                 color = Scenario, group = interaction(Grouped_Region, Variable, Scenario))) +
  geom_line(size = 0.75) +  # Line plot for emission changes
  facet_grid(Variable ~ Grouped_Region, scales = "free_y", labeller = labeller(Variable = variable_labels)) +  # Facet by emissions source & region
  labs(x = "Year",  
       y = expression("CO"[2]*" emissions [Mt CO"[2]*"/yr]"),  
       color = "Scenario") +  
  theme(panel.background = element_blank(),  
        panel.border = element_rect(color = "black", fill = NA),  
        panel.grid.major = element_line(color = "grey", linetype = "dotted", size = 0.05),  
        panel.grid.minor = element_blank(),  
        axis.line = element_line(color = "black"),  
        axis.text.x = element_text(color = "black", size = 12),  
        axis.text.y = element_text(color = "black", size = 14),  
        axis.title.y = element_text(angle = 90, hjust = 1, color = "black", size = 14, face = "bold"), 
        legend.title = element_text(size = 11, face = "bold"),  
        legend.text = element_text(size = 12),  
        legend.position = "bottom",  
        legend.justification = "center",  
        legend.box = "horizontal",  
        plot.title = element_text(hjust = 0.5, size = 18),
        strip.background = element_rect(fill = "grey90"),  
        strip.text = element_text(size = 14),
        panel.spacing = unit(1.5, "lines")) +
  scale_color_manual(values = scenario_colors, labels = scenario_labels) +  
  geom_hline(yintercept = 0, color = "grey10", size = 0.7) +  
  guides(color = guide_legend(nrow = 1)) +  # Arrange legend items in a single row
  scale_x_continuous(breaks = seq(2020, 2050, by = 10))  # Ensure proper year labels

Emissions_trajectory

# =============================================================================================================================
# === Figure S2a: Global CO2 emissions from energy and industrial processes with rising inflation === 
# =============================================================================================================================

selected_variables <- c("Emissions|CO2|Energy and Industrial Processes")
filtered_data <- Study6 %>% filter(Variable %in% selected_variables)

filtered_data <- filtered_data %>%
  mutate(Grouped_Region = region_mapping[Region])

value_columns <- colnames(filtered_data)[6:15]  # Select the columns for years 2005 to 2050
filtered_data_long <- filtered_data %>%
  pivot_longer(cols = all_of(value_columns), names_to = "Year", values_to = "Emissions") %>%
  mutate(Year = as.numeric(Year))

filtered_data_long <- filtered_data_long %>%
  filter(Year %in% c(2015, 2020, 2025, 2030, 2035, 2040, 2045, 2050))

grouped_data <- filtered_data_long %>%
  group_by(Model, Scenario, Grouped_Region, Year) %>%
  summarise(TotalEmissions = sum(Emissions, na.rm = TRUE)) %>%
  ungroup()

world_data <- grouped_data %>%
  filter(Grouped_Region == "World")

historical_data <- world_data %>%
  filter(Year %in% c(2015, 2020)) %>%
  mutate(Scenario = "Historical") 

# Scenario-specific data includes all years from 2020 to 2050
scenario_data <- world_data %>%
  filter(Year >= 2020) %>%
  filter(Scenario %in% c("Historical", "Survey_NDC", "Survey_NDC_Infl"))

historical_data$Year <- as.numeric(as.character(historical_data$Year))
scenario_data$Year <- as.numeric(as.character(scenario_data$Year))

scenario_order <- c("Historical", "Survey_NDC", "Survey_NDC_Infl")

historical_data$Scenario <- factor("Historical", levels = scenario_order)
scenario_data$Scenario <- factor(scenario_data$Scenario, levels = scenario_order)

Emissions_plot_inflation <- ggplot() +
  # Plot historical data (2015-2020) as a single black line, add to legend as "Historical"
  geom_line(data = historical_data, aes(x = Year, y = TotalEmissions / 100, 
                                        linetype = "Historical", color = "Historical", group = 1), size = 0.75) +
  geom_line(data = scenario_data, aes(x = Year, y = TotalEmissions / 100, 
                                      group = Scenario, color = Scenario, linetype = Scenario), size = 0.75) +
  theme_minimal() +
  labs(
    x = "Year",
    y = expression("CO"[2]*" emissions (Gt CO"[2]*")"),  
    color = "Scenario",
    linetype = "Scenario"
  ) +
  scale_linetype_manual(values = c(
    "Historical" = "solid",
    "Survey_NDC" = "solid",
    "Survey_NDC_Infl" = "solid"
  ), 
  labels = c(
    "Historical" = "Historical",
    "Survey_NDC" = "Sv",
    "Survey_NDC_Infl" = "Sv-inflation"
  ),
  breaks = scenario_order) +  
  scale_color_manual(values = c(
    "Historical" = "black",
    "Survey_NDC" = "#8BA0CD",            
    "Survey_NDC_Infl" = "#2AD0CC"     
  ), 
  labels = c(
    "Historical" = "Historical",
    "Survey_NDC" = "Sv",
    "Survey_NDC_Infl" = "Sv-inflation"
  ),
  breaks = scenario_order) +  
  
  theme(
    legend.position = "right",  # Move the legend to the right
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    axis.text.x = element_text(angle = 0, hjust = 0.7, size = 12),  
    axis.text.y = element_text(size = 12), 
    axis.title = element_text(hjust = 1, face = "bold", size = 14),  
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    panel.grid.major = element_line(color = "grey", linetype = "dotted", size = 0.05),  
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_rect(color = "black", fill = NA, size = 1),  
    axis.ticks = element_line(color = "black", size = 0.5) 
  ) +
  # Set y-axis limits to display from 200 to 400
  scale_y_continuous(limits = c(200, 400), breaks = seq(200, 400, by = 50), expand = c(0.02, 0)) +
  # Set x-axis limits to display from 2015 to 2050
  scale_x_continuous(limits = c(2015, 2050), breaks = seq(2015, 2050, by = 5), expand = c(0, 0)) +
  guides(
    color = guide_legend(order = 1, nrow = 10),  
    linetype = "none"  
  )

Emissions_plot_inflation

# =============================================================================================================================
# === Figure S2b: Secondary electricity production with rising inflation=== 
# =============================================================================================================================

# Define the variables you want to filter
selected_variables <- c("Secondary Energy|Electricity|Biomass",
                        "Secondary Energy|Electricity|Coal",
                        "Secondary Energy|Electricity|Gas",
                        "Secondary Energy|Electricity|Nuclear", 
                        "Secondary Energy|Electricity|Oil",
                        "Secondary Energy|Electricity|Solar",
                        "Secondary Energy|Electricity|Wind",
                        "Secondary Energy|Hydrogen")

filtered_Study6_long <- Study6_long %>%
  filter(Variable %in% selected_variables)

# Summarize the data according to grouped region, scenario, model, year, and variable
sec_summaries <- filtered_Study6_long %>%
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
    Nuclear = sum(total_value_TWh[Variable == "Secondary Energy|Electricity|Nuclear"], na.rm = TRUE),
    Hydrogen = sum(total_value_TWh[Variable == "Secondary Energy|Hydrogen"], na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(Total_Fossil_Fuels, Total_RES, Biomass, Nuclear, Hydrogen), 
               names_to = "group_var", values_to = "summed_value") %>%
  ungroup()

sec_summaries_grouped <-sec_summaries_grouped %>%
  mutate(group_var = case_when(
    group_var %in% c("Total_Fossil_Fuels")  ~ "Fossil fuels",  # Name the group containing var1 and var2
    group_var %in% c("Total_RES") ~ "Wind & solar",  # Name the group containing var4 and var5
    TRUE ~ as.character(group_var)  # Keep the original variable name for other variables
  ))


#Absolute Difference

#Filter out the NDC_EI_baseWACC scenario to use as the reference
NDC_sec <- sec_summaries_grouped %>%
  filter(Scenario == "NDC_EI_baseWACC") %>%
  select(Model, Grouped_Region, group_var, year, summed_value) %>%
  rename(reference_value = summed_value)


CP_sec <- sec_summaries_grouped %>%
  filter(Scenario == "CP_EI_baseWACC") %>%
  select(Model, Grouped_Region, group_var, year, summed_value) %>%
  rename(reference_value = summed_value)

sec_summaries_NDC <- sec_summaries_grouped %>%
  left_join(NDC_sec, by = c("Model", "Grouped_Region", "group_var", "year")) %>%
  mutate(abs_diff = summed_value - reference_value)


sec_summaries_CP <- sec_summaries_grouped %>%
  left_join(CP_sec, by = c("Model", "Grouped_Region", "group_var", "year")) %>%
  mutate(abs_diff = summed_value - reference_value)

selected_scenarios <- c("Survey_NDC",  "Survey_NDC_Infl")

sec_summaries_NDC <- sec_summaries_NDC  %>%
  filter(Grouped_Region == "World") %>%
  filter(group_var != "Hydrogen") %>%
  filter(Scenario %in% selected_scenarios) %>%
  filter(year %in% c(2025, 2030, 2035, 2040, 2045, 2050))

Tech_colours <- c(
  "Fossil fuels" = "black",
  "Wind & solar" = "#00CCCD",
  "Biomass" = "#FFB743",
  "Nuclear" = "#FF4D81"
)

sec_summaries_NDC <- sec_summaries_NDC %>%
  mutate(Scenario = factor(Scenario, levels = c("Survey_NDC", "Survey_NDC_Infl")))

# Create the plot
Elec_gen_inflation <- ggplot(sec_summaries_NDC, aes(x = year, y = abs_diff, fill = group_var, group = interaction(group_var, Scenario, Grouped_Region))) +
  geom_area(alpha = 1, position = "stack") + # Change position to "stack" to create a stacked area chart
  geom_hline(yintercept = 0, color = "black", size = 0.5) + # Add a black line at y = 0
  facet_grid(Scenario ~ Grouped_Region,
             labeller = labeller(
               Scenario = c("Survey_NDC" = "Sv", 
                            "Survey_NDC_Infl" = "Sv-Inflation"))) +
  scale_fill_manual(values = Tech_colours) + # Use custom colors for technologies
  scale_x_discrete(
    breaks = c(2025, 2030, 2035, 2040, 2045, 2050), # Show specific years on x-axis
    expand = c(0, 0) # Remove extra padding at the start and end
  ) +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Difference from baseline in electricity generation (TWh)",
    fill = "Technology"
  ) +
  theme(
    strip.text = element_text(size = 11), # Adjust facet label text size
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 12), # Rotate x-axis labels for readability
    legend.position = "bottom", # Position legend at the bottom
    legend.text = element_text(size = 12), # Set legend text size to 12
    panel.grid = element_blank(), # Remove all grid lines
    panel.border = element_rect(color = "black", fill = NA, size = 1), # Add black border around plot area
    axis.line = element_line(color = "black"), # Add black axis lines
    axis.text.y = element_text(color = "black", size = 12),  
    axis.title.y = element_text(angle = 90, hjust = 1, color = "black", size = 14),
    axis.ticks = element_line(color = "black"), # Add black tick marks
    strip.background = element_rect(fill = "grey90", color = NA) # Set facet background to grey
  )

Elec_gen_inflation


# =============================================================================================================================
# === Figure S3: Regional implications on carbon capture ans storage (CCS) of different WACC projections on top of NDCs== 
# =============================================================================================================================


# Define the variables you want to filter
selected_variables <- c("Carbon Sequestration|CCS|Biomass",
                        "Carbon Sequestration|CCS|Fossil")


# Filter for the selected variables
filtered_Study6_long <- Study6_long %>%
  filter(Variable %in% selected_variables)

summarized_CoC_long <- filtered_Study6_long %>%
  group_by(Model, Scenario, year, Grouped_Region, Variable) %>%
  summarise(sum_values = sum(value, na.rm = TRUE))


#NDC: relative share 
Emissions_share_NDC <- summarized_CoC_long %>%
  group_by(year, Grouped_Region, Variable) %>%
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

# Filter emissions_combined for the specified years and scenarios
filtered_emissions_ndc <- Emissions_share_NDC %>%
  filter(
    year %in% seq(2020, 2050, by = 5),  # Filter years 2020, 2025, ... 2050
    Scenario %in% c(
      "Fossil_NDC", "Fragmented_NDC", "NonFossil_NDC",
      "Survey_NDC")  # Filter  scenarios
  )


#Filter years
Emissions_share <- filtered_emissions_ndc %>% 
  filter(year %in% c(2050)) 

# Define color palette for regions
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


CCS_plot <- ggplot(Emissions_share, aes(
  x = Grouped_Region,
  y = change,
  fill = Grouped_Region
)) +
  geom_bar(
    stat     = "identity",
    position = "dodge",
    colour   = "grey20"
  ) +
  facet_grid(
    Variable ~ Scenario,
    scales   = "free_y",
    labeller = labeller(
      Scenario = c(
        "Survey_NDC"      = "Sv",
        "Fossil_NDC"      = "Sv-F",
        "NonFossil_NDC"   = "Sv-NF",
        "Fragmented_NDC"  = "Frag"
      ),
      Variable = c(
        "Carbon Sequestration|CCS|Biomass"       = "BECCS",
        "Carbon Sequestration|CCS|Fossil"        = "Fossil CCS"
      )
    )
  ) +
  labs(x = NULL,  # Remove x-axis label since regions are represented in legend
       y = expression("Difference from baseline in CCS [%]"),  # Use expression() for subscripts
       fill = "Region") + 
  theme(panel.background = element_blank(),  
        panel.border = element_rect(color = "black", fill = NA),  
        panel.grid.major = element_line(color = "grey40", linetype = "dotted", size = 0.1),  
        panel.grid.minor = element_blank(),  # Removed minor grid lines
        axis.line = element_line(color = "black"),  
        axis.text.x = element_blank(),  # Remove x-axis labels for regions
        axis.text.y = element_text(color = "black", size = 12),  
        axis.title.y = element_text(angle = 90, hjust = 1, color = "black", size = 12, face = "bold"), 
        axis.title.x = element_blank(),
        legend.title = element_text(size = 11, face = "bold"),  
        legend.text = element_text(size = 12),  
        legend.position = "bottom",  
        legend.justification = "center",  
        legend.box = "horizontal",  
        plot.title = element_text(hjust = 0.5, size = 18),
        strip.background = element_rect(fill = "grey90"),  # Add grey background to strip panels like typical R facets
        strip.text = element_text(size = 12)) +  
  scale_fill_manual(values = region_colors) + # Custom colors
  scale_y_continuous(limits = c(-20, 65), breaks = seq(-20, 60, by = 20)) +  # Add breaks every 5%
  geom_hline(yintercept = 0, color = "grey10", size = 0.7) +  
  guides(fill = guide_legend(nrow = 1))  # Arrange legend items in a single row


CCS_plot


