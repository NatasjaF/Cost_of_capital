# =============================================================================================================================
# === Load Libraries === 
# =============================================================================================================================
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(readr)

# =============================================================================================================================
# === Region mapping ===
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

# Load the world map with spatial data
world_map <- ne_countries(scale = "medium", returnclass = "sf")

world_map %>%
  filter(name %in% c("France", "Norway"))

world_map <- world_map %>%
  mutate(iso_a3 = case_when(
    name == "France" ~ "FRA",
    name == "Norway" ~ "NOR",
    TRUE ~ iso_a3 
  ))

world_map %>%
  filter(name %in% c("France", "Norway"))

# Load the region mapping file from Excel
region_mapping_ <- read_csv("../GCAM_region_mapping.csv")

# Merge the spatial data with region mapping
merged_data <- world_map %>%
  left_join(region_mapping_, by = c("iso_a3" = "ISO"))

setdiff(region_mapping_$ISO, world_map$iso_a3)

region_colors <- c(
  "Africa" = "#8DD3C7",
  "China" = "#377eb8",
  "Latin America" = "#fb9a99",
  "India" = "#FDB462",
  "RoA" = "#b2df8a",
  "S_E_Asia" = "#e78ac3",
  "West" = "#984ea3"
)

#Plot
map_plot <- ggplot(data = merged_data) +
  geom_sf(aes(fill = AggregatedRegion), color = "black") +
  theme_minimal() +
  scale_fill_manual(values = region_colors, na.translate = FALSE) + 
  labs(title = "Map by Aggregated Region", fill = "Aggregated Region") +
  theme(legend.text = element_text(size = 14))

map_plot
