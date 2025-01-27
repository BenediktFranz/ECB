# Chart 1 ----

# Clear everything
rm(list = ls())

# Load libraries
library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(plotly)
library(zoo)
library(treemapify)

# Set working directory
setwd("C:/Users/Franz/Desktop/ECB/")

# Load data on GHG emissions
url <- "https://edgar.jrc.ec.europa.eu/booklet/EDGAR_2024_GHG_booklet_2024.xlsx"
temp_file <- tempfile(fileext = ".xlsx")
download.file(url, destfile = temp_file, mode = "wb")
total_GHG <- read_excel(temp_file, sheet = "GHG_totals_by_country")
rm(temp_file, url)

# Rename columns
colnames(total_GHG)[1] <- "country_code"
colnames(total_GHG)[2] <- "country"

# Remove international aviation and shipping
total_GHG <- total_GHG %>% 
  filter(!country %in% c("International Aviation", "International Shipping"))

# Remove rows with NA
total_GHG <- total_GHG %>% 
  filter(!(is.na(country_code) & is.na(country)))

# Transform wide format to long format
total_GHG <- total_GHG %>%
  pivot_longer(
    cols = `1970`:`2023`,
    names_to = "year",
    values_to = "value"
  )

# Format
total_GHG$year <- as.numeric(total_GHG$year)

# Define Eurozone countries (ISO codes)
eurozone_countries <- c(
  "AUT", "BEL", "HRV", "CYP", "EST", "FIN", "FRA", "DEU", "GRC",
  "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "PRT", "SVK", "SVN", "ESP"
)
# Calculate the EUROZONE aggregate
eurozone_aggregate <- total_GHG %>%
  filter(country_code %in% eurozone_countries) %>% # Filter Eurozone countries
  group_by(year) %>%                               # Group by year
  summarise(
    country_code = "EUROZONE",                     # Assign "EUROZONE" as the country code
    value = sum(value, na.rm = TRUE)               # Sum absolute values
  ) %>%
  ungroup()
eurozone_aggregate$country <- "EUROZONE"
# Combine with the original dataset
total_GHG <- bind_rows(total_GHG, eurozone_aggregate)

# Growth Rates
total_GHG <- total_GHG %>%
  arrange(country_code, year) %>%                 # Ensure data is sorted by country and year
  group_by(country_code) %>%                     # Group by country
  mutate(growth_rate = (value / lag(value) - 1) * 100)

# 5-year growth rates
total_GHG <- total_GHG %>%
  arrange(country_code, year) %>% # Ensure data is sorted by country and year
  group_by(country_code) %>%     # Group by country
  mutate(moving_avg = rollmean(growth_rate, k = 5, fill = NA, align = "center"))


# Select Countries
selected_countries <- c("EU27", "EUROZONE", "GLOBAL TOTAL")  # Replace with your desired country codes
filtered_data <- total_GHG %>%
  filter(country_code %in% selected_countries)

# Reorder for legend
filtered_data$country_code <- factor(
  filtered_data$country_code,
  levels = c("GLOBAL TOTAL", "EUROZONE", "EU27") # Reverse the original order
)


# Plot
chart1 <- ggplot(filtered_data, aes(x = year, y = moving_avg, color = country_code)) +
  geom_line(size = 1) +
  labs(
    title = "GHG Emissions over Time",
    x = "Year",
    y = "Growth Rate (%)",
    color = ""
  ) +
  scale_color_manual(values = c("#FE5622", "#FDB72F", "#162E96")) +
  scale_y_continuous(breaks = seq(-4, 4, 1), minor_breaks = seq(-4, 4, 0.5)) +
  geom_hline(yintercept = 0, size = 0.5, color = "grey") + # Add a thicker 0-line
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.grid.major = element_line(color = "grey70"),
        panel.grid.minor = element_line(color = "gray80"),
        legend.position = "top")

ggsave("charts/chart1.png", plot = chart1, width = 8.27, height = 5, units = "in")



# Chart 2 ----
# Load World Bank data on countries' income categories
OGHIST <- read_excel("data/OGHIST.xlsx", sheet = "Country Analytical History", range = "A6:AM229", na = "..")

# Delete first 5 rows
OGHIST <- OGHIST %>%
  slice(-(1:5))

# Rename columns
colnames(OGHIST)[1] <- "country_code"
colnames(OGHIST)[2] <- "country"

# Transform wide format to long format
OGHIST <- OGHIST %>%
  pivot_longer(
    cols = `1987`:`2023`,
    names_to = "year",
    values_to = "income_class"
  )

# Format
OGHIST$year <- as.numeric(OGHIST$year)


# Load data on GHG emissions per capita
url <- "https://edgar.jrc.ec.europa.eu/booklet/EDGAR_2024_GHG_booklet_2024.xlsx"
temp_file <- tempfile(fileext = ".xlsx")
download.file(url, destfile = temp_file, mode = "wb")
GHG_per_capita <- read_excel(temp_file, sheet = "GHG_per_capita_by_country")
rm(temp_file, url)

# Transform wide format to long format
GHG_per_capita <- GHG_per_capita %>%
  pivot_longer(
    cols = `1970`:`2023`,
    names_to = "year",
    values_to = "value"
  )

# Rename columns
colnames(GHG_per_capita)[1] <- "country_code"
colnames(GHG_per_capita)[2] <- "country"

# Format
GHG_per_capita$year <- as.numeric(GHG_per_capita$year)

# Merge both datasets
GHG_per_capita <- GHG_per_capita %>%
  left_join(OGHIST %>% select(country_code, year, income_class), by = c("country_code", "year"))

# Correct LM* to NA for Jemen
GHG_per_capita$income_class[GHG_per_capita$income_class=="LM*"] <- "LM"

# Sum GHG emission by income class
GHG_per_capita <- GHG_per_capita %>% group_by(year, income_class) %>% summarise(total = sum(value), .groups = "drop")

# Filter for NA
GHG_per_capita <- GHG_per_capita %>%
  filter(!is.na(income_class))

# Reorder for legend
GHG_per_capita$income_class <- factor(
  GHG_per_capita$income_class,
  levels = c("H", "UM", "LM", "L") # Reverse the original order
)


# Plot
chart2 <- ggplot(GHG_per_capita, aes(x = year, y = total, color = income_class)) +
  geom_line(size = 1) +
  labs(
    title = "GHG Emissions per Capita over Time",
    x = "Year",
    y = "GHG Emissions (tons per capita)",
    color = ""
  ) +
  scale_color_manual(
    values = c("#FE5622", "#FDB72F", "#4CAF50", "#162E96"),
    labels = c("High Income", "Upper Middle Income", "Lower Middle Income", "Low Income")
  ) +
  scale_y_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 200)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.grid.major = element_line(color = "grey80"),
        panel.grid.minor = element_line(color = "gray90"),
        legend.position = "top")

ggsave("charts/chart2.png", plot = chart2, width = 8.27, height = 5, units = "in")


# Chart 3 ----

# Load UNSD M49 dataset to match country codes wtih corresponding continents
UNSD_M49 <- read_excel("data/UNSD_M49.xlsx")

# Rename column names for merging
colnames(UNSD_M49)[4] <- "continent"
colnames(UNSD_M49)[12] <- "country_code"

# Merge
total_GHG <- total_GHG %>%
  left_join(UNSD_M49 %>% select(continent, country_code), by = c("country_code"))

# Differentiate between North and South America
north_america <- c(
  "CAN", "USA", "MEX", "BLZ", "CRI", "SLV", "GTM", "HND", "NIC", "PAN",
  "ATG", "BHS", "BRB", "CUB", "DMA", "DOM", "GRD", "HTI", "JAM", "KNA", "LCA", "VCT", "TTO",
  "ABW", "AIA", "BMU", "CYM", "PRI", "SPM", "TCA", "VGB", "GRL")

south_america <- c(
  "ARG", "BOL", "BRA", "CHL", "COL", "ECU", "GUY", "PRY", "PER", "SUR", "URY", "VEN",
  "GUF", "MTQ", "GLP", "FLK")

total_GHG <- total_GHG %>%
  mutate(
    continent = case_when(
      country_code %in% north_america ~ "North America",
      country_code %in% south_america ~ "South America",
      TRUE ~ continent
    )
  )

# Calculate shares of countries on total emissions
relative_GHG <- total_GHG %>%
  group_by(year) %>%
  mutate(
    total = sum(value[country_code == "GLOBAL TOTAL"], na.rm = TRUE), # Extract `GLOBAL TOTAL`
    share = ifelse(country_code != "GLOBAL TOTAL", value / total * 100, NA)
  ) %>%
  ungroup() %>%
  filter(country_code != "GLOBAL TOTAL") # Remove `GLOBAL TOTAL` rows



# Add a decade column
relative_GHG <- relative_GHG %>%
  mutate(decade = case_when(
    year >= 1990 & year < 2000 ~ "1990s",
    year >= 2010 & year < 2020 ~ "2010s",
    TRUE ~ NA_character_
  ))

# Filter for relevant decades and calculate average shares
average_shares <- relative_GHG %>%
  filter(!is.na(decade)) %>% # Keep only rows in the 1990s or 2010s
  group_by(decade, country_code) %>%
  summarize(average_share = mean(share, na.rm = TRUE), .groups = "drop")

# Exclude Eurozone and EU27
average_shares <- average_shares %>%
  filter(!(country_code %in% c("EU27", "EUROZONE")))

# Group smaller countries into "Others"
data_grouped <- average_shares %>%
  group_by(decade) %>%
  mutate(rank = rank(-average_share)) %>% # Rank countries by share in descending order
  mutate(country_code = ifelse(rank > 15, "Others", country_code)) %>% # Group others
  group_by(decade, country_code) %>%
  summarize(average_share = sum(average_share), .groups = "drop") # Aggregate the "Others"

# Treemap for the 1990s
chart3_1990 <- ggplot(
  filter(data_grouped, decade == "1990s"),
  aes(
    area = average_share,
    fill = average_share, # Use the continuous variable
    label = paste(country_code, round(average_share, 2), "%")
  )
) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "centre", grow = FALSE) +
  scale_fill_gradient(
    low = "#162E96", # Light green
    high = "#FE5622", # Dark green
    name = "Share (%)"
  ) +
  labs(
    title = "GHG Emissions Shares by Country (1990s)"
  ) +
  theme_minimal() +
  theme(legend.position = "none") # Remove the legend

ggsave("charts/chart3_1990.png", plot = chart3_1990, width = 8.27, height = 5, units = "in")


# Treemap for the 2010s
chart3_2010 <- ggplot(
  filter(data_grouped, decade == "2010s"),
  aes(
    area = average_share,
    fill = average_share, # Use the continuous variable
    label = paste(country_code, round(average_share, 2), "%")
  )
) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "centre", grow = FALSE) +
  scale_fill_gradient(
    low = "#162E96", # Light green
    high = "#FE5622", # Dark green
    name = "Share (%)"
  ) +
  labs(
    title = "GHG Emissions Shares by Country (2010s)"
  ) +
  theme_minimal() +
  theme(legend.position = "none") # Remove the legend

ggsave("charts/chart3_2010.png", plot = chart3_2010, width = 8.27, height = 5, units = "in")

