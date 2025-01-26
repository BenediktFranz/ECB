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


# Load data on GHG emissions
url <- "https://edgar.jrc.ec.europa.eu/booklet/EDGAR_2024_GHG_booklet_2024.xlsx"
temp_file <- tempfile(fileext = ".xlsx")
download.file(url, destfile = temp_file, mode = "wb")
total_GHG <- read_excel(temp_file, sheet = "GHG_totals_by_country")
rm(temp_file, url)

# Rename columns
colnames(total_GHG)[1] <- "country_code"
colnames(total_GHG)[2] <- "country"

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

ggsave("C:/Users/Franz/Desktop/chart1.png", plot = chart1, width = 8.27, height = 5, units = "in")



# Chart 2 ----
# Load World Bank data on countries' income categories
OGHIST <- read_excel("C:/Users/Franz/Downloads/OGHIST.xlsx", sheet = "Country Analytical History", range = "A6:AM229", na = "..")

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

ggsave("C:/Users/Franz/Desktop/chart2.png", plot = chart2, width = 8.27, height = 5, units = "in")


# Chart 3 ----

# Calculate shares of countries on total emissions
relatvie_GHG <- total_GHG %>%
  group_by(year) %>%
  mutate(
    total = value[country_code == "GLOBAL TOTAL"], # Extract `GLOBAL TOTAL` for each year
    share = ifelse(country_code != "GLOBAL TOTAL", value / total * 100, NA) # Calculate shares
  ) %>%
  ungroup() %>%
  filter(country_code != "GLOBAL TOTAL") # Remove `GLOBAL TOTAL` rows

