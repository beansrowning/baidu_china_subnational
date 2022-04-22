library(dplyr)
library(readr)
library(pbapply)

source("R/funcs.R")

# === Read in previously extracted URLs ==============
all_urls <- read_csv("data/page_lookup.csv")
# === Scrape all data ======================

# Scrape
all_city_data <- pblapply(all_urls[["url"]], debouce_scrape)

# Parse into a data frame
all_city_parsed <- pblapply(all_city_data, attempt_parse)

# Make a list for all the cities that failed to parse
failed <- vector("character")

# Add english city name
for (i in seq_along(all_city_parsed)) {
  if (is.null(all_city_parsed[[i]])) {
    failed <- c(failed, all_urls[["location"]][i])
    next
  }

  all_city_parsed[[i]] <- all_city_parsed[[i]] %>%
    mutate(area_en = all_urls[["location"]][i])
}

# Combine together
all_city_data_df <- bind_rows(all_city_parsed) %>%
  left_join(
    distinct(all_urls, area_en = location, province),
    by = "area_en"
  ) %>%
  select(
    area_en, area_zh, province, date,
    new_asymptomatic = "\U65B0\U589E\U65E0\U75C7\U72B6",
    new_confirmed = "\U65B0\U589E\U672C\U571F"
  ) %>%
  mutate(
    date = as.Date(sprintf("%s.2022", date), "%m.%d.%Y")
  ) %>%
  arrange(date, province, area_en)

# Failed table
failed_table <- all_urls %>%
  filter(location %in% failed)

# === Write out ===============
write_csv(all_city_data_df, "data/china_adm2_covid_data.csv")
write_csv(failed_table, "data/parse_failures.csv")
