library(httr2)
library(dplyr)
library(readr)

API_KEY <- "9ee67ca3bb544e37bca294e322b3aa7f"  # https://comtradedeveloper.un.org/
YEAR    <- "2023"
HS_CODE <- "0901"  # Coffee

resp <- request("https://comtradeapi.un.org/data/v1/get/C/A/HS") |>
  req_url_query(
    cmdCode     = HS_CODE,
    flowCode    = "X",       # exports
    period      = YEAR,
    partnerCode = "0",       # world total
    partner2Code = "0",
    includeDesc = "True",
    `subscription-key` = API_KEY
  ) |>
  req_perform() |>
  resp_body_json(simplifyVector = T)

re_exporters <- c("Germany", "Switzerland", "Italy", "France", "Belgium",
                  "Netherlands", "Spain", "USA", "Canada",
                  "United Kingdom", "Sweden", "Poland", "Austria", 
                  "Estonia", "United Arab Emirates", "Slovenia", "Slovakia",
                  "Bulgaria", "Portugal", "Luxembourg", "Czechia", 
                  "Lithuania", "Hungary", "Denmark", "Türkiye")

country_exports <- resp$data |>
  as_tibble() |>
  select(
    Country      = reporterDesc,
    Country_Code = reporterCode,
    Year         = period,
    Value_USD    = primaryValue,
    Weight_kg    = netWgt
  ) |>
  group_by(Country, Country_Code, Year) |>
  summarise(Value_USD = sum(Value_USD), Weight_kg = sum(Weight_kg), .groups = "drop") |>
  mutate(
    Weight_tons = Weight_kg / 1000,
    Avg_Price_USD_per_ton = round(Value_USD / Weight_tons, 2)
  ) |>
  filter(!is.na(Value_USD), !is.na(Weight_kg), Value_USD > 0, Weight_kg > 0,
         Avg_Price_USD_per_ton >= 3400, Weight_tons >= 10000, Value_USD >= 100e6, !Country %in% re_exporters) |>
  arrange(Avg_Price_USD_per_ton) |>
  mutate(Rank = row_number()) |>
  select(Rank, everything(), -Weight_kg)

write_csv(country_exports, file.path(MART, "coffee_country_exports.csv"))