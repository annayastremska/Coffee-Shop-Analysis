library(readxl)
library(lubridate)
library(dplyr)
library(here)

RAW   <- here("raw")
STAGE <- here("stage")

coffee_shop_sales <- read_excel(file.path(RAW, 'Coffee Shop Sales.xlsx'))
coffee_shop_sales <- coffee_shop_sales |> 
  filter(product_category %in% c("Coffee", "Tea")) |> 
  select(transaction_id, transaction_date, transaction_qty, unit_price,
         product_category, product_type, product_detail) |> 
  na.omit()
write.csv(coffee_shop_sales, file.path(STAGE, "stg_coffee_shop_sales.csv"), row.names = F)

vending_machine_sales <- read.csv(file.path(RAW, 'index_1.csv'))
vending_machine_sales <- vending_machine_sales |> 
  select(datetime, money, coffee_name) |> 
  na.omit()
write.csv(vending_machine_sales, file.path(STAGE, "stg_vending_machine_sales.csv"), row.names = F)

holidays_elasticity <- read.csv(file.path(RAW, 'Walmart.csv'))
holidays_elasticity <- holidays_elasticity |>
  filter(
    Holiday_Flag == 1,
    year(as.Date(Date, format = "%d-%m-%Y")) %in% c(2010, 2011, 2012)
  ) |>
  select(Date, Weekly_Sales) |>
  na.omit() |>
  group_by(Date) |>
  summarise(
    Holiday_Sales = sum(Weekly_Sales),
    .groups = "drop"
  ) |>
  arrange(month(as.Date(Date, format = "%d-%m-%Y")),
          day(as.Date(Date, format = "%d-%m-%Y")))
write.csv(holidays_elasticity, file.path(STAGE, "stg_holidays_elasticity.csv"), row.names = F)


nyc_subway_locations <- read.csv(file.path(RAW, 'MTA_Subway_Stations.csv'))
colnames(nyc_subway_locations)
nyc_subway_locations <- nyc_subway_locations |> 
  select('Station.ID', 'GTFS.Latitude', 'GTFS.Longitude', 'Georeference', 'Complex.ID', 'Borough') |> 
  na.omit()
write.csv(nyc_subway_locations, file.path(STAGE, "stg_nyc_subway_locations.csv"), row.names = F)

crime_data <- read.csv(file.path(RAW,'NYPD_Arrest_Data__Year_to_Date_.csv'))
crime_data$ARREST_MONTH <- month(as.Date(crime_data$ARREST_DATE, , format = "%m/%d/%Y"))
unique(crime_data$ARREST_MONTH)
crime_data <- crime_data |>  
  filter(ARREST_MONTH %in% c(7,8,9,10,11,12)) |>
  select(ARREST_KEY, Latitude, Longitude, Location) |> 
  na.omit() 
write.csv(crime_data, file.path(STAGE, "stg_crime_data.csv"), row.names = F)
