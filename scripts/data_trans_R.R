library(dplyr)

MART <- here("mart")

coffee_sales <- coffee_shop_sales |> 
  filter(product_category == "Coffee") |>  
  group_by(product_type) |> 
  summarise(
    num_transactions = n(),                                      
    total_units_sold = sum(transaction_qty),       
    total_revenue    = sum(transaction_qty * unit_price)) |> 
  rename(product = product_type) |> 
  mutate(share_of_sales = round(total_revenue / sum(total_revenue) * 100, 1),
         score = percent_rank(total_revenue)
  ) |> 
  arrange(desc(total_revenue))

tea_sales <- coffee_shop_sales |> 
  filter(product_category == "Tea") |> 
  group_by(product_type) |> 
  summarise(
    num_transactions = n(),                                      
    total_units_sold = sum(transaction_qty),       
    total_revenue    = sum(transaction_qty * unit_price)                         
  ) |>
  rename(product = product_type) |> 
  mutate(share_of_sales = round(total_revenue / sum(total_revenue) * 100, 1),
         score = percent_rank(total_revenue)) |>
  arrange(desc(total_revenue))

vending_machine_sales <- vending_machine_sales |> 
  group_by(coffee_name) |> 
  summarise(
    total_revenue = sum(money),
    num_transactions = n()
  ) |> 
  rename(product = coffee_name) |> 
  mutate(share_of_sales = round(total_revenue / sum(total_revenue) * 100, 1),
         score = percent_rank(total_revenue)) |>
  filter(share_of_sales >= 10) |> 
  arrange(desc(total_revenue))

products_list <- bind_rows(
  coffee_sales,
  tea_sales,
  vending_machine_sales) |>
  select(product, score) |> 
  filter(score >= 0.5) |> 
  arrange(desc(score))
write.csv(products_list, file.path(MART, 'products_list.csv'), row.names = F)

  

holidays_elasticity <- holidays_elasticity |>
  mutate(Holiday_Name = ifelse(month(as.Date(Date, format = "%d-%m-%Y")) == 2,  "Valentine's Day",
                               ifelse(month(as.Date(Date, format = "%d-%m-%Y")) == 9,  "Labor Day",
                                      ifelse(month(as.Date(Date, format = "%d-%m-%Y")) == 11, "Thanksgiving",
                                             ifelse(month(as.Date(Date, format = "%d-%m-%Y")) == 12, "Christmas", NA))))) |>
  group_by(Holiday_Name) |>
  summarise(
    Avg_Sales = mean(Holiday_Sales),
    .groups = "drop"
  ) |>
  mutate(Date = case_when(
    Holiday_Name == "Valentine's Day" ~ "14-02",
    Holiday_Name == "Labor Day"       ~ "01-09",
    Holiday_Name == "Thanksgiving"    ~ "28-11",
    Holiday_Name == "Christmas"       ~ "25-12"
  )) |> 
  arrange(desc(Avg_Sales))
write.csv(holidays_elasticity, file.path(MART, 'holidays_elasticity.csv'), row.names = F)
