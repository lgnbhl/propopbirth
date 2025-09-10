# header ------------------------------------------------------------------

# Code to estimate future fertility rates 
# in general with the FSO method (optimization with optim function)
# KORSTAT forecast working group, April 2025



# preparation -------------------------------------------------------------

# packages
library(BFS)        # for FSO input data
library(forecast)   # ARIMA model
library(here)       # paths
library(tidyverse)
library(propopbirth)

# data(package="propopbirth")


# source functions
# path <- file.path(here::here(), "R")
# invisible(map(list.files(path, pattern = ".R", full.names = TRUE), source))


# overall parameters
year_first <- 2011
year_last <- 2023
age_fert_min <- 15
age_fert_max <- 49
spatial_code <- c("0261", "4566", "0198")
spatial_unit <- c("Stadt Zürich", "Frauenfeld", "Uster")





# get population and birth data -------------------------------------------

# end of year female population at 'fertile age' 
# comment: year and age minus 1 to calculate the mean annual population afterwards

# pop <- get_population_data(
#   number_fso = "px-x-0102010000_101",
#   year_first = year_first - 1,
#   year_last = year_last,
#   age_fert_min = age_fert_min - 1,
#   age_fert_max = age_fert_max,
#   spatial_code = spatial_code,
#   spatial_unit = spatial_unit
# )

   
pop <- fso_pop

# comments
# age should be as of 14, not as of 15
# year: should be as of 2010, not as of 2020 (according to parameters)


# bir <- get_birth_data(
#   year_first = year_first,
#   year_last = year_last,
#   age_fert_min = age_fert_min,
#   age_fert_max = age_fert_max,
#   spatial_code = spatial_code,
#   spatial_unit = spatial_unit,
#   with_nationality = TRUE
# )



# birth
bir <- fso_birth |> 
  filter(age >= age_fert_min,
         age <= age_fert_max,
         spatial_unit %in% spatial_unit_)
  
  
  

# get input data ----------------------------------------------------------

input <- create_input_data(
  pop = pop,
  bir = bir,
  year_first = year_first,
  year_last = year_last,
  age_fert_min = age_fert_min,
  age_fert_max = age_fert_max,
  fert_hist_years = 1
)

# comment:
# fer_y (fertility per year) is in the output to make plots
# at the very end of the fertility rate forecast


# forecast: tfr ------------------------------------------------------------

# optional: given forecast values
temporal_end_tfr <- expand_grid(
  spatial_unit = spatial_unit, 
  nat = c("ch", "int")) |> 
  mutate(y_end = c(0.8, 0.8, 1.0, 2.0, 1.2, 1.3))


# forecast
forecast_tfr <- forecast_tfr_mab(
  topic = "tfr", 
  input_dataset = input$tfr,
  trend_model = c("lm", 2024, 2026, trend_past = 7, trend_prop = 0.5),
  temporal_model = c(
    "cubic", 2027, 2055, trend_prop = 0.8, z0_prop = 0.7, z1_prop = 0
  ),
  temporal_end = NA,
  constant_model = c("constant", 2056, 2075)
  ) 


# plot
ggplot(forecast_tfr) +
  geom_line(aes(x = year, y = tfr, color = category)) +
  geom_point(aes(x = year, y = tfr, color = category)) +
  facet_grid(nat ~ spatial_unit)






# forecast: mab ------------------------------------------------------------

# optional: given forecast values
temporal_end_mab <- expand_grid(
  spatial_unit = spatial_unit, 
  nat = c("ch", "int")) |> 
  mutate(y_end = c(35, 38, 33, 31, 34, 32))


# forecast
forecast_mab <- forecast_tfr_mab(
  topic = "mab", input_dataset = input$mab,
  trend_model = c("lm", 2024, 2026, trend_past = 7, trend_prop = 0.5),
  temporal_model = c("Bezier", 2027, 2055, 
    trend_prop = 0.3, z0_prop = 0.7, z1_prop = 0),
  temporal_end = NA,  
  constant_model = c("constant", 2056, 2075)
  ) 



# plot
ggplot(forecast_mab) +
  geom_line(aes(x = year, y = mab, color = category)) +
  geom_point(aes(x = year, y = mab, color = category)) +
  facet_grid(nat ~ spatial_unit)



# forecast: fertility rate ------------------------------------------------

# forecast ('birth_rate', same name as in propop)
forecast_fer <- forecast_fertility_rate(
  fer_dat = input$fer,
  tfr_dat = forecast_tfr,
  mab_dat = forecast_mab, 
  year_begin = 2024, 
  year_end = 2075)


# plot: year on x-axis
    forecast_fer |>     
    bind_rows(input$fer_y) |>
    filter(age %% 5 == 0) |> 
    mutate(age = factor(age)) |>
    ggplot() +
    geom_vline(xintercept = year_last + 1, linetype = 2) +      
    geom_line(aes(year, birth_rate, color = age)) +
    facet_grid(nat ~ spatial_unit)


# plot: age on x-axis
    forecast_fer |>     
    bind_rows(input$fer_y ) |>
    filter(year %% 10 == 0) |>
    mutate(year = factor(year)) |>
    ggplot() +
    geom_line(aes(age, birth_rate, color = year)) +
    facet_grid(nat ~ spatial_unit)



