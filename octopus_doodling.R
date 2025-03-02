library(dplyr)
library(ggplot2)
library(octopusR)
library(lubridate)

set_api_key(readLines(con = "api_key.txt"))
set_meter_details(meter_type = "electricity",
                  mpan_mprn = readLines("elec_mtr_mpan.txt"),
                  serial_number = readLines("elec_mtr_srl_num.txt"))

# there are two mpan numbers, believe that one is for export and one is for import
# not sure which is which

elec_use <- get_consumption(meter_type = "elec", tz = "GMT", 
                            period_from = dmy("01-01-2024"),
                            period_to = dmy("31-12-2024"), 
                            group_by = "hour")

head(elec_use)

table(sign(elec_use$consumption))

elec_use <- elec_use %>% 
  mutate(mnth = month(interval_start), hr = hour(interval_start))

imprt_by_hr <- elec_use %>% 
  mutate(hr = hour(interval_start)) %>% 
  group_by(hr) %>% 
  summarise(mn_consumption = mean(consumption))

imprt_by_hr

ggplot(data = imprt_by_hr, aes(x = hr, y = mn_consumption)) + geom_col()

imprt_by_mnth <- elec_use %>% 
  mutate(mnth = month(interval_start)) %>% 
  group_by(mnth) %>% 
  summarise(mn_consumption = mean(consumption))

imprt_by_mnth

ggplot(data = imprt_by_mnth, aes(x = mnth, y = mn_consumption)) + geom_col()

# for November to Feb, how quickly in the morning will I use 10 kWh of battery?

winter_morning_use <- elec_use %>% 
  filter(mnth %in% c(1,2, 11,12)) %>% 
  group_by(hr) %>% 
  summarise(mn_consumption = mean(consumption))
  
winter_morning_use
ggplot(data = winter_morning_use, aes(x = hr, y = mn_consumption)) + geom_col()

# Highest consumption is 5 am to 8 am. If switch three hours to o/n battery, how much would I save?
# Approx 10 kWh, charged at the day rate. 

# export data ####

elec_use2 <- get_consumption(meter_type = "elec", tz = "GMT", 
                             mpan_mprn = readLines("elec_mtr_mpan_exprt.txt"), 
                             period_from = dmy("01-01-2024"),
                             period_to = dmy("31-12-2024"), 
                             group_by = "hour")
head(elec_use2)
range(elec_use2$interval_start)
exprt_by_hr <- elec_use2 %>% 
  mutate(hr = hour(interval_start)) %>% 
  group_by(hr) %>% 
  summarise(mn_consumption = mean(consumption))

exprt_by_hr

exprt_by_mnth <- elec_use2 %>% 
  mutate(mnth = month(interval_start)) %>% 
  group_by(mnth) %>% 
  summarise(mn_consumption = mean(consumption))

exprt_by_mnth

ggplot(data = exprt_by_mnth, aes(x = mnth, y = mn_consumption)) + geom_col() + 
  scale_x_discrete(breaks = seq(1,12,1), labels = seq(1,12,1))

# question - will I be better off on a different tariff ####
# In winter - charge batteries over-night and discharge during day
# In summer - charge during day and export surplus. 
# and at which point do I switch over?

# first, calculate existing costs under different tariffs

prods <- get_products()
prods

prods %>% 
  # filter(is_green == TRUE) %>% 
  select(display_name, direction, description, term) %>% 
  View()

# current tariff - flexible octopus @ 25.57p/kWh

elec_use <- elec_use %>% 
  mutate(flex_cost = consumption * 0.2557, 
         aira_zero_cost = case_when(
           # 
           between(hour(interval_start), 16, 19) ~ consumption * 0.4047, 
           between(hour(interval_start), 4, 7)   ~ consumption * 0.1323, 
           between(hour(interval_start), 13, 16)   ~ consumption * 0.1323,
           between(hour(interval_start), 22, 00) ~ consumption * 0.1323, 
           TRUE ~ consumption * 0.2698 
         ),
         flux_cost = case_when(
           # https://octopus.energy/smart/flux/
           between(hour(interval_start), 02, 05) ~ consumption * 0.1534, 
           between(hour(interval_start), 16, 19) ~ consumption * 0.2744, 
           TRUE ~ consumption * 0.2557
         ), 
         go_cost = case_when(
           # https://octopus.energy/smart/go/
           between(hour(interval_start), 01, 05) ~ consumption * 0.085, 
           TRUE ~ consumption * 0.271
         ),
         intel_go_cost = case_when(
           # https://octopus.energy/smart/go/
           between(hour(interval_start), 00, 05) ~ consumption * 0.07, 
           TRUE ~ consumption * 0.271
         ),
         mnth = month(interval_start))

elec_cost <- elec_use %>% 
  group_by(mnth) %>% 
  summarise(across(.cols = c(flex_cost, aira_zero_cost, flux_cost, go_cost, 
                             intel_go_cost), .fns = sum))
View(elec_cost)

elec_cost %>% 
  summarise(across(.cols = c(-mnth), .fns = sum))

