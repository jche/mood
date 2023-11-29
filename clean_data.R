
require(tidyverse)
require(lubridate)

all_files <- list.files("data/")


##### process daylio data

daylio_files <- all_files[startsWith(all_files, "daylio")]
daylio_raw <- daylio_files %>% 
  map_df(~read_csv(paste0("data/", .))) %>% 
  bind_rows() %>% 
  select(-date) %>%   # apple/android code "date" variable differently
  distinct()

# Whoops! have two activities titled "reading"....
#  --> just merge them for now, keep this in mind
daylio <- daylio_raw %>% 
  mutate(date = ymd(full_date),
         year = year(date),
         month = month(date),
         day = day(date),
         wday = wday(date),
         mood = mood %>% 
           factor(levels=c("awful", "bad", "meh", "good", "rad")) %>% 
           as.numeric()) %>%
  # process activities column
  separate_longer_delim(activities, "|") %>% 
  mutate(activities = trimws(activities),
         temp = 1) %>% 
  distinct() %>%    # merge two "reading" activities
  pivot_wider(names_from = activities,
              values_from = temp,
              names_prefix = "act_") %>% 
  mutate(across(starts_with("act_"), ~ifelse(is.na(.), 0, .))) %>% 
  select(date, year, month, day, wday,
         mood, starts_with("act_"), note)


##### process financial data
#  - note: ccs only allow access to 24 months of data!

# Chase: Accounts > click credit card > scroll to Account Activity > download button
chase_files <- all_files[startsWith(all_files, "Chase")]
chase_raw <-  chase_files %>% 
  map_df(~read_csv(paste0("data/", .))) %>% 
  bind_rows()

chase <- chase_raw %>% 
  mutate(date = mdy(`Transaction Date`),
         year = year(date),
         month = month(date),
         day = day(date),
         wday = wday(date)) %>% 
  select(date, year, month, day, wday, 
         category=Category, type=Type, amount=Amount,
         description=Description)


# Discover: Activity > All Activity & Statements > Show me > All Available Activity
discover_files <- all_files[startsWith(all_files, "Discover")]
discover_raw <-  discover_files %>% 
  map_df(~read_csv(paste0("data/", .))) %>% 
  bind_rows()

discover <- discover_raw %>% 
  mutate(date = mdy(`Trans. Date`),
         year = year(date),
         month = month(date),
         day = day(date),
         wday = wday(date),
         amount = -Amount) %>%    # discover flips credit/payment
  select(date, year, month, day, wday, 
         category=Category, amount)

