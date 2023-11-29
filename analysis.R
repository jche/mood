
# playground to analyze data!

require(tidyverse)

source("clean_data.R")
JOIN_COLS <- c("date", "year", "month", "day", "wday")


##### joining data!

# all credit card transactions
cc <- chase %>% 
  bind_rows(
    discover %>% 
      mutate(type = ifelse(category == "Payments and Credits",
                           "Payment", "Sale"),
             
             # TODO: key for matching discover categories to chase categories
             category = case_when(
               category == "Supermarkets" ~ "Groceries",
               category == "Payments and Credits" ~ NA,
               category == "Merchandise" ~ "Shopping",
               category == "Services" ~ "Professional Services",
               .default = NA))
    ) %>% 
  mutate(category = case_when(
    category %in% c("Automotive", "Gas", "Travel") ~ "transportation",
    category %in% c("Bills & Utilities", "Fees & Adjustments") ~ "bills",
    category %in% c("Education", "Entertainment", "Gifts & Donations") ~ "entertainment",
    category %in% c("Food & Drink") ~ "food",
    category %in% c("Groceries") ~ "grocery",
    category %in% c("Home", "Shopping") ~ "shopping",
    category %in% c("Health & Wellness", "Professional Services", "Personal") ~ "misc",
    .default = "payment"
  ))

# cc transactions, one row per day
daily_spend_raw <- cc %>% 
  group_by(across(all_of(JOIN_COLS))) %>% 
  summarize(
    raw_tot = sum(amount),
    .groups = "drop")

# cc transactions, one row per day, without cc payments
daily_spend <- cc %>% 
  filter(type != "Payment") %>% 
  group_by(across(all_of(JOIN_COLS))) %>% 
  summarize(
    num = n(),
    tot = sum(amount),
    tot_trans = sum(ifelse(category == "transportation", amount, 0)),
    tot_bills = sum(ifelse(category == "bills", amount, 0)),
    tot_ent  = sum(ifelse(category == "entertainment", amount, 0)),
    tot_food = sum(ifelse(category == "food", amount, 0)),
    tot_groc = sum(ifelse(category == "grocery", amount, 0)),
    tot_shop = sum(ifelse(category == "shopping", amount, 0)),
    tot_misc = sum(ifelse(category == "misc", amount, 0)),
    .groups = "drop"
  )

# final daily spending dataset
spending <- daily_spend %>% 
  left_join(daily_spend_raw, by=JOIN_COLS)

# final joined dataset
df <- daylio %>% 
  left_join(spending, by=JOIN_COLS)



##### plots!

### mood

df %>% 
  ggplot(aes(date, mood)) +
  geom_point() +
  geom_line() +
  stat_smooth()

# no monthly patterns...?
df %>% 
  group_by(year, month) %>% 
  summarize(mn = mean(mood)) %>% 
  ggplot(aes(x=month, y=mn, color=year)) +
  geom_point() +
  geom_line(aes(group=year))

df %>% 
  group_by(month) %>% 
  summarize(mn = mean(mood)) %>% 
  ggplot(aes(x=month, y=mn)) +
  geom_point() +
  geom_line()

# conclusion: consistently, monday is worst day
df %>% 
  group_by(year, wday) %>% 
  summarize(mn = mean(mood)) %>% 
  ggplot(aes(x=wday, y=mn, color=year)) +
  geom_point() +
  geom_line(aes(group=year))


### activities
work_activities <- paste0("act_", 
                    c("discussing", "mathing", "teaching", "presenting",
                      "coding", "reading", "coursework", "writing", "brainstorming"))

# find first occurrence of specific work activities: late july 2020
df %>% 
  filter(act_discussing | act_teaching | act_presenting | act_coding | 
           act_coursework | act_writing | act_brainstorming) %>% 
  arrange(date) %>% 
  head()

# 
df %>% 
  filter(date >= "2020-07-28") %>% 
  group_by(year, month) %>% 
  summarize(n = n(),
            s = sum(across(starts_with("act_"))),
            mn = s/n) %>% 
  ggplot(aes(x=month, y=mn, color=year)) +
  geom_point() +
  geom_line(aes(group=year)) +
  stat_smooth()


# trends! some of these proportions are crazy
df %>% 
  pivot_longer(starts_with("act_"),
               names_to = "activity") %>% 
  filter(activity %in% work_activities) %>% 
  group_by(wday, activity) %>% 
  summarize(prop = mean(value)) %>% 
  ggplot(aes(x=wday, y=prop)) +
  geom_point() +
  geom_line() +
  facet_wrap(~activity, scales="free")
df %>% 
  pivot_longer(starts_with("act_"),
               names_to = "activity") %>% 
  filter(!activity %in% work_activities) %>% 
  group_by(wday, activity) %>% 
  summarize(prop = mean(value)) %>% 
  ggplot(aes(x=wday, y=prop)) +
  geom_point() +
  geom_line() +
  facet_wrap(~activity, scales="free")


# trends & mood: see pretty much what's expected
# Q: how many of these differences are "significant"?
df %>% 
  pivot_longer(starts_with("act_"),
               names_to = "activity") %>% 
  filter(!activity %in% work_activities) %>% 
  group_by(wday, activity, value) %>% 
  summarize(n = n(),
            mood = mean(mood)) %>% 
  ggplot(aes(x=wday, y=mood, color=value)) +
  geom_point(aes(size=n)) +
  geom_line(aes(group=as.factor(value))) +
  facet_wrap(~activity)
df %>% 
  pivot_longer(starts_with("act_"),
               names_to = "activity") %>% 
  filter(activity %in% work_activities) %>% 
  group_by(wday, activity, value) %>% 
  summarize(n = n(),
            mood = mean(mood)) %>% 
  ggplot(aes(x=wday, y=mood, color=value)) +
  geom_point(aes(size=n)) +
  geom_line(aes(group=as.factor(value))) +
  facet_wrap(~activity)


# exploration of favorite work activities
tmp1 <- df %>% 
  filter(act_work == 1) %>%
  select(year, wday, mood, all_of(work_activities)) %>%
  group_by(year) %>% 
  summarize(across(all_of(work_activities),
                   ~sum(.*mood)/sum(.))) %>% 
  pivot_longer(starts_with("act_"), values_to = "val_doing")
tmp2 <- df %>% 
  filter(act_work == 1) %>%
  select(year, wday, mood, all_of(work_activities)) %>%
  group_by(year) %>% 
  mutate(across(all_of(work_activities),
                ~!.)) %>% 
  summarize(across(all_of(work_activities),
                   ~sum(.*mood)/sum(.))) %>% 
  pivot_longer(starts_with("act_"), values_to = "val_not_doing")


# conclusion: maybe I don't actually like teaching, 
#             I like discussing with peers?
#  - or just need to account for year effects, GLM year was a rough year
tmp1 %>% 
  left_join(tmp2, by=c("year", "name")) %>% 
  mutate(diff = val_doing - val_not_doing) %>% 
  ggplot(aes(x=year, y=diff)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept=0, lty="dotted") +
  facet_wrap(~name)




# work
df %>% 
  group_by(wday, act_work) %>% 
  summarize(n = n(),
            mood = mean(mood)) %>% 
  ggplot(aes(x=wday, y=mood, color=act_work)) +
  geom_point(aes(size=n)) +
  geom_line(aes(group=as.factor(act_work)))

# which work activities are best





# friends
df %>% 
  group_by(wday, act_friends) %>% 
  summarize(n = n(),
            mood = mean(mood)) %>% 
  ggplot(aes(x=wday, y=mood, color=act_friends)) +
  geom_point(aes(size=n)) +
  geom_line(aes(group=as.factor(act_friends)))

### spending

df %>% 
  group_by(year, month) %>% 
  summarize()







