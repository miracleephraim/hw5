###### Homework 3.1 Analysis

# Loading libraries and data

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)
library(modelsummary)

df <- read.table('C:/Users/mirac/Documents/GitHub/econ470_ma/hw5/data/output/acs_medicaid.txt', header=TRUE, sep="\t") 
# Question 1 
q1 <- df %>%
    mutate(dir_share = ins_direct/adult_pop) %>%
    group_by(year) %>%
    summarise(avg_dir_share = mean(dir_share, na.rm=TRUE)) %>%
    ggplot(aes(x=year, y=avg_dir_share)) +
    geom_line() + 
    xlab("Year") +
    ylab("Average Share") +
    ggtitle("Average Share of Population with Direct Insurance")

# Question 2 - Discussion

# Question 3 

q3 <- df %>%
    mutate(mdcd_share = ins_medicaid/adult_pop) %>%
    group_by(year) %>%
    summarise(avg_mdcd_share = mean(mdcd_share, na.rm=TRUE)) %>%
    ggplot(aes(x=year, y=avg_mdcd_share)) +
    geom_line() + 
    xlab("Year") +
    ylab("Average Share") +
    ggtitle("Average Share of Population with Medicaid")

# Question 4

table(df$expand_year)

q4_data <- df %>% 
    filter(expand_year == 2014 | is.na(expand_year) == FALSE) %>% # Drop rows where expand_year is NA or not 2014
    mutate(expand_2014 = ifelse(expand_year == 2014, 1, 0)) %>%
    mutate(unins = uninsured/adult_pop) %>%
    group_by(expand_2014, year) %>%
    summarise(avg_unins = mean(unins, na.rm=TRUE))


q4_plot <- ggplot(aes(x=year, y=avg_unins, color=factor(expand_2014), group=factor(expand_2014)), data = q4_data) +
    geom_line() +
    labs(color="Expand 2014") +
    xlab("Year") +
    ylab("Average Uninsured Share") +
    ggtitle("Average Uninsured Share by Expansion Status (2014)")

# question 5

df %>%
    filter((year == 2012) | (year == 2015)) %>%
    mutate(unins = uninsured/adult_pop) %>%
    group_by(year, expand) %>%
    summarise(avg_unins = mean(unins, na.rm=TRUE)) 

# question 6
reg.df <- df %>% filter(expand_year==2014 | is.na(expand_year), !is.na(expand_ever)) %>%
  mutate(perc_unins=uninsured/adult_pop,
         post = (year>=2014), 
         treat=post*expand_ever)
dd <- lm(perc_unins ~ post + expand_ever + post*expand_ever, data=reg.df)

modelsummary(list("DD (2014)"=dd),
             shape=term + statistic ~ model, 
             gof_map=NA,
             coef_omit='Intercept',
             vcov=~State,
             stars = TRUE
         )

# question 7
library(fixest)
dd.fe <- feols(perc_unins ~ treat | State + year, data=reg.df)
msummary(list("DD"=dd, "TWFE"=dd.fe),
         shape=term + statistic ~ model, 
         gof_map=NA,
         coef_omit='Intercept',
         vcov=~State,
         stars = TRUE
         )

# question 8

reg.df2 <- df %>% 
  mutate(perc_unins=uninsured/adult_pop,
         post = (year>=2014), 
         treat=post*expand_ever)

dd2 <- lm(perc_unins ~ post + expand_ever + post*expand_ever, data=reg.df2)
dd.fe2 <- feols(perc_unins ~ treat | State + year, data=reg.df2)

msummary(list("DD (2014)"=dd, "TWFE (2014)"=dd.fe, "DD (all)"=dd2, "TWFE (all)"=dd.fe2),
         shape=term + statistic ~ model, 
         gof_map=NA,
         coef_omit='Intercept',
         vcov=~State,
         stars = TRUE
         )

# question 9

mod.twfe <- feols(perc_unins~i(year, expand_ever, ref=2013) | State + year,
                  cluster=~State,
                  data=reg.df)

iplot(mod.twfe, 
      xlab = 'Time to treatment',
      main = 'Event study')

# question 10

# testing method, will check tmr if all this was necessary
reg.df14 <- df %>% filter(expand_year==2014 | is.na(expand_year), !is.na(expand_ever)) %>%
  mutate(perc_unins=uninsured/adult_pop,
         post = (year>=2014), 
         treat=post*expand_ever)
reg.df15 <- df %>% filter(expand_year==2015 | is.na(expand_year), !is.na(expand_ever)) %>%
  mutate(perc_unins=uninsured/adult_pop,
         post = (year>=2015), 
         treat=post*expand_ever)
reg.df16 <- df %>% filter(expand_year==2016 | is.na(expand_year), !is.na(expand_ever)) %>%
  mutate(perc_unins=uninsured/adult_pop,
         post = (year>=2016), 
         treat=post*expand_ever)
reg.df17 <- df %>% filter(expand_year==2017 | is.na(expand_year), !is.na(expand_ever)) %>%
  mutate(perc_unins=uninsured/adult_pop,
         post = (year>=2017), 
         treat=post*expand_ever)
reg.df18 <- df %>% filter(expand_year==2018 | is.na(expand_year), !is.na(expand_ever)) %>%
  mutate(perc_unins=uninsured/adult_pop,
         post = (year>=2019), 
         treat=post*expand_ever)

# alt

reg.dfall <- df %>%
  mutate(event_time = year - expand_year, # Calculate event time
         perc_unins = uninsured / adult_pop) %>% # Calculate uninsured percentage
  group_by(event_time) %>% # Group by event time
  summarise(avg_unins = mean(perc_unins, na.rm = TRUE)) # Calculate average uninsured share

mod.twfe <- feols(perc_unins~i(year, expand_ever, ref=2013) | State + year,
                  cluster=~State,
                  data=reg.df)

iplot(mod.twfe, 
      xlab = 'Time to treatment',
      main = 'Event study')

# Plot the event study
event_study_plot <- ggplot(reg.dfall, aes(x = event_time, y = avg_unins)) +
  geom_line() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") + # Mark expansion year
  xlab("Event Time (Years from Expansion)") +
  ylab("Average Uninsured Share") +
  ggtitle("Event Study: Effects of Medicaid Expansion on Uninsured Share") +
  theme_minimal()

print(event_study_plot)

# alt again

reg.dfall2 <- df %>%
  filter(!is.na(expand)) %>%  # Only keep expansion states (for event time)
  mutate(
    perc_unins = uninsured / adult_pop,
    event_time = year - expand_year
  )


mod.twfe2 <- feols(
  perc_unins ~ i(event_time, ref = -1) | State + year,
  cluster = ~State,
  data = reg.dfall2
)


iplot(
  mod.twfe2,
  xlab = "Years from Medicaid Expansion",
  main = "Event Study: Uninsured Rate Response to Medicaid Expansion"
)

reg.data2 <- df %>%
  mutate(time_to_treat  = ifelse(expand_ever==TRUE, year-expand_year, -1),
    time_to_treat =iflese(time_to_treat<-4,-4,time_to_treat))


mod.twfe3 <- feols(perc_unins~i(time_to_treat, expand_ever, ref=-1 | State + year))



save.image("C:/Users/mirac/Documents/GitHub/econ470_ma/hw5/submission2/results/hw_workspace5.Rdata")