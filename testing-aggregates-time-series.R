library(tidyverse) # dataframe functions
library(dslabs) # dataframe functions
library(dplyr) # math functions
library(readr) # for parsing csv files
library(ggplot2) # for graphing
library(lubridate) # for parsing date
library(scales) # scaling functions

# daily testing data

testing_aggregates <- read.csv("testing_aggregates.csv")

daily_testing_data <- testing_aggregates %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  group_by(report_date) %>% 
  summarize(positive_daily = sum(daily_output_positive_individuals),
            positive_cumulative = sum(cumulative_positive_individuals),
            unique_indivs_tested_daily = sum(daily_output_unique_individuals),
            backlogs_untested_samples = sum(backlogs)) %>%
  mutate(unique_indivs_tested_cumulative = cumsum(unique_indivs_tested_daily))

write_csv(daily_testing_data)

# daily case information

case_information <- read.csv("case_information.csv")
colnames(case_information)[5] <- "report_date"

daily_case_information <- case_information %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  group_by(report_date) %>%
  summarize(confirmed_daily = n()) %>%
  mutate(confirmed_cumulative = cumsum(confirmed_daily))

write_csv(daily_case_information)

# multiple time series data

multiple_time_series <- merge(daily_case_information, daily_testing_data, by = "report_date") %>%
  mutate(report_date = ymd(report_date),
         backlogs_unreported_daily = positive_daily - confirmed_daily,
         backlogs_unreported_cumulative = cumsum(backlogs_unreported_daily))

write_csv(multiple_time_series)

# cumulative plot

cumulative_plot <- multiple_time_series %>% 
  filter(report_date >= ymd("2020-05-19")) %>%
  select(report_date, confirmed_cumulative, positive_cumulative, backlogs_untested_samples, unique_indivs_tested_cumulative, backlogs_unreported_cumulative) %>%
  gather(key = "variable", value = "value", -report_date) %>%
  ggplot(aes(x = report_date, y = value)) +
  geom_line(aes(color = variable)) +
  scale_color_manual(values = c("darkred", "green", "blue", "violet", "orange")) +
  scale_y_log10(labels = comma) +
  labs(title = "Cumulative Counts") +
  xlab("Reporting Date") +
  ylab("Number of Cases/Samples") +
  scale_color_discrete(name = "Legend", labels = c("Unreported Backlogs", "Untested Samples", "Confirmed", "Positive", "Unique Individuals"))
cumulative_plot
ggsave("cumulative_plot.png")

# inverse hyperbolic sine transformation

asinh_trans <- function(){
  trans_new(name = 'asinh', transform = function(x) asinh(x), 
            inverse = function(x) sinh(x))
}

# daily plot

daily_plot <- multiple_time_series %>% 
  filter(report_date >= ymd("2020-05-19")) %>%
  select(report_date, confirmed_daily, positive_daily, backlogs_untested_samples, backlogs_unreported_daily, unique_indivs_tested_daily) %>%
  gather(key = "variable", value = "value", -report_date) %>%
  ggplot(aes(x = report_date, y = value)) +
  geom_line(aes(color = variable)) +
  scale_color_manual(values = c("red", "green", "blue", "violet", "orange")) +
  scale_y_log10() +
  labs(title = "Daily Counts") +
  xlab("Reporting Date") +
  ylab("Number of Cases/Samples") +
  scale_color_discrete(name = "Legend", labels = c("Unreported Backlogs", "Untested Samples", "Confirmed", "Positive", "Unique Individuals"))
daily_plot
ggsave("daily_plot.png")

# linear difference plot

linear_difference_plot <- multiple_time_series %>% 
  filter(report_date >= ymd("2020-04-03")) %>%
  select(report_date, confirmed_daily, backlogs_unreported_daily) %>%
  gather(key = "variable", value = "value", -report_date) %>%
  ggplot(aes(x = report_date, y = value)) +
  geom_bar(aes(fill = variable), position = "stack", stat = "identity", alpha = 0.3) +
  labs(title = "Daily Backlogs per Confirmed Cases") +
  xlab("Reporting Date") +
  ylab("Number of Cases") +
  scale_fill_discrete(name = "Legend", labels = c("Unreported Backlogs", "Confirmed"))
linear_difference_plot
ggsave("linear_difference_plot.png")

# cumulative difference plot

cumulative_difference_plot <- multiple_time_series %>% 
  filter(report_date >= ymd("2020-04-03")) %>%
  select(report_date, confirmed_cumulative, backlogs_unreported_cumulative, backlogs_untested_samples) %>%
  gather(key = "variable", value = "value", -report_date) %>%
  ggplot(aes(x = report_date, y = value)) +
  geom_bar(aes(fill = factor(variable, levels = c("backlogs_untested_samples", "backlogs_unreported_cumulative", "confirmed_cumulative"))), position = "stack", stat = "identity", alpha = 0.3) +
  labs(title = "Cumulative Backlogs per Confirmed Cases") +
  xlab("Reporting Date") +
  ylab("Number of Individuals") +
  scale_fill_discrete(name = "Legend", labels = c("Untested Samples", "Unreported Backlogs", "Confirmed"))
cumulative_difference_plot
ggsave("cumulative_difference_plot.png")
