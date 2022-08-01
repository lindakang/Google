library(tidyr)
library(tidyverse)
library(tibble)
library(dbplyr)
library(utils)
library(lubridate)

# import csv files
daily_activity <- read_csv("/Users/Linda/Desktop/RStudio/Google Data/CaseStudy/dailyActivity_merged.csv")
daily_calories <- read_csv("/Users/Linda/Desktop/RStudio/Google Data/CaseStudy/dailyCalories_merged.csv")
daily_intensity <- read_csv("/Users/Linda/Desktop/RStudio/Google Data/CaseStudy/dailyIntensities_merged.csv")
hourly_intensity <- read_csv("/Users/Linda/Desktop/RStudio/Google Data/CaseStudy/hourlyIntensities_merged.csv")
daily_steps <- read_csv("/Users/Linda/Desktop/RStudio/Google Data/CaseStudy/dailySteps_merged.csv")
hourly_steps <- read_csv("/Users/Linda/Desktop/RStudio/Google Data/CaseStudy/hourlySteps_merged.csv")
sleep_day <- read_csv("/Users/Linda/Desktop/RStudio/Google Data/CaseStudy/sleepDay_merged.csv")

# check duplicates
sum(duplicated(daily_activity))  # 0 duplicate
sum(duplicated(sleep_day))  # 3 duplicates
sum(duplicated(hourly_intensity))  # 0 duplicate
sum(duplicated(hourly_steps))  # 0 duplicate

# remove duplicates
sleep_day <- distinct(sleep_day)
sum(duplicated(sleep_day))  # 3 duplicates

# check NA
sum(is.na(daily_activity)) # 0 NAs
sum(is.na(sleep_day)) # 0 NAs
sum(is.na(hourly_intensity))  # 0 NAs
sum(is.na(hourly_steps)) # 0 NAs

# Remove time from "SleepDay" and assign as ActivityDate
sleep_day <- sleep_day %>%
  mutate("ActivityDate" = str_remove(SleepDay, " 12:00:00 AM")) %>%
  select(-SleepDay)

# Transform ActivityDate to date format
sleep_day$ActivityDate <- as.Date(sleep_day$ActivityDate, format = "%m/%d/%Y")

daily_activity$ActivityDate <- as_date(daily_activity$ActivityDate, format = "%m/%d/%Y")

# Add a "Weekday" column
daily_activity <- daily_activity %>%
  mutate(Weekday = weekdays(ActivityDate))

# Join daily_activity & sleep_day
daily_all <- merge(daily_activity, sleep_day, by = c("Id", "ActivityDate"), all = TRUE)

hourly_all <- merge(hourly_intensity, hourly_steps, by = c("Id", "ActivityHour"), all = TRUE)

daily_all$Weekday <-ordered(daily_all$Weekday,
                            levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Transform ActivityHour to date format & separate to Date & Time
hourly_all$ActivityHour = as.POSIXct(hourly_all$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())

hourly_all <- hourly_all %>%
  separate(ActivityHour, into = c("date", "time"), sep= " ")

# Transform Id to factor & check levels
daily_activity$Id <- factor(daily_activity$Id)
sleep_day$Id <- factor(sleep_day$Id)
levels(sleep_day$Id)
hourly_all$Id <- factor(hourly_all$Id)


# Most active time of the day
avg_hourly_activity <- hourly_all %>%
  select(Id, time, TotalIntensity, StepTotal) %>%
  group_by(time) %>%
  summarise(total_step_mean = mean(StepTotal),
            total_intensity_mean = mean(TotalIntensity))
  
ggplot(hourly_all, aes(x = time, y = StepTotal, alpha = TotalIntensity)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = 'Time of the day',
       y = 'Total Steps',
       title = 'Activity (Step & Intensity) of the day')

ggplot(avg_hourly_activity, aes(x = time, y = total_step_mean, fill = total_intensity_mean)) +
  geom_col()+
  scale_fill_gradient(low = "light blue", high = "dark blue") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = 'Time of the day',
       y = 'Average Steps',
       title = 'Average Activity of the Day',
       fill = 'Average\nIntensity')

# Daily activity in time
daily_time <-  daily_all %>%
  mutate(total_activity = VeryActiveMinutes + FairlyActiveMinutes + LightlyActiveMinutes + SedentaryMinutes,
         hour_usage = total_activity / 60,
         percent_usage = hour_usage / 24 * 100) %>%
  select(Id, ActivityDate, total_activity, hour_usage, percent_usage) %>%
  mutate(usage = case_when(
    percent_usage == 100 ~ "All day",
    percent_usage < 100 & percent_usage >= 75 ~ "At least 18 hours", 
    percent_usage < 75 & percent_usage >= 50 ~ "At least 12 hours",
    percent_usage < 50 & percent_usage > 0 ~ "Less than 12 hours"))

head(daily_time)

ggplot(daily_time, aes(x = usage, y = hour_usage)) +
  geom_point()
  
# Most active day of the week
avg_daily_activity <- daily_all %>%
  mutate(total_activity = VeryActiveMinutes + FairlyActiveMinutes + LightlyActiveMinutes + SedentaryMinutes,
        pct_very_active = VeryActiveMinutes / total_activity * 100,
        pct_fairly_active = FairlyActiveMinutes / total_activity * 100,
        pct_lightly_active = LightlyActiveMinutes / total_activity * 100,
        pct_sedentary = SedentaryMinutes / total_activity * 100) %>%
  group_by(Weekday) %>%
  summarise(pct_very_active_mean = mean(pct_very_active),
            pct_fairly_active_mean = mean(pct_fairly_active),
            pct_lightly_active_mean = mean(pct_lightly_active),
            pct_sedentary_mean = mean(pct_sedentary),
            total_step_mean = mean(TotalSteps),
            total_dist_mean = mean(TotalDistance),
            very_active_mean =  mean(VeryActiveMinutes),
            activity_mean = mean(total_activity - SedentaryMinutes),
            calories_mean = mean(Calories))

# Transform data to long form
activity <- avg_daily_activity %>%
  select(Weekday, pct_very_active_mean, pct_fairly_active_mean, pct_lightly_active_mean, pct_sedentary_mean) %>%
  pivot_longer(c("pct_very_active_mean", "pct_fairly_active_mean", "pct_lightly_active_mean", "pct_sedentary_mean"),
               names_to = 'activity_level', values_to = 'percent_activity')

ggplot(activity, 
       aes(x = Weekday, y = percent_activity, fill = activity_level)) +
  geom_bar(position = "fill",stat = "identity") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_brewer(palette = "Paired",
                    labels = c('Fairly active', 'Lightly active', 'Sedentary', 'Very Active')) +
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(title = 'Daily Activity',
       x = 'Day of the Week',
       y = '% of each activity level',
       fill = 'Activity Level')

# Very active vs. steps
ggplot(avg_daily_activity, aes(x = Weekday, y = total_step_mean, fill = very_active_mean)) +
  geom_col()+
  scale_fill_gradient(low = "light blue", high = "dark blue") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = 'Day of the week',
       y = 'Average Total Steps',
       title = 'Average Total Steps vs. Very Active Minutes',
       fill = 'Very Active Minutes')

# Total Steps vs. Total Distance
ggplot(avg_daily_activity, aes(x = Weekday, y = total_dist_mean, fill = total_step_mean)) +
  geom_col()+
  scale_fill_gradient(low = "light blue", high = "dark blue") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = 'Day of the week',
       y = 'Average Total Distance (km)',
       title = 'Average Activity of the Day',
       fill = 'Average Total Steps')

# Calories vs. Steps
ggplot(avg_daily_activity, aes(x = Weekday, y = calories_mean, fill = total_step_mean)) +
  geom_col()+
  scale_fill_gradient(low = "light blue", high = "dark blue") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = 'Day of the week',
       y = 'Average Calories Burned',
       title = 'Weekly Average Calories Burn vs. Steps',
       fill = 'Average Steps')

# Sleep quality
sleep_time <- daily_all %>%
  drop_na() %>%
  mutate(time_in_bed = TotalTimeInBed / 60,
         time_asleep = TotalMinutesAsleep / 60,
         percent_asleep = TotalMinutesAsleep / TotalTimeInBed * 100) %>%
  select(Id, ActivityDate, Weekday, time_in_bed, time_asleep, percent_asleep)

avg_sleep_time <- sleep_time %>%
  group_by(Weekday) %>%
  summarise(avg_total_in_bed = mean(time_in_bed),
            avg_total_asleep = mean(time_asleep),
            avg_per_asleep = mean(percent_asleep))

ggplot(avg_sleep_time, aes(x = Weekday, y = avg_total_in_bed, fill = avg_per_asleep)) +
  geom_col()+
  scale_fill_gradient(low = "skyblue", high = "navy blue") +
  theme(axis.text.x = element_text(angle =90)) +
  labs(title = 'Sleep Quality',
       x = 'Day of the Week',
       y = 'Time in bed (hours)',
       fill = 'Time Asleep (%)')

ggplot(sleep_time, aes(x = Weekday, y = time_in_bed, alpha = time_asleep)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 60)) + 
  labs(title = 'Sleep Quality',
       x = 'Day of the Week',
       y = 'Time in bed (hours)',
       alpha = 'Time Asleep (hours)')
  
