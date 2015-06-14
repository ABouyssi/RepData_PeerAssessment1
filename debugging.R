unzip(zipfile = ".\\activity.zip")
activity = read.csv(file = ".\\activity.csv", header = TRUE)

library(dplyr)
activity = tbl_df(activity)

install.packages("ggplot2")
library(ggplot2)
to_plot = as.data.frame(x = activity %>%
    group_by(date) %>%
    summarize(sum_steps = sum(x = steps, na.rm = TRUE)) %>%
    select(date, sum_steps))

p = ggplot(data = to_plot, aes(x = sum_steps))
p = p + geom_histogram(binwidth = 1000, fill = "blue")
p = p + labs(title = "Distribution of daily number of steps")
p = p + labs(x = "Total daily number of steps (bins of 1,000 steps)")
p = p + labs(y = "Frequency (number of days)")
p

activity %>%
  group_by(date) %>%
  summarize(sum_steps = sum(x = steps, na.rm = TRUE)) %>%
  summarize(avg_steps = mean(x = sum_steps)) %>%
  select(avg_steps)

to_plot = as.data.frame(x = activity %>%
  group_by(interval) %>%
  summarize(avg_steps = mean(x = steps, na.rm = TRUE)) %>%
  select(interval, avg_steps))

p = ggplot(data = to_plot, aes(x = interval, y = avg_steps))
p = p + geom_line(color = "red")
p = p + labs(title = "Average daily profile of number of steps")
p = p + labs(x = "1 day (segmented in 5 minute invervals)")
p = p + labs(y = "Average number of steps")
p

x = to_plot[to_plot$avg_steps == max(to_plot$avg_steps), 1]*60
Sys.setlocale(category = "LC_TIME", locale = "US") 
format(as.POSIXct('0001-01-01 00:00:00') + x, "%I:%M:%S %p") 

activity %>%
  summarize(missing_values = sum(is.na(steps))) %>%
  select(missing_values)

activity %>%
  filter(is.na(steps) == TRUE) %>%
  inner_join(to_plot, by = c("interval")) %>%
  select(avg_steps)

activity_missing = activity %>%
  select(steps, date, interval) %>%
  filter(is.na(steps) == TRUE) %>%
  inner_join(to_plot, by = c("interval")) %>%
  mutate(steps_imputed = round(avg_steps,0)) %>%
  select(date, interval, steps_imputed)

activity_present = activity %>%
  select(steps, date, interval) %>%
  filter(is.na(steps) == FALSE) %>%
  mutate(steps_imputed = steps) %>%
  select(date, interval, steps_imputed)

activity_imputed = bind_rows(activity_missing, activity_present) %>%
  arrange(date, interval)

inner_join(activity, to_plot, by = c("interval"))

num_days = as.numeric(activity %>%
  distinct(date) %>%
  summarize(num_days = n()) %>%
  select(num_days))

daily_profile = data.frame()
for (i in 1:num_days){
  daily_profile = rbind(daily_profile,to_plot)
}

activity_imputed = activity
attach(activity_imputed)
steps[is.na(steps)] = daily_profile[is.na(steps),2]


activity$steps_imputed = activity$steps
for (i in 1:nrow(activity)){
  attach(activity)
  if(is.na(steps[i])){
    steps_imputed[i] = daily_profile[interval[i],2]
  }
}


activity$steps[is.na(steps)] = 0

activity_imputed = activity %>%
  mutate(missing_values = is.na(steps) * 1) %>%
  mutate(steps_imputed = missing_values * daily_profile[interval, 2]) %>% 
  #mutate(steps_num = (steps[is.na(steps)] = 0))
  mutate(steps_final = steps_imputed + steps_num)


to_plot = as.data.frame(x = activity_imputed %>%
          group_by(date) %>%
          summarize(sum_steps = sum(x = steps_imputed)) %>%
          select(date, sum_steps))


day_type = function(d){
  if(weekdays(x = as.Date(d)) %in% c("Saturday", "Sunday")) {
    as.factor("Weekend")
  } else{as.factor("Weekday")}
}

to_plot = activity_imputed %>%
  mutate(day_type = sapply(X = date, FUN = day_type)) %>%
  group_by(day_type, interval) %>%
  summarize(avg_steps = mean(steps_imputed)) %>%
  select(day_type, interval, avg_steps)

p = ggplot(data = to_plot, aes(x = interval, y = avg_steps))
p = p + geom_line(color = "purple")
p = p + facet_grid(day_type ~ .)
p = p + labs(title = "Average daily profile of number of steps")
p = p + labs(x = "1 day (segmented in 5 minute invervals)")
p = p + labs(y = "Average number of steps")
p