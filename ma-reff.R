

###########
############  

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(zoo)
library(patchwork)

# Read the OO data
data <- read.csv("C:/Users/USER/OneDrive - The Hong Kong Polytechnic University/00000_UMass/OO-1st paper/Main_2/simul/hourly_data_full.csv")

# Convert date and time columns to appropriate formats
data$date <- as.Date(data$date, format="%m/%d/%Y")
data$datetime <- as.POSIXct(paste(data$date, data$hour), format="%Y-%m-%d %H:%M:%S")

# Hourly Analysis: Average number of infections per hour
data$hour <- hour(data$datetime)
hourly_avg <- data %>%
  group_by(hour) %>%
  summarise(avg_infections = mean(total_infection))

hourly_avg_plot <- ggplot(hourly_avg, aes(x = hour, y = avg_infections)) +
  geom_line(color = "green", size = 1.5) +
  labs(title = "(a) Average infections per hour",
       x = "Hour of the day",
       y = "Average infections") +
  theme_minimal()

# Daily analysis: total infections per day
daily_total_infections <- data %>%
  group_by(date) %>%
  summarise(daily_total = sum(total_infection))

moving_avg_plot <- ggplot(daily_total_infections, aes(x = date, y = daily_total)) +
  geom_line(color = "brown", size = 1.5) +
  geom_line(aes(y = rollmean(daily_total, 7, fill = NA)), color = "purple", size = 1.5) +
  labs(title = "(b) Daily total infections with 7-day moving average",
       x = "Date",
       y = "Daily total infections") +
  theme_minimal()

# Plotting Reff
SI.gamma.shape <- 2.25 # (2+2.5/2=2.25)
SI.gamma.scale <- 3 # (2+4/2=3)
SI.mean <- SI.gamma.shape * SI.gamma.scale

# Define reporting gap and calculate probabilities
reporting.gap <- 1 # Assuming hourly gap for this example
cum.prob.lag.array <- pgamma(q = reporting.gap * c(0:3), shape = SI.gamma.shape, scale = SI.gamma.scale, lower.tail = TRUE)
prob.lag.array <- diff(cum.prob.lag.array)

# Extract total infections
case.array <- data$total_infection

# Initialize storage for Reff values
Reff.record <- NULL

# Calculate the effective reproduction number (Reff)
for (i in (1 + length(prob.lag.array)):length(case.array)) {
  temp.prev.case.array <- case.array[(i - length(prob.lag.array)):(i - 1)]
  temp.post.case <- case.array[i]
  
  temp.Reff.array <- NULL
  for (ii in 1:1000) {
    curr.prev.case.array <- rpois(n = length(temp.prev.case.array), lambda = temp.prev.case.array)
    curr.trans.force <- sum(curr.prev.case.array * prob.lag.array) + 1
    curr.post.case <- rpois(n = length(temp.post.case), lambda = temp.post.case)
    curr.Reff <- (curr.post.case + 1) / curr.trans.force
    temp.Reff.array <- c(temp.Reff.array, curr.Reff)
  }
  Reff.record <- rbind(Reff.record, c(temp.Reff.array))
}

Reff.record <- as.data.frame(Reff.record)
Reff.ci.low.array <- apply(Reff.record, 1, function(x) { quantile(x, probs = 0.025) })
Reff.ci.up.array <- apply(Reff.record, 1, function(x) { quantile(x, probs = 0.975) })
Reff.med.array <- apply(Reff.record, 1, function(x) { quantile(x, probs = 0.50) })
Reff.mean.array <- rowMeans(Reff.record)

# Adjust time array
time.array <- data$datetime[(length(prob.lag.array) + 1):length(case.array)] - SI.mean / 24

# Effective Reproduction Number plot
Reff_plot <- ggplot() +
  geom_hline(yintercept = 1, col = 'red', linetype = 'dashed') +
  geom_point(aes(x = time.array, y = Reff.med.array), col = 'gray', size = 1) +
  geom_errorbar(aes(x = time.array, ymin = Reff.ci.low.array, ymax = Reff.ci.up.array), col = 'lightgray') +
  geom_line(aes(x = time.array, y = smooth(Reff.med.array, kind = '3R')), col = 'black', size = 1.5) +
  labs(title = "(c) Estimated effective reproduction number",
       x = "Date and time",
       y = "R_eff") +
  scale_y_continuous(breaks = seq(0, 20, by = 5), limits = c(0, 20)) +
  theme_minimal()

# Combine selected plots
combined_plot <- (hourly_avg_plot / moving_avg_plot / Reff_plot) + 
  plot_layout(ncol = 1)

# Print the combined plot
print(combined_plot)

# Save the combined plot
ggsave("combined_plots.png", plot = combined_plot, width = 10, height = 15)




