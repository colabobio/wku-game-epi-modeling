

###############################

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# Load the data
data <- read.csv("C:/Users/USER/OneDrive - The Hong Kong Polytechnic University/00000_UMass/OO-1st paper/Main_2/simul/hourly_data_full.csv")

# Convert 'date' and 'hour' to POSIXct format
data$datetime <- as.POSIXct(paste(data$date, data$hour), format="%m/%d/%Y %H:%M:%S")

# Generate cumulative cases column
data <- data %>%
  mutate(cumulative_case = cumsum(total_infection))

# Save the updated data as CSV
write.csv(data, "C:/Users/USER/OneDrive - The Hong Kong Polytechnic University/00000_UMass/OO-1st paper/Main_2/simul/hourly_data_with_cumulative.csv", row.names = FALSE)

# Plot ts for total infection and cumulative case
plot_total <- ggplot(data, aes(x=datetime, y=total_infection)) +
  geom_line(color="#1b9e77", size=1.2) +
  geom_point(color="#d95f02", size=2) +
  labs(x="Date and Time", y="# of cases (hourly)", title="") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_line(color = "gray90")
  ) +
  scale_x_datetime(labels = date_format("%b %d %H:%M"), breaks = date_breaks("2 days"))

plot_cumulative <- ggplot(data, aes(x=datetime, y=cumulative_case)) +
  geom_line(color="#7570b3", size=1.2) +
  geom_point(color="#e7298a", size=2) +
  labs(x="Date and time", y="cum. # of cases", title="") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_line(color = "gray90")
  ) +
  scale_x_datetime(labels = date_format("%b %d %H:%M"), breaks = date_breaks("2 days"))

# Print and save each plot
print(plot_total)
print(plot_cumulative)

# Save the plots in the same folder as the data
ggsave("C:/Users/USER/OneDrive - The Hong Kong Polytechnic University/00000_UMass/OO-1st paper/Main_2/simul/total_infection_plot.png", plot = plot_total, width = 12, height = 8, dpi = 300)
ggsave("C:/Users/USER/OneDrive - The Hong Kong Polytechnic University/00000_UMass/OO-1st paper/Main_2/simul/cumulative_case_plot.png", plot = plot_cumulative, width = 12, height = 8, dpi = 300)

######################################

