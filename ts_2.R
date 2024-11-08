
##################################

# Stacked Area Charts
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(patchwork)

# Load the data
data <- read.csv("C:/Users/USER/OneDrive - The Hong Kong Polytechnic University/00000_UMass/OO-1st paper/Main_2/simul/hourly_data_with_cumulative.csv")

# Convert 'date' and 'hour' to POSIXct format
data$datetime <- as.POSIXct(paste(data$date, data$hour), format="%m/%d/%Y %H:%M:%S")

# Plot time series for total infection
plot_total <- ggplot(data, aes(x=datetime, y=total_infection)) +
  geom_line(color="#1b9e77", size=1.2) +
  geom_point(color="#d95f02", size=2) +
  labs(y="# of cases (hourly)") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0, face = "bold"),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold"),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_line(color = "gray90"),
    plot.margin = margin(t = 10, r = 10, b = 0, l = 10)
  ) +
  scale_x_datetime(labels = date_format("%b %d %H:%M"), breaks = date_breaks("2 days")) +
  labs(tag = "(a)")

# Plot stacked area chart for cumulative cases
plot_cumulative <- ggplot(data, aes(x=datetime, y=cumulative_case, fill=cumulative_case)) +
  geom_area(alpha=0.6) +
  labs(x="Date and time", y="cum. # of cases") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_line(color = "gray90"),
    plot.margin = margin(t = 0, r = 10, b = 10, l = 10),
    legend.position = "none"
  ) +
  scale_x_datetime(labels = date_format("%b %d %H:%M"), breaks = date_breaks("2 days")) +
  scale_fill_gradient(low="#7570b3", high="#e7298a") +
  labs(tag = "(b)")

# Combine the plots into one figure
combined_plot <- plot_total / plot_cumulative +
  plot_layout(ncol = 1) +
  plot_annotation(title = "", theme = theme(plot.title = element_text(hjust = 0.5)))

# Print and save the combined plot
print(combined_plot)

# Save the combined plot in the same folder as the data
ggsave("C:/Users/USER/OneDrive - The Hong Kong Polytechnic University/00000_UMass/OO-1st paper/Main_2/simul/infection_data_combined_plot.png", plot = combined_plot, width = 12, height = 12, dpi = 300)

###########################################


