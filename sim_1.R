

###########  simul

# Clear the environment
rm(list = ls())

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(stringr)
library(cowplot)
library(pomp)

# Read the new CSV file with daily data
data <- read.csv("C:/Users/USER/OneDrive - The Hong Kong Polytechnic University/00000_UMass/OO-1st paper/Main_2/simul/daily_data_full_N1_ssm.csv")
#data <- read.csv("C:/Users/USER/OneDrive - The Hong Kong Polytechnic University/00000_UMass/OO-1st paper/Main_2/simul/hourly_data_with_cumulative.csv")

# Convert 'date' column to Date format
data$date <- as.Date(data$date, format = "%m/%d/%Y")

# Get the initial number of confirmed cases
initial_confirm <- data$confirm[1]

# Define the simulation function
simulate <- function(epsilon1 = 0.25, zeta = 0.25, Ro = 2.878329, initial_confirm = 14) {
  N <- 794
  Sn <- 472
  En <- 155 #50  # muna maida wannan 105 zeyi zig-zag
  An <- 80  #16
  In <- initial_confirm  # Start with the initial number of confirmed cases
  R <- 2  #N * 0.1
  C <- 0
  C0 <- 0
  P <- 0
  
  # Parameters
  #beta <- 0.8366158      #1.287825
  alpha <- 0.489124     #0.5963776
  omega <- 0.2411152    #0.5586008
  #
  sigma <- 0.126;    # init 0.026
  gamma <- 0.129;    # init 0.129
  tau <- 0.17;      # init 0.017
  delta <- 1-0.17;
  theta <- 0.18;     # init 0.08
  m <- 1/1.5;
  epsilon <- 0.1
  
  
  R0 <- Ro             #R0=2.878329 with current para #R0=11.43159 
  dt <- 1.8 # 1.7
  
  result <- numeric(40)
  
  for (i in 1:40) {
    beta <- ifelse(i > 20 & i < 30, 0.8366158, 0)
    
    n1 <- ceiling((( An + alpha * In) / N) * (R0 * (theta+ gamma)*(tau+delta) ) / ( (alpha*gamma)+(tau+delta) ) * Sn * dt * (1 - epsilon) * (1 - P / N)^zeta)
    n3 <- ceiling(omega * R * dt)
    n6 <- ceiling(sigma * En * dt)
    n11 <- ceiling((theta + gamma) * An * dt)
    n12 <- ceiling(gamma * An * dt)
    n15 <- ceiling((tau+delta) * In * dt)
    n16 <- ceiling(delta * In * dt)
    n17 <- ceiling(theta * An * dt)
    n18 <- ceiling(tau * In * dt)
    n19 <- ceiling(m * P * dt)
    
    Sn <- Sn - n1 + n3
    En <- En + n1 - n6
    An <- An + n6 - n11
    In <- In + n12 - n15
    R <- R + n17 + n18 - n3
    
    C <- C + n3+n12
    P <- P + n16 - n19
    C0 <- C0 + n1
    
    
    # # Round up a single number to one above it: ceiling(4.2) = 5
    Sn <- Sn - ceiling(Sn * beta)
    En <- En - ceiling(En * beta)
    An <- An - ceiling(An * beta)
    In <- In - ceiling(In * beta)
    R <- R - ceiling(R * beta)
    N <- N - ceiling(N * beta)
    if (i > 20) epsilon <- epsilon1 / 15
    if (i > 30) epsilon <- epsilon1
    result[i] <- C
  }
  result
}

# Define incidence values based on simulations
incid <- data.frame(
  value = c(
    diff(simulate(initial_confirm = initial_confirm)),
    diff(simulate(0, 100, initial_confirm = initial_confirm)),
    diff(simulate(0, 1000, initial_confirm = initial_confirm))
  ),
  type = rep(
    c("naive", "IP", "IP+NPI"),
    each = 39
  ),
  date = rep(seq(as.Date("2023-11-20"), length.out = 39, by = "day"), 3)
)

# Combine data with simulated data
incid <- rbind(
  data.frame(value = data$confirm, type = "reported", date = data$date),
  incid
)

# Adjust factor levels
incid$type <- factor(
  incid$type, 
  levels = c("reported", "naive", "IP", "IP+NPI")
)

# Calculate breaks and labels for x-axis
start_date <- as.Date("2023-11-20")
end_date <- as.Date("2024-01-01")
breaks <- seq(start_date, end_date, by = "week")
labels <- format(breaks, "%b")  # Abbreviated month names
#################################


#####################   #scale_y_continuous #scale_y_log10

# Plotting p.incid
p.incid <- incid %>%
  ggplot(aes(x = date, y = value, colour = type)) +
  geom_line(aes(linetype = type)) +
  geom_point(aes(shape = type)) +
  #scale_y_log10(breaks = 10^(1:3), labels = 10^(1:3)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10), labels = scales::comma) +  # Uniform scale
  scale_x_date(breaks = breaks, labels = labels) +  # Automatic breaks and labels
  scale_color_manual(
    labels = stringr::str_wrap(
      c("reported", "naive", "IP", "IP + NPI"), 5
    ),
    values = c("lightgray", "#E69F00", "red", "#009E73")  # #999999 #56B4E9
  ) +
  scale_linetype_manual(
    labels = stringr::str_wrap(
      c("reported", "naive", "IP", "IP + NPI"), 5
    ),
    values = c("dotted", "solid", "solid", "solid")  #solid
  ) +
  scale_shape_manual(
    labels = stringr::str_wrap(
      c("reported", "naive", "IP", "IP + NPI"), 5
    ),
    values = c(16, 10, 10, 16)
  ) +
  labs(
    x = "time (daily)",
    y = "reported incidences",  ## of reported cases (daily)
    colour = "",
    linetype = "",
    shape = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.8, 0.8),  # Adjust this to position the legend inside the plot
    # legend.background = element_rect(fill = "white", color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    text = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) +
  guides(shape = guide_legend(override.aes = list(shape = c(16, 10, 10, 16))))

# Print the plot before saving
print(p.incid)

# Save the plot
ggsave(
  filename = "C:/Users/USER/OneDrive - The Hong Kong Polytechnic University/00000_UMass/OO-1st paper/Main_2/simul/p_incid_ss3_sm.png",
  plot = p.incid,
  width = 12, height = 6
)
##################################


##################################
# Plotting p.incid
p.incid <- incid %>%
  ggplot(aes(x = date, y = value, colour = type)) +
  geom_line(aes(linetype = type), size = 1.0) +  # Thicker lines for better visibility
  geom_point(aes(shape = type), size = 4) +  # Larger points for better visibility
  #scale_y_log10(breaks = 10^(1:3), labels = 10^(1:3)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10), labels = scales::comma) +  # Uniform scale
  scale_x_date(breaks = breaks, labels = labels) +  # Automatic breaks and labels
  scale_color_manual(
    labels = c("Reported", "Naive", "IP", "IP + NPI"),  # Simplified labels
    values = c("black", "#E69F00", "red", "#009E73")  # Bold colors
  ) +
  scale_linetype_manual(
    labels = c("Reported", "Naive", "IP", "IP + NPI"),  # Simplified labels
    values = c("dotted", "solid", "solid", "solid")  # c("dotted", "solid", "dashed", "dotted")
  ) +
  scale_shape_manual(
    labels = c("Reported", "Naive", "IP", "IP + NPI"),  # Simplified labels
    values = c(20, NA, NA, NA)  # #c(16, 17, 18, 19) #16, 10, 10, 16
  ) +
  labs(
    x = "time (daily)",
    y = "reported incidences",  ## of reported cases (daily)
    colour = "",
    linetype = "",
    shape = ""
  ) +
  theme_minimal(base_size = 15) +  # Increase base font size for better readability
  theme(
    legend.position = c(0.8, 0.8),  # Adjust this to position the legend inside the plot
    # legend.background = element_rect(fill = "white", color = "black", size = 0.5, linetype = "solid"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    text = element_text(size = 14),  # Increase text size for better readability
    legend.text = element_text(size = 12),  # Increase legend text size for better readability
    panel.grid.major = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor = element_line(color = "grey90", linetype = "dotted"),
    plot.background = element_rect(fill = "white", color = NA), #aliceblue
    panel.background = element_rect(fill = "lightgray", color = NA)
  ) +
  guides(shape = guide_legend(override.aes = list(size = 4)))  # Increase legend shape size

# Print the plot before saving
print(p.incid)

# Save the plot
ggsave(
  filename = "C:/Users/USER/OneDrive - The Hong Kong Polytechnic University/00000_UMass/OO-1st paper/Main_2/simul/p_incid_ss3_sm.png",
  plot = p.incid,
  width = 12, height = 6
)
##################################


# Reporting rate with IP+NPI only
##################################
# Plotting p.ratio
p.ratio <- incid %>%
  filter(type == "IP+NPI") %>%
  filter(date %in% data$date) %>%
  mutate(ratio = data$confirm / value) %>%
  ggplot(aes(x = date, y = ratio)) +
  geom_point(size = 3, shape = 21, fill = "blue", color = "darkblue", stroke = 1) +
  geom_line(linetype = "dashed", color = "blue", size = 1) +
  coord_cartesian(ylim = c(0, 10)) +
  labs(y = "reporting ratio", x = "time (daily)") +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(colour = "gray", linetype = "dotted", size = 0.5),
    panel.grid.minor = element_line(colour = "gray", linetype = "dotted", size = 0.25),
    legend.position = "none",
    axis.text = element_text(color = "darkblue", size = 12),
    axis.title = element_text(color = "darkblue", size = 14, face = "bold"),
    plot.title = element_text(color = "darkblue", size = 16, face = "bold", hjust = 0.5),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "lightgray", color = "darkblue", size = 1)
  ) +
  ggtitle("")  #Reporting Ratio Over Time

# Print the ratio plot
print(p.ratio)

# Save the ratio plot as a PNG file
output_file_ratio <- "C:/Users/USER/OneDrive - The Hong Kong Polytechnic University/00000_UMass/OO-1st paper/simul_02/ratio_plot_sm.png"
ggsave(output_file_ratio, p.ratio, width = 16, height = 8, units = "cm", dpi = 300)

print(paste("Ratio plot saved to", output_file_ratio))

##################################


# Reporting rate with IP only
##################################
# Plotting p.ratio
p.ratio <- incid %>%
  filter(type == "IP") %>%
  filter(date %in% data$date) %>%
  mutate(ratio = data$confirm / value) %>%
  ggplot(aes(x = date, y = ratio)) +
  geom_point(size = 3, shape = 21, fill = "blue", color = "darkblue", stroke = 1) +
  geom_line(linetype = "dashed", color = "blue", size = 1) +
  coord_cartesian(ylim = c(0, 10)) +
  labs(y = "reporting ratio", x = "time (daily)") +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(colour = "gray", linetype = "dotted", size = 0.5),
    panel.grid.minor = element_line(colour = "gray", linetype = "dotted", size = 0.25),
    legend.position = "none",
    axis.text = element_text(color = "darkblue", size = 12),
    axis.title = element_text(color = "darkblue", size = 14, face = "bold"),
    plot.title = element_text(color = "darkblue", size = 16, face = "bold", hjust = 0.5),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "lightgray", color = "darkblue", size = 1)
  ) +
  ggtitle("")  #Reporting Ratio Over Time

# Print the ratio plot
print(p.ratio)

# Save the ratio plot as a PNG file
output_file_ratio <- "C:/Users/USER/OneDrive - The Hong Kong Polytechnic University/00000_UMass/OO-1st paper/simul_02/ratio_plot_sm.png"
ggsave(output_file_ratio, p.ratio, width = 16, height = 8, units = "cm", dpi = 300)

print(paste("Ratio plot saved to", output_file_ratio))

##################################




Figure 5:
  
  For Dec 6:
  # Define specific breaks for the x-axis
  start_date <- as.Date("2023-11-20")
end_date <- as.Date("2023-12-06")
breaks <- as.Date(c("2023-11-20", "2023-11-27", "2023-12-04", "2023-12-06"))  # Specify dates for labels

For Dec 4:
  # Calculate breaks and labels for x-axis
  start_date <- as.Date("2023-11-20")
end_date <- as.Date("2023-12-04")


# Plotting p.incid with unstacked area graph
p.incid <- incid %>%
  ggplot(aes(x = date, y = value, fill = type)) +  # Changed from colour to fill
  geom_area(aes(linetype = type), position = "identity", alpha = 0.6) +  # Add transparency with alpha
  scale_y_continuous(breaks = seq(0, 100, by = 10), labels = scales::comma) +  # Uniform scale
  # scale_x_date(breaks = breaks, labels = scales::date_format("%b %d")) +  # Automatic breaks and labels
  scale_x_date(
    breaks = breaks,
    labels = scales::date_format("%b %d"),
    limits = c(start_date, end_date)  # Set limits to start_date and end_date
  ) +
  scale_fill_manual(
    labels = stringr::str_wrap(
      c("reported", "naive", "IP", "IP + NPI"), 5
    ),
    values = c("black", "#E69F00", "red", "#009E73")  # Custom fill colours for areas
  ) +
  scale_linetype_manual(
    labels = stringr::str_wrap(
      c("reported", "naive", "IP", "IP + NPI"), 5
    ),
    values = c("dotted", "solid", "solid", "solid")  # Custom line types
  ) +
  labs(
    x = "time (daily)",
    y = "reported incidences",  ## of reported cases (daily)
    fill = "",  # Change from colour to fill
    linetype = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.8, 0.8),  # Adjust this to position the legend inside the plot
    axis.text.x = element_text(angle = 45, hjust = 1),
    text = element_text(size = 12),
    legend.text = element_text(size = 10),
    panel.border = element_rect(color = "black", fill = NA, size = 1)  # Add a black border around the plot
  )


# Print the plot before saving
print(p.incid)


