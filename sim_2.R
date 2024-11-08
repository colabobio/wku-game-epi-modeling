

###################

# rm(list=ls())

# library(dplyr)

case <- read.csv("C:/Users/USER/OneDrive - The Hong Kong Polytechnic University/00000_UMass/OO-1st paper/Main_2/simul/daily_data_full_N1_ssm.csv")

# Create a new column 'incid' with the same values as 'confirm'
case <- mutate(case, incid = confirm)

# Select only the 'date' and 'incid' columns
case <- select(case, date, incid)

#### Figure 5:
library(reshape2)
library(ggplot2)

# Define the simulation function
simulate <- function(epsilon1 = 0.25, zeta = 0.25, Ro = 2.878329, initial_confirm = 14) {
  N <- 794
  Sn <- 472
  En <- 155
  An <- 80
  In <- initial_confirm  # Start with the initial number of confirmed cases
  R <- 2
  C <- 0
  C0 <- 0
  P <- 0
  
  # Parameters
  alpha <- 0.489124
  omega <- 0.2411152
  sigma <- 0.126
  gamma <- 0.129
  tau <- 0.17
  delta <- 1 - 0.17
  theta <- 0.18
  m <- 1 / 1.5
  epsilon <- 0.1
  
  R0 <- Ro
  dt <- 1.8
  
  result <- numeric(40)
  
  for (i in 1:40) {
    beta <- ifelse(i > 20 & i < 30, 0.8366158, 0)
    
    n1 <- ceiling(((An + alpha * In) / N) * (R0 * (theta + gamma) * (tau + delta)) / ((alpha * gamma) + (tau + delta)) * Sn * dt * (1 - epsilon) * (1 - P / N)^zeta)
    n3 <- ceiling(omega * R * dt)
    n6 <- ceiling(sigma * En * dt)
    n11 <- ceiling((theta + gamma) * An * dt)
    n12 <- ceiling(gamma * An * dt)
    n15 <- ceiling((tau + delta) * In * dt)
    n16 <- ceiling(delta * In * dt)
    n17 <- ceiling(theta * An * dt)
    n18 <- ceiling(tau * In * dt)
    n19 <- ceiling(m * P * dt)
    
    Sn <- Sn - n1 + n3
    En <- En + n1 - n6
    An <- An + n6 - n11
    In <- In + n12 - n15
    R <- R + n17 + n18 - n3
    
    C <- C + n3 + n12
    P <- P + n16 - n19
    C0 <- C0 + n1
    
    # Round up a single number to one above it: ceiling(4.2) = 5
    Sn <- Sn - ceiling(Sn * beta)
    En <- En - ceiling(En * beta)
    An <- An - ceiling(An * beta)
    In <- In - ceiling(In * beta)
    R <- R - ceiling(R * beta)
    N <- N - ceiling(N * beta)
    if (i > 10) epsilon <- epsilon1 / 1
    if (i > 40) epsilon <- epsilon1
    result[i] <- C
  }
  result
}

########################

#### SA  
epsilons <- sapply(seq(0.25, 2.25, by = 0.5), function(a) {
  simulate(epsilon1 = a, zeta = 10) %>% diff()
}) %>% 
  as.data.frame() %>%
  cbind(date = seq(as.Date("2023-11-20"), length.out = 39, by = "day")) %>%
  melt(id.var = "date")

# 
zetas <- sapply(seq(10, 500, by = 100), function(k) {
  simulate(epsilon1 = 0.25, zeta = k) %>% diff()
}) %>%
  as.data.frame() %>%
  cbind(date = seq(as.Date("2023-11-20"), length.out = 39, by = "day")) %>%
  melt(id.var = "date")

# Plot for epsilons
p.epsilons <- epsilons %>%
  ggplot(aes(x = date, y = value, color = variable, linetype = variable)) +
  geom_line(size = 1) +
  geom_line(data = case, aes(y = incid, x = as.Date(date, format = "%Y-%m-%d")), colour = "grey", inherit.aes = FALSE, linetype = "solid", size = 1) +
  geom_point(data = case, aes(y = incid, x = as.Date(date, format = "%Y-%m-%d")), colour = "grey", inherit.aes = FALSE, size = 2) +
  #scale_y_log10(breaks = 10^(1:3), labels = 10^(1:3)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  scale_linetype_manual(values = c(1, 2, 3, 4, 5)) +
  scale_color_manual(values = c("black", "red", "darkgreen", "blue", "purple")) +
  coord_cartesian(ylim = c(0, 100)) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(colour = "lightgrey", linetype = "solid", size = 0.5),
    panel.grid.minor = element_line(colour = "lightgrey", linetype = "solid", size = 0.2),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    legend.position = "none"
  ) +
  labs(y = "daily new infections", x = "date (daily)", title = "")

# Plot for zetas
p.zetas <- zetas %>%
  ggplot(aes(x = date, y = value, color = variable, linetype = variable)) +
  geom_line(size = 1) +
  geom_line(data = case, aes(y = incid, x = as.Date(date, format = "%Y-%m-%d")), colour = "grey", inherit.aes = FALSE, linetype = "solid", size = 1) +
  geom_point(data = case, aes(y = incid, x = as.Date(date, format = "%Y-%m-%d")), colour = "grey", inherit.aes = FALSE, size = 2) +
  #scale_y_log10(breaks = 10^(1:3), labels = 10^(1:3)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  scale_linetype_manual(values = c(1, 2, 3, 4, 5)) +
  scale_color_manual(values = c("black", "red", "darkgreen", "blue", "purple")) +
  coord_cartesian(ylim = c(0, 100)) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(colour = "lightgrey", linetype = "solid", size = 0.5),
    panel.grid.minor = element_line(colour = "lightgrey", linetype = "solid", size = 0.2),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    legend.position = "none"
  ) +
  labs(y = "daily new infections", x = "date (daily)", title = "")


# Print the plots separately
print(p.epsilons)
print(p.zetas)

# save
ggsave("C:/Users/USER/OneDrive - The Hong Kong Polytechnic University/00000_UMass/OO-1st paper/Main_2/simul/sensitivity_plot_epsilons_sm.png", plot = p.epsilons, width = 7, height = 7, dpi = 300)
ggsave("C:/Users/USER/OneDrive - The Hong Kong Polytechnic University/00000_UMass/OO-1st paper/Main_2/simul/sensitivity_plot_zetas_sm.png", plot = p.zetas, width = 7, height = 7, dpi = 300)

#####################

# Combine the plots
library(cowplot)
plot_grid(p.epsilons, p.zetas, ncol = 2, align = "h", 
          labels = c("(a)", "(b)"),
          label_x = 0.05)

############################




#For Dec 6:
# Define specific breaks for the x-axis
start_date <- as.Date("2023-11-20")
end_date <- as.Date("2023-12-06")
breaks <- as.Date(c("2023-11-20", "2023-11-27", "2023-12-04", "2023-12-06"))  # Specify dates for labels

#For Dec 4:
# Calculate breaks and labels for x-axis
start_date <- as.Date("2023-11-20")
end_date <- as.Date("2023-12-04")

# Plot for epsilons
p.epsilons <- epsilons %>%
  ggplot(aes(x = date, y = value, color = variable, linetype = variable)) +
  geom_line(size = 1) +
  geom_line(data = case, aes(y = incid, x = as.Date(date, format = "%Y-%m-%d")), colour = "grey", inherit.aes = FALSE, linetype = "solid", size = 1) +
  geom_point(data = case, aes(y = incid, x = as.Date(date, format = "%Y-%m-%d")), colour = "grey", inherit.aes = FALSE, size = 2) +
  #scale_y_log10(breaks = 10^(1:3), labels = 10^(1:3)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  # scale_x_date(limits = c(min(epsilons$date), end_date)) +  # Adjust the x-axis limits
  scale_x_date(
    breaks = breaks,
    labels = scales::date_format("%b %d"),
    limits = c(start_date, end_date)  # Set limits to start_date and end_date
  ) +
  scale_linetype_manual(values = c(1, 2, 3, 4, 5)) +
  scale_color_manual(values = c("black", "red", "darkgreen", "blue", "purple")) +
  coord_cartesian(ylim = c(0, 50)) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(colour = "lightgrey", linetype = "solid", size = 0.5),
    panel.grid.minor = element_line(colour = "lightgrey", linetype = "solid", size = 0.2),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.background = element_rect(fill = "white", color = NA), #aliceblue
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add a black border around the plotplot.title = element_text(hjust = 0.5, size = 16, face = "bold")
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  ) +
  labs(y = "daily new infections", x = "date (daily)", title = "(a) Impact of Epilson (ϵ)")

# Plot for zetas
p.zetas <- zetas %>%
  ggplot(aes(x = date, y = value, color = variable, linetype = variable)) +
  geom_line(size = 1) +
  geom_line(data = case, aes(y = incid, x = as.Date(date, format = "%Y-%m-%d")), colour = "grey", inherit.aes = FALSE, linetype = "solid", size = 1) +
  geom_point(data = case, aes(y = incid, x = as.Date(date, format = "%Y-%m-%d")), colour = "grey", inherit.aes = FALSE, size = 2) +
  #scale_y_log10(breaks = 10^(1:3), labels = 10^(1:3)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10))  +
  # scale_x_date(limits = c(min(epsilons$date), end_date)) +  # Adjust the x-axis limits
  scale_x_date(
    breaks = breaks,
    labels = scales::date_format("%b %d"),
    limits = c(start_date, end_date)  # Set limits to start_date and end_date
  ) +
  scale_linetype_manual(values = c(1, 2, 3, 4, 5)) +
  scale_color_manual(values = c("black", "red", "darkgreen", "blue", "purple")) +
  coord_cartesian(ylim = c(0, 50)) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(colour = "lightgrey", linetype = "solid", size = 0.5),
    panel.grid.minor = element_line(colour = "lightgrey", linetype = "solid", size = 0.2),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.background = element_rect(fill = "white", color = NA), #aliceblue
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add a black border around the plotplot.title = element_text(hjust = 0.5, size = 16, face = "bold")
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  ) +
  labs(y = "daily new infections", x = "date (daily)", title = "(b) Impact of Zetas (ζ)")


#
print(p.epsilons)
print(p.zetas)

# Library to arrange the two plots side by side
install.packages("gridExtra")
library(gridExtra)

# Arrange the two plots side by side
grid.arrange(p.epsilons, p.zetas, ncol = 2)

p.epsilons <- p.epsilons + theme(plot.margin = unit(c(1, 2, 1, 1), "cm"))  # Add space on the right
p.zetas <- p.zetas + theme(plot.margin = unit(c(1, 1, 1, 2), "cm"))  # Add space on the left

# Save 
ggsave(
  filename = #define file path,
    plot = grid.arrange(p.epsilons, p.zetas, ncol = 2),
  width = 14,  # Adjust width to fit both plots
  height = 7,  # Adjust height as needed
  dpi = 300    # High DPI for better quality
)



