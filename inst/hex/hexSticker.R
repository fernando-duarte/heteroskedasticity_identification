# Create a cool hex sticker for the hetid package
# Theme: Visualizing heteroskedasticity-based identification

# Load required packages
library(hexSticker)
library(ggplot2)
library(dplyr)
library(here)

# Set seed for reproducibility
set.seed(2012) # Year of Lewbel's paper

# Create heteroskedastic data for visualization
n <- 500
x <- seq(-2, 2, length.out = n)
# Create heteroskedastic errors with variance increasing with x
variance <- 0.1 + 0.5 * abs(x)^1.5
errors <- rnorm(n, 0, sqrt(variance))
y <- -0.8 * x + errors # True coefficient is -0.8 (gamma1 from the package)

# Create data frame
data <- data.frame(x = x, y = y, variance = variance)

# Sample points for scatter (not all points for cleaner look)
sample_indices <- sample(1:n, 150)
data_sample <- data[sample_indices, ]

# Create the main plot
p <- ggplot() +
  # Add confidence bands showing heteroskedasticity
  geom_ribbon(
    data = data,
    aes(
      x = x,
      ymin = -0.8 * x - 2 * sqrt(variance),
      ymax = -0.8 * x + 2 * sqrt(variance)
    ),
    fill = "#2E86AB", alpha = 0.2
  ) +
  # Add scatter points
  geom_point(
    data = data_sample,
    aes(x = x, y = y),
    color = "#A23B72",
    size = 0.5,
    alpha = 0.6
  ) +
  # Add true regression line (Lewbel/2SLS estimate)
  geom_line(
    data = data.frame(x = c(-2, 2), y = -0.8 * c(-2, 2)),
    aes(x = x, y = y),
    color = "#F18F01",
    size = 1.5,
    alpha = 0.9
  ) +
  # Add biased OLS line (very dramatically slanted)
  # The bias makes the slope much less negative (e.g., -0.2 instead of -0.8)
  geom_line(
    data = data.frame(x = c(-2, 2), y = -0.2 * c(-2, 2)),
    aes(x = x, y = y),
    color = "#6C757D",
    size = 0.7,
    alpha = 0.8,
    linetype = "dashed"
  ) +
  # Minimal theme
  theme_void() +
  theme(plot.background = element_rect(fill = "transparent", color = NA))

# Create hex sticker (clean version without URL)
sticker(
  subplot = p,
  s_x = 1,
  s_y = 0.9,
  s_width = 1.4,
  s_height = 1.2,
  package = "hetid",
  p_size = 20,
  p_x = 1,
  p_y = 1.5,
  p_color = "#2E86AB",
  p_family = "sans",
  p_fontface = "bold",
  h_fill = "#F5F5F5",
  h_color = "#2E86AB",
  h_size = 1.5,
  spotlight = TRUE,
  l_x = 0.5,
  l_y = 0.7,
  l_width = 3,
  l_height = 3,
  l_alpha = 0.1,
  filename = here::here("inst/hex/hex.png"),
  dpi = 300,
  asp = 1
)

cat("Hex sticker created successfully!\n")
cat("File saved: inst/hex/hex.png\n")
