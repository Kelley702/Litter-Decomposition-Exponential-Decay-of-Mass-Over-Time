library(ggplot2)

library(ggplot2)

# Create a data frame with time values from 0 to 5
t <- seq(from = 0, to = 5, by = 0.1)

# Setting exponential decay equations. K is multiplied by t
RMCON <- 2.71^(-0.3865 * t)
RMHeat <- 2.71^(-0.4084 * t)
RSControl <- 2.71^(-0.3619 * t)
RSHeat <- 2.71^(-0.4224 * t)

# Create a data frame with the values
df <- data.frame(t = rep(t, times = 4),
                 Equation = rep(c("RMCON", "RMHeat", "RSControl", "RSHeat"), each = length(t)),
                 Value = c(RMCON, RMHeat, RSControl, RSHeat))

# Plot the graph using ggplot2
ggplot(df, aes(x = t, y = Value, color = Equation)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("#0072B2", "#D55E00", "#E69F00", "#009E73")) +
  labs( x = "Time (years)", y = "Percent Mass Remaining") +
  theme_gray()

