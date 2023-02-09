# Create a data frame with time values from 0 to 5
t <- seq(from = 0, to = 5, by = 0.1)

# Calculate percent mass remaining for each time value
Beech <- exp(-0.08 * t)
WhiteAsh <- exp(-0.47 * t)

# Combine the time and percent mass values into a data frame
df <- data.frame(t = t, X1 = X1, X2 = X2)


#plotting the graph, where the annotate function adds the equation label and the scale_color_manual adds the legend

ggplot(df, aes(x = t, y = Beech, color = "Beech")) +
  geom_line() +
  geom_line(aes(y = WhiteAsh, color = "WhiteAsh")) +
  xlab("Time (t)") +
  ylab("Percent Mass Remaining (X)") +
  ggtitle("Exponential Decay of Mass over Time") +
  #annotate function adds the equation label
  annotate(geom = "text", x = 2, y = 1, label = expression(paste("X = e^{-0.08t}")), size = 4) +
  annotate(geom = "text", x = 3, y = 0.21, label = expression(paste("X= e^{-0.47t}")), size = 4) +
  scale_y_log10() +
  scale_x_continuous(limits = c(0, 5), expand = c(0, 0)) +
  scale_color_manual(name = "Species", values = c("Beech" = "green", "WhiteAsh" = "gray")) +
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5)) +
  theme_classic()