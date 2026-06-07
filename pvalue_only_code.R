library(ggplot2)

# Choose the table you imported in RStudio.
# The error "object of type 'closure' is not subsettable" happens when data is
# still R's built-in data() function instead of your imported table.
if (exists("my_data") && is.data.frame(my_data)) {
  plot_data <- my_data
} else if (exists("data") && is.data.frame(data)) {
  plot_data <- data
} else {
  stop("Please import your table first, and name it my_data or data.")
}

to_number <- function(x) {
  as.numeric(gsub("[^0-9eE+.-]", "", as.character(x)))
}

required_cols <- c("pathway", "pvalue")
missing_cols <- setdiff(required_cols, names(plot_data))
if (length(missing_cols) > 0) {
  stop(paste("Missing required column(s):", paste(missing_cols, collapse = ", ")))
}

# Convert p value to log10(p value), so smaller p values become more negative.
plot_data$pvalue <- to_number(plot_data$pvalue)
plot_data$logP <- log10(plot_data$pvalue)

# Put the most significant terms at the top of the y-axis.
plot_data <- plot_data[order(plot_data$logP), ]
plot_data$pathway <- factor(plot_data$pathway, levels = plot_data$pathway)
x_min <- min(plot_data$logP, na.rm = TRUE)
label_space <- abs(x_min) * 0.65

ggplot(plot_data, aes(x = logP, y = pathway, fill = logP)) +
  geom_col(width = 0.75) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.6) +
  geom_text(
    aes(x = 1, label = pathway),
    hjust = 0,
    size = 3.5,
    color = "black",
    family = "Arial"
  ) +
  scale_fill_gradient(low = "#4A90E2", high = "#E94E77") +
  scale_x_continuous(
    limits = c(x_min * 1.05, label_space),
    breaks = scales::breaks_pretty(n = 5)(c(x_min, 0)),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  geom_vline(
    xintercept = log10(0.05),
    linetype = "dashed",
    color = "gray50",
    linewidth = 0.8
  ) +
  theme_classic() +
  labs(
    x = "Log10(P-Value)",
    y = "",
    fill = "Significance\n(log10 P-Value)"
  ) +
  guides(
    fill = guide_colorbar(
      title.position = "top",
      barwidth = grid::unit(0.75, "cm"),
      barheight = grid::unit(6, "cm")
    )
  ) +
  theme(
    text = element_text(family = "Arial"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    axis.text.x = element_text(size = 10, color = "black"),
    axis.title.x = element_text(size = 12, face = "bold"),
    legend.position = "left",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.margin = margin(t = 0, r = 8, b = 0, l = 0),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  )
