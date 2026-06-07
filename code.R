library(ggplot2)

# Use my_data if that is the imported table name; otherwise fall back to data.
plot_data <- if (exists("my_data")) my_data else data

to_number <- function(x) {
  as.numeric(gsub("[^0-9eE+.-]", "", as.character(x)))
}

# Keep the y-axis in the same top-to-bottom order as the input table.
plot_data$enrichment <- to_number(plot_data$enrichment)
plot_data$pvalue <- to_number(plot_data$pvalue)
plot_data$count <- to_number(plot_data$count)
plot_data$pathway <- factor(plot_data$pathway, levels = rev(unique(plot_data$pathway)))

ggplot(plot_data, aes(x = enrichment, y = pathway)) +
  geom_point(aes(size = count, color = pvalue)) +
  scale_color_gradient(low = "red", high = "blue") +
  scale_x_continuous(
    breaks = scales::pretty_breaks(n = 5),
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  theme_bw() +
  labs(
    x = "Log2 Fold Enrichment",
    y = "",
    size = "Protein Number",
    color = "P value"
  ) +
  theme(
    axis.text.y = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, color = "black"),
    axis.title.x = element_text(size = 12, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks.x = element_line(color = "black")
  )
