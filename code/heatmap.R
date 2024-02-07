heatmap_data <- read_csv("./data/heatmap_data.csv")

heatmap_data %>%
ggplot(aes(x = week, y = transect, fill = density)) +
	geom_tile() +
	facet_grid(treatment_type ~ .) +
	scale_fill_gradient(low = "white", high = "black") +
	guides(fill = guide_colorbar(title = "Density of nymphs per 1000m2"))  +
	labs(y = "") +
	theme(legend.position = "bottom",
				legend.key.size = unit(1, 'cm')) +
	theme(panel.background = element_blank(),
				plot.background = element_blank(),
				axis.text = element_text(size = 19))+
	geom_vline(xintercept = 1.5, linetype = "dashed", size = 1.5) +
	geom_vline(xintercept = 5.5, linetype = "dashed", size = 1.5)+
	theme(axis.title = element_text(size = 16),
				legend.text = element_text(size = 16),
				legend.title = element_text(size = 19),
				title = element_text(size = 15),
				strip.text = element_text(size = 10))
