library(tidyverse)
library(readxl)
library(ggtext)
library(showtext)

font_add_google(family="patua-one", "Patua One")
font_add_google(family="roboto", "Roboto")


rm(list =ls())

addline_format <- function(x,...){
	gsub('\\s','\n',x)
}

tick_report_data <- read_csv("data/tick_report_data.csv")

plot_theme <- list(
	cowplot::theme_cowplot(),
	scale_alpha_manual(values = c(0.4, 1)),
	scale_x_discrete(labels=addline_format(c(unique(tick_report_data$tickidnet_prediction_class), "Other\ unknown"))),
	theme(axis.text.x = element_text(angle = 45, vjust = .4, hjust=.5)),
	labs(x = "Expert assignment",
			 y = "Number of submissions",
			 fill = "Model prediction",
			 alpha = NA),
	guides(alpha = 'none'),
	scale_fill_manual(values = c("darkolivegreen1", "deepskyblue", "darkgoldenrod1"))
)


make_accuracy_plot <- function(tick_report_data, probability = 0) {
	plot_data <- tick_report_data %>%
		filter(tickidnet_prediction_probability > probability) %>%
		mutate(review_tick_species = if_else(review_tick_species == "other",
																				 true = "unknown",
																				 false = review_tick_species)) %>%
		filter(!is.na(review_tick_species)) %>%
		select(contains("species"), tickidnet_prediction_class) %>%
		mutate(across(where(is.character), tolower)) %>%
		mutate(across(where(is.character), str_to_sentence)) %>%
		group_by(review_tick_species, tickidnet_prediction_class) %>%
		summarise(cases = n(), .groups = "drop") %>%
		group_by(review_tick_species) %>%
		mutate(total = sum(cases)) %>%
		mutate(percentage = cases/total) %>%
		#ungroup() %>%
		mutate(correct = review_tick_species == tickidnet_prediction_class) %>%
		arrange(correct, percentage) %>%
		mutate(order = row_number())

	plot_data <<- plot_data
	min_probability <<-  probability

	plot_setup <-   list(
		geom_col(colour="black"),
		scale_alpha_manual(values = c(0.4, 1)),
		scale_x_discrete(labels=addline_format(c(unique(tick_report_data$tickidnet_prediction_class), "Other\ unknown"))),
		cowplot::theme_cowplot(),
		theme(axis.text.x = element_text(angle = 45, vjust = .4, hjust=.5)),
		labs(x = "Expert assignment",
				 y = "Number of submissions",
				 fill = "Model prediction",
				 alpha = NA),
		guides(alpha = 'none'),
		scale_fill_manual(values = c("darkolivegreen1", "deepskyblue", "darkgoldenrod1"))
	)

	p1 <- plot_data %>%
		ggplot(aes(x=review_tick_species, y = percentage,
							 fill =  tickidnet_prediction_class,
							 #alpha = correct,
							 group = order)) +
		geom_col(color = "black") +
		scale_y_continuous(labels = scales::percent,expand = c(0, 0)) +
		plot_theme

	p2 <- plot_data %>%
		ggplot(aes(x=review_tick_species, y = cases,
							 fill =  tickidnet_prediction_class,
							 #alpha = correct,
							 group = order)) +
		geom_col(color = "black") +
		scale_y_continuous() +
		plot_theme

	legend <- cowplot::get_legend(
		## create some space to the left of the legend
		p1 +
			theme(legend.position = "bottom") +
			guides(fill=guide_legend(nrow=1))
	)

	prow <- cowplot::plot_grid(
		p1 + theme(legend.position="none"),
		p2 + theme(legend.position="none"),
		align = 'vh',
		hjust = -1,
		nrow = 1
	)

	legend = cowplot::plot_grid(NULL, legend, NULL, nrow = 1)
	cowplot::plot_grid(prow,  legend, ncol = 1, rel_heights = c(1, .1))
}

make_accuracy_plot(tick_report_data, probability = 1)

set_tile_plot <- list(
	geom_tile(color = "black"),
	geom_text(aes(label=text_label)),
	scale_fill_gradient(low="white", high="#009194"),
	cowplot::theme_cowplot(),
	theme(legend.position = "none"),
	scale_x_discrete(expand = c(0, 0)),
	scale_y_discrete(expand = c(0, 0))


)

p1 <- plot_data %>%
	mutate(text_label = paste0(round(percentage,2)*100, "%")) %>%
	ggplot(aes(review_tick_species,tickidnet_prediction_class, fill= percentage)) +
	set_tile_plot +
	labs(x = "Expert", y = "Tick Id net",
			 title = "Percent of TickidNet assignment per species")

p2 <- plot_data %>%
	group_by(tickidnet_prediction_class) %>%
	mutate(total = sum(cases)) %>%
	mutate(percentage = cases/total) %>%
	mutate(text_label = paste0(round(percentage,2)*100, "%")) %>%
	ggplot(aes(tickidnet_prediction_class, review_tick_species, fill= percentage)) +
	set_tile_plot +
	labs(x = "Tick Id net", y = "Expert",
			 title = "Percent of expert assignment per species",
			 caption = paste("The minimum probrability was", min_probability))


cowplot::plot_grid(p1, p2)

