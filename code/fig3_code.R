load('./data/data_for_fig3.RData')
library(broom)
library(survival)
library(tidyverse)
library(cowplot)


text_height = 3.6
A <- coxph(data = xdata_females_with_social, Surv(statage, adult_survival_status) ~ cumulative_adversity + jDSI_paternal + dad_overlap_years) %>%
	tidy() %>%
	mutate(term = case_when(term == "cumulative_adversity" ~ "cumulative adversity",
													term == "jDSI_paternal" ~ "paternal dyadic bond strength",
													term == "dad_overlap_years" ~ "co-residency with father")) %>%
	mutate(term = forcats::fct_relevel(term, "cumulative adversity", after = Inf)) %>%
	mutate(low.95 = exp(estimate - 1.96*std.error),
				 high.95 = exp(estimate + 1.96*std.error),
				 low.99 = exp(estimate - 2.575*std.error),
				 high.99 = exp(estimate + 2.575*std.error)) %>%
	ggplot(aes(y = term, x = exp(estimate))) +
	geom_segment(aes(x = low.99, xend = high.99, yend = term),
							 size = 5, color = "dodgerblue", alpha = .7) +
	geom_segment(aes(x = low.95, xend = high.95, yend = term),
							 size = 5, color = "dodgerblue4") +
	geom_point(size = 2,, color = "black") +
	theme_cowplot() +
	# scale_x_continuous(trans='log10',
	# 									 breaks = c(.5, .7, 1, 1,3, 1.5, 2),
	# 									 limits = c(.45, 2)
	labs(x="Hazard ratio") +
	geom_vline(xintercept = 1) +
	annotate("text",x= .8,y=text_height,label="Enhanced survival", color = "gray")+
	annotate("text",x= 1.2,y=text_height,label="Reduces survival", color = "gray")+
	geom_curve(x = .63, y = text_height - 0.025, xend = .55, yend = text_height - 0.025, curvature = 0,
						 arrow = arrow(length = unit(0.08, "inch")), size = 1,
						 color = "gray") +
	geom_curve(x = 1.36, y = text_height - 0.025, xend = 1.44, yend = text_height - 0.025, curvature = 0,
						 arrow = arrow(length = unit(0.08, "inch")), size = 1,
						 color = "gray")  +
	#scale_y_discrete(expand = expand_scale(mult = c(0.5, .5))) +
	coord_cartesian(ylim=c(1.2,3),clip="off") +
	theme(aspect.ratio = .2) +
	labs(y="")

## survival plotea and paternal
B <- ggplot() +
	geom_line(data = surv_data_cumpat_long, aes(x = age,
																							y = predicted.value,
																							colour = type,
																							linetype = type),
						size = 1.2) +
	geom_segment(aes(x = 0, xend = medium_high_Pat_low_cum,
									 y  = 0.5, yend = 0.5),
							 color = 'black', linetype = 'dashed') +


	geom_segment(aes(x = medium_high_Pat_low_cum, xend = medium_high_Pat_low_cum,
									 y  = 0, yend = 0.5),
							 color = 'black', linetype = 'dashed') +
	geom_segment(aes(x = medium_low_Pat_low_cum, xend = medium_low_Pat_low_cum,
									 y  = 0, yend = 0.5),
							 color = 'black', linetype = 'dashed') +
	geom_segment(aes(x = medium_high_Pat_low_cum - 0.3, xend = medium_low_Pat_low_cum + 0.3,
									 y  = 0.07, yend = 0.07),
							 size = .2, arrow = arrow(length = unit(0.05, "inches"))) +
	geom_segment(aes(x = medium_low_Pat_low_cum + 0.3, xend = medium_high_Pat_low_cum - 0.3,
									 y  = 0.07, yend = 0.07),
							 size = .2, arrow = arrow(length = unit(0.05, "inches"))) +
	annotate("text",x = medium_low_Pat_low_cum + (medium_high_Pat_low_cum - medium_low_Pat_low_cum)/2, y = 0.03, size = 2,
					 label = paste0(round(medium_high_Pat_low_cum - medium_low_Pat_low_cum,2), " y")) +



	geom_segment(aes(x = 0, xend = medium_high_Pat_high_cum,
									 y  = 0.5, yend = 0.5),
							 color = 'black', linetype = 'dotdash') +
	geom_segment(aes(x = medium_low_Pat_high_cum, xend = medium_low_Pat_high_cum,
									 y  = 0, yend = 0.5),
							 color = 'black', linetype = 'dotted') +
	geom_segment(aes(x = medium_high_Pat_high_cum, xend = medium_high_Pat_high_cum,
									 y  = 0, yend = 0.5),
							 color = 'black', linetype = 'dotted') +
	geom_segment(aes(x = medium_low_Pat_high_cum + 0.3, xend = medium_high_Pat_high_cum -0.3,
									 y  = 0.06, yend = 0.06),
							 size = .2, arrow = arrow(length = unit(0.05, "inches"))) +
	geom_segment(aes(x = medium_high_Pat_high_cum - 0.3, xend = medium_low_Pat_high_cum + 0.3,
									 y  = 0.06, yend = 0.06),
							 size = .2, arrow = arrow(length = unit(0.05, "inches"))) +
	annotate("text",x = medium_low_Pat_high_cum + (medium_high_Pat_high_cum - medium_low_Pat_high_cum)/2
					 , y = 0.03, size = 2, size = 5,
					 label = paste0(round(medium_low_Pat_low_cum - medium_low_Pat_high_cum,2), " y")) +
	scale_x_continuous(expand = c(0, 0), limits = c(0, 31)) +
	scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
	cowplot::theme_cowplot(font_size = 8) +
	theme(legend.position = "none",
				legend.text=element_text(size=7),
				plot.margin = unit(c(0, 0, 0, 0), "cm")) +
	scale_color_manual(values = c("firebrick", "firebrick", "dodgerblue4", "dodgerblue4")) +
	scale_linetype_manual(values = c("solid", "dotted", "solid", "dotted")) +
	labs(x = "Age",
			 y = "Survival change",
			 linetype = "",
			 color = "")  +
	guides(colour = guide_legend(nrow = 2))


C <- ggplot() +
	geom_line(data = surv_data_cumres_long, aes(x = age,
																							y = predicted.value,
																							colour = type,
																							linetype = type),
						size = 1.2) +
	geom_segment(aes(x = medium_low_pres_low_cum, xend = medium_high_pres_low_cum,
									 y  = 0.5, yend = 0.5),
							 color = 'black', linetype = 'dashed') +
	geom_segment(aes(x = medium_high_pres_high_cum, xend = medium_high_pres_high_cum,
									 y  = 0, yend = 0.5),
							 color = 'black', linetype = 'dashed') +
	geom_segment(aes(x = medium_high_pres_low_cum-.1, xend = medium_high_pres_low_cum-.1,
									 y  = 0, yend = 0.5),
							 color = 'black', linetype = 'dashed') +
	geom_segment(aes(x = 0, xend = medium_low_pres_low_cum,
									 y  = 0.5, yend = 0.5),
							 color = 'black', linetype = 'dotdash') +
	geom_segment(aes(x = medium_low_pres_low_cum, xend = medium_low_pres_low_cum,
									 y  = 0, yend = 0.5),
							 color = 'black', linetype = 'dotted') +
	geom_segment(aes(x = medium_low_pres_high_cum, xend = medium_low_pres_high_cum,
									 y  = 0, yend = 0.5),
							 color = 'black', linetype = 'dotted') +
	geom_segment(aes(x = medium_low_pres_high_cum + 0.3, xend = medium_low_pres_low_cum -0.3,
									 y  = 0.06, yend = 0.06),
							 size = .2, arrow = arrow(length = unit(0.05, "inches"))) +
	geom_segment(aes(x = medium_low_pres_low_cum - 0.3, xend = medium_low_pres_high_cum + 0.3,
									 y  = 0.06, yend = 0.06),
							 size = .2, arrow = arrow(length = unit(0.05, "inches"))) +
	annotate("text",x = medium_low_pres_high_cum + (medium_low_pres_low_cum - medium_low_pres_high_cum)/2,
					 y = 0.03, size = 2,
					 label = paste0(round(medium_low_pres_low_cum - medium_low_pres_high_cum,2), " y")) +

	geom_segment(aes(x = medium_high_pres_high_cum + 0.3, xend = medium_high_pres_low_cum -0.3,
									 y  = 0.07, yend = 0.07),
							 size = .2, arrow = arrow(length = unit(0.05, "inches"))) +
	geom_segment(aes(x = medium_high_pres_low_cum - 0.3, xend = medium_high_pres_high_cum + 0.3,
									 y  = 0.07, yend = 0.07),
							 size = .2, arrow = arrow(length = unit(0.05, "inches"))) +
	annotate("text",x = medium_high_pres_high_cum + (medium_high_pres_low_cum -
																									 	medium_high_pres_high_cum)/2, y = 0.03, size = 2,
					 label = paste0(round(medium_high_pres_low_cum - medium_high_pres_high_cum,2), " y")) +
	scale_x_continuous(expand = c(0, 0), limits = c(0, 31)) +
	scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
	cowplot::theme_cowplot(font_size = 8) +
	#theme(legend.position = "bottom") +
	theme(#legend.position = c(.6, .92),
		legend.position = "none",
		legend.text=element_text(size=7),
		plot.margin = unit(c(0, 0, 0, 0), "cm")) +
	scale_color_manual(values = c("firebrick", "firebrick", "dodgerblue4", "dodgerblue4")) +
	scale_linetype_manual(values = c("solid", "dotted", "solid", "dotted")) +
	labs(x = "Age",
			 y = "Survival change",
			 linetype = "",
			 color = "")  +
	guides(colour = guide_legend(nrow = 2))

plot_grid(plot_grid(A, labels = c("A.")
										, label_size = 8
										, label_x = 0
										, label_y = 1
										, hjust = -.5),
					plot_grid(B, C, rel_heights = c(1, 2),
										labels = c("B.", "C.")
										, align = "v"
										, label_size = 8
										, label_x = 0
										, label_y = 1
										, hjust = -.5
					), nrow = 2)
