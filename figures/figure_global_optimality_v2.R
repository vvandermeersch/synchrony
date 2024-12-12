
#---------------#
# Main figure 1 #
#---------------#

library(patchwork)
wd <- "C:/Users/vandermeersch/Documents/CEFE/projects/synchrony"
# source(file.path(wd, "scripts", "preamble.R"))

# Load data
optimality <- readRDS(file.path(wd, "data/processed/era5land", paste0("optimality3_","1951_2020",".rds")))
global_optimum <- optimality %>% 
  group_by(doy) %>%
  summarise(opt = mean(opt), growth_pot = mean(growth_pot), env_pred = mean(env_pred)) %>%
  mutate(qt = quantile(opt, 0.90), opt_period = opt > qt)

local_optima <- optimality %>%
  group_by(id) %>%
  mutate(qt = quantile(opt, 0.90)) %>%
  dplyr::filter(opt > qt) %>%
  ungroup() %>%
  summarise(q025 = quantile(doy, .025), q975 = quantile(doy, .975), med = median(doy),
            q25 = quantile(doy, .25), q75 = quantile(doy, .75))

optimum_plot <- ggplot() +
  geom_vline(xintercept = 172, linetype = "dashed", 
             color = "grey70", linewidth = 0.3) +
  # geom_boxplot(aes(x = doy, y = max(global_optimum$opt) +0.03),
  #              width = 0.02, color = "#c1121f",
  #              linewidth = 0.3, outliers = FALSE,
  #              data = local_optima_q975) +
  geom_rect(aes(xmin = q025, xmax = q975, ymin = max(global_optimum$opt) +0.02, ymax = max(global_optimum$opt) +0.035), data = local_optima, 
            alpha = 0.5, fill = "#c1121f") +
  geom_rect(aes(xmin = q25, xmax = q75, ymin = max(global_optimum$opt) +0.02, ymax = max(global_optimum$opt) +0.035), data = local_optima, 
            alpha = 0.5, fill = "#c1121f") +
  geom_rect(aes(xmin = med-1.7, xmax = med+1.7, ymin = max(global_optimum$opt) +0.02, ymax = max(global_optimum$opt) +0.035), data = local_optima,
            fill = "white") +
  geom_rect(aes(xmin = med-0.6, xmax = med+0.6, ymin = max(global_optimum$opt) +0.02, ymax = max(global_optimum$opt) +0.035), data = local_optima,
            fill = "#c1121f") +



  # geom_point(aes(x = mean(local_optima$doy), y = max(global_optimum$opt) +0.03), color = "#c1121f") +
  # geom_line(aes(y = opt, x = doy), 
  #           data = global_optimum,
  #           color = "white", linewidth = 1.5) +
  scale_y_continuous(position = "right") +
  geom_line(aes(y = opt, x = doy, color = opt_period, group = 1), 
            data = global_optimum,
            linewidth = 0.6, lineend = "round") +
  scale_color_manual(values = c("#457b9d", "#c1121f")) +
  theme_bw() +
  theme(legend.position = 'none', panel.grid = element_blank(), strip.background = element_blank(),
        axis.text = element_text(size = 7.5), axis.title = element_text(size = 8),
        plot.margin = margin(t = 0, b = 0, l = 6.5, r = 0)) +
  labs(y = "Optimality", x= "DOY") +
  coord_cartesian(xlim = c(0,365), 
                  ylim = c(min(global_optimum$opt), max(global_optimum$opt) + 0.06), 
                  expand = FALSE)

# Gather & save!
cowplot::ggsave2(filename = file.path(wd, "figures", "global_optimality.pdf"),
                 plot = pareto_front + optimum_plot, device = cairo_pdf, width = 100, height = 60, unit = "mm")
