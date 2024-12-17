
#---------------#
# Main figure 1 #
#---------------#

library(patchwork)
wd <- "C:/Users/vandermeersch/Documents/CEFE/projects/synchrony"
# source(file.path(wd, "scripts", "preamble.R"))

# Load data
optimality <- readRDS(file.path(wd, "data/processed/era5land",  paste0("optimality_", 1951 ,"_", 2020, "_", "tlow", 5, "_tupp", 35, ".rds")))
global_optimum <- optimality %>% 
  group_by(doy) %>%
  summarise(opt = mean(opt), growth_pot = mean(growth_pot), env_pred = mean(env_pred)) %>%
  mutate(qt = quantile(opt, 0.90), opt_period = opt > qt)
local_optima <- optimality %>%
  group_by(id) %>%
  mutate(qt = quantile(opt, 0.90)) %>%
  dplyr::filter(opt > qt)

# Left panel: pareto front
pareto_dist <- function(x, y) {sqrt((x - 1)^2 + (y - 1)^2)}
d <- expand.grid(x = seq(0, 1, 0.02), y = seq(0, 1, 0.02))
d$dist <- mapply(pareto_dist, x = d$x, y = d$y)
bmin <- global_optimum %>%
  filter(opt_period) %>%
  filter(growth_pot == min(growth_pot))
bmax <- global_optimum %>%
  filter(opt_period) %>%
  filter(growth_pot == max(growth_pot))
max_growth_pot <- max(global_optimum$growth_pot)
pareto_front <- ggplot() +
  geom_raster(aes(x*max_growth_pot, y, fill = dist), data = d,
              interpolate = T, alpha = 0.9) +
  stat_contour(aes(x*max_growth_pot, y, z = dist), data = d,
               col = 'white', linewidth = 0.2, linetype = "dotted") +
  scale_fill_distiller(type = "seq", direction = 1, palette = "Greys") +
  geom_line(aes(y = env_pred, x = growth_pot), 
            data = global_optimum, 
            color = "white", linewidth = 1.1) +
  geom_line(aes(y = env_pred, x = growth_pot, color = opt_period, group = 1), 
            data = global_optimum, lineend = "round",
            linewidth = 0.6) +
  geom_point(aes(y = env_pred, x = growth_pot, color = opt_period), 
            data = global_optimum,
            size = 0.3)
  geom_segment(aes(x = max_growth_pot, y = 1, xend = bmin$growth_pot, yend = bmin$env_pred),
               color = "#c1121f", linewidth = 0.3, alpha = 0.9, linetype = "dashed") +
  geom_segment(aes(x = max_growth_pot, y = 1, xend = bmax$growth_pot, yend = bmax$env_pred),
               color = "#c1121f", linewidth = 0.3, alpha = 0.9 , linetype = "dashed") +
  scale_color_manual(values = c("#17a353", "#c1121f")) +
  scale_x_continuous(position = "top") +
  theme_bw() +
  theme(legend.position = 'none', panel.grid = element_blank(), strip.background = element_blank(),
        axis.text = element_text(size = 7.5), axis.title = element_text(size = 8),
        plot.margin = margin(t = 0, b = 0, l = 0, r = 6.5)) +
  coord_fixed(ratio = max_growth_pot,
              xlim = c(0, max_growth_pot),
              ylim = c(0,1), expand = FALSE) +
  labs(y = "Environmental predictability", x= "Growth potential (°C)")

# Right panel: optimality ~ DOY
optimum_plot <- ggplot() +
  geom_vline(xintercept = 172, linetype = "dashed", 
             color = "grey70", linewidth = 0.3) +
  geom_boxplot(aes(x = doy, y = max(global_optimum$opt) +0.03),
               width = 0.02, color = "#c1121f",
               linewidth = 0.3, outliers = FALSE,
               data = local_optima) +
  # geom_point(aes(x = mean(local_optima$doy), y = max(global_optimum$opt) +0.03), color = "#c1121f") +
  # geom_line(aes(y = opt, x = doy), 
  #           data = global_optimum,
  #           color = "white", linewidth = 1.5) +
  scale_y_continuous(position = "right") +
  geom_line(aes(y = opt, x = doy, color = opt_period, group = 1), 
            data = global_optimum,
            linewidth = 0.6, lineend = "round") +
  scale_color_manual(values = c("#17a353", "#c1121f")) +
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
                 plot = pareto_front + plot_spacer( )+ optimum_plot +  plot_layout(widths = c(1, 0.1, 1)), device = cairo_pdf, width = 130, height = 70, unit = "mm")
