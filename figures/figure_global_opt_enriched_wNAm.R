
#--------------------------------------#
# Main figure 1: new version, enriched #
#--------------------------------------#

wd <- "~/projects/synchrony"

## Load previous version of Fig. 1 ##
source(file.path(wd, "figures", "figure_global_optimality.R"))

## Load supp. figure about Holocene, modify it ##
source(file.path(wd, "figures", "supp", "figure_global_optimality_holocene.R"))
# - Keep only 11k and 2k BP to simplify
local_optima_hol$period <- factor(local_optima_hol$period, levels = c("11000 BP", "2000 BP"))
global_optimum_hol$period <- factor(global_optimum_hol$period, levels = c("11000 BP", "2000 BP"))
local_optima_hol <- na.omit(local_optima_hol)
global_optimum_hol <- na.omit(global_optimum_hol)
optimum_plot_hol <- ggplot() +
  facet_wrap(~ period, nrow = 1) +
  geom_vline(xintercept = 172, linetype = "dashed", 
             color = "grey70", linewidth = 0.4) +
  geom_boxplot(aes(x = doy, y = 0.91),
               width = 0.0225, color = "#c1121f",
               linewidth = 0.3, outliers = FALSE,
               data = local_optima_hol) +
  # geom_line(aes(y = opt, x = doy), 
  #           data = global_optimum,
  #           color = "white", linewidth = 1.5) +
  geom_line(aes(y = opt, x = doy, color = opt_period, group = 1), 
            data = global_optimum_hol,
            linewidth = 0.6, lineend = "round") +
  scale_color_manual(values = c("#87c7ee", "#c1121f")) +
  theme_bw() +
  labs(y = "Optimality", x= "DOY") +
  scale_y_continuous(breaks = seq(0.5,1.1,0.2)) +
  coord_cartesian(xlim = c(0,365), 
                  ylim = c(min(global_optimum_hol$opt), 0.95), 
                  expand = FALSE) +
  theme(legend.position = 'none', panel.grid = element_blank(),
        strip.background = element_blank(), 
        strip.text = element_text(size = 7, color = "grey20"),
        axis.title = element_text(size = 7, color = "grey20"),
        axis.text = element_text(size = 6.5, color = "grey30"),
        panel.border= element_blank(),
        axis.line = element_line(color = "grey30", linewidth = 0.3),
        axis.ticks = element_line(color = "grey30", linewidth = 0.3))

## Load supp. figure about future, modify it  ##
source(file.path(wd, "figures", "supp", "figure_global_optimality_future.R"))
optimum_plot_fut <- ggplot() +
  facet_grid(~ ssp) +
  geom_vline(xintercept = 172, linetype = "dashed", 
             color = "grey70", linewidth = 0.3) +
  geom_boxplot(aes(x = doy, y = 1.1),
               width = 0.03, color = "#c1121f",
               linewidth = 0.3, outliers = FALSE,
               data = local_optima_fut %>% filter(period == "2071_2100")) +
  geom_line(aes(y = opt, x = doy, group = paste0(gcm, ssp)), 
            data = global_optimum_fut_pergcm %>% filter(period == "2071_2100"),
            alpha = 0.3, color = "#f59c10",
            linewidth = 0.2, lineend = "round") +
  geom_line(aes(y = opt, x = doy, group = 1), 
            data = global_optimum_fut  %>% filter(period == "2071_2100"), color = "white",
            linewidth = 1.1, lineend = "round") +
  geom_line(aes(y = opt, x = doy, color = opt_period, group = 1), 
            data = global_optimum_fut %>% filter(period == "2071_2100"),
            linewidth = 0.6, lineend = "round") +
  scale_color_manual(values = c("#f59c10", "#c1121f")) +
  theme_bw() +
  labs(y = "Optimality", x= "DOY") +
  scale_y_continuous(position = 'left', breaks = seq(0.5,1.1,0.2)) +
  coord_cartesian(xlim = c(0,365), 
                  ylim = c(min(global_optimum_fut$opt), 1.15), 
                  expand = FALSE) +
  theme(legend.position = 'none', panel.grid = element_blank(),
        strip.background = element_blank(), 
        strip.text = element_text(size = 7, color = "grey20"),
        axis.title = element_text(size = 7, color = "grey20"),
        axis.text = element_text(size = 6.5, color = "grey30"),
        panel.border= element_blank(),
        axis.line = element_line(color = "grey30", linewidth = 0.3),
        axis.ticks = element_line(color = "grey30", linewidth = 0.3))





newfigure1 <- 
  pareto_front + 
  plot_spacer() +
  optimum_plot +   scale_x_continuous(position = "bottom") + scale_y_continuous(position = 'left', breaks = seq(0.5,1.1,0.1)) +
  optimum_plot_hol + 
  plot_spacer() +
  optimum_plot_fut + 
  plot_layout(widths = c(1, 0.01, 1), heights = c(1,0.50), nrow = 2) +
  plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(size = 10, color = "grey30", face ="bold"),
        plot.tag.location = 'panel',
        plot.tag.position = c(-0.07,1.15))





cowplot::ggsave2(filename = file.path(wd, "figures", "figure1_enriched.pdf"),
                 plot = newfigure1, device = cairo_pdf, width = 130, height = 100, unit = "mm")

# Add North America?

# Load data
optimality <- readRDS(file.path(wd, "data/processed/norame/era5land",  paste0("optimality_", 1951 ,"_", 2020, "_", "tlow", 5, "_tupp", 35, ".rds")))
global_optimum <- optimality %>% 
  group_by(doy) %>%
  summarise(opt = mean(opt), growth_pot = mean(growth_pot), env_pred = mean(env_pred)) %>%
  mutate(qt = quantile(opt, 0.90), opt_period = opt > qt)
local_optima <- optimality %>%
  group_by(id) %>%
  mutate(qt = quantile(opt, 0.90)) %>%
  dplyr::filter(opt > qt)

optimum_plot_norame <- ggplot() +
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
  scale_y_continuous(position = 'left', breaks = seq(0.5,1.1,0.1)) +
  scale_x_continuous(position = "bottom") +
  geom_line(aes(y = opt, x = doy, color = opt_period, group = 1), 
            data = global_optimum,
            linewidth = 0.6, lineend = "round") +
  scale_color_manual(values = c("#17a353", "#c1121f")) +
  theme_bw() +
  theme(legend.position = 'none', panel.grid = element_blank(), strip.background = element_blank(),
        axis.text = element_text(size = 6, color = "grey20"), axis.title = element_blank(),
        plot.margin = margin(t = 0, b = 0, l = 0, r = 0),
        panel.border= element_blank(),  axis.ticks = element_line(color = "grey30", linewidth = 0.3),
        axis.line = element_line(color = "grey30", linewidth = 0.3)) +
  labs(y = "Optimality", x= "DOY") +
  coord_cartesian(xlim = c(0,365), 
                  ylim = c(min(global_optimum$opt), max(global_optimum$opt) + 0.06), 
                  expand = FALSE)

cowplot::ggsave2(filename = file.path(wd, "figures", "global_optimality_inset.pdf"),
                 plot = optimum_plot_norame, 
                 device = cairo_pdf, width =  32, height = 30, unit = "mm")
