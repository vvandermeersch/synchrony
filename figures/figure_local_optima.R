
#---------------#
# Main figure 2 #
#---------------#

library(patchwork)
wd <- "C:/Users/vandermeersch/Documents/CEFE/projects/synchrony"
source(file.path(wd, "scripts", "preamble.R"))

# Load data
optimality <- readRDS(file.path(wd, "data/processed/eobs", paste0("optimality_","1951_2020",".rds")))
local_optima <- optimality %>%
  group_by(id) %>%
  mutate(qt = quantile(opt, 0.95), opt_period = opt > qt)

# Clustering in 3 clusters
local_clusters <- local_optima %>% 
  dplyr::filter(opt_period) %>%
  group_by(id) %>%
  summarise(optdoy = median(doy))
kmeans_cl <- kmeans(local_clusters$optdoy, centers = 2)
local_clusters$cluster <- kmeans_cl$cluster

local_optima <- local_optima %>%
  left_join(local_clusters[,c("id", "cluster")], by = "id")

local_optima_plot <- ggplot() +
  geom_vline(xintercept = 172, linetype = "dashed", 
             color = "grey70", linewidth = 0.3) +
  geom_line(aes(y = opt, x = doy, group = id), 
            color = "grey50", alpha = 0.1,
            linewidth = 0.15,
            data = local_optima) +
  geom_boxplot(aes(x = doy, y = max(local_optima$opt) +0.03, 
                   group = as.character(cluster), color = as.character(cluster)),
               width = 0.06, 
               linewidth = 0.25, outliers = FALSE,
               data = local_optima %>% dplyr::filter(opt_period)) +
  geom_line(aes(y = opt, x = doy, color = as.character(cluster), 
                group = id, alpha = opt_period), 
            data = local_optima, lineend = "round",
            linewidth = 0.2) +
  scale_alpha_manual(values = c(0, 1)) +
  scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
  theme_bw() +
  theme(legend.position = 'none', panel.grid = element_blank(), strip.background = element_blank(),
        axis.text = element_text(size = 7.5), axis.title = element_text(size = 8),
        plot.margin = margin(t = 0, b = 0, l = 2, r = 2)) +
  labs(y = "Optimality", x= "DOY") +
  coord_cartesian(xlim = c(0,365), 
                  ylim = c(min(local_optima$opt), max(local_optima$opt) + 0.08), 
                  expand = FALSE)

cowplot::ggsave2(filename = file.path(wd, "figures", "local_optimality_v1.pdf"),
                 plot = local_optima_plot, device = cairo_pdf, width = 80, height = 60, unit = "mm")

cluster_optimum <- optimality %>% 
  left_join(local_clusters[,c("id", "cluster")], by = "id") %>%
  group_by(cluster, doy) %>%
  summarise(opt = mean(opt), growth_pot = mean(growth_pot), env_pred = mean(env_pred)) %>%
  mutate(qt = quantile(opt, 0.95), opt_period = opt > qt)

local_optima_plot <- ggplot() +
  geom_vline(xintercept = 172, linetype = "dashed", 
             color = "grey70", linewidth = 0.3) +
  geom_boxplot(aes(x = doy, y = max(local_optima$opt) +0.03, 
                   color = as.character(cluster)),
               width = 0.06, 
               linewidth = 0.25, outliers = FALSE,
               data = local_optima %>% dplyr::filter(opt_period)) +
  geom_line(aes(y = opt, x = doy, color = as.character(cluster), 
                group = id), alpha = 0.1, 
            data = local_optima, lineend = "round",
            linewidth = 0.2) +
  geom_line(aes(y = opt, x = doy,
                group = as.character(cluster)),
            data = cluster_optimum, lineend = "round",
            linewidth = 0.9, color = "white") +
  geom_line(aes(y = opt, x = doy, color = as.character(cluster), 
                group = as.character(cluster)),
            data = cluster_optimum, lineend = "round",
            linewidth = 0.5) +
  scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
  theme_bw() +
  theme(legend.position = 'none', panel.grid = element_blank(), strip.background = element_blank(),
        axis.text = element_text(size = 7.5), axis.title = element_text(size = 8),
        plot.margin = margin(t = 0, b = 0, l = 2, r = 2)) +
  labs(y = "Optimality", x= "DOY") +
  coord_cartesian(xlim = c(0,365), 
                  ylim = c(min(local_optima$opt), max(local_optima$opt) + 0.08), 
                  expand = FALSE)

cowplot::ggsave2(filename = file.path(wd, "figures", "local_optimality_v2.pdf"),
                 plot = local_optima_plot, device = cairo_pdf, width = 80, height = 60, unit = "mm")



sites$cluster <- local_clusters$cluster
cluster_map <- ggplot() +
  geom_raster(data = as.data.frame(mask_r, xy = TRUE),
              aes(x,y), fill = "grey40") +
  geom_point(data = as.data.frame(sites, geom = "XY"), 
             aes(x, y),
              color = "white", size = 0.8) +
  geom_point(data = as.data.frame(sites, geom = "XY"), 
             aes(x, y, color = as.character(cluster)),
             size = 0.5) +
  scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
  theme_void() + theme(legend.position = 'none')

# Gather & save!
cowplot::ggsave2(filename = file.path(wd, "figures", "local_optimality_wmap.pdf"),
                 plot = local_optima_plot + cluster_map + plot_layout(widths = c(1.2, 1)), 
                                                                        device = cairo_pdf, width = 120, height = 60, unit = "mm")
