
#---------------#
# Main figure 2 #
#---------------#

library(patchwork)
wd <- "C:/Users/vandermeersch/Documents/CEFE/projects/synchrony"
source(file.path(wd, "scripts", "preamble.R"))

# Load data
optimality <- readRDS(file.path(wd, "data/processed/era5land", paste0("optimality3_","1951_2020",".rds")))
local_optima <- optimality %>%
  group_by(id) %>%
  mutate(qt = quantile(opt, 0.90), opt_period = opt > qt, 
         optdoy = median(doy[opt_period]), deltaopt = optdoy-172,
         deltaopt = if_else(deltaopt > 20, 20, if_else(deltaopt < -20, -20, deltaopt)))

sites <- readRDS(file.path(wd, "data/processed", "sites.rds"))
sites <- as.data.frame(sites, geom = "XY") %>%
  left_join(unique(local_optima[c("id", "deltaopt")]), join_by(id)) %>%
  vect(geom = c("x", "y"))
sites_df <- as.data.frame(sites, geom = "XY")
crs(sites) <- "EPSG:4326"
south_pt <- vect(sites_df[53,], geom = c("x", "y"))
north_pt <- vect(sites_df[409,], geom = c("x", "y"))
crs(south_pt) <- crs(north_pt) <- "EPSG:4326"


# ------------ #
# Local optima #
# ------------ #

local_optima_plot <- ggplot() +
  geom_vline(xintercept = 172, linetype = "dashed", 
             color = "grey70", linewidth = 0.3) +
  geom_line(aes(y = opt, x = doy, group = id), 
            color = "grey50", alpha = 0.1,
            linewidth = 0.15,
            data = local_optima) +
  geom_line(aes(y = opt, x = doy, color = deltaopt, 
                group = id, alpha = opt_period), 
            data = local_optima, lineend = "round",
            linewidth = 0.2) +
  scale_alpha_manual(values = c(0, 1)) +
  scale_color_gradient2(low = "#d95f02", mid = "#1b9e77", high = "#7570b3",
                        breaks = seq(-20, 20, 20), 
                        labels = c(paste0("\u2264\u2212","20"),  "0", paste0("\u2265","20"))) +
  theme_bw() +
  theme(legend.position = 'none', panel.grid = element_blank(), strip.background = element_blank(),
        axis.text = element_text(size = 7.5), axis.title = element_text(size = 8),
        plot.margin = margin(t = 0, b = 0, l = 2, r = 4)) +
  labs(y = "Optimality", x= "DOY") +
  coord_cartesian(xlim = c(00,365), 
                  ylim = c(min(local_optima$opt), max(local_optima$opt) + 0.08), 
                  expand = FALSE)


# -------------------------------------- #
# Example of Northern and Southern sites #
# -------------------------------------- #

optimality_samples <- optimality %>%
  filter(id %in% sites_df[c(53,409), "id"]) %>% 
  mutate(point = if_else(id == sites_df[53,"id"], "Southern site", "Northern site")) %>%
  group_by(point) %>%
  mutate(growth_pot_scaled = growth_pot/max(growth_pot))


upper_plot <- ggplot(data = optimality_samples, aes(x = doy)) + 
  geom_vline(xintercept = 172, linetype = "dashed", 
             color = "grey70", linewidth = 0.3) +
  facet_wrap(~ point, ncol = 1) +
  geom_line(aes(y = env_pred), 
            linewidth = 1.8, color = "white") +
  geom_line(aes(y = env_pred), 
            linewidth = 0.6, color = "#048BA8") +
  geom_line(aes(y = growth_pot_scaled), 
            linewidth = 1.8, color = "white") +
  geom_line(aes(y = growth_pot_scaled), 
            linewidth = 0.6, color = "#8FF7A7") +
  theme_bw() +
  theme(legend.position = 'none', panel.grid = element_blank(),
        strip.background = element_blank(), 
        strip.text.x.top = element_text(size = 7.5),
        axis.text = element_text(size = 7.5),
        axis.title.y = ggtext::element_markdown(size = 8),
        plot.margin = margin(t = 0, b = 0, l = 2, r = 2)) +
  labs(x = "DOY") +
  scale_y_continuous("<span style='color:#048BA8;'>Env. predictability</span> / <span style='color:#8FF7A7;'>Growth potential</span>", position = "right",  
                     breaks = c(0,0.25,0.5,0.75,1), labels = c("0", "", "0.5", "", "1")) +
  coord_cartesian(xlim = c(0,365), ylim = c(0,1.1), expand = FALSE)


# ------------ #
# Map of sites #
# ------------ #

map <- ggplot() +
  tidyterra::geom_spatraster(data = mask_r %>% project("EPSG:3035")) +
  scale_fill_gradient(low = "grey50", high = "grey45", na.value = "transparent", guide = FALSE) +
  tidyterra::geom_spatvector(data = sites %>% project("EPSG:3035"), 
                             color = "white", size = 0.7) +
  tidyterra::geom_spatvector(data = sites %>% project("EPSG:3035"), 
                             aes(color = deltaopt),
                             size = 0.4) +
  tidyterra::geom_spatvector(data = vect(c(south_pt, north_pt)) %>% project("EPSG:3035"), 
                             color = "black", size = 1.5, shape = 15) +
  tidyterra::geom_spatvector(data = vect(c(south_pt, north_pt)) %>% project("EPSG:3035"), 
                             aes(color = deltaopt), size = 1, shape = 15) +
  scale_color_gradient2(low = "#d95f02", mid = "#1b9e77", high = "#7570b3",
                        breaks = seq(-20, 20, 20), 
                        labels = c(paste0("\u2264\u2212","20"),  "0", paste0("\u2265","20"))) +
  # scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
  theme_void() + theme(
    legend.position = "inside",
    legend.position.inside =c(0.2,.8),
    legend.direction="horizontal",
    legend.title = element_blank(),
    plot.margin = margin(t = -100, b = -100, l = -100, r = -100))+
  guides(
    color = guide_colorbar(order = 1,
                           frame.colour = "grey20", ticks.colour = NA,
                           frame.linewidth = 0.2,
                           theme = theme(legend.key.height  = unit(3, "pt"),
                                         legend.key.width  = unit(80, "pt"),
                                         legend.text = element_text(size = 6, margin = margin(t = 3.5)))))

design <-
  "123
   425
   425"

test <- guide_area() + map +  plot_spacer() + local_optima_plot + upper_plot +
  plot_layout(design = design, heights = c(0.25, 1, 0.1), widths = c(0.8, 1.2, 0.65)) + plot_layout(guides = "collect")


cowplot::ggsave2(filename = file.path(wd, "figures", "test4.pdf"),
                 plot = test, 
                 device = cairo_pdf, width =  183, height = 80, unit = "mm")


