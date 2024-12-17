
#--------------------------#
# Supp figure S1: workflow #
#--------------------------#

wd <- "C:/Users/vandermeersch/Documents/CEFE/projects/synchrony"
source(file.path(wd, "scripts", "preamble.R"))

# Load data
optimality <- readRDS(file.path(wd, "data/processed/era5land",  paste0("optimality_", 1951 ,"_", 2020, "_", "tlow", 5, "_tupp", 35, ".rds")))
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
pts <- south_pt


gdd <- readRDS(file.path(wd, "data/processed/era5land", paste0("gdd_", 1951 ,"_", 2020, "_", "tlow", 5, "_tupp", 35, ".rds")))
nyr <- nlyr(gdd)/365
i <- rep(1:nyr, each = 365)
gdd_tot <- tapp(gdd, i, fun=max) # calculate total GDD accumulated over the season

gdd <- aggregate(mask(gdd, pts),15, na.rm=TRUE)
gdd_tot <- aggregate(mask(gdd_tot, pts),15, na.rm=TRUE)
time(gdd) <- rep(1:365, nyr)

data_plot <- lapply(c(60,172,305), function(d){
  rsq_d <- subset(gdd_tot, 1) # make a copy (in which we will save R2 values)
  ind <- 1:nrow(as.data.frame(gdd_tot))
  gdd_tot_df <- as.data.frame(gdd_tot)
  gdd_d_df <- as.data.frame(subset(gdd, which(time(gdd)==d)))
  day_list <- lapply(ind, function(i){
    y <- as.numeric(gdd_tot_df[i,])
    x <- as.numeric(gdd_d_df[i,])
    m <- lm(y ~ x)
    s <- summary(m)
    data.frame(gdd_tot = y, gdd_d = x, r2 = s$r.squared, id = i, day = d)
  })
  as.data.frame(do.call(rbind, day_list))
})
data_plot <- as.data.frame(do.call(rbind, data_plot))


rsq <- data_plot %>%
  group_by(day) %>%
  summarise(r2 = mean(r2), x = max(gdd_d)-0.18*(max(gdd_d)-min(gdd_d)), y = min(gdd_tot))

site_names <- c(
  `1`="Northen site",
  `2`="Southern site",
  `60`="March 1",
  `172`="Summer solstice",
  `305`="November 1"
)

optimality_samples <- optimality %>%
  filter(id %in% sites_df[c(53), "id"]) %>% 
  mutate(point = if_else(id == sites_df[53,"id"], "Southern site", "Northern site")) %>%
  group_by(point) %>%
  mutate(growth_pot_scaled = growth_pot/max(growth_pot),
         qt = quantile(opt, 0.90), opt_period = opt > qt)

predictability_plot <- ggplot(data = optimality_samples, aes(x = doy)) +
  geom_vline(aes(xintercept = x), data = data.frame(x = c(60, 172,305)), linetype = "dashed", 
             color = "grey70", linewidth = 0.3) +
  geom_line(aes(y = env_pred), 
            linewidth = 1.8, color = "white") +
  geom_line(aes(y = env_pred), 
            linewidth = 0.7, color = "#048BA8") +
  theme_bw() +
  theme(legend.position = 'none', panel.grid = element_blank(),
        strip.background = element_blank(), 
        strip.text.x.top = element_text(size = 7.5, color = "grey20"),
        axis.text = element_text(size = 6.5, color = "grey30"),
        axis.title.y = ggtext::element_markdown(size = 8),
        axis.title.x = element_text(size = 7.5, color = "grey20"),
        plot.margin = margin(t = 0, b = 0, l = 2, r = 2),
        rect=element_rect(fill="transparent"), 
        panel.border=element_rect(color = "grey30"),
        axis.ticks = element_line(color = "grey30", linewidth = 0.3)) +
  labs(x = "DOY", y = "Predictability") +
  coord_cartesian(xlim = c(0,365), ylim = c(0,1.1), expand = FALSE) +
  scale_x_continuous(position = "bottom") +
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1), labels = c("0", "", "0.5", "", "1"))



day_example_plot <- ggplot(data = data_plot, aes(x = gdd_d, y = gdd_tot)) +
  facet_wrap(~ day, scales = "free_x", labeller = as_labeller(site_names),
                     strip.position="bottom")+
  geom_point(color = "#048BA8", fill = "#048BA8", alpha = 0.5, size = 0.7, shape = 21)  +
  geom_smooth(method = 'lm', se = FALSE, color = "#048BA8", linewidth = 0.5) +
  theme_bw() +
  theme_bw() +
  theme(legend.position = 'none', panel.grid = element_blank(),
        strip.background = element_blank(), 
        strip.text.x.bottom = element_text(size = 7.5, color = "grey50"),
        axis.text = element_text(size = 6.5, color = "grey30"),
        axis.title.y = element_text(size = 7.5, color = "#048BA8"),
        axis.title.x = element_text(size = 7.5, color = "grey20"),
        plot.margin = margin(t = 0, b = 0, l = 2, r = 2),
        rect=element_rect(fill="transparent"), 
        panel.border=element_rect(color = "grey30"),
        axis.ticks = element_line(color = "grey30", linewidth = 0.3)) +
  labs(x = "GDD accumulated", y = "Total GDD") +
  geom_text(data = rsq, aes(x = x, y = y, label = paste0("~R^{2} == ", round(r2,2))), parse = TRUE, size = 2.5) +
  scale_x_continuous(position = 'top')

design <-
  "444
   123"

assemble_fig <- plot_spacer() + predictability_plot + plot_spacer() + day_example_plot +
  plot_layout(design = design, widths = c(0.4, 1.4, 0.4), heights = c(0.8,1))

cowplot::ggsave2(filename = file.path(wd, "figures/supp", "predictability_method.pdf"),
                 plot = assemble_fig, 
                 device = cairo_pdf, width =  110, height = 70, unit = "mm")


years <- 1951:2020
gdd_south <- t(extract(gdd, south_pt, ID = FALSE))
data_plot <- rbind(data.frame(point = "South", doy = time(gdd), year = rep(years, each = 365), gdd = gdd_south, 
                              type = "GDD")) %>%
  group_by(point, year) %>%
  mutate(delta_gdd = gdd - lag(gdd), gdd_tot = max(gdd))

gdd_plot <- ggplot(data = data_plot, aes(x = doy)) + 
  geom_vline(aes(xintercept = x), data = data.frame(x = c(172)), linetype = "dashed", 
             color = "grey70", linewidth = 0.3) +
  geom_line(aes(y = delta_gdd*160, group = year), 
            linewidth = 0.2, alpha = 0.05, color = "grey20") +
  stat_summary(aes(y = delta_gdd*160, group = 1), fun=mean, geom="line", 
               linewidth = 1.1, colour="white") +
  stat_summary(aes(y = delta_gdd*160, group = 1), fun=mean, geom="line", 
               linewidth = 0.3, colour="grey20") +
  
  geom_line(aes(y = gdd_tot-gdd, group = year), 
            linewidth = 0.3, alpha = 0.2, color = "#e8a202") +
  stat_summary(aes(y = gdd_tot-gdd, group = 1), fun=mean, geom="line", 
               linewidth = 1.8, colour="white") +
  stat_summary(aes(y = gdd_tot-gdd, group = 1), fun=mean, geom="line", 
               linewidth = 0.7, colour="#e8a202") +
  labs(x = "DOY") +
  theme_bw() +
  theme(legend.position = 'none', panel.grid = element_blank(),
        strip.background = element_blank(), 
        axis.title.x = element_text(size = 7.5), axis.title.y = element_text(size = 7.5, color = "#e8a202"),
        axis.title.y.right = element_text(color = "grey20"),
        axis.text = element_text(size = 6.5, color = "grey30"),
        panel.border=element_rect(color = "grey30"),
        axis.ticks = element_line(color = "grey30", linewidth = 0.3)) +
  scale_y_continuous("GDD remaining", sec.axis = sec_axis(~ . / 160, name = "Daily GDD")) +
  coord_cartesian(xlim = c(0,365), expand = FALSE)

cowplot::ggsave2(filename = file.path(wd, "figures/supp", "growth_pot_method.pdf"),
                 plot = gdd_plot, 
                 device = cairo_pdf, width =  75, height = 50, unit = "mm")



# Pareto front
pareto_dist <- function(x, y) {sqrt((x - 1)^2 + (y - 1)^2)}
d <- expand.grid(x = seq(0, 1, 0.02), y = seq(0, 1, 0.02))
d$dist <- mapply(pareto_dist, x = d$x, y = d$y)
bmin <- optimality_samples %>%
  filter(opt_period) %>%
  filter(growth_pot == min(growth_pot))
bmax <- optimality_samples %>%
  filter(opt_period) %>%
  filter(growth_pot == max(growth_pot))
max_growth_pot <- max(optimality_samples$growth_pot)
pareto_front <- ggplot() +
  geom_raster(aes(x, y, fill = dist), data = d,
              interpolate = T, alpha = 0.9) +
  stat_contour(aes(x, y, z = dist), data = d,
               col = 'white', linewidth = 0.2, linetype = "dotted") +
  scale_fill_distiller(type = "seq", direction = 1, palette = "Greys") +
  geom_line(aes(y = env_pred, x = growth_pot/max_growth_pot), 
            data = optimality_samples, 
            color = "white", linewidth = 1.1) +
  geom_line(aes(y = env_pred, x = growth_pot/max_growth_pot, color = opt_period, group = 1), 
            data = optimality_samples, lineend = "round",
            linewidth = 0.6) +
  geom_segment(aes(x = 1, y = 1, xend = bmin$growth_pot/max_growth_pot, yend = bmin$env_pred),
               color = "#c1121f", linewidth = 0.3, alpha = 0.9, linetype = "dashed") +
  geom_segment(aes(x = 1, y = 1, xend = bmax$growth_pot/max_growth_pot, yend = bmax$env_pred),
               color = "#c1121f", linewidth = 0.3, alpha = 0.9 , linetype = "dashed") +
  scale_color_manual(values = c("#17a353", "#c1121f")) +
  geom_point(aes(y = env_pred, x = growth_pot/max_growth_pot), data = optimality_samples %>% filter(doy == 172), size = 2.2, color = "white") +
  geom_point(aes(y = env_pred, x = growth_pot/max_growth_pot), data = optimality_samples %>% filter(doy == 172), size = 1.3) +
  scale_x_continuous(position = "top") +
  theme_bw() +
  theme(legend.position = 'none', panel.grid = element_blank(), strip.background = element_blank(),
        axis.text = element_text(size = 7.5), axis.title = element_text(size = 8),
        plot.margin = margin(t = 0, b = 0, l = 0, r = 6.5)) +
  theme(legend.position = 'none', panel.grid = element_blank(),
        strip.background = element_blank(), 
        axis.title = element_text(size = 7.5, color = "grey20"),
        axis.text = element_text(size = 6.5, color = "grey30"),
        panel.border=element_rect(color = "grey30"),
        axis.ticks = element_line(color = "grey30", linewidth = 0.3)) +
  coord_fixed(ratio = 1,
              xlim = c(0, 1),
              ylim = c(0,1), expand = FALSE) +
  labs(y = "Environmental predictability", x= "Growth potential (scaled)")

cowplot::ggsave2(filename = file.path(wd, "figures/supp", "pareto_front_method.pdf"),
                 plot = pareto_front, 
                 device = cairo_pdf, width =  60, height = 60, unit = "mm")

optimum_plot <- ggplot() +
  geom_vline(xintercept = 172, linetype = "dashed", 
             color = "grey70", linewidth = 0.3) +
  geom_boxplot(aes(x = doy, y = max(optimality_samples$opt) +0.03),
               width = 0.02, color = "#c1121f",
               linewidth = 0.3, outliers = FALSE,
               data = optimality_samples %>% filter(opt_period)) +
  # geom_point(aes(x = mean(local_optima$doy), y = max(global_optimum$opt) +0.03), color = "#c1121f") +
  # geom_line(aes(y = opt, x = doy), 
  #           data = global_optimum,
  #           color = "white", linewidth = 1.5) +
  scale_y_continuous(position = "left") +
  geom_line(aes(y = opt, x = doy, color = opt_period, group = 1), 
            data = optimality_samples,
            linewidth = 0.6, lineend = "round") +
  scale_color_manual(values = c("#17a353", "#c1121f")) +
  theme_bw() +
  theme(legend.position = 'none', panel.grid = element_blank(), strip.background = element_blank(),
        axis.text = element_text(size = 7.5), axis.title = element_text(size = 8),
        plot.margin = margin(t = 0, b = 0, l = 6.5, r = 0)) +
  labs(y = "Optimality", x= "DOY") +
  coord_cartesian(xlim = c(0,365), 
                  ylim = c(min(optimality_samples$opt), max(optimality_samples$opt) + 0.06), 
                  expand = FALSE) +
  theme(legend.position = 'none', panel.grid = element_blank(),
        strip.background = element_blank(), 
        axis.title = element_text(size = 7.5, color = "grey20"),
        axis.text = element_text(size = 6.5, color = "grey30"),
        panel.border=element_rect(color = "grey30"),
        axis.ticks = element_line(color = "grey30", linewidth = 0.3))

cowplot::ggsave2(filename = file.path(wd, "figures/supp", "optimum_method.pdf"),
                 plot = optimum_plot, 
                 device = cairo_pdf, width =  70, height = 40, unit = "mm")
