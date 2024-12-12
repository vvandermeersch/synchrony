

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
pts <- vect(union(south_pt, north_pt))


gdd <- readRDS(file.path(wd, "data/processed/era5land", "gdd_1951_2020.rds"))
nyr <- nlyr(gdd)/365
i <- rep(1:nyr, each = 365)
gdd_tot <- tapp(gdd, i, fun=max) # calculate total GDD accumulated over the season

gdd <- aggregate(mask(gdd, pts),15, na.rm=TRUE)
gdd_tot <- aggregate(mask(gdd_tot, pts),15, na.rm=TRUE)
time(gdd) <- rep(1:365, nyr)

d <- 172

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
  group_by(id,day) %>%
  summarise(r2 = mean(r2), x = min(gdd_d)+0.15*(max(gdd_d)-min(gdd_d)), y = max(gdd_tot)-0.05*(max(gdd_tot)-min(gdd_tot)))

site_names <- c(
  `1`="Northen site",
  `2`="Southern site",
  `60`="March 1",
  `172`="Summer solstice",
  `305`="November 1"
)

optimality_samples <- optimality %>%
  filter(id %in% sites_df[c(53,409), "id"]) %>% 
  mutate(point = if_else(id == sites_df[53,"id"], "Southern site", "Northern site")) %>%
  group_by(point) %>%
  mutate(growth_pot_scaled = growth_pot/max(growth_pot),
         qt = quantile(opt, 0.90), opt_period = opt > qt)

predictability_plot <- ggplot(data = optimality_samples, aes(x = doy)) +
  geom_vline(aes(xintercept = x), data = data.frame(x = c(60, 172,305)), linetype = "dashed", 
             color = "grey70", linewidth = 0.3) +
  facet_wrap(~ point, ncol = 1, strip.position="right") +
  geom_line(aes(y = env_pred), 
            linewidth = 1.8, color = "white") +
  geom_line(aes(y = env_pred), 
            linewidth = 0.6, color = "#048BA8") +
  theme_bw() +
  theme(legend.position = 'none', panel.grid = element_blank(),
        strip.background = element_blank(), 
        strip.text.x.top = element_text(size = 7.5, color = "grey20"),
        axis.text = element_text(size = 7.5, color = "grey20"),
        axis.title.y = ggtext::element_markdown(size = 8),
        axis.title.x = element_text(size = 7.5, color = "grey20"),
        plot.margin = margin(t = 0, b = 0, l = 2, r = 2),
        rect=element_rect(fill="transparent"), 
        panel.border=element_rect(color = "grey30"),
        axis.ticks = element_line(color = "grey30", linewidth = 0.3)) +
  labs(x = "DOY", y = "Predictability") +
  coord_cartesian(xlim = c(0,365), ylim = c(0,1.1), expand = FALSE) +
  scale_x_continuous(position = "top")



day_example_plot <- ggplot(data = data_plot, aes(x = gdd_d, y = gdd_tot)) +
  ggh4x::facet_grid2(id ~ day, scales = "free", labeller = as_labeller(site_names), independent = "x")+
  
  # facet_grid(, scales = "free", )) +
  geom_point(color = "#048BA8", alpha = 0.5)  +
  geom_smooth(method = 'lm', se = FALSE, color = "#048BA8") +
  theme_bw() +
  theme_bw() +
  theme(legend.position = 'none', panel.grid = element_blank(),
        strip.background = element_blank(), 
        strip.text.x.top = element_text(size = 7.5, color = "grey20"),
        axis.text = element_text(size = 7.5, color = "grey20"),
        axis.title.y = ggtext::element_markdown(size = 8),
        axis.title.x = element_text(size = 7.5, color = "grey20"),
        plot.margin = margin(t = 0, b = 0, l = 2, r = 2),
        rect=element_rect(fill="transparent"), 
        panel.border=element_rect(color = "grey30"),
        axis.ticks = element_line(color = "grey30", linewidth = 0.3)) +
  labs(x = "GDD accumulated", y = "Total GDD") +
  geom_text(data = rsq, aes(x = x, y = y, label = paste0("~R^{2} == ", round(r2,2))), parse = TRUE, size = 3)

design <-
  "123
   444"

plot <- plot_spacer() + predictability_plot + plot_spacer() + day_example_plot +
  plot_layout(design = design, widths = c(0.6, 1.2, 0.6))




