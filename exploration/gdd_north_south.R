

sites_df <- as.data.frame(sites, geom = "XY")

south_pt <- vect(sites_df[15,], geom = c("x", "y"))
north_pt <- vect(sites_df[400,], geom = c("x", "y"))
plot(subset(gdd,1))
points(south_pt)
points(north_pt)
gdd_south <- t(extract(gdd, south_pt, ID = FALSE))
gdd_north <- t(extract(gdd, north_pt, ID = FALSE))


data_plot <- rbind(data.frame(point = "South", doy = time(gdd), year = rep(years, each = 365), gdd = gdd_south, 
                              id =  sites_df[15, "id"], type = "GDD"),
                   data.frame(point = "North", doy = time(gdd), year = rep(years, each = 365), gdd = gdd_north, 
                              id =  sites_df[400, "id"], type = "GDD")) %>%
  group_by(point, year) %>%
  mutate(delta_gdd = gdd - lag(gdd))

gdd_plot <- ggplot(data = data_plot, aes(x = doy)) + 
  facet_wrap(~ point) +
  geom_line(aes(y = delta_gdd*200, group = year), 
            linewidth = 0.2, alpha = 0.05, color = "darkblue") +
  stat_summary(aes(y = delta_gdd*200, group = 1), fun=mean, geom="line", 
               linewidth = 1.7, colour="white") +
  stat_summary(aes(y = delta_gdd*200, group = 1), fun=mean, geom="line", 
               linewidth = 0.5, colour="darkblue") +
  
  geom_line(aes(y = gdd, group = year), 
            linewidth = 0.3, alpha = 0.1, color = "black") +
  stat_summary(aes(y = gdd, group = 1), fun=mean, geom="line", 
               linewidth = 2, colour="white") +
  stat_summary(aes(y = gdd, group = 1), fun=mean, geom="line", 
               linewidth = 0.8, colour="black") +
  geom_vline(aes(xintercept = 172), linetype = "dashed", linewidth = 0.5, color = "darkred") +
  theme_bw() +
  theme(legend.position = 'none', panel.grid = element_blank(),
        strip.background = element_blank(), 
        axis.title.x = element_text(size = 8), axis.title.y = element_text(size = 9),
        axis.title.y.right = element_text(color = "darkblue")) +
  scale_y_continuous("GDD", sec.axis = sec_axis(~ . / 200, name = "Daily GDD")) +
  coord_cartesian(xlim = c(0,365), ylim = c(0,5000), expand = FALSE)

cowplot::ggsave2(filename = file.path(wd, "analysis", "gdd_plot.pdf"),
                 plot = gdd_plot, device = cairo_pdf, width = 140, height = 58, unit = "mm")

# Add optimality?
optimality_historical <- readRDS(file.path(wd, "data/processed/eobs", paste0("optimality_","1951_2020",".rds"))) %>%
  filter(id %in% sites_df[c(15,400), "id"]) %>% 
  mutate(point = if_else(id == sites_df[15,"id"], "South", "North")) %>%
  group_by(point) %>%
  mutate(growth_pot_scaled = growth_pot/max(growth_pot))


upper_plot <- ggplot(data = optimality_historical, aes(x = doy)) + 
  facet_grid(~ point) +
  geom_line(aes(y = env_pred), 
            linewidth = 2, color = "white") +
  geom_line(aes(y = env_pred), 
            linewidth = 0.8, color = "darkgreen") +
  geom_line(aes(y = growth_pot_scaled), 
            linewidth = 2, color = "white") +
  geom_line(aes(y = growth_pot_scaled), 
            linewidth = 0.8, color = "darkorange") +
  geom_vline(aes(xintercept = 172), linetype = "dashed", linewidth = 0.5, color = "darkred") +
  theme_bw() +
  theme(legend.position = 'none', panel.grid = element_blank(),
        strip.background = element_blank(), 
        axis.title.x = element_text(size = 8), axis.title.y = element_text(size = 9),
        axis.title.y.left = element_text(color = "darkgreen"),
        axis.title.y.right = element_text(color = "darkorange"),
        plot.margin = margin(t = 0, b = 0, l = 2, r = 2)) +
  labs(x= "DOY") +
  scale_y_continuous("Env. predictability", sec.axis = sec_axis(~ . / 1, name = "Growth potential")) +
  coord_cartesian(xlim = c(0,365), ylim = c(0,1.1), expand = FALSE)

lower_plot <- ggplot(data = optimality_historical, aes(x = doy)) + 
  facet_grid(~ point) +
  geom_line(aes(y = opt), 
            linewidth = 2, color = "white") +
  geom_line(aes(y = opt), 
            linewidth = 0.8, color = "darkblue") +
  geom_vline(aes(xintercept = 172), linetype = "dashed", linewidth = 0.5, color = "darkred") +
  theme_bw() +
  theme(legend.position = 'none', panel.grid = element_blank(),
        strip.background = element_blank(), 
        axis.title.x = element_text(size = 8), axis.title.y = element_text(size = 9),
        axis.title.y.left = element_text(color = "darkblue"),
        strip.text.x = element_text(color = NA), plot.margin = margin(t = 0, b = 0, l = 2, r = 2)) +
  labs(x= "DOY") +
  scale_y_continuous("Optimality") +
  coord_cartesian(xlim = c(0,365), ylim = c(0.4,1.1), expand = FALSE)

cowplot::ggsave2(filename = file.path(wd, "analysis", "optimality_examples.pdf"),
                 plot = upper_plot + lower_plot + plot_layout(ncol = 1), 
                 device = cairo_pdf, width = 130, height = 100, unit = "mm")

optimality_historical <- optimality_historical %>%
  group_by(id) %>%
  mutate(
    qt975 = quantile(opt, 0.975), 
    qt95 = quantile(opt, 0.95), 
    qt90 = quantile(opt, 0.90))

quantile_def_plot <- ggplot(data = optimality_historical, aes(x = doy)) + 
  facet_grid(~ point) +
  geom_line(aes(y = opt), 
            linewidth = 2, color = "white") +
  geom_line(aes(y = opt), 
            linewidth = 0.8, color = "grey") +
  geom_hline(aes(yintercept = qt975), data = unique(optimality_historical[c("qt975", "point")]), linetype = "dashed", linewidth = 0.5, color = "#441752") +
  geom_hline(aes(yintercept = qt95), data = unique(optimality_historical[c("qt95", "point")]), linetype = "dashed", linewidth = 0.5, color = "#AB4459") +
  geom_hline(aes(yintercept = qt90), data = unique(optimality_historical[c("qt90", "point")]), linetype = "dashed", linewidth = 0.5, color = "#F29F58") +
  geom_boxplot(aes(x = doy, y = 0.96),
               width = 0.03, linewidth = 0.3, outliers = FALSE, color = "#441752",
               data = optimality_historical %>% group_by(id) %>% dplyr::filter(opt > qt975)) +
  geom_boxplot(aes(x = doy, y = 1),
               width = 0.03, linewidth = 0.3, outliers = FALSE, color = "#AB4459",
               data = optimality_historical %>% group_by(id) %>% dplyr::filter(opt > qt95)) +
  geom_boxplot(aes(x = doy, y = 1.04),
               width = 0.03, linewidth = 0.3, outliers = FALSE, color = "#F29F58",
               data = optimality_historical %>% group_by(id) %>% dplyr::filter(opt > qt90)) +
  geom_text(data = data.frame(x = 205, y = c(0.96), label = c("Q97.5"), point = "South"), aes(x, y, label = label),
            size = 2, color = "#441752") +
  geom_text(data = data.frame(x = 205, y = c(1), label = c("Q95"), point = "South"), aes(x, y, label = label),
            size = 2, color = "#AB4459") +
  geom_text(data = data.frame(x = 205, y = c(1.04), label = c("Q90"), point = "South"), aes(x, y, label = label),
            size = 2, color = "#F29F58") +
  theme_bw() +
  theme(legend.position = 'none', panel.grid = element_blank(),
        strip.background = element_blank(), 
        axis.title.x = element_text(size = 8), axis.title.y = element_text(size = 9),
        axis.title.y.left = element_text(color = "grey"),
        strip.text.x = element_text(color = "black"), plot.margin = margin(t = 0, b = 0, l = 2, r = 2)) +
  labs(x= "DOY") +
  scale_y_continuous("Optimality") +
  coord_cartesian(xlim = c(0,365), ylim = c(0.4,1.1), expand = FALSE)

cowplot::ggsave2(filename = file.path(wd, "analysis", "quantile_def_plots.pdf"),
                 plot = quantile_def_plot, 
                 device = cairo_pdf, width = 130, height = 60, unit = "mm")
