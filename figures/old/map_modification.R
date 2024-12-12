
sites$deltaopt <- sites$optdoy-172
sites$deltaopt <- ifelse(sites$deltaopt > 20, 20, ifelse(sites$deltaopt < -20, -20, sites$deltaopt))


map <- ggplot() +
  geom_raster(data = as.data.frame(mask_r %>% project("EPSG:3035"), xy = TRUE),
              aes(x,y), fill = "grey45") +
  geom_point(data = as.data.frame(sites %>% project("EPSG:3035"), geom = "XY"), 
             aes(x, y),
             color = "white", size = 0.6) +
  geom_point(data = as.data.frame(sites %>% project("EPSG:3035"), geom = "XY"), 
             aes(x, y, color = deltaopt),
             size = 0.3) +
  scale_color_gradient2(low = "#d95f02", mid = "#1b9e77", high = "#7570b3",
                        breaks = seq(-20, 20, 20), 
                        labels = c(paste0("\u2264\u2212","20"),  "0", paste0("\u2265","20"))) +
  # scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
  theme_void() + theme(
    legend.position = "inside",
    legend.position.inside =c(0.2,.8),
    legend.direction="horizontal",
    legend.title = element_blank())+
  guides(
    color = guide_colorbar(order = 1,
                          frame.colour = "grey20", ticks.colour = NA,
                          frame.linewidth = 0.2,
                          theme = theme(legend.key.height  = unit(3, "pt"),
                                        legend.key.width  = unit(50, "pt"),
                                        legend.text = element_text(size = 6, margin = margin(t = 3.5)))))

cowplot::ggsave2(filename = file.path(wd, "figures", "deltaoptim_map.pdf"),
                 plot = map, 
                 device = cairo_pdf, width = 60, height = 60, unit = "mm")
