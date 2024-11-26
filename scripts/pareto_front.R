

data_plot <- global_optimum %>% filter(period == "2000 BP")

bmin <- data_plot %>%
  filter(opt_period) %>%
  filter(growth_pot == min(growth_pot))

bmax <- data_plot %>%
  filter(opt_period) %>%
  filter(growth_pot == max(growth_pot))
  
pareto_front <- ggplot() +
  geom_raster(aes(x*3400, y, fill = dist), data = d,
              interpolate = T, alpha = 0.9) +
  stat_contour(aes(x*3400, y, z = dist), data = d,
               col = 'black', linewidth = 0.2, linetype = "dotted") +
  scale_fill_distiller(type = "seq", direction = 1, palette = "Greys") +
  geom_line(aes(y = env_pred, x = growth_pot), 
            data = data_plot, 
            color = "white", linewidth = 1.1) +
  geom_line(aes(y = env_pred, x = growth_pot, color = opt_period, group = 1), 
            data = data_plot, lineend = "round",
            linewidth = 0.6) +
  geom_segment(aes(x = max(data_plot$growth_pot), y = 1, xend = bmin$growth_pot, yend = bmin$env_pred),
               color = "#c1121f", linewidth = 0.3, alpha = 0.9, linetype = "dashed") +
  geom_segment(aes(x = max(data_plot$growth_pot), y = 1, xend = bmax$growth_pot, yend = bmax$env_pred),
               color = "#c1121f", linewidth = 0.3, alpha = 0.9 , linetype = "dashed") +
  scale_color_manual(values = c("#457b9d", "#c1121f")) +
  scale_x_continuous(position = "top") +
  theme_bw() +
  theme(legend.position = 'none', panel.grid = element_blank(), strip.background = element_blank(),
        axis.text = element_text(size = 7), axis.title = element_text(size = 8)) +
  coord_fixed(ratio = max(data_plot$growth_pot),
              xlim = c(0, max(data_plot$growth_pot)),
              ylim = c(0,1), expand = FALSE) +
  labs(y = "Environmental predictability", x= "Growth potential (°C)")

cowplot::ggsave2(filename = file.path(wd, "figures/supp", "pareto_front_2000BP.pdf"),
                 plot = pareto_front, device = cairo_pdf, width = 58, height = 58, unit = "mm")
