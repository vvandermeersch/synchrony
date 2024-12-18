
models <- c("GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL")
optimality_future_ssp2 <- lapply(models, function(m) readRDS(file.path(wd, "data/processed/cmip6",  paste0("optimality_","ssp245","_",m,".rds"))))
optimality_future_ssp2 <- as.data.frame(do.call(rbind, optimality_future_ssp2))
optimality_future_ssp2$ssp <-"SSP2-4.5"
optimality_future_ssp5 <- lapply(models, function(m) readRDS(file.path(wd, "data/processed/cmip6",  paste0("optimality_","ssp585","_",m,".rds"))))
optimality_future_ssp5 <- as.data.frame(do.call(rbind, optimality_future_ssp5))
optimality_future_ssp5$ssp <-"SSP5-8.5"
optimality_future <- rbind(optimality_future_ssp2, optimality_future_ssp5)

local_optima <- optimality_future %>%
  group_by(ssp,id) %>%
  mutate(qt = quantile(opt, 0.90), opt_period = opt > qt, 
         optdoy = median(doy[opt_period]), deltaopt = optdoy-172,
         deltaopt = if_else(deltaopt > 20, 20, if_else(deltaopt < -20, -20, deltaopt)))

sites <- readRDS(file.path(wd, "data/processed", "sites.rds"))
sites <- as.data.frame(sites, geom = "XY") %>%
  left_join(unique(local_optima[c("ssp","id", "deltaopt")]), join_by(id)) %>%
  vect(geom = c("x", "y"))
crs(sites) <- "EPSG:4326"
sites_df <- as.data.frame(sites, geom = "XY")


map <- ggplot() +
  facet_wrap(~ssp) +
  tidyterra::geom_spatvector(data = aggregate(eu_map) %>% crop(ext(mask_r)) %>% project("EPSG:3035"), fill = "white",
                             linewidth = 0.1, color = "grey60") +
  tidyterra::geom_spatvector(data = sites %>% project("EPSG:3035"), 
                             color = "white", size = 1.1) +
  tidyterra::geom_spatvector(data = sites %>% project("EPSG:3035"), 
                             aes(color = deltaopt),
                             size = 0.7) +
  scale_color_gradientn(colors = kippenberger, breaks = seq(-20, 20, 20), limits = c(-20,20),
                        labels = c(paste0("\u2264\u2212","20"),  "0", paste0("\u2265","20")),
                        name = "Optimal timing (relative to solstice)") +
  # scale_color_viridis_c(direction = -1, breaks = seq(-20, 20, 20), option = "D",
  #                      labels = c(paste0("\u2264\u2212","20"),  "0", paste0("\u2265","20")),
  #                      name = "Optimal timing (relative to solstice)") +
  # scale_color_gradient2(low = "#d95f02", mid = "#1b9e77", high = "#7570b3",
  #                       breaks = seq(-20, 20, 20), 
  #                       labels = c(paste0("\u2264\u2212","20"),  "0", paste0("\u2265","20")),
  #                       name = "Optimal timing (relative to solstice)") +
  # scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
  theme_void() + theme(
    legend.position = "inside",
    legend.position.inside =c(0.2,.8),
    legend.direction="horizontal",
    legend.title = element_text(size = 7, color = "grey20"),
    plot.margin = margin(t = 0, b = 0, l = 0, r = 0))+
  guides(
    color = guide_colorbar(order = 1,
                           frame.colour = "grey30", ticks.colour = NA,
                           frame.linewidth = 0.2,
                           title.position="top", title.hjust = 0.5,
                           theme = theme(legend.key.height  = unit(3, "pt"),
                                         legend.key.width  = unit(80, "pt"),
                                         legend.text = element_text(size = 7, 
                                                                    margin = margin(t = 3.5), color = "grey20"))))

map +  guide_area() +  plot_layout(guides = "collect", ncol = 1, heights = c(1,0.1))

cowplot::ggsave2(filename = file.path(wd, "figures/supp", "local_optimality_future.pdf"),
                 plot = map, device = cairo_pdf, width = 90, height = 90, unit = "mm")
