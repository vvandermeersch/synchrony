
periods <- seq(2000,11000,3000)
optimality_holocene <- lapply(periods, function(p) readRDS(file.path(wd, "data/processed/holocene", paste0("optimality_",p,"BP.rds"))))
optimality_holocene <- as.data.frame(do.call(rbind, optimality_holocene))
optimality_holocene$period <- factor(paste(optimality_holocene$period, "BP"), levels = paste(periods, "BP"))

# keep only cells in the 4 periods
ids <- optimality_holocene %>%
  dplyr::select(id, period) %>% unique() %>%
  group_by(id) %>%
  dplyr::filter(all(paste(periods, "BP") %in% period))
id_sampled <- sample(unique(ids$id),10)
id_sampled1 <- id_sampled[1:150]
id_sampled2 <- id_sampled[151:300]
id_sampled3 <- id_sampled[301:450]


optimality_holocene_summ <- optimality_holocene %>% 
  dplyr::filter(id %in% id_sampled) %>%
  group_by(period, id) %>%
  mutate(q95 = quantile(opt, 0.95), opt_period = opt > q95) %>%
  ungroup() %>%
  mutate(add_group_var = factor(paste0(period, opt_period), 
                                levels = c("2000 BPFALSE", "2000 BPTRUE",
                                           "5000 BPFALSE", "5000 BPTRUE",
                                           "8000 BPFALSE", "8000 BPTRUE",
                                           "11000 BPFALSE", "11000 BPTRUE")))

local_optima <- optimality_holocene %>%
  dplyr::filter(id %in% id_sampled) %>%
  group_by(period, id) %>%
  mutate(q95 = quantile(opt, 0.95)) %>%
  dplyr::filter(opt > q95) %>%
  ungroup() %>%
  mutate(id_group = ifelse(id %in% id_sampled1, "A", ifelse(id %in% id_sampled2, "B", "C")))


ggplot() +
  facet_wrap(~ id, nrow = 5) +
  geom_vline(xintercept = 172, linetype = "dashed", 
             color = "grey70", linewidth = 0.4) +
  geom_boxplot(aes(x = doy, y = max(optimality_holocene_summ$opt)+0.1, color = period),
               width = 0.1,
               linewidth = 0.5, outliers = FALSE,
               data = local_optima) +
  scale_color_manual(values = c("#93459d", "#457b9d", "#7b9d45", "#d1bb11")) +
  # geom_line(aes(y = opt, x = doy), 
  #           data = global_optimum,
  #           color = "white", linewidth = 1.5) +
  ggnewscale::new_scale_color() +
  geom_line(aes(y = opt, x = doy, color = add_group_var, group = period), 
            data = optimality_holocene_summ,
            linewidth = 0.6, lineend = "round") +
  scale_color_manual(values = c("#93459d", "#c1121f", "#457b9d", "#c1121f", "#7b9d45", "#c1121f", "#d1bb11", "#c1121f"),
                     guide = "none") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(), 
        axis.title.x = element_text(size = 8), axis.title.y = element_text(size = 9)) +
  labs(y = "Optimality", x= "DOY") +
  coord_cartesian(xlim = c(0,365), 
                  ylim = c(min(optimality_holocene_summ$opt), max(optimality_holocene_summ$opt) +0.2), 
                  expand = FALSE)

ggplot() +
  facet_wrap(~ id_group, nrow = 3, scales = "free_x") +
  geom_rect(
    data = data.frame(ymin = rep(0,50), ymax = rep(365,50),
                      xmin = seq(0.5, 49.5, 1), xmax = seq(1.5, 50.5, 1),
                      alpha = rep(c("0","0.5"),25)),
    aes(ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax, alpha = alpha),
    fill = "grey97") +
  geom_hline(yintercept = 172, linetype = "dashed", 
             color = "grey70", linewidth = 0.4) +
  geom_boxplot(aes(y = doy, x = factor(id), color = period, group = factor(paste0(id,period), levels = unique(paste0(id,period)))),  
               width = 0.7,
               linewidth = 0.5, outliers = FALSE,
               data = local_optima) +
  scale_color_manual(values = c("#93459d", "#457b9d", "#7b9d45", "#d1bb11")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(), strip.text.x.top = element_blank(),
        legend.position = 'none',
        axis.text.x = element_blank(), axis.ticks.length.x = unit(0, "cm"),
        axis.title.x = element_blank(), axis.title.y = element_text(size = 9)) +
  labs(y = "DOY") +
  coord_cartesian(ylim = c(0,365),
                  expand = FALSE)

ggplot() +
  facet_wrap(~ id_group, nrow = 3, scales = "free_x") +
  geom_rect(
    data = data.frame(ymin = rep(0,150), ymax = rep(365,150),
                      xmin = seq(0.5, 149.5, 1), xmax = seq(1.5, 150.5, 1),
                      alpha = rep(c("0","0.5"),75)),
    aes(ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax, alpha = alpha),
    fill = "grey97") +
  geom_hline(yintercept = 172, linetype = "dashed", 
             color = "grey70", linewidth = 0.4) +
  geom_boxplot(aes(y = doy, x = factor(id), group = factor(id)), 
               color =  "#c1121f",
               width = 0.6,
               linewidth = 0.5, outliers = FALSE,
               data = local_optima) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(), strip.text.x.top = element_blank(),
        legend.position = 'none',
        axis.text.x = element_blank(), axis.ticks.length.x = unit(0, "cm"),
        axis.title.x = element_blank(), axis.title.y = element_text(size = 9)) +
  labs(y = "DOY") +
  coord_cartesian(ylim = c(0,365),
                  expand = FALSE)


