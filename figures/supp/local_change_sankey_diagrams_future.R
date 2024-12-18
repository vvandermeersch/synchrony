
#--------------------------------------------------#
# Supp figure S5: supp script for Sankey diagrams  #
#--------------------------------------------------#

library(ggsankey)
wd <- "C:/Users/vandermeersch/Documents/CEFE/projects/synchrony"
source(file.path(wd, "scripts", "preamble.R"))

models <- c("GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL")
optimality_future_ssp2 <- lapply(models, function(m) readRDS(file.path(wd, "data/processed/cmip6",  paste0("optimality_","ssp245","_",m,".rds"))))
optimality_future_ssp2 <- as.data.frame(do.call(rbind, optimality_future_ssp2))
optimality_future_ssp2$ssp <-"SSP2-4.5"
optimality_future_ssp5 <- lapply(models, function(m) readRDS(file.path(wd, "data/processed/cmip6",  paste0("optimality_","ssp585","_",m,".rds"))))
optimality_future_ssp5 <- as.data.frame(do.call(rbind, optimality_future_ssp5))
optimality_future_ssp5$ssp <-"SSP5-8.5"
optimality_future <- rbind(optimality_future_ssp2, optimality_future_ssp5)

local_optima_future <- optimality_future %>%
  group_by(ssp,id) %>%
  mutate(qt = quantile(opt, 0.90), opt_period = opt > qt, 
            optdoy = median(doy[opt_period]), deltaopt = optdoy-172,
            deltaopt = if_else(deltaopt > 20, 20, if_else(deltaopt < -20, -20, deltaopt))) %>%
  group_by(ssp,id) %>%
  summarise(`2071-2100` = mean(deltaopt))

optimality <- readRDS(file.path(wd, "data/processed/era5land",  paste0("optimality_", 1951 ,"_", 2020, "_", "tlow", 5, "_tupp", 35, ".rds")))
local_optima <- optimality %>%
  group_by(id) %>%
  mutate(qt = quantile(opt, 0.90), opt_period = opt > qt, 
         optdoy = median(doy[opt_period]), deltaopt = optdoy-172,
         deltaopt = if_else(deltaopt > 20, 20, if_else(deltaopt < -20, -20, deltaopt))) %>%
  group_by(id) %>%
  summarise(`1951-2020` = mean(deltaopt))

df_ssp2 <- left_join(local_optima, local_optima_future %>% filter (ssp == "SSP2-4.5")) %>%
  make_long(`1951-2020`, `2071-2100`) %>% mutate(ssp = "SSP2-4.5")

df_ssp5 <- left_join(local_optima, local_optima_future %>% filter (ssp == "SSP5-8.5")) %>%
  make_long(`1951-2020`, `2071-2100`) %>% mutate(ssp = "SSP5-8.5")

sankey_diagrams <- ggplot(rbind(df_ssp2,df_ssp5), 
       aes(x = x, 
               next_x = next_x, 
               node = node, 
               next_node = next_node,
               fill = node)) +
  facet_wrap(~ ssp) +
  geom_sankey(flow.alpha = 0.5, node.color = NA, width = 0.1, type = "sankey", space = 1) +
  scale_fill_gradientn(colors = kippenberger, breaks = seq(-20, 20, 20), limits = c(-20,20),
                        labels = c(paste0("\u2264\u2212","20"),  "0", paste0("\u2265","20")),
                        name = "Optimal timing (relative to solstice)") +
  theme_bw() +
  theme(legend.position = 'none') +
  theme_bw() +
  theme(legend.position = 'none', panel.grid = element_blank(), strip.background = element_blank(),
        axis.text.x = element_text(size = 8, color = "grey20"), 
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        plot.margin = margin(t = 0, b = 10, l = 20, r = 20),
        rect=element_rect(fill="transparent"), 
        panel.border= element_blank(),
        axis.ticks = element_blank(),
        panel.spacing = unit(25, "pt"),
        strip.text.x.top = element_blank()) +
  coord_cartesian(expand = FALSE) +
  scale_x_discrete(position = 'top', labels = c("   1951-2020", "2071-2100   "))
  
  

