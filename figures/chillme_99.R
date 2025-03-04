
params <- data.frame()
chillsim <- data.frame()
wangsim <- data.frame()
for(i in 1:500){
  
  print(i)
  cc <- runif(1,50,300)
  tmin <- runif(1,-30,10)
  tmax <- runif(1,-10,30)
  while(tmax < tmin & tmax-tmin > 30){
    tmax <- runif(1,-5,30)
  }
  optm1 <- optim(par=c(cc,tmin,tmax), fn=chillopt_wostart, x=temp_df, obs = as.Date("2000-01-30"), d0 = d0)
  param1 <- data.frame(t(optm1$par), case = i, optm1$value)
  names(param1) <- c("d0", "tmin", "tmax", "case", "optval")
  chill1 <- chillme(data = temp_df, d0 = d0, 
                    C = optm1$par[1], tmin = optm1$par[2], topt = (optm1$par[2]+optm1$par[3])/2, tmax = optm1$par[3])
  chillsim1 <- data.frame(chill1$dchill, case = i)
  wangsim1 <- data.frame(temp = seq(-40,40,0.05), value = sapply(seq(-40,40,0.05), wang, tmin = optm1$par[2], topt = (optm1$par[2]+optm1$par[3])/2, tmax = optm1$par[3]),
                         case = i)
  
  params <- rbind(params,param1 )
  chillsim <- rbind(chillsim, chillsim1)
  wangsim <- rbind(wangsim, wangsim1)
  
}

keeps <- params[params$tmin > -30 & params$tmax < 30 & params$optval == 0, "case"]

kippenberger <- c("#8B174DFF", "#AE2565FF", "#C1447EFF", "#D06C9BFF", "#DA9FB8FF",
                  "#ADBE7CFF", "#8BA749FF", "#6E8537FF", "#4F5F28FF", "#343D1FFF")

dacc <-ggplot(data = wangsim[wangsim$case %in% keeps & wangsim$value > 0,]) +
  geom_line(
    aes(x = temp, y = value, group = case, col = case), linewidth = .2) +
  # geom_line(
  #   aes(x = temp, y = value, group = case), linewidth = .5, col = "white") +
  theme_bw() +
  coord_cartesian(ylim = c(-0, 1.1), expand = FALSE) +
  theme(panel.grid = element_blank(), legend.position = 'none',
        plot.margin = margin(r = 20)) +
  labs(x = "Temperature", y = "Daily chilling") +
  scale_color_gradientn(colors = kippenberger)

acc <- ggplot() +
  geom_segment(aes(x = obs, xend = obs, y = -5, yend = max(optm1$par[1], optm2$par[1])),
               linetype = "dashed", alpha = 0.5) +
  geom_line(
    data = chillsim %>% dplyr::filter(date <= obs & case %in% keeps),
    aes(x = date, y = sum, group = case, col = case), linewidth = 0.2) +
  # geom_line(
  #   data = chillsim%>% dplyr::filter(date <= obs),
  #   aes(x = date, y = sum, group = case), linewidth = .5, col = "white") +
  geom_line(
    data = chillsim %>% dplyr::filter(date >= obs %m-% days(1) & date < obs %m+% days(15) & case %in% keeps),
    aes(x = date, y = sum, group = case, col = case), linetype = "dotted", linewidth = 0.2, alpha = 0.4) +
  theme_bw() +
  coord_cartesian(ylim = c(-2, 150), expand = FALSE) +
  scale_y_continuous(position = "right") +
  theme(panel.grid = element_blank(), legend.position = 'none',
        plot.margin = margin(l = 20)) +
  labs(x = "", y = "Chilling accumulation") +
  scale_color_gradientn(colors = kippenberger)

assemble <- dacc + acc

cowplot::ggsave2(filename = file.path(wd, "non_identifiability_99cases.pdf"),
                 plot = assemble, 
                 device = cairo_pdf, width =  180, height = 60, unit = "mm")


parameters <- ggplot(data = params[params$case %in% keeps,]) +
  geom_segment(aes(x = d0, xend = d0, y = tmin, yend = tmax, color = case),
               linewidth = 0.4) +
  scale_color_gradientn(colors = kippenberger) +
  theme_bw() +
  theme(panel.grid = element_blank(), legend.position = 'none') +
  scale_x_continuous(expand = c(0.01,0.01)) +
  labs(x = "Total sum of required chilling", y = "Range of temperatures")

cowplot::ggsave2(filename = file.path(wd, "non_identifiability_99parametersets.pdf"),
                 plot = parameters, 
                 device = cairo_pdf, width =  150, height = 60, unit = "mm")
