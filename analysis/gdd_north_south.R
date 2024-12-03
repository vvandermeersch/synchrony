
south_pt <- vect(data.frame(x = -8.5, y = 40), geom = c("x", "y"))
north_pt <- vect(data.frame(x = 14, y = 60), geom = c("x", "y"))
plot(subset(gdd,1))
points(south_pt)
points(north_pt)
gdd_south <- t(extract(gdd, south_pt, ID = FALSE))
gdd_north <- t(extract(gdd, north_pt, ID = FALSE))

data_plot <- rbind(data.frame(point = "South", day = time(gdd), year = rep(years, each = 365), gdd = gdd_south),
                   data.frame(point = "North", day = time(gdd), year = rep(years, each = 365), gdd = gdd_north)) %>%
  group_by(point, year) %>%
  mutate(delta_gdd = gdd - lag(gdd))

gdd_plot <- ggplot(data = data_plot, aes(x = day)) + 
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
  labs(x= "DOY") +
  scale_y_continuous("GDD", sec.axis = sec_axis(~ . / 200, name = "Daily GDD")) +
  coord_cartesian(xlim = c(0,365), ylim = c(0,5000), expand = FALSE)

cowplot::ggsave2(filename = file.path(wd, "analysis", "gdd_plot.pdf"),
                 plot = gdd_plot, device = cairo_pdf, width = 140, height = 58, unit = "mm")

