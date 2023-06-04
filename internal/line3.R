options(warn = 2)
#setMKLthreads(6)

if (!"pacman" %in% installed.packages()) install.packages("pacman")
#install.packages("/fs/lib/m1sac03/fedplot.tar.gz", source = TRUE, repos=NULL)
pacman::p_load(tidyverse, ggplot2, here, lubridate, glue, scales) #, fedplot)
devtools::load_all()
#install_fed_font()

# Plot
caption <- "Source: CoreLogic Real Estate Data; Zillow, Inc., Zillow Real Estate Data; S&P Case-Shiller Home Price Indices."


fedplot::FSR_1_17 |>
  ggplot(aes(x = date, y = value, group=source )) +
  geom_recessions() + # draw_top_bar=T, fill="red", alpha=0.1
  geom_hline_zero() +
  #geom_line_fed() +
  #geom_line(aes(color=after_stat(factor(group)), linewidth=after_stat(factor(group))), na.rm = T, linejoin = "mitre", lineend = "round") +
  geom_line(aes(color=source, linewidth=source), na.rm = T, linejoin = "mitre", lineend = "round") +
  labs(y = "12-month percent change") +
  scale_x_date(minor_breaks=seq(from=as.Date("2003-01-01"), to=as.Date("2023-01-01"), by="1 years"),
               breaks=seq(from=as.Date("2004-06-30"), to=as.Date("2023-06-30"), by="3 years"),
               date_labels="%Y",
               expand=expansion(mult=.05)) +
  scale_y_continuous(sec.axis = dup_axis(),
                     breaks = seq(-25, 25, by=5),
                     limits = c(-25, 25),
                     expand = expansion(mult=0),
                     labels = scales::label_number(style_negative = "minus")) +
  #annotate_last_date(repel = T, color="blue") +
  annotate_last_date(nudge_y = -3, nudge_x = 300) + # (repel=T)
  theme_fed(legend_position = c(.8, .1))


save_plot('example', size='narrow', extension='all')

