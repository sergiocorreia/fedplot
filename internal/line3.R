options(warn = 0)
#setMKLthreads(6)

if (!"pacman" %in% installed.packages()) install.packages("pacman")
#install.packages("/fs/lib/m1sac03/fedplot.tar.gz", source = TRUE, repos=NULL)
pacman::p_load(tidyverse, ggplot2, here, lubridate, glue, scales) #, fedplot)
devtools::document()
devtools::load_all()

#install_fed_font()
#devtools::install()
#library(fedplot)



# Plot
caption <- "Source: CoreLogic Real Estate Data; Zillow, Inc., Zillow Real Estate Data; S&P Case-Shiller Home Price Indices."

# Rescaling trick so people can see the RStudio preview better
#rescale = 2.0
#options(fedplot.linewidth_adj = rescale * 0.4686131)
#options(fedplot.font_size = rescale * 8L)


p <- fedplot::FSR_1_17 |>
  ggplot(aes(x = date, y = value, group=source)) +
  geom_recessions() +
  geom_hline_zero() +
  geom_line_fed() +
  labs(y = "12-month percent change") +
  #scale_x_year(from=2003, to=2023, by=3, expand=.05) +
  scale_x_date(minor_breaks=seq(from=as.Date("2003-01-01"), to=as.Date("2023-01-01"), by="1 years"),
               breaks=seq(from=as.Date("2004-06-30"), to=as.Date("2023-06-30"), by="3 years"),
               date_labels="%Y",
               expand=expansion(mult=.10)) +
  scale_y_continuous(sec.axis = dup_axis(),
                   breaks = seq(-25, 25, by=5),
                   limits = c(-25, 25),
                   expand = expansion(mult=0),
                   labels = scales::label_number(style_negative = "minus")) +
  annotate_last_date(nudge_y = -3, nudge_x = 0) +
  theme_fed(legend_position = c(.55, .5))

p


save_plot('example', size='narrow', extension='all')

