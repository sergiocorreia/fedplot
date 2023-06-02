#options(warn = 2)

# TODO: Move to vignette

# ggplot style guide:
# https://www.njtierney.com/post/2018/07/06/style-ggplot/

# Inspiration:
# https://www.cararthompson.com/talks.html

# https://statsandr.com/blog/an-efficient-way-to-install-and-load-r-packages/

# Load dependencies
# install.packages("ggpubr") # used by pacman
if (!"pacman" %in% installed.packages()) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, here, lubridate, glue, scales)

devtools::load_all()

install_fed_font()
load_fed_font() # extrafont::loadfonts(device = "win", quiet = TRUE)

#Load and format data
data <- readr::read_csv(here("data","FSR_1.17.csv"), col_types=list(Date=col_date(format="%b-%y")))
data <- data |> rename(CaseShiller=`Case-Shiller`)
plotdata <- data |> pivot_longer(!Date, names_to = "source") #pivot for ggplot formatting

# Plot
source <- "Source: CoreLogic Real Estate Data; Zillow, Inc., Zillow Real Estate Data; S&P Case-Shiller Home Price Indices."


p <-
ggplot(plotdata, aes(x = Date, y = value, group=source )) +
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

#View(layer_data(p))
#View(inspect_after_stat(p=p))
p

save_plot('image3', size='narrow', extension='pdf')

