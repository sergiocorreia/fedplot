options(warn = 2)


# TODO: Move to vignette


# ggplot style guide:
# https://www.njtierney.com/post/2018/07/06/style-ggplot/

# Inspiration:
# https://www.cararthompson.com/talks.html

# https://statsandr.com/blog/an-efficient-way-to-install-and-load-r-packages/

# Load dependencies
# install.packages("ggpubr") # used by pacman
#install.packages("/fs/research3/m1sac03/production/fedplot/cmapplot-1.2.1.tar.gz", repos = NULL, type = "source")
if (!"pacman" %in% installed.packages()) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, here, lubridate, glue, scales) # , cmapplot

load_all()
install_fed_font()

#Load and format data
data <- readr::read_csv(here("data","FSR_1.17.csv"), col_types=list(Date=col_date(format="%b-%y")))
data <- data |> rename(CaseShiller=`Case-Shiller`)
plotdata <- data |> pivot_longer(!Date, names_to = "source") #pivot for ggplot formatting

# Plot
source <- "Source: CoreLogic Real Estate Data; Zillow, Inc., Zillow Real Estate Data; S&P Case-Shiller Home Price Indices."
font <- "ITCFranklinGothic LT BookCn"

# WE NEED THIS ON WINDOWS EVERY TIME WE RUN THE PLOT!
#extrafont::loadfonts(device = "win")


ggplot(plotdata,
       aes(x = Date,
           y = value,
           group = source,
           color = source,
           linewidth = source)) +
  geom_recessions(draw_top_bar=T) + # fill="red", alpha=0.1
  geom_hline(yintercept=0, linewidth=0.5 / (72/25.4) ) +
  geom_line(na.rm=T) +
  labs(y = "12-month percent change") + #, caption = source) +
  scale_x_date(minor_breaks=seq(from=as.Date("2003-01-01"), to=as.Date("2023-01-01"), by="1 years"),
               breaks=seq(from=as.Date("2004-06-30"), to=as.Date("2023-06-30"), by="3 years"),
               date_labels="%Y",
               expand=expansion(mult=.05)) +
  scale_y_continuous(sec.axis = dup_axis(),
                     breaks = seq(-25, 25, by=5),
                     limits = c(-25, 25),
                     expand = expansion(mult=0),
                     labels = scales::label_number(style_negative = "minus")) +
  #geom_text_lastonly(mapping=aes(label=strftime(Date, "%b")),
  #                   text_aes=list(family=font, color="black", hjust=1, size=7/ggplot2::.pt), nudge_y=-8) +
  theme_fed()

# https://rdrr.io/r/base/strptime.html

save_plot('example-1', wide=F, extension='pdf')

