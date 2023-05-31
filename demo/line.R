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
source <- NULL
source <- "Source: CoreLogic Real Estate Data; Zillow, Inc., Zillow Real Estate Data; S&P Case-Shiller Home Price Indices."
font <- "ITCFranklinGothic LT BookCn"

# WE NEED THIS ON WINDOWS EVERY TIME WE RUN THE PLOT!
extrafont::loadfonts(device = "win", quiet = TRUE)



ggplot(plotdata, aes(x = Date, y = value, color = source)) +
  labs(y = "12-month percent change") +
  geom_line(na.rm=T) + theme_fed()
save_plot('image1', extension='pdf')
#stop(1)


ggplot(plotdata,
       aes(x = Date,
           y = value,
           label = Date,
           group = source,
           color = source,
           linewidth = source)) +
  geom_recessions(draw_top_bar=T) + # fill="red", alpha=0.1
  geom_hline(yintercept=0, linewidth=0.5 * fedplot:::fedplot_constants$line_size_adjustment) +
  geom_line(na.rm=T, lineend="round") +
  labs(y = "12-month percent change", caption = source) +
  scale_x_date(minor_breaks=seq(from=as.Date("2003-01-01"), to=as.Date("2023-01-01"), by="1 years"),
               breaks=seq(from=as.Date("2004-06-30"), to=as.Date("2023-06-30"), by="3 years"),
               date_labels="%Y",
               expand=expansion(mult=.05)) +
  scale_y_continuous(sec.axis = dup_axis(),
                     breaks = seq(-25, 25, by=5),
                     limits = c(-25, 25),
                     expand = expansion(mult=0),
                     labels = scales::label_number(style_negative = "minus")) +
  #geom_text_lastdate(nudge_y = -3) + #mapping=aes(label=strftime(Date, "%b")),
                     #text_aes=list(family=font, color="black", hjust=1, size=7/ggplot2::.pt), nudge_y=-8) +
  #geom_text_lastonly(mapping = ggplot2::aes(label = Date)) +
  #geom_text_lastonly(add_points=TRUE) +
  #annotate_last_date(text_aes = aes(box.padding = .25)) + #nudge_y = -3, nudge_x = 300) +
  #annotate_last_date(nudge_y = -3, nudge_x = 300) +
  #annotate_last_date(repel = T, color="blue") +
  annotate_last_date(repel = F, nudge_y = -3, nudge_x = 300) +
  theme_fed()

# https://rdrr.io/r/base/strptime.html

save_plot('image4', size='n', extension='pdf')

