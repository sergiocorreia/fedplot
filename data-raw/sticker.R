library(hexSticker)

extrafont::loadfonts(device = "win", quiet = TRUE)

library(showtext)
## Loading Google fonts (http://www.google.com/fonts)
font_add_google("Gochi Hand", "gochi")
font_add_google("Lato", "lato")
font_add_google("Roboto Mono", "robotomono")
font_add_google("Merriweather", "merriweather")
## Automatically use showtext to render text for future devices
showtext_auto()


imgurl <- "./data-raw/sticker-base-alt.jpg"
sticker(imgurl, package = "fedplot",
        p_size = 24,
        p_color="#000000",
        p_family = "merriweather",
        p_y = .63,
        p_x = 1.0,
        h_fill="#ffffff",
        h_color="#000000",
        s_x=1, s_y=1, s_width = 1, s_height = 1,
        white_around_sticker=T,
        filename = "./inst/figures/hexsticker.png")
