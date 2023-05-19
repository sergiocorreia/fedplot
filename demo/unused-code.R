
# Custom annotation function based on annotation_custom() -----------------

# We prefer this over annotation_custom() b/c the latter modifies the viewport
# so we cannot write outside the grid area without setting coord_cartesian(clip="off")
# See: https://github.com/tidyverse/ggplot2/blob/HEAD/R/annotation-custom.R

annotation_fire <- function(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) {
  gp=grid::gpar(fontfamily = "ITCFranklinGothic LT BookCn", fontsize = 7)
  text1_grob = grid::textGrob("Hola!", x=.9, y=.9, gp = gp)
  text2_grob = grid::textGrob("Bye!", x=1, y=1, gp = gp)

  list(
    layer(
      data = ggplot2:::dummy_data(),
      stat = StatIdentity,
      position = PositionIdentity,
      geom = GeomCustomFire,
      inherit.aes = FALSE,
      params = list(
        grob = text1_grob,
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax
      )
    ),
    layer(
      data = ggplot2:::dummy_data(),
      stat = StatIdentity,
      position = PositionIdentity,
      geom = GeomCustomFire,
      inherit.aes = FALSE,
      params = list(
        grob = text2_grob,
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax
      )
    ))
}

GeomCustomFire <- ggplot2::ggproto("GeomCustomFire", ggplot2::Geom,
                          extra_params = "",
                          handle_na = function(data, params) {
                            data
                          },

                          draw_panel = function(data, panel_params, coord, grob, xmin, xmax,
                                                ymin, ymax) {
                            if (!inherits(coord, "CoordCartesian")) {
                              cli::cli_abort("{.fn annotation_custom} only works with {.fn coord_cartesian}")
                            }
                            corners <- ggplot2:::data_frame0(
                              x = c(xmin, xmax),
                              y = c(ymin, ymax),
                              .size = 2
                            )
                            data <- coord$transform(corners, panel_params)

                            x_rng <- range(data$x, na.rm = TRUE)
                            y_rng <- range(data$y, na.rm = TRUE)

                            vp <- grid::viewport(x = mean(x_rng), y = mean(y_rng),
                                                 width = diff(x_rng), height = diff(y_rng),
                                                 just = c("center","center"))

                            vp <- grid::viewport(clip="off") #x=0.1,y=0.1,width=0.5,height=1, just=c("left", "top"))

                            grid::editGrob(grob, vp = vp, name = paste(grob$name, annotation_id()))
                          },

                          default_aes = ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
)

annotation_id <- local({
  i <- 1
  function() {
    i <<- i + 1
    i
  }
})






# Hacks to ggplot2 internals ----------------------------------------------


# Hacks to ggplot2 internals ----------------------------------------------

# Idea: go back to simple theme
# Then have a bunch of stuff in a list that I group together as e.g.
# + set_on_fire() or + burn() or just + fire_theme() b/c easier to remember
# Maybe allow these elems to be on/off

# Key: how to add elements outside of the plot area
# so we can avoid the hack coord_cartesian(clip="off")






#annotation_fire <- function(){
#  list(
#    geom_abline(aes(slope = 1, intercept = 0, alpha = "Identity line"),
#                linetype = "dashed", color = "red"),
#    labs(alpha = ""),
#    scale_alpha_manual(values = c(1,1))
#  )
#}


grob_fire <- function() {
  print('<<<')
  print(grid.ls(viewports=T, fullNames=T))
  print('>>>')
  grid::textGrob("Hi!!! Hello!", x=0, y=0, vp=grid::pushViewport(grid::viewport(width=0.8, height=0.8, name="A"))
  )
}





grob_top_tick <- function() {
  gp <- grid::gpar(lwd = 0.5, lineend = "butt", clip="off")
  width <- grid::current.viewport()$xscale[2]

  length <- 12
  x0_left <- grid::convertWidth(-unit(0.4, "bigpts"), "npc")
  x1_left <- grid::convertWidth(unit(length, "bigpts"), "npc")
  x0_right <- grid::convertWidth(unit(1, "npc")-unit(length, "bigpts"), "npc")
  x1_right <- grid::convertWidth(unit(1, "npc")+unit(0.4, "bigpts"), "npc")

  grid::segmentsGrob(x0 = c(x0_left, x0_right),
                     x1 = c(x1_left, x1_right),
                     y0 = c(1, 1),
                     y1 = c(1, 1),
                     gp=gp)
}

