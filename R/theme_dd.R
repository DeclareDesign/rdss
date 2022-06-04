#' ggplot Theme used in the book "Research Design: Declare, Diagnose, Redesign" (Blair, Coppock, Humphreys)
#'
#' @return ggplot theme
#'
#' @export
#' @importFrom ggplot2 theme_minimal theme element_blank element_text margin element_line
#'
theme_dd <-
  function() {
    theme_minimal() +
      theme(
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(color = '#eeeeee'),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        legend.position = "none"
      )
  }

