
#' Add alpha transparency to a color defined in hexadecimal
#'
#' @param col A color as a six-digit hex code, e.g. `"#72B4F3"`.
#' @param alpha Opacity, from 0 (transparent) to 1 (opaque).
#'
#' @return The color as an eight-digit hex code.
#'
#' @examples
#' hex_add_alpha("#72B4F3", 0.5)
#'
#' @export
#'
hex_add_alpha <- function(col, alpha) {
  sprintf("%s%02X", col, pmin(floor(alpha * 256), 255))
}

dd_palettes <- list(
  # ColorPalette = c(dd_light_blue, dd_orange, dd_pink)
  three_color_palette = c("#72B4F3", "#F38672", "#C6227F"),
  # GreyPalette = c(dd_light_blue, dd_orange, dd_pink, dd_light_gray)
  grey_palette = c("#72B4F3", "#F38672", "#C6227F", gray(0.8)),
  # quilt_palette = c(dd_light_gray, dd_pink, dd_purple , dd_light_blue, dd_orange)
  quilt_palette = c(gray(0.8), "#C6227F", "#7E43B6" , "#72B4F3", "#F38672"),

  # two_color_palette = c(dd_dark_blue, dd_pink)
  two_color_palette = c("#3564ED", "#C6227F"),

  # c(dd_light_gray, "#72B4F366", dd_light_blue)
  quilt_three_color_palette = c(gray(0.8), "#72B4F366", "#72B4F3"),

  # dark blue, light gray
  two_color_gray = c("#3564ED", gray(0.8)),

  dd_dark_blue = "#3564ED",
  dd_light_blue = "#72B4F3",
  dd_orange = "#F38672",
  dd_purple = "#7E43B6",
  dd_gray = gray(0.2),
  dd_pink = "#C6227F",
  dd_light_gray = gray(0.8),
  dd_dark_blue_alpha = "#3564EDA0",
  dd_light_blue_alpha = "#72B4F3A0"
)

# one_color_palette <- dd_light_blue
# two_color_palette <- c(dd_dark_blue, dd_pink)
# three_color_palette <- c(dd_light_blue, dd_orange, dd_pink)
#
# one_color_palette_gray <- c(dd_dark_blue, dd_light_gray)
# two_color_palette_gray <- c(two_color_palette, dd_light_gray)
# three_color_palette_gray <- c(three_color_palette, dd_light_gray)

#' Color palettes used in the book Research Design in the Social Sciences
#'
#' Based on Karthik Ram's wesanderson package (https://github.com/karthik/wesanderson)
#'
#' @param name The palette name, as a string. One of the names listed below.
#' @param n The number of colors to return, from the start of the palette. Defaults to all of them.
#'
#' @details
#'
#' Palettes:
#'
#' * `three_color_palette`: light blue, orange, pink
#' * `grey_palette`: light blue, orange, pink, light gray
#' * `quilt_palette`: light gray, pink, purple, light blue, orange
#' * `two_color_palette`: dark blue, pink
#' * `quilt_three_color_palette`: light gray, translucent light blue, light blue
#' * `two_color_gray`: dark blue, light gray
#'
#' Single colors: `dd_dark_blue` (`"#3564ED"`), `dd_light_blue` (`"#72B4F3"`), `dd_orange` (`"#F38672"`), `dd_purple` (`"#7E43B6"`), `dd_gray` (`gray(0.2)`), `dd_pink` (`"#C6227F"`), `dd_light_gray` (`gray(0.8)`), and the translucent `dd_dark_blue_alpha` and `dd_light_blue_alpha`.
#'
#' @examples
#' dd_palette("three_color_palette")
#' dd_palette("quilt_palette", n = 3)
#'
#' @return A character vector of hex color codes.
#'
#' @export
#'
dd_palette <- function(name, n) {
  pal <- dd_palettes[[name]]
  if (is.null(pal))
    stop("Palette not found.")

  if (missing(n)) {
    n <- length(pal)
  }

  pal[1:n]
  }

