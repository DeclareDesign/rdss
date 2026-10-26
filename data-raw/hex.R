# Hex logo: a swatch of the line-and-dot pattern from the lower right of the
# book's cover, with the package name in the cover's magenta. The cover is only
# available as a 600px JPEG, so the pattern is redrawn rather than cropped.
# Colors are sampled from that JPEG; Montserrat stands in for the cover's
# geometric sans.

library(tidyverse)
library(sysfonts)
library(showtext)
library(magick)

font_add_google("Montserrat", "montserrat", regular.wt = 800)
showtext_auto()
showtext_opts(dpi = 120)

magenta <- "#B5257A"
navy <- "#22395C"
steel <- "#6584A3"
pale <- "#C3D2E1"
lavender <- "#A09EB6"

# Pointy-top hexagon, height 2, centered at the origin
hexagon <-
  tibble(angle = seq(90, 450, by = 60) * pi / 180) |>
  mutate(x = cos(angle), y = sin(angle))

# Pattern ----

# Vertical lines rise from below the frame and end in a dot
verticals <-
  tribble(
    ~x, ~y_top, ~dot_size, ~dot_color, ~color,
    -0.62, -0.30, 3.5, navy, steel,
    -0.47, -0.55, 1.6, navy, steel,
    -0.30, -0.12, 4.2, navy, steel,
    -0.12, -0.42, 2.2, lavender, pale,
    0.04, -0.05, 2.0, navy, steel,
    0.20, -0.30, 4.6, navy, steel,
    0.36, 0.08, 2.4, lavender, pale,
    0.52, -0.20, 3.8, navy, steel,
    0.72, -0.40, 1.8, navy, steel
  ) |>
  mutate(y_bottom = -1.1)

# Horizontal lines, each with a dot at one end
horizontals <-
  tribble(
    ~y, ~x_start, ~x_end, ~dot_x, ~dot_size, ~dot_color, ~color,
    -0.22, -0.30, 0.90, -0.30, 0, navy, steel,
    -0.50, -0.95, 0.20, 0.20, 2.0, navy, steel,
    -0.68, -0.12, 0.95, -0.12, 3.0, navy, steel,
    -0.80, -0.95, -0.25, -0.25, 1.6, lavender, pale,
    0.06, 0.05, 0.95, 0.05, 1.4, lavender, pale
  )

# Floating dots with no line
loose_dots <-
  tribble(
    ~x, ~y, ~size, ~color,
    -0.68, 0.02, 1.8, lavender,
    -0.40, 0.14, 1.2, pale,
    0.60, 0.22, 1.5, lavender,
    -0.05, -0.78, 3.6, navy,
    0.42, -0.62, 1.4, navy,
    -0.78, -0.62, 1.2, navy,
    0.63, -0.86, 3.0, navy
  )

# Columns of stacked triangles
make_column <- function(x, y_start, n, color, direction = "up", step = 0.042) {
  tibble(
    x = x,
    y = y_start + step * (seq_len(n) - 1),
    color = color,
    shape = if_else(direction == "up", 17, 25)
  )
}

triangles <-
  bind_rows(
    make_column(-0.54, -0.82, 9, lavender),
    make_column(-0.21, -0.70, 7, navy),
    make_column(0.28, -0.98, 6, lavender, "down"),
    make_column(0.64, -0.62, 16, navy)
  )

# Dotted vertical lines of small squares
make_dotted <- function(x, y_start, n, color, step = 0.025) {
  tibble(x = x, y = y_start + step * (seq_len(n) - 1), color = color)
}

dotted <-
  bind_rows(
    make_dotted(-0.38, -1.0, 14, lavender),
    make_dotted(0.12, -1.0, 9, navy),
    make_dotted(0.80, -0.30, 12, lavender)
  )

# Plot ----

gg_hex <-
  ggplot() +
  geom_polygon(data = hexagon, aes(x, y), fill = "white") +
  geom_segment(
    data = horizontals,
    aes(x = x_start, xend = x_end, y = y, yend = y, color = color),
    linewidth = 0.9
  ) +
  geom_segment(
    data = verticals,
    aes(x = x, xend = x, y = y_bottom, yend = y_top, color = color),
    linewidth = 0.9
  ) +
  geom_point(data = dotted, aes(x, y, color = color), shape = 15, size = 0.9) +
  geom_point(
    data = triangles,
    aes(x, y, color = color, fill = color, shape = shape),
    size = 1.9
  ) +
  geom_point(
    data = filter(horizontals, dot_size > 0),
    aes(dot_x, y, color = dot_color, size = dot_size)
  ) +
  geom_point(data = verticals, aes(x, y_top, color = dot_color, size = dot_size)) +
  geom_point(data = loose_dots, aes(x, y, color = color, size = size)) +
  annotate(
    "text",
    x = 0, y = 0.48, label = "rdss",
    family = "montserrat", color = magenta, size = 34
  ) +
  geom_polygon(data = hexagon, aes(x, y), fill = NA, color = navy, linewidth = 3) +
  scale_color_identity() +
  scale_fill_identity() +
  scale_shape_identity() +
  scale_size_identity() +
  coord_fixed(xlim = c(-cos(pi / 6), cos(pi / 6)), ylim = c(-1, 1), expand = FALSE) +
  theme_void()

# Render, then cut away everything outside the hexagon ----

# A 5in-tall canvas at 120dpi gives the usual 519x600 sticker, with marks sized
# in ggplot's millimetres at the scale of the cover
width_in <- 5 * cos(pi / 6)
height_in <- 5

ggsave(
  "man/figures/logo_full.png", gg_hex,
  width = width_in, height = height_in, dpi = 120, bg = "transparent"
)

logo <- image_read("man/figures/logo_full.png")

mask <-
  image_blank(image_info(logo)$width, image_info(logo)$height, "none") |>
  image_draw()
polygon(
  (hexagon$x + cos(pi / 6)) / (2 * cos(pi / 6)) * image_info(logo)$width,
  (1 - hexagon$y) / 2 * image_info(logo)$height,
  col = "black", border = NA
)
dev.off()

logo |>
  image_composite(mask, operator = "DstIn") |>
  image_write("man/figures/logo.png")

file.remove("man/figures/logo_full.png")
