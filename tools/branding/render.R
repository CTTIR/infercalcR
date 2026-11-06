# The infercalcR logo: a CTTIR v5 hex drawn entirely in R with the shared CTTIR
# renderer in R/ (primitives, palette, tokens). Statistical inference family
# (`methods`), shared with inferguidR and infereportR.
# Motif: a forest plot of three estimates with their confidence intervals
# and the pooled diamond, around a dashed null line on an axis.
#
#   Rscript tools/branding/render.R /path/to/review   # review copy
#   Rscript tools/branding/render.R --apply           # update man/figures
#
# Requires the installed JetBrains Mono ExtraBold for the name: the supplied
# v5 names are heavier than the bundled SemiBold cut, and ExtraBold is the
# closest measured match.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L || !nzchar(args)) stop("Supply an output directory or --apply.")
script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
branding <- dirname(normalizePath(script, mustWork = TRUE))
repository <- dirname(dirname(branding))
Sys.setenv(CTTIR_FONTCONFIG = "off")
source(file.path(branding, "R", "cttir_hex.R"))
if (!capabilities("cairo")) stop("Cairo graphics are required.")

installed <- systemfonts::system_fonts()
if (!any(installed$family == "JetBrains Mono" & installed$style == "ExtraBold")) {
  stop("Install JetBrains Mono ExtraBold.")
}
name_font <- "JetBrains Mono ExtraBold"
cttir_name <- function(name, y = .049, max_width = .90, size = 34, colour = "#212121",
                       font = name_font) {
  repeat {
    g <- grid::textGrob(name, gp = grid::gpar(fontfamily = name_font, fontsize = size))
    if (grid::convertWidth(grid::grobWidth(g), "npc", valueOnly = TRUE) <= max_width ||
        size <= 12) break
    size <- size - .25
  }
  grid::grid.text(name, .5, y, gp = grid::gpar(fontfamily = name_font, fontsize = size,
                                               col = colour))
  invisible(size)
}

infercalcR_motif <- function(s, small = FALSE) {
  g <- cttir_primitives(s, small)
  a <- s$mid; p <- s$paper; l <- s$light
  g$L(c(.50, .50), c(.22, .82), l, if (small) 4 else 3, lty = if (small) 1 else 2)
  g$L(c(.16, .84), c(.19, .19), l, if (small) 5 else 4)
  if (!small) for (x in c(.25, .50, .75)) g$L(c(x, x), c(.19, .155), l, 3.5)
  rows <- list(c(.72, .45, .24, .64), c(.59, .64, .44, .84), c(.46, .56, .34, .76))
  for (r in rows) {
    g$L(c(r[3], r[4]), c(r[1], r[1]), l, if (small) 7 else 6)
    if (!small) for (x in r[3:4]) g$L(c(x, x), r[1] + c(-.03, .03), l, 4)
    g$B(r[2], r[1], .10, .10, a, r = .014, shadow = TRUE)
  }
  g$P(c(.38, .55, .72, .55), c(.315, .38, .315, .25), p, shadow = TRUE)
}
v5_motif <- cttir_motif
cttir_motif <- function(name, s, small = FALSE) {
  if (name == "infercalcR") infercalcR_motif(s, small) else v5_motif(name, s, small)
}

output <- if (identical(args, "--apply")) tempfile("infercalcR-logo-") else args
dir.create(output, recursive = TRUE, showWarnings = FALSE)
font <- cttir_token("type", "brand_font")
for (mode in c("light", "dark", "mono")) for (layout in c("hex", "lockup")) {
  stem <- paste0(layout, if (mode == "light") "" else paste0("-", mode))
  d <- cttir_dimensions(layout)
  draw <- function() cttir_draw("infercalcR", "methods", mode, layout)
  cttir_device(function() grDevices::svg(file.path(output, paste0(stem, ".svg")),
    width = d$width, height = d$height, bg = "transparent", family = font), draw)
  cttir_device(function() grDevices::png(file.path(output, paste0(stem, ".png")),
    width = 1600L, height = 1848L, res = 1600 / d$width, bg = "transparent",
    type = "cairo", antialias = "gray"), draw)
}

if (identical(args, "--apply")) {
  targets <- c(
    "lockup.svg" = "man/figures/logo.svg",
    "lockup.png" = "man/figures/logo.png",
    "lockup-dark.svg" = "man/figures/logo-dark.svg",
    "lockup-dark.png" = "man/figures/logo-dark.png",
    "lockup-mono.svg" = "man/figures/logo-mono.svg",
    "hex.svg" = "man/figures/logo-symbol.svg")
  dir.create(file.path(repository, "man", "figures"), showWarnings = FALSE)
  for (from in names(targets)) {
    if (!file.copy(file.path(output, from), file.path(repository, targets[[from]]),
                   overwrite = TRUE)) stop("Could not update ", targets[[from]])
  }
  unlink(output, recursive = TRUE)
  message("Updated the infercalcR logo in man/figures.")
} else message("infercalcR logo rendered into ", normalizePath(output))
