BASE.SIZE <- 10

theme_paper_base <- function() {
  ggplot2::theme_bw(base_size = BASE.SIZE) +
    theme(
      axis.title.x = element_text(size = BASE.SIZE),
      axis.title.y = element_text(size = BASE.SIZE),
      legend.title = element_text(size = BASE.SIZE),
      legend.position = "top",
      plot.margin = unit(c(0, 0, 0, 0), "cm")
    )
}
theme_paper_ridges <- function() {
	ggridges::theme_ridges() +
    theme(
      axis.title.x = element_text(size = BASE.SIZE),
      axis.title.y = element_text(size = BASE.SIZE),
      legend.title = element_text(size = BASE.SIZE),
      legend.position = "top",
      plot.margin = unit(c(0, 0, 0, 0), "cm")
    )
}

do.save.tikz <- function(g, out.name, .width, .height) {
  tikz(str_c(OUTDIR, "/", out.name, ".tex"), width = .width, height = .height, sanitize = TRUE)
  print(g)
  dev.off()
}

library(tikzDevice)
INCH.PER.CM <- 1 / 2.54

## Adapt for your paper layout
WIDTH <- 18.1 * INCH.PER.CM
COL.WIDTH <- 8.85 * INCH.PER.CM

BASE.SIZE <- 10
FORMAT <- "tex"

options(
  tikzSanitizeCharacters = c("%", "$", "}", "{", "^", "_", "→"),
  tikzReplacementCharacters = c("\\%", "\\$", "\\}", "\\{", "\\^{}", "\\textunderscore", "$\\to$"),
  tikzLatexPackages = c(
    getOption("tikzLatexPackages"),
    "\\usepackage[T1]{fontenc}"
  )
)
