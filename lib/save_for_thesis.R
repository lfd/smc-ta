save_for_thesis <- function(filename, plot = last_plot(), prop = 0.75, full_page = F, ...) {
  mm2inch <- function(mm) mm / 25.4

  width <- mm2inch(156)
  height <- prop * width
  if (full_page){
    height <- mm2inch(225)
  }

  path <- OUT_DIR
  f_path <- function(filename) file.path(path, filename)

  # PNG for preview purposes
  ggplot2::ggsave(
    filename = str_c("preview-", filename, ".png"),
    plot,
    path = path,
    width = width * 25.4,
    height = height * 25.4,
    units = "mm",
    ...
  )

  tikzDevice::tikz(
    f_path(str_c(filename, ".tex")),
    width = width,
    height = height,
    sanitize = TRUE,
    # pointsize = 11,
    engine = "pdftex",
    timestamp = T,
  )
  print(plot)
  dev.off()
}
