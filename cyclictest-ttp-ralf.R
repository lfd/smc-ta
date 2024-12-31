library("here")
library("rlang", exclude = c("type_of"))

NAME <- "cyclictest-ttp-ralf"
OUT_DIR <<- here("img", NAME)
DATA_DIR <<- here("..", "data", NAME)

dir.create(OUT_DIR, showWarnings = FALSE)

source("includes.R")

config <- config::get(config = NAME, use_parent = F)



mapping <- list(
  "expected→2→6→4→3→1→real" = 1,
  "expected→4→2→6→4→3→1→real" = 2,
  "expected→2→6→4→3→6→4→1→real" = 3,
  "expected→4→1→6→4→2→6→4→3→1→real" = 4,
  "expected→6→4→3→6→4→1→real" = 5,
  "expected→1→2→6→4→3→1→real" = 6,
  "expected→3→6→4→1→6→4→2→6→4→3→1→real" = 7,
  "expected→1→6→4→2→6→4→3→1→real" = 8,
  "expected→6→4→1→6→4→2→6→4→3→1→real" = 9,
  "expected→6→4→2→6→4→3→1→real" = 10,
  "expected→4→1→2→6→4→3→1→real" = 11,
  "expected→6→4→1→6→4→1→2→6→4→3→1→real" = 12,
  "expected→4→1→6→4→1→2→6→4→3→1→real" = 13,
  "expected→1→6→4→1→2→6→4→3→1→real" = 14,
  "expected→6→4→1→2→6→4→3→1→real" = 15,
  "expected→3→6→4→1→6→4→1→2→6→4→3→1→real" = 16
)
