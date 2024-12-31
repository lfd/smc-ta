library("here")
library("rlang", exclude = c("type_of"))

NAME <- "jittertest-ttp"
OUT_DIR <<- here("img", NAME)
DATA_DIR <<- here("..", "data", NAME)

source("includes.R")

config <- config::get(config = NAME, use_parent = F)

mapping <- list(
  "expected→6→4→real" = 1,
  "expected→real" = 2
)
