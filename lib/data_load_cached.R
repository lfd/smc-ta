data_load_cached <- function(name, envir = .GlobalEnv, verbose = FALSE) {
  path_for <- function(name, path = DATA_DIR) {
    filename <- paste0(name, ".rda")
    do.call("file.path", as.list(c(path, filename)))
  }

  path <- path_for(name)
  if (!file.exists(path)) {
    warn(sprintf("Cannot find file for '%s'", name),
      body = sprintf("Looked for '%s'", path)
    )
    return()
  }

  #  It is considerably safer to use envir = to load into a different environment, or to attach(file) which load()s into a new entry in the search path.
  load(path, envir, verbose)

  inform(sprintf("Loaded cached '%s'", name))
  # TODO: remove this line
  # rm(var, envir)

  invisible(envir[[name]])
}
