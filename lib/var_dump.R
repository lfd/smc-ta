dump_var <- function(name = NULL, path = NULL, envir = parent.frame()) {
	filename <-paste0(name, ".rda")

  save(
    list = name,
    envir = envir,
    # use do.call to handle both cases,path==NULL and path!=NULL
    file = do.call("file.path", as.list(c(path, filename)))
  )
}
