
transform1 <- function(dt, context=NULL) {
  by <- c("run", context)
  required_columns <- c("timestamp", "event", by)
  stopifnot(all(required_columns %in% colnames(dt)))

  dt[, .(
  	event.from = event,
  	event.to = shift(event, type = "lead"),
  	#timestamp = timestamp,
  	#start.since = offset,
  	duration = shift(timestamp, type = "lead") - timestamp
  	), by = by ]
}
