if (!exists("event.log")) {
  abort("Object not found")
}

require(ggh4x)
require(dplyr)
require(ggplot2)

filename <- file.path(DATA_DIR, "events-meta.csv")
stopifnot(file.exists(filename))

format_percent <- scales::label_percent(accuracy = 1)

data <- filename |>
  readr::read_csv(col_names = T) |>
  mutate(
    Source = as.factor(Source),
    Event = as.factor(Event),
    Used = Total - After - Before
  )

max_total <- max(data$Total)

data <- data |>
  mutate(
    max_total = max(Total),
    other = (Used < 0.005 * max_total),
    Event = factor(if_else(other, "other", Event)),
    .by = Source
  ) |>
  select(!c(max_total, other))

data |>
  select(!Total) |>
  tidyr::pivot_longer(!c(Event, Source)) |>
  mutate(
    filtered = (name != "Used"),
    name = if_else(filtered, str_c("Filtered (", name, ")"), "Unfiltered"),
  ) |>
  mutate(
    cumulative_value = cumsum(value),
    mid_value = cumulative_value - value / 2, # Calculate the midpoint for labels,
    .by = c(Event, Source)
  ) |>
  ggplot(aes(x = interaction(Event, Source))) +
  geom_bar(aes(y = value, fill = name), stat = "identity", position = "stack") +
  geom_label(data = data, aes(y = 0, label = format_percent(Used / Total), fill = "Unfiltered"), show.legend = FALSE, nudge_y = 0.1 * max_total) +
  scale_x_discrete(NULL, guide = "axis_nested") +
  scale_y_continuous("Count") +
  scale_fill_discrete("") +
  guides()

save_for_thesis(filename = "events-meta")
dev.off()
