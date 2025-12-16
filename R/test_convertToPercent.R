test_convertToPercent <- function(x) {
  x %>%
  filter(variable != "7GR-GI") |>
  ggplot() +
  theme_bw() +
  geom_histogram(aes(x = value),
    binwidth = 1,
    color = "orange",
    fill = "orange"
  ) +
  xlab("%") +
  facet_wrap(. ~ variable,
    scales = "free"
  )
}