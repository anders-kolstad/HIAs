test_most_common <- function(x) {
  x |>
  as_tibble() |>
  count(natureType, sort=T) |>
  mutate(natureType = fct_reorder(natureType, n)) |>
  ggplot(aes(x = natureType, y = n))+
  geom_col()+
  coord_flip()
}