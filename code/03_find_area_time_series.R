library(magrittr)
library(ggplot2)

load("data/weapons.RData")
artefacts <- weapons

artefacts %<>%
  dplyr::group_by(
    find_area
  ) %>%
  dplyr::mutate(
    n = dplyr::n()
  ) %>%
  dplyr::filter(
    n >= 5
  )

area_amount <- artefacts %>% 
  dplyr::group_by(
    find_area
  ) %>%
  dplyr::summarise(
    n = dplyr::n()
  )

artefact_timeseries_df <- aoristAAR::aorist(
  artefacts,
  split_vars = c("find_area"),
  from = "dating_typology_start",
  to = "dating_typology_end",
  method = "weight"
)

artefact_timeseries_df %<>% dplyr::filter(
  sum != 0
)

artefact_timeseries_df_roll_avg <- artefact_timeseries_df %>%
  dplyr::group_by(
    find_area
  ) %>%
  dplyr::arrange(
    date
  ) %>%
  dplyr::mutate(
    roll_sum = zoo::rollapply(
      data = sum, 
      width = 10, 
      FUN = mean, 
      align = "right", 
      fill = NA, 
      na.rm = T
    )
  ) %>%
  dplyr::mutate(
    total_sum = sum(sum, na.rm = T),
    cum_sum = cumsum(sum),
    fit = cum_sum/total_sum,
    center = abs(fit - 0.5) == min(abs(fit - 0.5), na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(
    -total_sum, -cum_sum, -fit
  )

find_area_centers <- artefact_timeseries_df_roll_avg %>%
  dplyr::filter(
    center
  )

find_area_levels <- find_area_centers %>%
  dplyr::group_by(
    find_area
  ) %>%
  dplyr::filter(dplyr::row_number() == 1) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(
    date
  ) %$%
  find_area %>%
  as.character()

artefact_timeseries <- artefact_timeseries_df_roll_avg %>%
  dplyr::mutate(
    find_area = factor(find_area, levels = find_area_levels)
  )

p <- ggplot() +
  ggridges::geom_density_ridges(
    data = artefact_timeseries,
    mapping = aes(x = date, y = find_area, height = roll_sum),
    stat = "identity",
    alpha = 0.5
  ) +
  geom_point(
    data = find_area_centers,
    mapping = aes(x = date, y = find_area)
  ) +
  geom_label(
    data = area_amount,
    mapping = aes(x = -800, y = find_area, label = n),
    size = 5,
    fill = "darkgrey",
    color = "white"
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 15),
    axis.text.y = element_text(hjust = 0)
  ) +
  xlim(-800, -400) +
  xlab("Year BC") +
  ylab("")
  
ggsave(
  filename = "03_find_area_time_series.png",
  plot = p,
  device = "png",
  path = "plots",
  width = 200,
  height = 180,
  units = "mm",
  dpi = 300
)

