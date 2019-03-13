library(magrittr)
library(ggplot2)

load("data/weapons.RData")
artefacts <- weapons

artefacts <- artefacts %>%
  dplyr::filter(
    !is.na(dating_typology_start)
  )

artefact_timeseries_df <- aoristAAR::aorist(
  artefacts,
  split_vars = c("typology_class_3"),
  from = "dating_typology_start",
  to = "dating_typology_end"
)

artefact_timeseries_df_roll_avg <- artefact_timeseries_df %>%
  dplyr::group_by(
    typology_class_3
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

typology_class_3_centers <- artefact_timeseries_df_roll_avg %>%
  dplyr::filter(
    center
  )

typology_class_3_levels <- typology_class_3_centers %>%
  dplyr::group_by(
    typology_class_3
  ) %>%
  dplyr::filter(dplyr::row_number() == 1) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(
    date
  ) %$%
  typology_class_3 %>%
  as.character()

artefact_timeseries <- artefact_timeseries_df_roll_avg %>%
  dplyr::mutate(
    typology_class_3 = factor(typology_class_3, levels = typology_class_3_levels)
  )

p <- ggplot() +
  ggridges::geom_density_ridges(
    data = artefact_timeseries,
    mapping = aes(x = date, y = typology_class_3, height = roll_sum, fill = typology_class_3),
    stat = "identity",
    alpha = 0.5
  ) +
  geom_point(
    data = typology_class_3_centers,
    mapping = aes(x = date, y = typology_class_3),
    color = "black"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1),
    legend.position = "bottom"
  )

ggsave(
  filename = "03_typology_class_3_time_series.png",
  plot = p,
  device = "png",
  path = "plots",
  width = 300,
  height = 200,
  units = "mm",
  dpi = 300
)

