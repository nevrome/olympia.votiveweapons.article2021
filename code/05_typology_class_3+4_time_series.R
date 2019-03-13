library(magrittr)
library(ggplot2)

load("data/weapons.RData")
artefacts <- weapons %>%
  dplyr::mutate(
    typology_fine = ifelse(!is.na(typology_class_4), paste0(typology_class_2, "+", typology_class_4), typology_class_2)
  )

artefact_timeseries_df <- aoristAAR::aorist(
  artefacts,
  split_vars = c("typology_class_2", "typology_fine"),
  from = "dating_typology_start",
  to = "dating_typology_end"
)

artefact_timeseries_df_roll_avg <- artefact_timeseries_df %>%
  dplyr::group_by(
    typology_fine
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

typology_fine_centers <- artefact_timeseries_df_roll_avg %>%
  dplyr::filter(
    center
  )

typology_fine_levels <- typology_fine_centers %>%
  dplyr::group_by(
    typology_fine
  ) %>%
  dplyr::filter(dplyr::row_number() == 1) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(
    date
  ) %$%
  typology_fine %>%
  as.character()

artefact_timeseries <- artefact_timeseries_df_roll_avg %>%
  dplyr::mutate(
    typology_fine = factor(typology_fine, levels = typology_fine_levels)
  )

p <- ggplot() +
  ggridges::geom_density_ridges(
    data = artefact_timeseries,
    mapping = aes(x = date, y = typology_fine, height = roll_sum, fill = typology_class_2),
    stat = "identity",
    alpha = 0.5
  ) +
  geom_point(
    data = typology_fine_centers,
    mapping = aes(x = date, y = typology_fine, color = typology_class_2)
  ) +
  geom_point(
    data = typology_fine_centers,
    mapping = aes(x = -1200, y = typology_fine, color = typology_class_2),
    size = 3
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1),
    legend.position = "bottom"
  )

ggsave(
  filename = "05_typology_class_3+4_time_series.png",
  plot = p,
  device = "png",
  path = "plots",
  width = 300,
  height = 500,
  units = "mm",
  dpi = 300
)

