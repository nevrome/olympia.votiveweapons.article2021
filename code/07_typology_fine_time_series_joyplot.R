library(magrittr)
library(ggplot2)
library(viridis)

load("data/weapons.RData")
artefacts <- weapons %>%
  dplyr::mutate(
    typology_fine = ifelse(
      !is.na(typology_class_4), stringr::str_trunc(paste0(typology_class_2, " ~ ", typology_class_4), 40), as.character(typology_class_2)
    )
  )

# artefact_timeseries_df$typology_fine <- gsub("+", "\n", artefact_timeseries_df$typology_fine)
# levels(artefact_timeseries_df$typology_fine) <- gsub("+", "\n", levels(artefact_timeseries_df$typology_fine))

artefacts <- artefacts %>% 
  dplyr::group_by(
    typology_fine
  ) %>%
  dplyr::mutate(
    n = dplyr::n()
  ) %>%
  dplyr::filter(
    n >= 10
  ) %>%
  dplyr::ungroup()

type_fine_amount <- artefacts %>% 
  dplyr::group_by(
    typology_fine
  ) %>%
  dplyr::summarise(
    n = dplyr::n(),
    typology_class_2 = dplyr::first(typology_class_2)
  )

artefact_timeseries_df <- aoristAAR::aorist(
  artefacts,
  split_vars = c("typology_class_2", "typology_fine"),
  from = "dating_typology_start",
  to = "dating_typology_end"
)

artefact_timeseries_df %<>% dplyr::filter(
  sum != 0
)

artefact_timeseries_df <- artefact_timeseries_df %>%
  dplyr::group_by(
    typology_fine
  ) %>%
  dplyr::arrange(
    date
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

typology_fine_centers <- artefact_timeseries_df %>%
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

artefact_timeseries <- artefact_timeseries_df %>%
  dplyr::mutate(
    typology_fine = factor(typology_fine, levels = typology_fine_levels)
  )

p <- ggplot() +
  ggridges::geom_density_ridges(
    data = artefact_timeseries,
    mapping = aes(x = date, y = typology_fine, height = sum, fill = typology_class_2),
    stat = "identity",
    alpha = 0.5
  ) +
  geom_point(
    data = typology_fine_centers,
    mapping = aes(x = date, y = typology_fine, color = typology_class_2),
    size = 3
  ) +
  geom_label(
    data = type_fine_amount,
    mapping = aes(x = -800, y = typology_fine, label = n, fill = typology_class_2),
    size = 5,
    color = "white"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 17),
    axis.text.y = element_text(size = 15, hjust = 0),
    axis.title.x = element_text(size = 17),
    legend.text = element_text(size = 15),
    legend.position = "bottom",
    legend.direction = "horizontal"
  ) +
  guides(fill = guide_legend("", nrow = 6), color = guide_legend("", nrow = 6)) +
  xlim(-800, -400) +
  ylab("") +
  xlab("Year BC") +
  scale_fill_manual(
    values = wesanderson::wes_palette(
      "Zissou1", 
      n = length(unique(artefact_timeseries$typology_class_2)), 
      type = "continuous"
    )
  ) +
  scale_color_manual(
    values = wesanderson::wes_palette(
      "Zissou1", 
      n = length(unique(artefact_timeseries$typology_class_2)), 
      type = "continuous"
    )
  )

ggsave(
  filename = "07_typology_fine_time_series.png",
  plot = p,
  device = "png",
  path = "plots",
  width = 300,
  height = 620,
  units = "mm",
  dpi = 300
)

