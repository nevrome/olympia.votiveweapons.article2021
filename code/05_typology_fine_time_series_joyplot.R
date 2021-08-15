library(magrittr)
library(ggplot2)

#### data preparation ####

# load data
load("data/weapons.RData")
artefacts <- weapons

# simplify names
artefacts <- artefacts %>%
  dplyr::mutate(
    typology_class_3 = gsub(" \\(Nach Randornamenten\\)", "", typology_class_3)
  )

# count artefacts and remove artefact categories with less than 10 entries 
artefacts <- artefacts %>%
  dplyr::group_by(
    typology_class_2, typology_class_3, typology_class_4
  ) %>%
  dplyr::mutate(
    n = dplyr::n()
  ) %>% 
  dplyr::filter(
    n >= 10
  ) %>%
  dplyr::ungroup()

# create a variable that combines typology_class_2 and typology_class_3
artefacts <- artefacts %>%
  dplyr::mutate(
    typology_fine = dplyr::case_when(
      !is.na(typology_class_3) & !is.na(typology_class_4) ~ 
        paste(typology_class_2, typology_class_3, typology_class_4, sep = " ~ "),
      !is.na(typology_class_3) ~ 
        paste(typology_class_2, typology_class_3, sep = " ~ "),
      TRUE ~ as.character(typology_class_2)
     )
  )

# count again for the plot
type_fine_amount <- artefacts %>%
  dplyr::group_by(
    typology_fine
  ) %>%
  dplyr::summarise(
    n = dplyr::n(),
    typology_class_2 = dplyr::first(typology_class_2)
  )

# calculate time series
artefact_timeseries_df <- aoristAAR::aorist(
  artefacts,
  split_vars = c("typology_class_2", "typology_fine"),
  from = "dating_typology_start",
  to = "dating_typology_end",
  method = "weight"
)

# find centers of category distributions
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
  dplyr::filter(center)

# order by center points
typology_fine_levels <- typology_fine_centers %>%
  dplyr::group_by(
    typology_fine
  ) %>%
  dplyr::filter(dplyr::row_number() == 1) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(date) %$%
  typology_fine %>%
  as.character()

artefact_timeseries <- artefact_timeseries_df %>%
  dplyr::mutate(
    typology_fine = factor(typology_fine, levels = typology_fine_levels)
  )
# 
# type_fine_amount <- type_fine_amount %>%
#   dplyr::mutate(
#     typology_fine = factor(typology_fine, levels = typology_fine_levels)
#   )

#### plot ####
p <- ggplot() +
  ggridges::geom_density_ridges(
    data = artefact_timeseries,
    mapping = aes(x = date, y = typology_fine, height = sum, fill = typology_class_2),
    stat = "identity",
    alpha = 0.5,
    size = 0.2,
    scale = 1.5
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
    legend.text = element_text(size = 16),
    legend.position = "bottom",
    legend.direction = "horizontal"
  ) +
  guides(fill = guide_legend("", nrow = 5), color = guide_legend("", nrow = 5)) +
  xlim(-800, -400) +
  ylab("") +
  xlab("Year BC") +
  scale_fill_manual(
    values = wesanderson::wes_palette(
      "Darjeeling1", 
      n = length(unique(artefact_timeseries$typology_class_2)), 
      type = "continuous"
    )
  ) +
  scale_color_manual(
    values = wesanderson::wes_palette(
      "Darjeeling1", 
      n = length(unique(artefact_timeseries$typology_class_2)), 
      type = "continuous"
    )
  )

ggsave(
  filename = "05_typology_fine_time_series.png",
  plot = p,
  device = "png",
  path = "plots",
  width = 400,
  height = 620,
  units = "mm",
  dpi = 300
)
