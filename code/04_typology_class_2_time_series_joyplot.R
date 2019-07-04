library(magrittr)
library(ggplot2)

#### data preparation ####

# load data
load("data/weapons.RData")
artefacts <- weapons

# count by artefact category
types_amount <- artefacts %>% 
  dplyr::group_by(
    typology_class_2
  ) %>%
  dplyr::summarise(
    n = dplyr::n()
  )

# time series by artefact category
artefact_timeseries_df <- aoristAAR::aorist(
  artefacts,
  split_vars = c("typology_class_2"),
  from = "dating_typology_start",
  to = "dating_typology_end",
  method = "weight"
)

# remove time steps without information
artefact_timeseries_df %<>% dplyr::filter(
  sum != 0
)

# find centers of category distributions
artefact_timeseries <- artefact_timeseries_df %>%
  dplyr::group_by(
    typology_class_2
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

typology_class_2_centers <- artefact_timeseries %>%
  dplyr::filter(
    center
  )

# order by center points
typology_class_2_levels <- typology_class_2_centers %>%
  dplyr::group_by(
    typology_class_2
  ) %>%
  dplyr::filter(dplyr::row_number() == 1) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(
    date
  ) %$%
  typology_class_2 %>%
  as.character()

artefact_timeseries %<>%
  dplyr::left_join(
    types_amount
  ) %>%
  dplyr::mutate(
    typology_class_2 = factor(typology_class_2, levels = typology_class_2_levels)
  )

#### plot ####
p <- ggplot() +
  ggridges::geom_density_ridges(
    data = artefact_timeseries,
    mapping = aes(x = date, y = typology_class_2, height = sum, fill = n),
    stat = "identity",
    alpha = 0.5
  ) +
  geom_point(
    data = typology_class_2_centers,
    mapping = aes(x = date, y = typology_class_2)
  ) +
  geom_label(
    data = types_amount,
    mapping = aes(x = -980, y = typology_class_2, label = n, fill = n),
    size = 4,
    color = "white"
  ) +
  scale_fill_gradientn(
    colours = wesanderson::wes_palette(
      "Zissou1", 
      type = "continuous"
    ),
    limits = c(0, 850)
  ) +
  guides(
    fill = guide_colorbar(title = "Number of Artefacts")
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 15),
    axis.text.y = element_text(hjust = 0),
    legend.key.width = unit(1.5, "cm"),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 13),
    plot.margin = unit(c(0.5,0.5,4,0.5), "lines"),
    legend.position = c(0.11, -0.16),
    legend.direction = "horizontal"
  ) +
  xlim(-1000, -400) +
  xlab("Year BC") +
  ylab("")
  
ggsave(
  filename = "04_typology_class_2_time_series.png",
  plot = p,
  device = "png",
  path = "plots",
  width = 200,
  height = 160,
  units = "mm",
  dpi = 300
)

