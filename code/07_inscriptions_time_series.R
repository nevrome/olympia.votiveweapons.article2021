library(magrittr)
library(ggplot2)

load("data/weapons.RData")

# artefacts with inscriptions
artefacts <- weapons %>% dplyr::filter(
  sapply(
    weapons$modification_classes,
    function(x) {
      "Inschrift" %in% x
    }
  )
) %>%
  dplyr::filter(
    !is.na(dating_typology_start) &
      !is.na(find_area)
  ) %>%
  dplyr::group_by(
    find_area
  ) %>%
  dplyr::arrange(
    find_area, dating_typology_start
  ) %>%
  dplyr::ungroup()

find_area_order <- artefacts %>%
  dplyr::group_by(
    find_area
  ) %>%
  dplyr::summarise(
    dating_typology_start = min(dating_typology_start)
  ) %>%
  dplyr::arrange(
    dating_typology_start
  ) %$%
  find_area %>%
  as.character()

artefacts$general_id <- factor(artefacts$general_id, as.character(artefacts$general_id))
artefacts$find_area <- forcats::fct_relevel(artefacts$find_area, rev(find_area_order))

p <- artefacts %>%
  ggplot() +
  geom_point(
    mapping = aes(
      x = general_id,
      y = dating_typology_start,
      color = typology_class_3
    ),
    position = position_dodge(1),
    shape = 18
  ) +
  geom_linerange(
    mapping = aes(
      x = general_id,
      ymin = dating_typology_start,
      ymax = dating_typology_end,
      color = typology_class_3
    ),
    position = position_dodge(1)
  ) +
  coord_flip() +
  facet_grid(
    rows = dplyr::vars(find_area),
    scales = "free_y",
    space = "free_y"
  ) +
  theme_bw() +
  theme(
    strip.text.y = element_text(size = 11, angle = 0),
    strip.background = element_rect(fill = NA)
  )

ggsave(
  filename = "07_inscriptions_time_series.png",
  plot = p,
  device = "png",
  path = "plots",
  width = 300,
  height = 250,
  units = "mm",
  dpi = 300
)
