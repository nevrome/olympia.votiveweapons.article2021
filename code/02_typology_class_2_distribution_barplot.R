library(magrittr)
library(ggplot2)

load("data/weapons_unfiltered.RData")
artefacts <- weapons_unfiltered

type_count <- artefacts %>%
  dplyr::filter(
    !is.na(typology_class_2)
  ) %>%
  # dplyr::mutate(
  #   dated = dplyr::case_when(
  #     
  #   )
  #     
  #     
  #     ifelse(!is.na(dating_typology_start) & !is.na(dating_typology_end), "typological dating", "no dating information")
  # ) %>%
  dplyr::group_by(
    typology_class_2#, dated
  ) %>%
  dplyr::summarise(
    n = dplyr::n()
  ) %>%
  dplyr::ungroup()

general_count <- artefacts %>%
  dplyr::filter(
    !is.na(typology_class_2)
  ) %>%
  dplyr::group_by(
    typology_class_2
  ) %>%
  dplyr::summarise(
    n = dplyr::n()
  ) %>%
  dplyr::arrange(
    n
  )

type_count$typology_class_2 <- factor(type_count$typology_class_2, levels = general_count$typology_class_2)

p <- type_count %>%
  ggplot() +
  geom_bar(
    aes(x = typology_class_2, y = n),#, fill = dated),
    stat = "identity"
  ) +
  geom_label(
    data = general_count,
    aes(x = typology_class_2, y = -100, label = n),
    size = 3,
    fill = "darkgrey",
    color = "white"
  ) +
  scale_fill_manual(
    values = c("darkgrey", "black")
  ) +
  theme_bw() +
  theme(
    legend.position = c(0.95, 0.11),
    legend.justification = c(1, 1),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    axis.text.y = element_text(size = 10, hjust = 0),
    axis.text.x = element_text(size = 10)
  ) +
  xlab("") +
  ylab("Number of artefacts") +
  ylim(-140, max(general_count$n)) +
  coord_flip()

ggsave(
  filename = "02_typology_class_2_distribution.png",
  plot = p,
  device = "png",
  path = "plots",
  width = 150,
  height = 180,
  units = "mm",
  dpi = 300
)

