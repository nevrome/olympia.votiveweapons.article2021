library(magrittr)
library(ggplot2)

load("data/weapons_unfiltered.RData")
artefacts <- weapons_unfiltered

type_count <- artefacts %>%
  dplyr::filter(
    !is.na(typology_class_2)
  ) %>%
  dplyr::mutate(
    dated = ifelse(!is.na(dating_typology_start) & !is.na(dating_typology_end), "typological dating", "no dating information")
  ) %>%
  dplyr::group_by(
    typology_class_2, dated
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
    aes(x = typology_class_2, y = n, fill = dated),
    stat = "identity"
  ) +
  geom_text(
    data = general_count,
    aes(x = typology_class_2, y = n, label = n), 
    position = position_dodge(width = 0.9), 
    hjust = -0.25,
    size = 2.5,
    angle = 90
  ) +
  scale_fill_manual(
    values = c("grey", "black")
  ) +
  theme_bw() +
  theme(
    legend.position = c(0.01, 0.99),
    legend.justification = c(0, 1),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7)
  ) +
  xlab("") +
  ylab("Amount of artefacts") +
  ylim(0, max(general_count$n) + 200)

ggsave(
  filename = "04_typology_class_2_distribution.png",
  plot = p,
  device = "png",
  path = "plots",
  width = 150,
  height = 100,
  units = "mm",
  dpi = 300
)

