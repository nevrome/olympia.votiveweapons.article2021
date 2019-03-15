library(magrittr)
library(ggplot2)

load("data/weapons.RData")
artefacts <- weapons

type3_count <- artefacts %>%
  dplyr::group_by(
    typology_class_3
  ) %>%
  dplyr::summarise(
    n = dplyr::n()
  ) %>%
  dplyr::arrange(
    n
  )

type3_count$typology_class_3 <- factor(type3_count$typology_class_3, levels = type3_count$typology_class_3)

p <- type3_count %>%
  ggplot() +
  geom_bar(
    aes(x = typology_class_3, y = n),
    stat = "identity"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7)
  ) +
  xlab("") +
  ylab("Amount of artefacts")

ggsave(
  filename = "03_typology_class_3_distribution.png",
  plot = p,
  device = "png",
  path = "plots",
  width = 130,
  height = 100,
  units = "mm",
  dpi = 300
)

