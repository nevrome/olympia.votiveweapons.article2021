library(magrittr)
library(ggplot2)

#### data preparation ####

# colour palette
wescolors <- wesanderson::wes_palette("Zissou1", 5)

# load data
load("data/weapons_unfiltered.RData")
artefacts <- weapons_unfiltered

# artefact type count considering a classified dating variable
type_count <- artefacts %>%
  # remove artefacts without typological attribution
  dplyr::filter(
    !is.na(typology_class_2)
  ) %>%
  # classify dating information
  dplyr::mutate(
    dated = dplyr::case_when(
      abs(dating_typology_start - dating_typology_end) >= 100 ~ "less precise (>100 years)",
      abs(dating_typology_start - dating_typology_end) < 100 ~ "more precise (<100 years)",
      is.na(dating_typology_start) & is.na(dating_typology_end) ~ "no dating information"
    )
  ) %>%
  # count by type
  dplyr::group_by(
    typology_class_2, dated
  ) %>%
  dplyr::summarise(
    n = dplyr::n()
  ) %>%
  dplyr::ungroup()

# define factor levels for the classified dating information
type_count$dated <- factor(type_count$dated, levels = c(
  "no dating information", "less precise (>100 years)", "more precise (<100 years)"
))

# general artefact type count
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

# define factor order for typology variable
type_count$typology_class_2 <- factor(type_count$typology_class_2, levels = general_count$typology_class_2)

#### plot ####

p <- type_count %>%
  ggplot() +
  geom_bar(
    aes(x = typology_class_2, y = n, fill = dated),
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
    values = c("darkgrey", wescolors[c(4,1)])
  ) +
  theme_bw() +
  theme(
    legend.position = c(0.95, 0.15),
    legend.justification = c(1, 1),
    legend.title = element_blank(),
    legend.key.size = unit(1, 'lines'),
    legend.spacing.x = unit(0.2, 'cm'),
    legend.text = element_text(size = 10),
    axis.text.y = element_text(size = 10, hjust = 0),
    axis.text.x = element_text(size = 10)
  ) +
  xlab("") +
  ylab("Number of artefacts") +
  ylim(-140, max(general_count$n)) +
  coord_flip()

ggsave(
  filename = "01_typology_class_2_distribution.png",
  plot = p,
  device = "png",
  path = "plots",
  width = 150,
  height = 180,
  units = "mm",
  dpi = 300
)
