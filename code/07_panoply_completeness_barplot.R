library(magrittr)
library(ggplot2)

#### data preparation ####

# colour palette
wescolors <- wesanderson::wes_palette("Zissou1", 5)

# load data
load("data/weapons.RData")
artefacts <- weapons

# establish artefact groups within the panoply
artefacts %<>% 
  dplyr::mutate(
    equipment_type = dplyr::case_when(
      typology_class_2 == "Helmet" ~ "Helmet",
      typology_class_3 %in% c(
        "Aussenbeschläge (Nach Randornamenten)", 
        "Schildbänder", 
        "Armbügel und deren Ansatzplatten",
        "Treibverzierte Bronzerundschilde",
        "Silhouettenbleche"
      ) ~ "Shield(fragment)",
      typology_class_2 %in% c(
        "Lance head (bronze)",
        "Spear head (bronze)",
        "Lance head (iron)",
        "Spear head (iron)",
        "Stick head"
      ) ~ "Lance or Spear",
      typology_class_2 == "Sauroter" ~ "Sauroter",
      typology_class_2 == "Mitra" ~ "Mitra",
      typology_class_2 == "Cuirass" ~ "Cuirass",
      typology_class_2 == "Vambrace" ~ "Vambrace",
      typology_class_2 == "Greave" ~ "Greave",
      typology_class_2 == "Ankle armour" ~ "Ankle armour",
      TRUE ~ NA_character_
    )
  )

# remove artefacts that are not part of the typical panoply
equip_artefacts <- artefacts %>%
  dplyr::filter(
    !is.na(equipment_type) 
  )

# define artefact type level order
equip_artefacts$equipment_type <- factor(
  equip_artefacts$equipment_type,
  levels = c(
    "Helmet",
    "Cuirass",
    "Mitra",
    "Vambrace",
    "Lance or Spear",
    "Sauroter",
    "Shield(fragment)",
    "Greave",
    "Ankle armour"
  ) %>% rev
)

# count the number of artefacts by type
equip_count_general <- equip_artefacts %>%
  dplyr::group_by(
    equipment_type
  ) %>%
  dplyr::summarise(
    sum = dplyr::n()
  )

#### plot A: simple panoply artefact distribution ####
A <- equip_artefacts %>%
  ggplot() +
  geom_bar(
    aes(
      x = equipment_type,
      fill = forcats::fct_rev(greave_orientation)
    )
  ) +
  geom_label(
    data = equip_count_general,
    aes(
      x = equipment_type,
      y = -50,
      label = sum
    ),
    size = 2.5,
    fill = "darkgrey",
    color = "white"
  ) +
  theme_bw() +
  theme(
    axis.text.y = element_text(hjust = 0, size = 10),
    axis.title.x = element_text(size = 10),
    legend.position = "bottom"
  ) +
  xlab("") +
  ylab("Number of artefacts") +
  coord_flip(
    ylim = c(-50, 750)
  ) +
  scale_fill_manual(
    limits = c("left", "right"),
    values = wescolors[c(1,4)],
    name = "Orientation",
    na.value = "darkgrey"
  ) +
  guides(
    fill = FALSE
  )

#### further data preparation: segregation by time ####

# calculate temporal distribution with aoristAAR
equip_time <- aoristAAR::aorist(
  equip_artefacts, 
  from = "dating_typology_start", 
  to = "dating_typology_end", 
  split_vars = c("equipment_type", "greave_orientation"),
  stepstart = -1000,
  stepstop = -400,
  method = "number"
)

# remove information outside of the relevant time window
equip_time_red <- equip_time %>%
  dplyr::filter(
    date %in% seq(-800, -400, 50)
  )

# count artefacts by year and type 
equip_time_count <- equip_time_red %>%
  dplyr::group_by(
    date, equipment_type
  ) %>%
  dplyr::summarise(
    sum = sum(sum)
  )

# count artefacts by year and type and greave orientation
equip_time_count_greave <- equip_time_red %>%
  dplyr::group_by(
    date, equipment_type, greave_orientation
  ) %>%
  dplyr::summarise(
    sum = sum(sum)
  )

#### plot B: segregation by time steps ####
B <- equip_time_count_greave %>% ggplot() +
  facet_wrap(~date) +
  geom_bar(
    aes(
      x = equipment_type,
      y = sum,
      fill = forcats::fct_rev(greave_orientation)
    ),
    stat = "identity"
  ) +
  geom_label(
    data = equip_time_count,
    aes(
      x = equipment_type,
      y = -110,
      label = sum
    ),
    size = 2.5,
    fill = "darkgrey",
    color = "white"
  ) +
  theme_bw() +
  theme(
    axis.text.y = element_text(hjust = 0, size = 10),
    axis.title.x = element_text(size = 10),
    legend.position = "bottom"
  ) +
  xlab("") +
  ylab("Number of artefacts") +
  coord_flip(
    ylim = c(-150, 750)
  ) +
  scale_fill_manual(
    limits = c("left", "right"),
    values = wescolors[c(1,4)],
    name = "Orientation",
    na.value = "darkgrey"
  )

#### combine plots ####
p <- cowplot::plot_grid(A, B, labels = "AUTO", ncol = 1, rel_heights = c(0.3, 1))

ggsave(
  filename = "07_panoply_completeness.png",
  plot = p,
  device = "png",
  path = "plots",
  width = 200,
  height = 240,
  units = "mm",
  dpi = 300
)

