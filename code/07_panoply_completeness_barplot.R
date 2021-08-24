library(magrittr)
library(ggplot2)
source("code/00D_aoristAAR_functions.R")

#### plot A: schematic panoply ####
image <- magick::image_read_svg("data/panoply.svg", width = 1000) %>%
  magick::image_background(color = "white")
A <- cowplot::ggdraw() + cowplot::draw_image(image, scale = 1.1, valign = 0.75, halign = 0.6)

#### data preparation ####

# colour palette
wescolors <- wesanderson::wes_palette("Darjeeling1", 5)[c(1,5,4,2)]

# load data
load("data/weapons.RData")
artefacts <- weapons

# establish artefact groups within the panoply
artefacts %<>% 
  dplyr::mutate(
    equipment_type = dplyr::case_when(
      typology_class_2 == "Helmet" ~ "Helmet",
      typology_class_2 == "Shield and accessories" ~ "Shield(fragment)",
      typology_class_2 %in% c(
        "Lance head (bronze)",
        "Spear head (bronze)",
        "Lance head (iron)",
        "Spear head (iron)",
        "Stick head"
      ) ~ "Lance or Spear head",
      typology_class_2 == "Sauroter" ~ "Sauroter",
      typology_class_2 == "Mitra" ~ "Mitra",
      typology_class_2 == "Cuirass" ~ "Cuirass",
      typology_class_2 == "Arm guard" ~ "Arm guard",
      typology_class_2 == "Greave" ~ "Greave",
      typology_class_2 == "Ankle guard" ~ "Ankle guard",
      typology_class_2 == "Foot guard" ~ "Foot guard",
      TRUE ~ NA_character_
    )
  )

# remove artefacts that are not part of the typical panoply
equip_artefacts <- artefacts %>%
  dplyr::filter(
    !is.na(equipment_type) 
  )

na_if_not <- function(x, y) {
  purrr::map_chr(x, function(z) { if (z %in% y) { z } else { NA_character_ } })
}

translate <- function(x) {
  dplyr::case_when(
    x == "Korinthischer Helm" ~ "Corinthian",
    x == "Kegelhelm" ~ "Cone",
    x == "Illyrischer Helm" ~ "Illyrian",
    x == "Chalkidischer Helm" ~ "Chalcidian",
    x == "Glockenpanzer, Rückenschale" ~ "back",
    x == "Glockenpanzer, Brustschale" ~ "front",
    x == "Schildbänder" ~ "straps",
    x == "Aussenbeschläge (Nach Randornamenten)" ~ "fittings",
    x == "Armbügel und deren Ansatzplatten" ~ "grips",
    x == "Silhouettenbleche" ~ "sheets",
    x == "Runde Appliken" ~ "appliques",
    x == "left" ~ "left",
    x == "right" ~ "right",
    TRUE ~ NA_character_
  )
}

# introducing variable for coloured plotting
equip_artefacts <- equip_artefacts %>%
  dplyr::mutate(
    special = dplyr::case_when(
      equipment_type == "Helmet" ~ 
        na_if_not(
          as.character(typology_class_3), 
          c("Korinthischer Helm", "Kegelhelm", "Illyrischer Helm", "Chalkidischer Helm")
        ),
      equipment_type == "Greave" ~ 
        dplyr::na_if(as.character(orientation), "(Missing)"),
      equipment_type == "Shield(fragment)" ~ 
        na_if_not(
          as.character(typology_class_3),
          c("Aussenbeschläge (Nach Randornamenten)", 
            "Schildbänder", 
            "Armbügel und deren Ansatzplatten",
            "Silhouettenbleche",
            "Runde Appliken")
        ),
      equipment_type == "Cuirass" ~ 
        dplyr::na_if(as.character(typology_class_3), "Glockenpanzer"),
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::mutate(
    special = special %>% translate() %>% tidyr::replace_na("\u229B")
  )

# define artefact type level order
equip_artefacts$equipment_type <- factor(
  equip_artefacts$equipment_type,
  levels = c(
    "Helmet",
    "Cuirass",
    "Mitra",
    "Arm guard",
    "Lance or Spear head",
    "Sauroter",
    "Shield(fragment)",
    "Greave",
    "Ankle guard",
    "Foot guard"
  )
)

# count the number of artefacts by type
equip_count_general <- equip_artefacts %>%
  dplyr::group_by(
    equipment_type, special
  ) %>%
  dplyr::summarise(
    sum = dplyr::n()
  )

#### plot B: simple panoply artefact distribution ####
B <- equip_artefacts %>%
  ggplot() +
  facet_grid(rows = "equipment_type", scales = "free_y", space = "free_y", switch = "y") +
  geom_bar(
    aes(
      x = special,
      fill = special
    ),
    position = position_dodge(width = 1)
  ) +
  geom_text(
    data = equip_count_general,
    aes(
      x = special,
      y = -30,
      label = sum,
    ),
    size = 3,
    position = position_dodge(width = 1)
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 11),
    axis.text.y = element_text(hjust = 0, size = 10),
    axis.title.x = element_text(size = 11),
    legend.position = "bottom",
    strip.text = element_text(size = 11),
    strip.text.y.left = element_text(angle = 0),
    panel.spacing = unit(0.1, "lines")
  ) +
  xlab("") +
  ylab("Number of artefacts") +
  coord_flip() +
  scale_fill_manual(
    limits = c("right", "left", "front", "back"),
    values = wescolors,
    name = "Orientation",
    na.value = "darkgrey"
  ) +
  guides(
    fill = "none"
  ) +
  scale_x_discrete(position = "top")

#### further data preparation: segregation by time ####

equip_artefacts_timeseries <- aorist(
  equip_artefacts,
  split_vars = c("equipment_type"),
  from = "dating_typology_start",
  to = "dating_typology_end",
  method = "weight"
) %>%
  dplyr::group_by(
    equipment_type
  ) %>%
  dplyr::mutate(
    normalized_weight = sum/max(sum)
  ) %>%
  dplyr::ungroup()

# sample per type from weight distributions
equip_sample <- equip_artefacts %>%
  dplyr::mutate(
    dating_sampled = Map(function(x, y) {
        runif(1,x,y)
      }, .$dating_typology_start, .$dating_typology_end
    ) %>% unlist() %>% round()
  )

#### plot C: possible sample ####
C <- ggplot() +
  ggridges::geom_density_ridges(
    data = equip_artefacts_timeseries,
    mapping = aes(x = date, y = forcats::fct_rev(equipment_type), height = normalized_weight),
    stat = "identity",
    alpha = 0.5,
    size = 0.2,
    scale = 0.4,
    position = position_nudge(y = 0.3)
  ) +
  geom_jitter(
    data = equip_sample,
    aes(x = dating_sampled, y = forcats::fct_rev(equipment_type), colour = special),
    size = 0.5, height = 0.15
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 12),
    axis.text.y = element_text(hjust = 0),
    axis.title.x = element_text(size = 12),
    legend.position = "right",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12)
  ) +
  xlab("Year BC") +
  ylab("") +
  coord_cartesian(
    xlim = c(-800,-400),
    ylim = c(1.3,equip_artefacts$equipment_type %>% unique %>% length() + 0.4)
  ) +
  scale_colour_manual(
    limits = c("right", "left", "front", "back"),
    values = wescolors,
    name = "Orientation",
    na.value = "darkgrey"
  ) +
  guides(
    colour = guide_legend(override.aes = list(size = 5))
  ) 

#### combine plots ####
top_row <- cowplot::plot_grid(
  B, A, 
  labels = c('A', 'B'), rel_widths = c(0.8, 0.5),
  label_size = 16
)

p <- cowplot::plot_grid(
  top_row, C, 
  labels = c('', 'C'), ncol = 1, rel_heights = c(0.7, 0.7),
  label_size = 16
)

ggsave(
  filename = "07_panoply_completeness.png",
  plot = p,
  device = "png",
  path = "plots",
  width = 240,
  height = 230,
  units = "mm",
  dpi = 300
)

