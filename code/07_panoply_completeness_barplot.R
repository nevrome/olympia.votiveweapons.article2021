library(magrittr)
library(ggplot2)

#### plot A: schematic panoply ####
image <- magick::image_read_svg("data/panoply.svg", width = 1000) %>%
  magick::image_background(color = "white")
A <- cowplot::ggdraw() + cowplot::draw_image(image, scale = 1.1)

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

#### plot B: simple panoply artefact distribution ####
B <- equip_artefacts %>%
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
    size = 3.7,
    fill = "darkgrey",
    color = "white"
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 12),
    axis.text.y = element_text(hjust = 0),
    axis.title.x = element_text(size = 12),
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
    fill = "none"
  ) +
  annotate(
    "text", 
    x = "Shield(fragment)", y = 320, 
    label = "Fragments often do not represent complete shields!",
    color = "white",
    size = 3.2
  )

#### further data preparation: segregation by time ####

# count number of artefacts per type
equip_count <- equip_artefacts %>%
  dplyr::group_by(
    equipment_type, greave_orientation
  ) %>%
  dplyr::summarise(
    sum = dplyr::n()
  )

# get aoristic weights per year
equip_time <- aoristAAR::aorist(
  equip_artefacts, 
  from = "dating_typology_start", 
  to = "dating_typology_end", 
  split_vars = c("equipment_type", "greave_orientation"),
  method = "weight"
)

# sample per type from weight distributions
equip_sample <- equip_time %>% dplyr::group_split(
  equipment_type, greave_orientation
) %>% 
  Map(
    function(x) {
      tibble::tibble(
        equipment_type = x$equipment_type[1],
        greave_orientation = x$greave_orientation[1],
        date_sampled = sample(
          x = x$date, 
          size = equip_count$sum[
            equip_count$equipment_type == equipment_type & 
              equip_count$greave_orientation == greave_orientation],
          prob = x$sum,
          replace = TRUE
        )
      )
    },
    .
  ) %>%
  dplyr::bind_rows()

#### plot C: possible sample ####
C <- ggplot() +
  geom_jitter(
    data = equip_sample,
    aes(x = date_sampled, y = equipment_type, colour = greave_orientation),
    size = 1, height = 0.3
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 12),
    axis.text.y = element_text(hjust = 0),
    axis.title.x = element_text(size = 12),
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12)
  ) +
  xlab("Year BC") +
  ylab("") +
  coord_cartesian(
    xlim = c(-800,-400)
  ) +
  scale_alpha_continuous(
    range = c(0.1,1)
  ) +
  scale_colour_manual(
    limits = c("left", "right"),
    values = wescolors[c(1,4)],
    name = "Orientation",
    na.value = "darkgrey"
  ) +
  guides(
    colour = guide_legend(override.aes = list(size = 5))
  ) 

#### combine plots ####
top_row <- cowplot::plot_grid(A, B, labels = c('A', 'B'), align = 'h', rel_widths = c(0.5, 1))

p <- cowplot::plot_grid(top_row, C, labels = c('', 'C'), ncol = 1, rel_heights = c(0.5, 0.8))

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

