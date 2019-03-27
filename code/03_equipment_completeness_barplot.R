library(magrittr)
library(ggplot2)

wescolors <- wesanderson::wes_palette("Zissou1", 5)

load("data/weapons_unfiltered.RData")
artefacts <- weapons_unfiltered

artefacts %<>% 
  dplyr::mutate(
    equipment_type = dplyr::case_when(
      typology_class_2 == "Helm" ~ "Helm",
      typology_class_3 %in% c(
        "Aussenbeschläge (Nach Randornamenten)", 
        "Schildbänder", 
        "Armbügel und deren Ansatzplatten",
        "Treibverzierte Bronzerundschilde",
        "Silhouettenbleche"
      ) ~ "Schild",
      typology_class_2 %in% c(
        "Bronzene Lanzenspitze",
        "Eiserne Lanzenspitze",
        "Eiserne Speerspitze",
        "Bronzene Speerspitze",
        "Stockspitzen"
      ) ~ "Lanze oder Speer",
      typology_class_2 == "Sauroter" ~ "Lanzenschuh",
      typology_class_2 == "Mitra" ~ "Mitra",
      typology_class_2 == "Panzer" ~ "Panzer",
      typology_class_2 == "Armschiene" ~ "Armschiene",
      typology_class_2 == "Beinschiene" ~ "Beinschiene",
      typology_class_2 == "Knöchelpanzer" ~ "Knöchelpanzer",
      TRUE ~ NA_character_
    )
  )

equip_artefacts <- artefacts %>%
  dplyr::filter(
    !is.na(equipment_type) 
  )

equip_artefacts$equipment_type <- factor(
  equip_artefacts$equipment_type,
  levels = c(
    "Helm",
    "Panzer",
    "Mitra",
    "Armschiene",
    "Lanze oder Speer",
    "Lanzenschuh",
    "Schild",
    "Beinschiene",
    "Knöchelpanzer"
  ) %>% rev
)

equip_count_general <- equip_artefacts %>%
  dplyr::group_by(
    equipment_type, cuisse_orientation
  ) %>%
  dplyr::tally() %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    type = dplyr::case_when(
      "Helm" == equipment_type ~ "single",
      "Beinschiene" == equipment_type ~ "double",
      "Armschiene" == equipment_type ~ "single",
      "Lanze oder Speer" == equipment_type ~ "single",
      "Schild" == equipment_type ~ "single",
      "Panzer" == equipment_type ~ "single",
      "Lanzenschuh" == equipment_type ~ "single",
      "Knöchelpanzer" == equipment_type ~ "double",
      "Mitra" == equipment_type ~ "single",
      TRUE ~ NA_character_
    )
  ) 

single <- equip_count_general %>%
  dplyr::filter(
    type == "single"
  ) %>%
  dplyr::mutate(
    exp_min = min(n),
    exp_mean = mean(n),
    exp_max = max(n)
  )

double <- equip_count_general %>%
  dplyr::filter(
    type == "double"
  ) %>%
  dplyr::mutate(
    exp_min = NA,
    exp_mean = NA,
    exp_max = NA
  )

equip_count <- base::rbind(single, double)

equip_count <- equip_count %>%
  dplyr::mutate(
    exp_min = ifelse(
      is.na(exp_min), mean(exp_min, na.rm = T) * 2, exp_min
    ),
    exp_mean = ifelse(
      is.na(exp_mean), mean(exp_mean, na.rm = T) * 2, exp_mean
    ),
    exp_max = ifelse(
      is.na(exp_max), mean(exp_max, na.rm = T)* 2, exp_max
    )
  )

p <- ggplot(equip_count) +
  geom_bar(
    aes(
      x = equipment_type,
      y = n,
      fill = forcats::fct_rev(cuisse_orientation)
    ),
    stat = "identity"
  ) +
  geom_errorbar(
    aes(
      x = equipment_type,
      ymin = exp_min,
      ymax = exp_max
    ),
    color = "black",
    width = 0.3
  ) +
  geom_point(
    aes(
      x = equipment_type,
      y = exp_mean
    ),
    color = "black"
  ) +
  geom_label(
    data = equip_count %>% 
      dplyr::group_by(equipment_type) %>% 
      dplyr::summarise(
        n = sum(n)
      ),
    aes(
      x = equipment_type,
      y = -110,
      label = n
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
    ylim = c(-130, 1250)
  ) +
  scale_fill_manual(
    limits = c("left", "right"),
    values = wescolors[c(2,4)],
    name = "Orientation",
    na.value = "darkgrey"
  )
  

ggsave(
  filename = "03_equipment_completeness.png",
  plot = p,
  device = "png",
  path = "plots",
  width = 130,
  height = 90,
  units = "mm",
  dpi = 300
)

