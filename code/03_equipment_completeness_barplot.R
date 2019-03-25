library(magrittr)
library(ggplot2)

wescolors <- wesanderson::wes_palette("Zissou1", 5)

load("data/weapons_unfiltered.RData")
artefacts <- weapons_unfiltered

artefacts %<>% 
  tidyr::unite(
    "typology_collection", 
    typology_class_1, 
    typology_class_2, 
    typology_class_3, 
    sep = " ", 
    remove = FALSE
  ) %>%
  dplyr::mutate(
    equipment_type = dplyr::case_when(
      grepl("Helm", typology_collection) ~ "Helm",
      grepl("Beinschiene", typology_collection) ~ "Beinschiene",
      grepl("Armschiene", typology_collection) ~ "Armschiene",
      grepl("Lanze|Speer|Sauroter|Stock", typology_collection) ~ "Lanze",
      grepl("Schild", typology_collection) ~ "Schild",
      grepl("Panzer", typology_collection) ~ "Panzer",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::select(
    -typology_collection
  )

equip_artefacts <- artefacts %>%
  dplyr::filter(
    !is.na(equipment_type) 
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
      "Lanze" == equipment_type ~ "single",
      "Schild" == equipment_type ~ "single",
      "Panzer" == equipment_type ~ "single",
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

equip_count$equipment_type <- factor(
  equip_count$equipment_type, 
  levels = unique(equip_count$equipment_type[order(equip_count$n)])
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

