library(magrittr)
library(ggplot2)

wescolors <- wesanderson::wes_palette("Zissou1", 5)

load("data/weapons.RData")
artefacts <- weapons

artefacts$cuisse_orientation <- artefacts$cuisse_orientation %>% forcats::fct_explicit_na()

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

equip_artefacts$typology_shield <- "NA"
equip_artefacts$typology_shield[equip_artefacts$typology_class_2 == "Schild oder Schildfragment"] <- 
  as.character(equip_artefacts$typology_class_3[equip_artefacts$typology_class_2 == "Schild oder Schildfragment"])

equip_count_general <- equip_artefacts %>%
  dplyr::group_by(
    equipment_type
  ) %>%
  dplyr::summarise(
    sum = dplyr::n()
  )

#### Plot A: Simple panoply artefact distribution ####

A <- equip_artefacts %>%
  ggplot() +
  geom_bar(
    aes(
      x = equipment_type,
      fill = forcats::fct_rev(cuisse_orientation)
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
    ylim = c(-50, 1000)
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

#### Plot B: Segregation by time steps ####

# temporal distribution
equip_time <- aoristAAR::aorist(
  equip_artefacts, 
  from = "dating_typology_start", 
  to = "dating_typology_end", 
  split_vars = c("equipment_type", "cuisse_orientation", "typology_shield"),
  stepstart = -1000,
  stepstop = -400,
  method = "number"
)

equip_time_red <- equip_time %>%
  dplyr::filter(
    date %in% seq(-800, -400, 50)
  ) 

equip_time_red_non_shield <- equip_time_red %>% 
  dplyr::filter(
    equipment_type != "Schild"
  ) %>%
  dplyr::select(
    date, equipment_type, cuisse_orientation, sum
  )

equip_time_red_shield <- equip_time_red %>% 
  dplyr::filter(
    equipment_type == "Schild"
  ) %>%
  dplyr::group_by(
    date, equipment_type, cuisse_orientation
  ) %>%
  dplyr::summarise(
    sum = round(max(sum), 0)
  ) %>%
  dplyr::ungroup()

equip_time_merged <- rbind(equip_time_red_non_shield, equip_time_red_shield)

equip_time_sd <- equip_time_merged %>%
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

single <- equip_time_sd %>%
  dplyr::filter(
    type == "single"
  ) %>%
  dplyr::group_by(
    date
  ) %>%
  dplyr::mutate(
    exp_min = min(sum),
    exp_mean = mean(sum),
    exp_max = max(sum)
  ) %>%
  dplyr::ungroup()

double <- equip_time_sd %>%
  dplyr::filter(
    type == "double"
  ) %>%
  dplyr::mutate(
    exp_min = NA_real_,
    exp_mean = NA_real_,
    exp_max = NA_real_
  )

equip_count <- base::rbind(single, double)

equip_count <- equip_count %>%
  dplyr::group_by(
    date
  ) %>%
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
  ) %>%
  dplyr::ungroup()

# plot
B <- ggplot(equip_count) +
  facet_wrap(~date) +
  geom_bar(
    aes(
      x = equipment_type,
      y = sum,
      fill = forcats::fct_rev(cuisse_orientation)
    ),
    stat = "identity"
  ) +
  geom_hline(
    aes(
      yintercept = exp_max,
      linetype = type
    )
  ) +
  geom_point(
    aes(
      x = equipment_type,
      y = exp_mean
    ),
    color = "black",
    size = 0.7
  ) +
  geom_label(
    data = equip_count %>%
      dplyr::group_by(
        date, equipment_type
      ) %>%
      dplyr::summarise(
        sum = sum(sum)
      ),
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
    ylim = c(-150, 1000)
  ) +
  scale_fill_manual(
    limits = c("left", "right"),
    values = wescolors[c(1,4)],
    name = "Orientation",
    na.value = "darkgrey"
  ) +
  scale_linetype_manual(
    values = c("dotted", "dashed"),
    name = "Theoretical number of panoplies"
  )



#### combine plots ####
  
p <- cowplot::plot_grid(A, B, labels = "AUTO", ncol = 1, rel_heights = c(0.3, 1))

ggsave(
  filename = "02_equipment_completeness.png",
  plot = p,
  device = "png",
  path = "plots",
  width = 200,
  height = 240,
  units = "mm",
  dpi = 300
)

