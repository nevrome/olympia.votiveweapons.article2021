library(magrittr)
library(ggplot2)

load("data/weapons.RData")
artefacts <- weapons

site_areas <- sf::read_sf(
  "../olympia-dev-votive-offerings/public_data/olympia_area_polygons_25834/olympia_areas_25834.shp", 
  options = "ENCODING=UTF-8"
)

artefact_timeseries_per_area_number_df <-  aoristAAR::aorist(
  artefacts,
  split_vars = c("find_area"),
  from = "dating_typology_start",
  to = "dating_typology_end",
  method = "number"
)

seqofinterest <- seq(-800, -400, 10)

artefact_number_timesteps <- artefact_timeseries_per_area_number_df %>%
  dplyr::filter(
    date %in% seqofinterest
  )

artefact_number_timesteps_sf <- artefact_number_timesteps %>%
  dplyr::right_join(
    expand.grid(
      date = seqofinterest,
      find_area = site_areas$area_name,
      stringsAsFactors = F
    ),
    by = c("date", "find_area")
  ) %>%
  dplyr::left_join(
    site_areas, 
    by = c("find_area" = "area_name")
  ) %>%
  sf::st_as_sf()

#### plot ####

p <- ggplot() +
  geom_sf(
    data = artefact_number_timesteps_sf,
    mapping = aes(fill = sum)
  ) +
  coord_sf(
    xlim = c(555350, 556030),
    ylim = c(4165620, 4166150),
    datum = sf::st_crs(25834)
  ) +
  scale_fill_gradient2(
    low = "white", mid = "orange", high = "darkred", 
    guide = "colorbar", na.value = "lightgrey",
    midpoint = mean(range(artefact_number_timesteps_sf$sum, na.rm = TRUE))
  ) +
  facet_wrap(
    ~date
  ) +
  theme_bw() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_line(colour = "white"), 
    panel.grid.minor = element_line(colour = "white"),
    legend.position = "bottom",
    legend.key.width = unit(4, "cm"),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 13),
    strip.text = element_text(size = 15),
    strip.background = element_rect(fill = NA)
  )

ggsave(
  filename = "09.png",
  plot = p,
  device = "png",
  path = "plots/",
  width = 400,
  height = 400,
  units = "mm",
  dpi = 300
)

