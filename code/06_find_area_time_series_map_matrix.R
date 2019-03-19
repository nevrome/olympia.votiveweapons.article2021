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

artefact_number_timesteps_sf$sum[artefact_number_timesteps_sf$sum == 0] <- NA

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
  scale_fill_gradientn(
    colours = wesanderson::wes_palette(
      "Zissou1", 
      type = "continuous"
    )
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
  ) +
  guides(
    fill = guide_colorbar(title = "Number of artefacts")
  )

ggsave(
  filename = "06_find_area_time_series_map.png",
  plot = p,
  device = "png",
  path = "plots/",
  width = 350,
  height = 300,
  units = "mm",
  dpi = 300
)

