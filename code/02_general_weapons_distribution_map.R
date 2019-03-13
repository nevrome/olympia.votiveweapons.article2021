library(magrittr)
library(ggplot2)

load("data/weapons.RData")
artefacts <- weapons

site_areas <- sf::read_sf(
  "../olympia-dev-votive-offerings/public_data/olympia_area_polygons_25834/olympia_areas_25834.shp", 
  options = "ENCODING=UTF-8"
)

background_map <- raster::brick("../olympia-dev-votive-offerings/private_data/Raster/Karte_Olympia_small_modified_25834.tif")
background_map_low_res <- raster::aggregate(background_map, 5)

artefacts_per_area <- artefacts %>% 
  dplyr::group_by(find_area) %>%
  dplyr::count()

# lack of attribution
artefacts_per_area$find_area[!(artefacts_per_area$find_area %in% site_areas$area_name)]
site_areas$area_name[!(site_areas$area_name %in% artefacts_per_area$find_area)]

areas_artefacts <- site_areas %>% 
  dplyr::left_join(artefacts_per_area, by = c("area_name" = "find_area"))

p <- ggplot() +
  ggspatial::layer_spatial(background_map_low_res) +
  geom_sf(
    data = areas_artefacts,
    mapping = aes(fill = n),
    alpha = 0.5
  ) +
  coord_sf(
    xlim = c(555350, 556030),
    ylim = c(4165620, 4166150),
    datum = sf::st_crs(25834)
  ) +
  scale_fill_gradient2(
    low = "white", mid = "orange", high = "darkred", 
    guide = "colorbar", na.value = "lightgrey",
    midpoint = mean(range(areas_artefacts$n, na.rm = TRUE))
  ) +
  guides(
    fill = guide_colorbar(title = "Number of Artefacts")
  ) +
  geom_sf_label(
    data = areas_artefacts,
    mapping = aes(label = area_name),
    size = 2.8,
    alpha = 0.5
  ) +
  theme_bw() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_line(colour = "white"), 
    panel.grid.minor = element_line(colour = "white")
  ) +
  annotate(
    "text", x = 555900, y = 4165700,
    label = "200m"
  ) +
  annotate(
    "segment", x = 555800, xend = 556000, y = 4165680, yend = 4165680,
    size = 2, colour = "black"
  ) +
  annotate(
    "text", x = 555900, y = 4165660,
    label = "EPSG: 25834"
  )

ggsave(
  filename = "02_general_weapns_distribution_map.png",
  plot = p,
  device = "png",
  path = "plots/",
  width = 350,
  height = 250,
  units = "mm",
  dpi = 300
)

