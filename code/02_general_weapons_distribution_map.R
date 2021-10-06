library(magrittr)
library(ggplot2)

set.seed(123)

#### data preparation ####

# load data
load("data/weapons.RData")
artefacts <- weapons
site_areas <- sf::read_sf(
  "data/olympia_area_polygons_25834/olympia_areas_25834.shp", 
  options = "ENCODING=UTF-8"
)
background_map <- raster::brick("data/background_map_olympia_epsg25834.tif")

# reduce raster resolution
background_map_low_res <- raster::aggregate(background_map, 5)

# translate area names in sf data.frame
translation_find_area <- readr::read_csv(
  "data/translation_de_en_find_area.csv",
  col_types = readr::cols(
    de = readr::col_character(),
    en = readr::col_character()
  )
)
site_areas %<>%
  dplyr::mutate(
    area_name = translation_find_area$en[
      match(area_name, translation_find_area$de)
      ]
  )

# count artefacts by area
artefacts_per_area <- artefacts %>% 
  dplyr::group_by(find_area) %>%
  dplyr::count()

# # identify areas with lack of attribution
# artefacts_per_area$find_area[!(artefacts_per_area$find_area %in% site_areas$area_name)]
# site_areas$area_name[!(site_areas$area_name %in% artefacts_per_area$find_area)]

# merge count info and spatial info
areas_artefacts <- site_areas %>% 
  dplyr::left_join(artefacts_per_area, by = c("area_name" = "find_area"))

# compile centroid coordinates of polygons for labels
area_label_positions <- areas_artefacts %>%
  sf::st_centroid() %>%
  dplyr::mutate(
    x = sf::st_coordinates(.)[,1],
    y = sf::st_coordinates(.)[,2]
  ) %>%
  sf::st_drop_geometry() %>%
  # replace Alpheios coordinates
  dplyr::mutate(
    x = ifelse(area_name == "Alpheios", 555900, x),
    y = ifelse(area_name == "Alpheios", 4165850, y)
  )

#### plot ####
p <- ggplot() +
  ggspatial::layer_spatial(background_map_low_res) +
  geom_sf(
    data = areas_artefacts,
    mapping = aes(fill = n),
    alpha = 0.5,
    size = 1
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
  guides(
    fill = guide_colorbar(title = "Number of Artefacts")
  ) +
  ggrepel::geom_label_repel(
    data = area_label_positions,
    mapping = aes(x = x, y = y, label = area_name),
    size = 6,
    alpha = 0.6
  ) +
  theme_bw() +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11, angle = 90, hjust = 0.5),
    panel.grid.major = element_line(colour = "white"), 
    panel.grid.minor = element_line(colour = "white"),
    legend.position = "bottom",
    legend.key.width = unit(4, "cm"),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 13)
  ) +
  annotate(
    "text", x = 555875, y = 4165700,
    label = "250m",
    size = 6
  ) +
  annotate(
    "segment", x = 555750, xend = 555800, y = 4165680, yend = 4165680,
    size = 3, colour = "black"
  ) +
  annotate(
    "segment", x = 555800, xend = 555850, y = 4165680, yend = 4165680,
    size = 3, colour = "white"
  ) +
  annotate(
    "segment", x = 555850, xend = 555900, y = 4165680, yend = 4165680,
    size = 3, colour = "black"
  ) +
  annotate(
    "segment", x = 555900, xend = 555950, y = 4165680, yend = 4165680,
    size = 3, colour = "white"
  ) +
  annotate(
    "segment", x = 555950, xend = 556000, y = 4165680, yend = 4165680,
    size = 3, colour = "black"
  ) +
  annotate(
    "text", x = 556030, y = 4165680,
    label = "⇑",
    size = 15
  ) +
  annotate(
    "text", x = 555875, y = 4165660,
    label = "EPSG: 25834",
    size = 6
  )

ggsave(
  filename = "02_general_weapons_distribution_map.png",
  plot = p,
  device = "png",
  path = "plots/",
  width = 350,
  height = 290,
  units = "mm",
  dpi = 300
)
