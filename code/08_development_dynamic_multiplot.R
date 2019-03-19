library(magrittr)
library(ggplot2)

wescolors <- wesanderson::wes_palette("Zissou1", 5)

load("data/weapons.RData")

#### weapons time series ####
weapons_timeseries_number <- aoristAAR::aorist(
  weapons,
  split_vars = c(),
  from = "dating_typology_start",
  to = "dating_typology_end",
  method = "number"
) %>%
  dplyr::transmute(
    date = date,
    number = sum
  )

weapons_timeseries_weight <- aoristAAR::aorist(
  weapons,
  split_vars = c(),
  from = "dating_typology_start",
  to = "dating_typology_end",
  method = "weight"
) %>%
  dplyr::transmute(
    date = date,
    weight = sum
  )

weapons_timeseries <- dplyr::full_join(weapons_timeseries_number, weapons_timeseries_weight)

#### Plot A: Weapons time series ####
A <- ggplot(weapons_timeseries, aes(x = date)) +
  geom_hline(
    yintercept = 0,
    color = "darkgrey",
    linetype = 3
  ) +
  geom_vline(
    xintercept = c(-1000, -400),
    color = "darkgrey",
    linetype = 3
  ) +
  geom_line(
    mapping = aes(y = number, colour = "Artefact number")
  ) +
  geom_line(
    mapping = aes(y = weight * (max(weapons_timeseries_number$number) / max(weapons_timeseries_weight$weight)), colour = "Corrected artefact weight"),
    linetype = "dashed",
    size = 0.5
  ) +
  scale_x_continuous(
    breaks = seq(-1000, -400, 100), limits = c(-1000, -400)
  ) +
  scale_y_continuous(
    name = "Artefact number",
    sec.axis = sec_axis(~./(max(weapons_timeseries_number$number) / max(weapons_timeseries_weight$weight)), name = "Artefact weight")
  ) +
  scale_colour_manual(
    values = c("black", wescolors[5])
  ) +
  theme_bw() +
  xlab("Year BC") +
  theme(
    legend.position = c(0.01, 0.99),
    legend.justification = c(0, 1),
    legend.title = element_blank(),
    axis.title.y.left = element_text(color = "black"),
    axis.title.y.right = element_text(color = wescolors[5]),
    axis.text.y.left = element_text(color = "black"),
    axis.text.y.right = element_text(color = wescolors[5])
  )

  
#### artefact classes ####
artefacts <- weapons %>% dplyr::filter(
  !is.na(typology_class_1), !is.na(typology_class_2), !is.na(typology_class_3)
) %>%
  dplyr::mutate(
    typology = paste(typology_class_1, typology_class_2, typology_class_3, sep = "_")
  )

classes <- artefacts %>%
  dplyr::group_by(
    dating_typology_start, dating_typology_end, typology
  ) %>%
  dplyr::summarise() %>%
  dplyr::ungroup()

classes_timeseries <- aoristAAR::aorist(
  classes,
  split_vars = c(),
  from = "dating_typology_start",
  to = "dating_typology_end",
  method = "number"
)

ct <- classes_timeseries
ct$sum <- tidyr::replace_na(ct$sum, 0)

spline_model <- smooth.spline(ct$date, ct$sum, spar = 0.5)
prediction_spline <- predict(spline_model, ct$date)

spline <- tibble::tibble(
  date = prediction_spline$x,
  pred = prediction_spline$y
)

classes_timeseries$name <- "Artefact classes number"
spline$name <- "Cubic smoothing spline (spar = 0.5)"

#### Plot B: number of classes per year + spline ####
B <- ggplot() +
  geom_hline(
    yintercept = 0,
    color = "darkgrey",
    linetype = 3
  ) +
  geom_vline(
    xintercept = c(-1000, -400),
    color = "darkgrey",
    linetype = 3
  ) +
  geom_line(
    data = classes_timeseries,
    mapping = aes(x = date, y = sum, color = name, size = name)
  ) +
  geom_line(
    data = spline,
    mapping = aes(x = date, y = pred, color = name, size = name)
  ) +
  scale_x_continuous(breaks = seq(-1000, -400, 100), limits = c(-1000, -400)) +
  theme_bw() +
  ylab("Classes number") +
  xlab("") +
  theme(
    legend.position = c(0.01, 0.99),
    legend.justification = c(0, 1),
    legend.title = element_blank()
  ) +
  scale_color_manual(
    values = c("black", wescolors[3])
  ) +
  scale_size_manual(
    values = c(0.5, 1.3)
  )

#### derivative ####
prediction_deriv <- predict(spline_model, ct$date, deriv = 1)

deri <- tibble::tibble(
  date = prediction_deriv$x,
  deriv = prediction_deriv$y
)

#### Plot C: derivative of spline ####
C <- ggplot(deri) +
  geom_hline(
    yintercept = 0,
    color = "darkgrey",
    linetype = 3
  ) +
  geom_vline(
    xintercept = c(-1000, -400),
    color = "darkgrey",
    linetype = 3
  ) +
  geom_line(
    mapping = aes(x = date, y = deriv, color = "First derivative of spline"),
    size = 1.3
  ) +
  scale_x_continuous(breaks = seq(-1000, -400, 100), limits = c(-1000, -400)) +
  theme_bw() +
  ylab("Slope") +
  xlab("") +
  theme(
    legend.position = c(0.01, 0.99),
    legend.justification = c(0, 1),
    legend.title = element_blank()
  ) +
  scale_color_manual(
    values = wescolors[1]
  ) 

#### cultural distance from one timestep to the next ####
artefacts_timeseries <- aoristAAR::aorist(
  artefacts,
  split_vars = c("typology"),
  stepwidth = 10,
  stepstart = -1000,
  stepstop = -400,
  from = "dating_typology_start",
  to = "dating_typology_end",
  method = "number"
)

df <- artefacts_timeseries %>%
  tidyr::spread(
    key = typology, value = sum
  )

ma <- df %>%
  dplyr::select(
    -date
  ) %>%
  as.matrix() %>%
  tidyr::replace_na(0)

distance_timesteps <- c()
for (i in 1:(nrow(ma) - 1)) {
  distance_timesteps[i] <- dist(ma[c(i, i+1),])
}

distance <- tibble::tibble(
  start = df$date[-length(df$date)],
  end = df$date[-1],
  mean = (start + end)/2,
  ed = distance_timesteps   
)

#### Plot D: cultural distance from one timestep to the next ####
D <- ggplot() +
  geom_hline(
    yintercept = 0,
    color = "darkgrey",
    linetype = 3
  ) +
  geom_vline(
    xintercept = c(-1000, -400),
    color = "darkgrey",
    linetype = 3
  ) +
  geom_rect(
    data = distance %>% dplyr::filter(ed != 0),
    aes(xmin = start, xmax = end, ymin = 0, ymax = max(ed), fill = ed, size = ed)
  ) +
  geom_line(
    data = distance %>% dplyr::filter(ed != 0),
    aes(x = mean, y = ed),
    color = "black",
    size = 0.3
  ) +
  geom_point(
    data = distance %>% dplyr::filter(ed != 0),
    aes(x = mean, y = ed)
  ) +
  scale_fill_gradientn(
    colours = wesanderson::wes_palette(
      "Zissou1", 
      type = "continuous"
    )
  ) +
  scale_size(range = c(0.3, 2), guide = FALSE) +
  scale_x_continuous(breaks = seq(-1000, -400, 100), limits = c(-1000, -400)) +
  ylim(0, max(distance$ed)) +
  theme_bw() +
  theme(
    legend.position = c(0.01, 0.99),
    legend.justification = c(0, 1),
    legend.direction = "horizontal",
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.background = element_rect(fill = "white")
  ) +
  ylab("Euclidean distance") +
  xlab("Year BC") +
  guides(
    fill = guide_colorbar(
      title = "",
      title.vjust = 0.6
    ),
    color = guide_legend()
  )

#### combine plots ####
p <- cowplot::plot_grid(A, B, C, D, labels = "AUTO", ncol = 1, align = 'v', rel_heights = c(1, 1, 1, 1))

ggsave(
  filename = "08_development_dynamic.png",
  plot = p,
  device = "png",
  path = "plots",
  width = 150,
  height = 220,
  units = "mm",
  dpi = 300
)
