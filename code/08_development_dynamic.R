library(magrittr)
library(ggplot2)

load("data/weapons.RData")

artefacts <- weapons %>% dplyr::filter(
  !is.na(typology_class_1), !is.na(typology_class_2), !is.na(typology_class_3), !is.na(typology_class_4)
) %>%
  dplyr::mutate(
    typology = paste(typology_class_1, typology_class_2, typology_class_3, typology_class_4, sep = "_")
  )

#### artefact classes amound timeseries ###
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

#### fit spline ####

ct <- classes_timeseries
ct$sum <- tidyr::replace_na(ct$sum, 0)

spline_model <- smooth.spline(ct$date, ct$sum, spar = 0.5)
prediction_spline <- predict(spline_model, ct$date)

spline <- tibble::tibble(
  date = prediction_spline$x,
  pred = prediction_spline$y
)

#### Plot A: number of classes per year + spline ####
A <- ggplot() +
  geom_hline(
    yintercept = 0,
    color = "blue",
    linetype = 3
  ) +
  geom_line(
    data = classes_timeseries,
    mapping = aes(x = date, y = sum)
  ) +
  geom_line(
    data = spline,
    mapping = aes(x = date, y = pred),
    color = "red",
    size = 1.4
  ) +
  scale_x_continuous(breaks = seq(-1000, -400, 100), limits = c(-1000, -400)) +
  theme_bw() +
  ylab("Amount of classes") +
  xlab("") +
  annotate("text", x = -900, y = 10, label = "Cubic smoothing spline", color = "red", size = 3) 

#### derivative ####
prediction_deriv <- predict(spline_model, ct$date, deriv = 1)

deri <- tibble::tibble(
  date = prediction_deriv$x,
  deriv = prediction_deriv$y
)

#### Plot B: derivative of spline ####
B <- ggplot(deri) +
  geom_hline(
    yintercept = 0,
    color = "blue",
    linetype = 3
  ) +
  geom_line(
    mapping = aes(x = date, y = deriv),
    color = "darkgreen",
    size = 1.4
  ) +
  scale_x_continuous(breaks = seq(-1000, -400, 100), limits = c(-1000, -400)) +
  theme_bw() +
  ylab("Slope") +
  xlab("")  +
  annotate("text", x = -900, y = 0.2, label = "Derivative of spline", color = "darkgreen", size = 3) 

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

#### Plot C: cultural distance from one timestep to the next ####
C <- ggplot() +
  geom_rect(
    data = distance %>% dplyr::filter(ed != 0),
    aes(xmin = start, xmax = end, ymin = 0, ymax = max(ed), fill = ed, size = ed)
  ) +
  geom_point(
    data = distance %>% dplyr::filter(ed != 0),
    aes(x = mean, y = ed)
  ) +
  scale_fill_gradient2(low = "lightgrey", mid = "yellow", high = "red", midpoint = mean(range(distance$ed))) +
  scale_size(range = c(0.3, 2), guide = FALSE) +
  scale_x_continuous(breaks = seq(-1000, -400, 100), limits = c(-1000, -400)) +
  ylim(0, max(distance$ed)) +
  theme_bw() +
  theme(
    legend.position = c(0.15, 0.85),
    legend.direction = "horizontal",
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.background = element_rect(fill = "white")
  ) +
  ylab("Euclidean distance") +
  xlab("Time") +
  guides(
    fill = guide_colorbar(
      title = "",
      title.vjust = 0.6
    ),
    color = guide_legend()
  )

#### combine plots ####
p <- cowplot::plot_grid(A, B, C, labels = "AUTO", ncol = 1, align = 'v', rel_heights = c(1, 1, 1))

ggsave(
  filename = "08_development_dynamic.png",
  plot = p,
  device = "png",
  path = "plots",
  width = 150,
  height = 200,
  units = "mm",
  dpi = 300
)
