library(magrittr)
library(ggplot2)
source("code/00D_aoristAAR_functions.R", local = T)

#### load data ####
load("data/weapons.RData")
wescolors <- wesanderson::wes_palette("Zissou1", 5)

#### A: weapons time series ####

# calculate time series with method = "number"
weapons_timeseries_number <- aorist(
  weapons,
  split_vars = c(),
  from = "dating_typology_start",
  to = "dating_typology_end",
  method = "number",
  stepstart = -1000,
  stepstop = -400
) %>%
  dplyr::transmute(
    date = date,
    number = sum
  )

# calculate time series with method = "weight"
weapons_timeseries_weight <- aorist(
  weapons,
  split_vars = c(),
  from = "dating_typology_start",
  to = "dating_typology_end",
  method = "weight",
  stepstart = -1000,
  stepstop = -400
) %>%
  dplyr::transmute(
    date = date,
    weight = sum
  )

# merge time series
weapons_timeseries <- dplyr::full_join(
  weapons_timeseries_number, 
  weapons_timeseries_weight,
  by = "date"
)

#### Plot A: weapons time series ####
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
    mapping = aes(y = number, colour = "Maximum number of artefacts")
  ) +
  geom_line(
    mapping = aes(y = weight * (max(weapons_timeseries_number$number) / max(weapons_timeseries_weight$weight)), colour = "Weight corrected artefact distribution"),
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
    axis.title.y.right = element_text(color = wescolors[5]),
    axis.text.y.right = element_text(color = wescolors[5])
  )

#### B: artefact classes time series ####

artefacts <- weapons
  
# count artefacts and remove artefact categories with less than 10 values 
artefacts <- artefacts %>% 
  dplyr::group_by(
    typology_class_2, typology_class_3, typology_class_4
  ) %>%
  dplyr::mutate(
    n = dplyr::n()
  ) %>% 
  dplyr::filter(
    n >= 10
  ) %>%
  dplyr::ungroup()

# create a variable that combines typology_class_2, typology_class_3 and typology_class_4
artefacts <- artefacts %>%
  dplyr::mutate(
    typology_fine = dplyr::case_when(
      !is.na(typology_class_3) & !is.na(typology_class_4) ~ 
        paste(typology_class_2, typology_class_3, typology_class_4, sep = " ~ "),
      !is.na(typology_class_3) ~ 
        paste(typology_class_2, typology_class_3, sep = " ~ "),
      TRUE ~ as.character(typology_class_2)
    )
  )

# reduce dataset to relevant variables
classes <- artefacts %>%
  dplyr::group_by(
    dating_typology_start, dating_typology_end, typology_fine
  ) %>%
  dplyr::summarise(
    .groups = "drop"
  )

# calculate time series
classes_timeseries <- aorist(
  classes,
  split_vars = c(),
  from = "dating_typology_start",
  to = "dating_typology_end",
  method = "number"
)

ct <- classes_timeseries

# replace NA with 0
ct$sum <- tidyr::replace_na(ct$sum, 0)

# calculate spline
spline_model <- smooth.spline(ct$date, ct$sum, spar = 0.7)
prediction_spline <- predict(spline_model, ct$date)
spline <- tibble::tibble(
  date = prediction_spline$x,
  pred = prediction_spline$y
)

classes_timeseries$name <- "Maximum number of artefact classes"
spline$name <- "Smoothing spline (cubic, spar = 0.7)"

#### Plot B: artefact classes time series ####
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

#### C: derivative ####

# calculate derivative of spline
prediction_deriv <- predict(spline_model, ct$date, deriv = 1)
deri <- tibble::tibble(
  date = prediction_deriv$x,
  deriv = prediction_deriv$y
)

#### Plot C: derivative ####
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

#### cultural distance decades ####

# calculate time series
artefacts_timeseries <- aorist(
  artefacts,
  split_vars = c("typology_fine"),
  stepwidth = 10,
  from = "dating_typology_start",
  to = "dating_typology_end",
  method = "number"
)

# transform data from wide to long
df <- artefacts_timeseries %>%
  tidyr::spread(
    key = typology_fine, value = sum
  )

# remove column date and replace NA with 0
ma_corr <- df %>%
  dplyr::select(
    -date
  ) %>%
  as.matrix() %>%
  tidyr::replace_na(0)

# calculate euclidian distance
distance_timesteps <- c()
for (i in 1:(nrow(ma_corr) - 1)) {
  distance_timesteps[i] <- dist(ma_corr[c(i, i+1),])
}

# create nice distance dataset
distance <- tibble::tibble(
  start = df$date[-length(df$date)],
  end = df$date[-1],
  mean = (start + end)/2,
  ed = distance_timesteps,
  # cumulutive sum of euclidian distance
  cumsum_ed = cumsum(ed)
)

#### Plot D: cultural distance decades ####
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
    data = distance,
    aes(xmin = start, xmax = end, ymin = 0, ymax = max(cumsum_ed), fill = ed, size = ed)
  ) +
  geom_line(
    data = distance,
    aes(x = mean, y = cumsum_ed),
    color = "black",
    size = 0.3
  ) +
  geom_point(
    data = distance,
    aes(x = mean, y = cumsum_ed)
  ) +
  scale_fill_gradientn(
    colours = wesanderson::wes_palette(
      "Zissou1", 
      type = "continuous"
    ), 
    breaks = c(0, 100, 200)
  ) +
  scale_size(range = c(0.3, 2), guide = "none") +
  scale_x_continuous(breaks = seq(-1000, -400, 100), limits = c(-1000, -399)) +
  ylim(0, max(distance$cumsum_ed)) +
  theme_bw() +
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.direction = "horizontal",
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.background = element_rect(fill = "white"),
    axis.title.y = element_text(hjust = -0.1)
  ) +
  ylab(expression("Cumulative distance")) +
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
  filename = "06_development_dynamic.png",
  plot = p,
  device = "png",
  path = "plots",
  width = 150,
  height = 220,
  units = "mm",
  dpi = 300
)
