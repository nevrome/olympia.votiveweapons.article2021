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

spline_model <- smooth.spline(ct$date, ct$sum, spar = 1)
prediction_spline <- predict(sm, ct$date)

spline <- tibble::tibble(
  date = prediction_model$x,
  pred = prediction_model$y
)

#### Plot A: number of classes per year + spline ####
A <- ggplot() +
  geom_line(
    data = classes_timeseries,
    mapping = aes(x = date, y = sum)
  ) +
  geom_line(
    data = spline,
    mapping = aes(x = date, y = pred),
    color = "red"
  ) +
  xlim(-1000, -400)

#### derivative ####
prediction_deriv <- predict(sm, ct$date, deriv = 1)

deri <- tibble::tibble(
  date = prediction_deriv$x,
  deriv = prediction_deriv$y
)

#### Plot B: derivative of spline ####
B <- ggplot(deri) +
  geom_hline(
    yintercept = 0,
    color = "blue"
  ) +
  geom_line(
    mapping = aes(x = date, y = deriv),
    color = "red"
  ) +
  xlim(-1000, -400) +
  theme_bw()

#### cultural distance from one timestep to the next ####
artefacts_timeseries <- aoristAAR::aorist(
  artefacts,
  split_vars = c("typology"),
  stepwidth = 1,
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

res <- c()
for (i in 1:(nrow(ma) - 1)) {
  res[i] <- dist(ma[c(i, i+1),])
}

distance <- tibble::tibble(
  date = df$date[-length(df$date)],
  ed = res   
)

edi <- distance %>% dplyr::filter(ed != 0) %$% ed

C <- ggplot() +
  geom_vline(
    data = distance %>% dplyr::filter(ed != 0),
    aes(xintercept = date, color = ed, size = ed)
  ) +
  geom_point(
    data = distance %>% dplyr::filter(ed != 0),
    aes(x = date, y = ed)
  ) +
  scale_colour_gradient2(low = "grey", mid = "yellow", high = "red", midpoint = mean(range(edi))) +
  scale_size(range = c(0.3, 2), guide = FALSE) +
  theme_bw() +
  theme(legend.position = "bottom")


#### combine plots ####

cowplot::plot_grid(A, B, C, labels = "AUTO", ncol = 1, align = 'v')




