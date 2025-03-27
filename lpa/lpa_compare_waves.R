library(ggplot2)
library(ggdist)
library(gghalves)
library(dplyr)
library(tidyr)
library(readr)
library(patchwork)
library(here)
library(scales)

main_text_size <- 12
set.seed(42)

pivot_participant_profiles_long <- function(data_wide, shorten_labels = FALSE) {
  labels_long <- c(
    "Equal outcomes",
    "Limitarian",
    "Sufficientarian",
    "Utilitarian"
  )
  
  labels_short <- c(
    "Equal",
    "Lim",
    "Suff",
    "Util"
  )
  
  data_wide |>
    pivot_longer(
      cols = c(
        "egalitarian",
        "sufficientarian",
        "limitarian",
        "utilitarian"
      ),
      names_to = "principle",
      values_to = "value"
    ) |>
    mutate(
      principle = factor(
        principle,
        levels = c(
          "egalitarian",
          "limitarian",
          "sufficientarian",
          "utilitarian"
        ),
        labels = if (shorten_labels) labels_short else labels_long
      )
    )
}

## get profiles per participant
plot_participant_profiles <- function(data) {
  plot <- ggplot(data, aes(
    x = principle,
    y = value,
    group = id,
    color = justice_class
  )) +
    geom_line(alpha = 0.4, size = 0.5) +
    labs(
      title = NULL,
      x = "Justice principle",
      y = "Sum score",
      color = "Justice orientation"
    ) +
    theme_classic() +
    scale_color_viridis_d(
      end = .8,
      guide = guide_legend(override.aes = list(alpha = 1))
    ) +
    theme(
      legend.position = "right",
      text = element_text(size = main_text_size),
      strip.background = element_rect(size = 0),
      strip.text.x = element_text(size = main_text_size, face = "bold")
    )

  return(plot)
}

## get raincloud plots
plot_raincloud <- function(data, main_text_size = 12) {
#   justice_class_proportions <- as.list(prop.table(table(data$justice_class)))
  
  data |>
    pivot_participant_profiles_long() |>
    # mutate(
    #   justice_class_prop = case_when(
    #     justice_class == "Egalitarianists" ~ paste0("Egalitarianists (", percent(justice_class_proportions$Egalitarian, accuracy = 0.1), ")"),
    #     justice_class == "Universalists" ~ paste0("Universalists (", percent(justice_class_proportions$Universal, accuracy = 0.1), ")"),
    #     justice_class == "Utilitarianists" ~ paste0("Utilitarianists (", percent(justice_class_proportions$Utilitarian, accuracy = 0.1), ")")
    #   )
    # ) |>
    ggplot(aes(
      x = principle,
      y = value,
      fill = justice_class,
      colour = justice_class
    )) +
    stat_slab(
      alpha = .6,
      adjust = 5,
      width = .6,
      .width = c(.5, .95),
      density = "bounded"
    ) +
    geom_boxplot(
      width = .15,
      fill = "white",
      outlier.shape = NA
    ) +
    # geom_half_point(
    #   side = "l",
    #   range_scale = .4,
    #   alpha = .2
    # ) +
    coord_flip() +
    scale_y_continuous(breaks = seq(0, 15, 1)) +
    scale_fill_viridis_d(end = .8) +
    scale_colour_viridis_d(end = .8) +
    facet_wrap(~justice_class_prop, ncol = 1) +
    labs(
      y = "Sum score",
      x = "Justice principle",
      colour = NULL,
      fill = NULL
    ) +
    theme_classic() +
    theme(
      legend.position = "bottom",
      text = element_text(size = main_text_size),
      strip.background = element_rect(size = 0),
      strip.text.x = element_text(size = main_text_size, face = "bold")
    )
}


################ test wave by wave #######################

process_lpa_data <- function(file_path) {
  read_csv(file_path, show_col_types = FALSE)[, -1] |>
    filter(!is.na(justice_class))
}

# Process datasets
wave1_data <- process_lpa_data(here("data", "lpa_output_w1.csv"))
wave2_data <- process_lpa_data(here("data", "lpa_output_w2.csv"))
wave3_data <- process_lpa_data(here("data", "lpa_output_w3.csv"))

# MANUALLY set factor levels for justice_class based on results
wave1_data <- wave1_data |>
  mutate(justice_class = factor(justice_class, levels = c("1", "3", "2"), labels = c("Egalitarianists", "Universalists", "Utilitarianists")))

wave2_data <- wave2_data |>
  mutate(justice_class = factor(justice_class, levels = c("2", "3", "1"), labels = c("Egalitarianists", "Universalists", "Utilitarianists")))

wave3_data <- wave3_data |>
  mutate(justice_class = factor(justice_class, levels = c("1", "3", "2"), labels = c("Egalitarianists", "Universalists", "Utilitarianists")))

# List of datasets for iteration
datasets <- list(wave1 = wave1_data, wave2 = wave2_data, wave3 = wave3_data)

profile_plots_list <- lapply(datasets, function(data) {
  profile_plots = data |>
    group_by(justice_class) |>
    slice_sample(n = 50) |>
    pivot_participant_profiles_long(shorten_labels = TRUE) |>
    plot_participant_profiles() +
    facet_wrap(~justice_class, ncol = 3)
})

combined_profile_plots <- wrap_plots(profile_plots_list, ncol = 1)  + 
  plot_layout(
    guides = "collect",
    axis_titles = "collect",
    axes = "collect"
   ) +
   plot_annotation(
    tag_levels = "1",
    tag_prefix = "Wave "
  )

datasets_long <- bind_rows(
  lapply(seq_along(datasets), function(i) {
    datasets[[i]] %>%
      mutate(dataset = names(datasets)[i])
  })
)

raincloud_plot_waves <- plot_raincloud(datasets_long) +
  facet_wrap(~dataset, ncol = 3)


ggsave(
  here("output", "lpa_raincloud_per_wave.png"),
  plot = raincloud_plot_waves,
  height = 6, width = 11
)

ggsave(
  here("output", "lpa_profiles_per_wave.png"),
  plot = combined_profile_plots,
  height = 6, width = 11
)
