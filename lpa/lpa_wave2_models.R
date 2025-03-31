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

process_lpa_data <- function(file_path) {
  read_csv(file_path, show_col_types = FALSE)[, -1] |>
    filter(!is.na(justice_class))
}

wave2_3p <- process_lpa_data(here("data", "lpa_output_w2.csv"))
wave2_5p <- wave2_3p |>
  select(-justice_class) |>
  rename(justice_class = justice_class_5)

wave2_3p <- wave2_3p |>
  mutate(justice_class = factor(justice_class, levels = c("2", "3", "1"), labels = c("Egalitarianists", "Universalists", "Utilitarianists")))

wave2_5p <- wave2_5p |>
  mutate(
    justice_class = factor(
      justice_class,
      levels = c(
        "1",
        "3",
        "5",
        "4",
        "2"
      ),
      labels = c(
        "Limitarianists",
        "Equal outcomes",
        "Universalists",
        "Sufficientarianists",
        "Utilitarianists"
      )
    )
  )

# List of datasets for iteration
datasets <- list(wave2_3p = wave2_3p, wave2_5p = wave2_5p)

lapply(names(datasets), function(name) {
  prop_data <- as.data.frame(prop.table(table(datasets[[name]]$justice_class)))
  write.csv(prop_data, file = paste0("output/", name, "_proportions.csv"), row.names = FALSE)
})

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
    geom_line(alpha = 0.4, linewidth = 0.5) +
    labs(
      title = NULL,
      x = "Justice principle",
      y = "Sum score",
      color = NULL
    ) +
    theme_classic() +
    scale_color_viridis_d(
      end = .8,
      guide = guide_legend(override.aes = list(alpha = 1))
    ) +
    theme(
      legend.position = "bottom",
      text = element_text(size = main_text_size),
      strip.background = element_rect(size = 0),
      strip.text.x = element_text(size = main_text_size, face = "bold")
    )

  return(plot)
}

profile_plots_list <- lapply(datasets, function(data) {
  profile_plots = data |>
    group_by(justice_class) |>
    slice_sample(n = 50) |>
    pivot_participant_profiles_long(shorten_labels = TRUE) |>
    plot_participant_profiles() +
    facet_wrap(~justice_class, ncol = 5)
})

ggsave(
  here("output", "lpa_profiles_wave2_5p.png"),
  plot = profile_plots_list$wave2_5p,
  height = 4, width = 11
)
