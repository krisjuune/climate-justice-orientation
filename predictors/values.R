library(dplyr)
library(tidyr)
library(tibble)
library(readr)
library(here)
library(nnet)
library(ggplot2)

main_text_size <- 12
set.seed(42)

#################### regression #########################

lpa_data <- read.csv(here("data", "lpa_output_w3.csv")) |>
  select(-X)
value_data <- read.csv(here("data", "value_data_wave3.csv")) |>
  select(-X)

merged_data <- lpa_data |>
  inner_join(value_data, by = "id") |>
  na.omit() |>
  mutate(
    justice_class = factor(
      justice_class, levels = c(
        "3", "1", "2"
      ),
      labels = c(
        "Universalists", "Egalitarianists", "Utilitarianists"
      )
    )
  )

run_model <- function(base_level) {
  # set base
  merged_data <- merged_data |> 
    mutate(justice_class = relevel(justice_class, ref = base_level))
  
  # run model
  model <- multinom(justice_class ~ lreco + galtan + socio_ecol, data = merged_data)
  
  print(paste("Base level:", base_level))
  print(exp(coefs))

  coefs <- coef(model)
  se <- summary(model)$standard.errors
  odds_ratios <- exp(coefs)
  lower_ci <- exp(coefs - 1.96 * se)
  upper_ci <- exp(coefs + 1.96 * se)

  # create plot data
  coefs_df <- data.frame(odds_ratios = as.vector(odds_ratios),
                         lower_ci = as.vector(lower_ci),
                         upper_ci = as.vector(upper_ci))

  coefs_df$group <- rep(rownames(coefs), each = ncol(coefs))
  coefs_df$variable <- rep(c("(Intercept)", "lreco", "galtan", "socio_ecol"), times = length(rownames(coefs)))
  coefs_df |>
    mutate(base_level = base_level)
}

base_levels <- c("Universalists", "Egalitarianists", "Utilitarianists")
plot_data <- bind_rows(lapply(base_levels, function(base_level) {
  run_model(base_level)
}))

value_plot <- ggplot(plot_data, aes(x = variable, y = odds_ratios, color = group)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci),
                width = 0.1,
                position = position_dodge(width = 0.5)) +
  scale_y_log10(labels = label_number(accuracy = 0.01)) +
  scale_color_viridis_d(end = .8) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  coord_flip() +
  facet_wrap(~ base_level, ncol = 3) +
  labs(title = NULL,
       x = "Value indices",
       y = "Odds Ratio (Log Scale)",
       colour = NULL) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    text = element_text(size = main_text_size)
  )

value_plot

ggsave(
  here("output", "values_regression_plot.png"),
  plot = value_plot,
  height = 6, width = 11
)
