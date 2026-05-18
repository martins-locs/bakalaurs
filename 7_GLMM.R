# ============================================================================
# SUGU-SPECIFISKI CONFIDENCE THRESHOLD AR GLMM
# AR STANDARTIZĀCIJU (BEZ AUTO-LABOŠANAS)
# ============================================================================

library(tidyverse)
library(lme4)
library(pROC)
library(caret)
library(ggplot2)

set.seed(4)

# ----------------------------------------------------------------------------
# 1. DATU SAGATAVOŠANA
# ----------------------------------------------------------------------------

cat("============================================\n")
cat("GLMM ANALĪZE AR STANDARTIZĀCIJU\n")
cat("============================================\n\n")

# Ielādē rezultātus
df <- read_csv("./rezultati/TP_FP_FN_rezultati.csv")

# Filtrē tikai BirdNET detekcijas
df_birdnet <- df %>%
  filter(!is.na(confidence))

cat("BirdNET detekcijas:", nrow(df_birdnet), "\n")

# Sugu statistika
species_stats <- df_birdnet %>%
  filter(overlap == 0) %>%
  group_by(scientific_name) %>%
  summarise(
    TP = sum(TP),
    FP = sum(FP),
    total = n(),
    .groups = "drop"
  )

# Filtrē sugas
species_to_model <- species_stats %>%
  filter(
    TP >= 30,
    FP >= 10,
    total >= 50
  ) %>%
  pull(scientific_name)

cat("\nSugas modelēšanai:", length(species_to_model), "\n")
print(species_to_model)

# Filtrē datus
df_model <- df_birdnet %>%
  filter(
    scientific_name %in% species_to_model,
    overlap == 0
  )

cat("\nKopā novērojumi:", nrow(df_model), "\n")

# *** STANDARTIZĀCIJA ***
cat("\n=== STANDARTIZĀCIJA ===\n")

scaling_params <- df_model %>%
  summarise(
    conf_mean = mean(confidence, na.rm = TRUE),
    conf_sd = sd(confidence, na.rm = TRUE),
    site_mean = mean(site_confidence, na.rm = TRUE),
    site_sd = sd(site_confidence, na.rm = TRUE),
    week_mean = mean(site_week_confidence, na.rm = TRUE),
    week_sd = sd(site_week_confidence, na.rm = TRUE)
  )

cat("Parametri:\n")
print(scaling_params)

write_csv(scaling_params, "./rezultati/scaling_parameters.csv")

df_model <- df_model %>%
  mutate(
    confidence_z = (confidence - scaling_params$conf_mean) / scaling_params$conf_sd,
    site_conf_z = (site_confidence - scaling_params$site_mean) / scaling_params$site_sd,
    site_week_z = (site_week_confidence - scaling_params$week_mean) / scaling_params$week_sd
  )

cat("Standartizācija pabeigta!\n\n")

# ----------------------------------------------------------------------------
# 2. TRAIN/TEST SPLIT
# ----------------------------------------------------------------------------

stratified_split_temporal_balanced <- function(data, species, train_prop = 0.75, min_test_per_week = 1) {
  data %>%
    group_by({{ species }}, week, response) %>%
    mutate(
      n_group = n(),
      n_test_group = pmax(min_test_per_week, ceiling(n_group * (1 - train_prop))),
      n_train_group = n_group - n_test_group,
      row_in_group = row_number(),
      train = row_in_group <= n_train_group
    ) %>%
    ungroup() %>%
    select(-n_group, -n_test_group, -n_train_group, -row_in_group)
}

df_split <- df_model %>%
  stratified_split_temporal_balanced(scientific_name, train_prop = 0.75, min_test_per_week = 1)

# Pārbaude
cat("\n=== SPLIT PĀRBAUDE ===\n")

overall_check <- df_split %>%
  group_by(scientific_name, train) %>%
  summarise(
    n = n(),
    TP = sum(response == 1),
    FP = sum(response == 0),
    prop_TP = mean(response),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = train,
    values_from = c(n, TP, FP, prop_TP),
    names_prefix = "set_"
  )

cat("\nProporciju pārbaude:\n")
print(overall_check, n = Inf)

# ----------------------------------------------------------------------------
# 3. FUNKCIJA: GLMM MODELĒŠANA
# ----------------------------------------------------------------------------

fit_species_models <- function(species_name, data) {
  
  cat("\n========================================\n")
  cat("MODELĒ:", species_name, "\n")
  cat("========================================\n")
  
  sp_data <- data %>% filter(scientific_name == species_name)
  train_data <- sp_data %>% filter(train == TRUE)
  test_data <- sp_data %>% filter(train == FALSE)
  
  cat("Train n =", nrow(train_data), "(TP:", sum(train_data$response), ")\n")
  cat("Test n =", nrow(test_data), "(TP:", sum(test_data$response), ")\n")
  
  # Pārbaudes
  test_tp <- sum(test_data$response == 1)
  test_fp <- sum(test_data$response == 0)
  
  if (test_tp == 0 | test_fp == 0) {
    cat("BRĪDINĀJUMS: Nav abu klašu test setā!\n")
    return(NULL)
  }
  
  # *** MODEL 1: confidence_z ***
  cat("\n[Model 1] confidence_z\n")
  m1 <- NULL
  
  tryCatch({
    m1 <- glmer(response ~ confidence_z + (1 | site_id), 
                data = train_data, 
                family = binomial,
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
  }, error = function(e) {
    cat("KĻŪDA Model 1:", e$message, "\n")
    m1 <<- NULL
  })
  
  # *** MODEL 2: + site_conf_z ***
  cat("[Model 2] + site_conf_z\n")
  m2 <- NULL
  
  tryCatch({
    m2 <- glmer(response ~ confidence_z + site_conf_z + (1 | site_id), 
                data = train_data, 
                family = binomial,
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
  }, error = function(e) {
    cat("KĻŪDA Model 2:", e$message, "\n")
    m2 <<- NULL
  })
  
  # *** MODEL 3: + site_week_z ***
  cat("[Model 3] + site_week_z\n")
  m3 <- NULL
  
  tryCatch({
    m3 <- glmer(response ~ confidence_z + site_conf_z + site_week_z + (1 | site_id), 
                data = train_data, 
                family = binomial,
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
  }, error = function(e) {
    cat("KĻŪDA Model 3:", e$message, "\n")
    m3 <<- NULL
  })
  
  # Novērtē
  models <- list(m1 = m1, m2 = m2, m3 = m3)
  models <- models[!sapply(models, is.null)]
  
  if (length(models) == 0) {
    cat("NEIZDEVĀS!\n")
    return(NULL)
  }
  
  results <- list()
  
  for (model_name in names(models)) {
    model <- models[[model_name]]
    
    tryCatch({
      test_data$pred_prob <- predict(model, newdata = test_data, 
                                     type = "response", allow.new.levels = TRUE)
      
      roc_obj <- roc(test_data$response, test_data$pred_prob, quiet = TRUE)
      auc_val <- as.numeric(auc(roc_obj))
      
      coords_obj <- coords(roc_obj, x = "best", best.method = "youden", 
                           ret = c("threshold", "sensitivity", "specificity"))
      optimal_threshold <- coords_obj$threshold
      
      test_data$pred_class <- if_else(test_data$pred_prob >= optimal_threshold, 1, 0)
      
      cm <- confusionMatrix(
        factor(test_data$pred_class, levels = c(0, 1)),
        factor(test_data$response, levels = c(0, 1)),
        positive = "1"
      )
      
      precision_val <- ifelse(is.na(cm$byClass["Precision"]), 0, cm$byClass["Precision"])
      recall_val <- ifelse(is.na(cm$byClass["Sensitivity"]), 0, cm$byClass["Sensitivity"])
      f1_val <- ifelse(is.na(cm$byClass["F1"]), 0, cm$byClass["F1"])
      
      aic_val <- AIC(model)
      
      results[[model_name]] <- tibble(
        model = model_name,
        AIC = aic_val,
        AUC = auc_val,
        threshold = optimal_threshold,
        precision = precision_val,
        recall = recall_val,
        F1 = f1_val
      )
      
      cat(sprintf("[%s] AIC: %.1f | AUC: %.3f | F1: %.3f\n",
                  model_name, aic_val, auc_val, f1_val))
      
    }, error = function(e) {
      cat("KĻŪDA novērtējot", model_name, "\n")
    })
  }
  
  if (length(results) == 0) return(NULL)
  
  results_df <- bind_rows(results) %>% mutate(species = species_name)
  
  best_model_name <- results_df %>%
    arrange(desc(F1), desc(AUC), AIC) %>%
    slice(1) %>%
    pull(model)
  
  cat("\n>>> LABĀKAIS:", best_model_name, "<<<\n")
  
  best_model <- models[[best_model_name]]
  coefs <- fixef(best_model)
  
  list(
    species = species_name,
    performance = results_df,
    best_model = best_model_name,
    coefficients_scaled = coefs
  )
}

# ----------------------------------------------------------------------------
# 4. MODELĒ VISAS SUGAS
# ----------------------------------------------------------------------------

all_results <- map(species_to_model, ~fit_species_models(.x, df_split))
all_results <- all_results[!sapply(all_results, is.null)]

cat("\n========================================\n")
cat("MODELĒTAS SUGAS:", length(all_results), "\n")
cat("========================================\n")

# ----------------------------------------------------------------------------
# 5. SAGLABĀ REZULTĀTUS
# ----------------------------------------------------------------------------

# Performance
performance_table <- map_df(all_results, ~.x$performance)
write_csv(performance_table, "./rezultati/glmm_performance.csv")

# Koeficienti (STANDARTIZĒTIE)
safe_extract_coef <- function(coef_vec, coef_name) {
  if (coef_name %in% names(coef_vec)) {
    return(as.numeric(coef_vec[coef_name]))
  } else {
    return(NA_real_)
  }
}

coefficients_table <- map_df(all_results, function(x) {
  coefs <- x$coefficients_scaled
  
  tibble(
    species = x$species,
    best_model = x$best_model,
    intercept_z = safe_extract_coef(coefs, "(Intercept)"),
    beta_confidence_z = safe_extract_coef(coefs, "confidence_z"),
    beta_site_conf_z = safe_extract_coef(coefs, "site_conf_z"),
    beta_site_week_conf_z = safe_extract_coef(coefs, "site_week_z")
  )
})

# Pievieno performance
final_table <- coefficients_table %>%
  left_join(
    performance_table %>%
      group_by(species) %>%
      arrange(desc(F1), desc(AUC), AIC) %>%
      slice(1) %>%
      ungroup(),
    by = c("species", "best_model" = "model")
  )

write_csv(final_table, "./rezultati/glmm_final_coefficients_SCALED.csv")

# ----------------------------------------------------------------------------
# 6. LIETOTĀJU INSTRUKCIJA
# ----------------------------------------------------------------------------

generate_user_guide <- function(coefficients_table, scaling_params) {
  
  coefficients_table %>%
    mutate(
      usage_instruction = case_when(
        best_model == "m1" ~ sprintf(
          "Standartizē: conf_z = (conf - %.3f) / %.3f\nAprēķina: P = 1/(1+exp(-(%.3f + %.3f×conf_z)))\nIF P ≥ %.3f → ACCEPT",
          scaling_params$conf_mean, scaling_params$conf_sd,
          intercept_z, beta_confidence_z, threshold
        ),
        best_model == "m2" ~ sprintf(
          "Standartizē: conf_z = (conf-%.3f)/%.3f, site_z = (site-%.3f)/%.3f\nP = 1/(1+exp(-(%.3f + %.3f×conf_z + %.3f×site_z)))\nIF P ≥ %.3f → ACCEPT",
          scaling_params$conf_mean, scaling_params$conf_sd,
          scaling_params$site_mean, scaling_params$site_sd,
          intercept_z, beta_confidence_z, beta_site_conf_z, threshold
        ),
        best_model == "m3" ~ sprintf(
          "Standartizė: conf_z=(conf-%.3f)/%.3f, site_z=(site-%.3f)/%.3f, week_z=(week-%.3f)/%.3f\nP = 1/(1+exp(-(%.3f + %.3f×conf_z + %.3f×site_z + %.3f×week_z)))\nIF P ≥ %.3f → ACCEPT",
          scaling_params$conf_mean, scaling_params$conf_sd,
          scaling_params$site_mean, scaling_params$site_sd,
          scaling_params$week_mean, scaling_params$week_sd,
          intercept_z, beta_confidence_z, beta_site_conf_z, beta_site_week_conf_z, threshold
        )
      )
    ) %>%
    select(species, best_model, F1, AUC, threshold, usage_instruction) %>%
    arrange(desc(F1))
}

user_guide <- generate_user_guide(final_table, scaling_params)
write_csv(user_guide, "./rezultati/user_guide.csv")

# ----------------------------------------------------------------------------
# 7. KOPSAVILKUMS
# ----------------------------------------------------------------------------

cat("\n========================================\n")
cat("LABĀKO MODEĻU SADALĪJUMS:\n")
cat("========================================\n")
print(table(final_table$best_model))

cat("\n========================================\n")
cat("TOP 5 SUGAS:\n")
cat("========================================\n")
final_table %>%
  arrange(desc(F1)) %>%
  select(species, best_model, F1, AUC, threshold) %>%
  head(5) %>%
  print()

cat("\n========================================\n")
cat("KOEFICIENTU RANGE:\n")
cat("========================================\n")
final_table %>%
  summarise(
    intercept_min = min(intercept_z),
    intercept_max = max(intercept_z),
    beta_conf_min = min(beta_confidence_z),
    beta_conf_max = max(beta_confidence_z)
  ) %>%
  print()

# ----------------------------------------------------------------------------
# 8. VIZUALIZĀCIJAS
# ----------------------------------------------------------------------------

# Model comparison
ggplot(performance_table, aes(x = model, y = F1, fill = model)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(
    title = "Modeļu salīdzinājums",
    x = "Modelis",
    y = "F1 Score"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave("./rezultati/model_comparison_f1.png", width = 8, height = 6, dpi = 300, bg = "white")

# Species performance
final_table %>%
  mutate(species_short = str_extract(species, "^[^ ]+ [^ ]+")) %>%
  ggplot(aes(x = reorder(species_short, F1), y = F1, fill = best_model)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Performance pa sugām",
    x = "Suga",
    y = "F1 Score",
    fill = "Modelis"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave("./rezultati/species_performance.png", width = 10, height = 8, dpi = 300, bg = "white")

cat("\n========================================\n")
cat("PABEIGTS!\n")
cat("========================================\n")
cat("\nIzveidotie faili:\n")
cat("1. scaling_parameters.csv\n")
cat("2. glmm_performance.csv\n")
cat("3. glmm_final_coefficients_SCALED.csv\n")
cat("4. user_guide.csv\n")
cat("5. model_comparison_f1.png\n")
cat("6. species_performance.png\n")
cat("========================================\n")