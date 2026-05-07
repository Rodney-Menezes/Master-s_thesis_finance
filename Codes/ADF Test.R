# =========================================================================
# SCRIPT DE DIAGNÓSTICO Y TRANSFORMACIÓN DE ESTACIONARIEDAD
# (VERSIÓN FINAL Y CORREGIDA PARA CONSERVAR TODAS LAS VARIABLES)
# =========================================================================

# 1) INSTALACIÓN Y CARGA DE PAQUETES
# -------------------------------------------------------------------------
req_pkgs <- c("dplyr", "haven", "purrr", "tidyr", "tseries")
install.packages(setdiff(req_pkgs, installed.packages()))

library(dplyr)
library(haven)
library(purrr)
library(tidyr)
library(tseries)


# 2) CARGA DE DATOS Y DEFINICIÓN DE PARÁMETROS
# -------------------------------------------------------------------------
dir_path <- "C:/Users/joser/Desktop/Data Base/Imputed_Data_MICE_Complete"
imputed_files <- list.files(dir_path, pattern = "\\.dta$", full.names = TRUE)
list_of_datasets <- map(imputed_files, haven::read_dta)
cat(sprintf("Se han cargado %d datasets imputados. ✅\n\n", length(list_of_datasets)))

endogenous_vars <- c("growth_gdp", "infl", "cbrate", "ln_reer", "ln1p_value_traded_gdp", "ln1p_mcap_gdp")
alpha <- 0.05


# 3) DIAGNÓSTICO INICIAL (SIN CAMBIOS)
# -------------------------------------------------------------------------
cat("========================================================\n")
cat("--- DIAGNÓSTICO INICIAL: PRUEBAS DE RAÍZ UNITARIA ---\n")
cat("========================================================\n")

safe_adf_test <- function(series) {
  series_clean <- na.omit(series)
  if(length(series_clean) > 4 && sd(series_clean, na.rm = TRUE) > 1e-6) {
    return(adf.test(series_clean, alternative = "stationary")$p.value)
  }
  return(NA_real_)
}

initial_adf_results <- list_of_datasets[[1]] %>%
  group_by(country) %>%
  summarise(across(all_of(endogenous_vars), safe_adf_test, .names = "p_{.col}"), .groups = "drop")

non_stationary_before <- initial_adf_results %>%
  pivot_longer(-country, names_to = "variable", values_to = "p_value") %>%
  filter(p_value > alpha | is.na(p_value)) %>%
  mutate(variable = gsub("p_", "", variable))

cat("Series no estacionarias ANTES de la transformación:\n")
print(non_stationary_before, n = 10)


# 4) TRANSFORMACIÓN A PRIMERA DIFERENCIA
# -------------------------------------------------------------------------
cat("\n========================================================\n")
cat("--- APLICANDO TRANSFORMACIÓN DE PRIMERA DIFERENCIA ---\n")
cat("========================================================\n")

vars_to_difference <- c("growth_gdp", "infl", "cbrate", "ln_reer", "ln1p_value_traded_gdp", "ln1p_mcap_gdp")
cat("Se aplicará la primera diferencia a TODAS las variables endógenas.\n")

transform_dataset <- function(df) {
  df %>%
    group_by(country) %>%
    mutate(across(
      all_of(vars_to_difference),
      ~ .x - lag(.x),
      .names = "d_{.col}"
    )) %>%
    ungroup()
}

list_of_transformed_datasets <- map(list_of_datasets, transform_dataset)
cat("Transformación completada para los 5 datasets. ✅\n\n")


# 5) DIAGNÓSTICO FINAL
# -------------------------------------------------------------------------
cat("========================================================\n")
cat("--- DIAGNÓSTICO FINAL: VERIFICANDO NUEVAS SERIES ---\n")
cat("========================================================\n")

new_endogenous_vars <- paste0("d_", endogenous_vars)

final_adf_results <- list_of_transformed_datasets[[1]] %>%
  group_by(country) %>%
  summarise(across(all_of(new_endogenous_vars), safe_adf_test, .names = "p_{.col}"), .groups = "drop")

non_stationary_after <- final_adf_results %>%
  pivot_longer(-country, names_to = "variable", values_to = "p_value") %>%
  filter(p_value > alpha | is.na(p_value)) %>%
  mutate(variable = gsub("p_", "", variable))

if (nrow(non_stationary_after) > 0) {
  cat("¡ADVERTENCIA! Algunas series AÚN pueden ser no estacionarias (esperado por baja potencia de la prueba):\n")
  print(non_stationary_after, n = 10)
} else {
  cat("¡Éxito! Todas las series transformadas ahora son estacionarias. ✅\n")
}


# 6) GUARDAR LOS DATASETS TRANSFORMADOS (VERSIÓN CORREGIDA)
# -------------------------------------------------------------------------
output_dir <- file.path(dirname(dir_path), "Imputed_Data_Stationary")
if (!dir.exists(output_dir)) dir.create(output_dir)

walk2(list_of_transformed_datasets, 1:length(list_of_transformed_datasets), function(df, i) {
  
  # --- MODIFICACIÓN CLAVE AQUÍ ---
  # En lugar de seleccionar qué mantener, ahora seleccionamos qué QUITAR.
  # Se eliminan las columnas originales que fueron diferenciadas, pero se
  # conservan todas las demás (identificadores, dummies, y cualquier otra).
  df_to_save <- df %>%
    select(-all_of(vars_to_difference))
  
  file_name <- paste0("panel_stationary_", i, ".dta")
  output_file <- file.path(output_dir, file_name)
  haven::write_dta(df_to_save, output_file)
  cat("Guardado:", output_file, "\n")
})

cat(paste("\nProceso completado. Los 5 datasets estacionarios se han guardado en la carpeta:", basename(output_dir), "✅\n"))