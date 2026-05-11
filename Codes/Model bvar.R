# =========================================================================
# SCRIPT FINAL CON 'bvartools' - VERSIÓN CON MODELO EXTENDIDO (7 VARIABLES)
# =========================================================================

# 1) INSTALACIÓN Y CARGA DE PAQUETES
# -------------------------------------------------------------------------
req_pkgs <- c("bvartools", "MCMCpack", "dplyr", "haven", "purrr", "ggplot2", 
              "tidyr", "abind", "lubridate", "future", "future.apply", "readxl", "zoo")
install.packages(setdiff(req_pkgs, installed.packages()))

library(bvartools)
library(dplyr)
library(haven)
library(purrr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(future)
library(future.apply)
library(readxl)
library(zoo)


# 2) CARGA DE DATOS Y TRANSFORMACIÓN
# -------------------------------------------------------------------------
# Se cargan los datos originales (no estacionarios) para tener acceso a todas las variables
dir_path <- "C:/Users/joser/Desktop/Data Base/Imputed_Data_MICE_Complete"
imputed_files <- list.files(dir_path, pattern = "\\.dta$", full.names = TRUE)
list_of_datasets <- map(imputed_files, haven::read_dta)
cat(sprintf("Se han cargado %d datasets originales imputados. ✅\n\n", length(list_of_datasets)))

# Se realiza la transformación a primeras diferencias directamente en el script
vars_to_transform <- c("growth_gdp", "infl", "cbrate", "private_credit_gdp", "ln_reer", 
                       "ln1p_value_traded_gdp", "ln1p_mcap_gdp")

list_of_datasets <- map(list_of_datasets, function(df) {
  df %>%
    group_by(country) %>%
    mutate(across(all_of(vars_to_transform), ~ .x - lag(.x), .names = "d_{.col}")) %>%
    ungroup() %>%
    filter(year >= 1996 & year <= 2024)
})
cat("Todos los datasets han sido transformados a primeras diferencias y filtrados. ✅\n\n")


# 3) DEFINICIÓN DE GRUPOS DE PAÍSES
# -------------------------------------------------------------------------
g7_countries <- c("CAN", "FRA", "DEU", "ITA", "JPN", "GBR", "USA")
oecd_countries <- c("AUS", "AUT", "BEL", "CAN", "CHL", "COL", "CRI", "CZE", "DNK", "ESP", "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ISL", "ISR", "ITA", "JPN", "KOR", "LUX", "MEX", "NLD", "NZL", "NOR", "POL", "PRT", "SVK", "SVN", "SWE", "CHE", "TUR", "GBR", "USA")
latam_caribbean <- c("ARG", "BRA", "BRB", "CHL", "COL", "CRI", "ECU", "JAM", "MEX", "PAN", "PER", "PRY")
north_america <- c("CAN", "USA")
asia <- c("ARE", "ARM", "AZE", "BGD", "BHR", "CHN", "HKG", "IDN", "IND", "ISR", "JPN", "JOR", "KAZ", "KOR", "KWT", "LBN", "LKA", "MYS", "OMN", "PAK", "PHL", "PNG", "PSE", "QAT", "SAU", "SGP", "THA", "TUR", "VNM")
europe <- c("AUT", "BEL", "BGR", "BLR", "CHE", "CYP", "CZE", "DEU", "DNK", "ESP", "FIN", "FRA", "GBR", "GRC", "HRV", "HUN", "IRL", "ISL", "ITA", "LUX", "MLT", "NLD", "NOR", "POL", "PRT", "ROU", "RUS", "SRB", "SVK", "SVN", "SWE", "UKR")
africa <- c("BWA", "CIV", "DZA", "EGY", "GHA", "KEN", "MAR", "MUS", "NAM", "NGA", "RWA", "SWZ", "SYC", "TZA", "ZAF", "ZMB")
oceania <- c("AUS", "NZL")


# 4) DEFINICIÓN DE FUNCIONES DE ANÁLISIS
# -------------------------------------------------------------------------
# Nuevo sistema VAR de 7 variables con ordenamiento teórico de Cholesky (de lentas a rápidas)
vars_to_transform <- c("d_private_credit_gdp", "d_infl", "d_ln_reer", 
                       "d_ln1p_value_traded_gdp", "d_ln1p_mcap_gdp", 
                       "d_growth_gdp", "d_cbrate")
p <- 2

# --- Función principal que ejecuta el análisis para un grupo de datos ---
run_analysis <- function(datasets, model_title, filter_expr = NULL, save_results = FALSE) {
  
  cat(paste0("\n\n--- EJECUTANDO MODELO: ", model_title, " ---\n"))
  
  diagnostic_saved <- FALSE
  
  list_of_all_irfs <- map(datasets, function(panel_data) {
    
    if (!is.null(filter_expr)) {
      panel_data <- panel_data %>% filter(!!rlang::parse_expr(filter_expr))
    }
    
    if (nrow(panel_data) == 0 || n_distinct(panel_data$country) == 0) return(NULL)
    
    data_prepared_list <- panel_data %>%
      group_by(country) %>%
      filter(n() > 15) %>%
      ungroup() %>%
      group_split(country)
    
    if (length(data_prepared_list) == 0) return(NULL)
    
    country_irfs <- imap(data_prepared_list, function(country_df, country_index) {
      
      aligned_data <- country_df %>%
        select(year, all_of(endogenous_vars)) %>%
        arrange(year) %>%
        mutate(across(all_of(endogenous_vars), ~ na.locf(.x, na.rm = FALSE))) %>%
        na.omit()
      
      if(nrow(aligned_data) < 15) return(NULL)
      
      country_ts <- ts(aligned_data %>% select(all_of(endogenous_vars)))
      
      set.seed(123)
      model_spec <- gen_var(country_ts, p = p, deterministic = "const", iterations = 200, burnin = 50)
      model_with_priors <- add_priors(model_spec,
                                      coef = list(minnesota = list(kappa0 = 0.5, kappa1 = 0.5, kappa2 = 0.5, kappa3 = 1)))
      posterior <- draw_posterior(model_with_priors)
      
      if (!diagnostic_saved && country_index == 1) {
        diagnostics_path <- "C:/Users/joser/Desktop/TFM - Fin/Diagnostic"
        if (!dir.exists(diagnostics_path)) { dir.create(diagnostics_path, recursive = TRUE) }
        diagnostics_file_name <- paste0("diagnostic_results_", gsub("[^[:alnum:]]", "_", model_title), ".rds")
        saveRDS(posterior, file = file.path(diagnostics_path, diagnostics_file_name))
        message("Resultados de diagnóstico para '", model_title, "' guardados en: ", diagnostics_file_name)
        diagnostic_saved <<- TRUE
      }
      
      irf_mcap <- irf(posterior, impulse = "d_ln1p_mcap_gdp", response = "d_growth_gdp", n.ahead = 10, keep_draws = TRUE)
      irf_vtrd <- irf(posterior, impulse = "d_ln1p_value_traded_gdp", response = "d_growth_gdp", n.ahead = 10, keep_draws = TRUE)
      
      return(list(mcap = irf_mcap, vtrd = irf_vtrd))
    })
    
    return(compact(country_irfs))
  })
  
  pool_and_plot(list_of_all_irfs, "mcap", model_title, "Respuesta a Shock de Capitalización Bursátil")
  pool_and_plot(list_of_all_irfs, "vtrd", model_title, "Respuesta a Shock de Valor Negociado")
  
  if (save_results) {
    results_path <- "C:/Users/joser/Desktop/TFM - Fin/Diagnostic"
    if (!dir.exists(results_path)) { dir.create(results_path, recursive = TRUE) }
    file_name <- paste0("irf_results_", gsub("[^[:alnum:]]", "_", model_title), ".rds")
    saveRDS(list_of_all_irfs, file = file.path(results_path, file_name))
    cat(paste("\n✅ Objeto IRF guardado como:", file_name, "\n"))
  }
}

# --- Función para agrupar y graficar ---
pool_and_plot <- function(all_irfs_list, shock_name, model_title, shock_title) {
  all_draws <- all_irfs_list %>% unlist(recursive = FALSE) %>% compact() %>% map(~ .x[[shock_name]])
  if (length(all_draws) == 0 || is.null(all_draws[[1]])) {
    cat(paste("No hay resultados válidos para graficar en el modelo:", model_title, "\n"))
    return(NULL)
  }
  
  pooled_draws <- do.call(rbind, all_draws)
  if(nrow(pooled_draws) == 0) return(NULL)
  
  pooled_summary <- apply(pooled_draws, 2, function(h) quantile(h, c(0.05, 0.16, 0.5, 0.84, 0.95), na.rm = TRUE))
  
  plot_data <- as.data.frame(t(pooled_summary)) %>%
    rename(lower_90 = `5%`, lower_68 = `16%`, median = `50%`, upper_68 = `84%`, upper_90 = `95%`) %>%
    mutate(Horizon = 0:(n() - 1))
  
  irf_plot <- ggplot(plot_data, aes(x = Horizon, y = median)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    geom_ribbon(aes(ymin = lower_90, ymax = upper_90), fill = "#2c7fb8", alpha = 0.3) +
    geom_ribbon(aes(ymin = lower_68, ymax = upper_68), fill = "#2c7fb8", alpha = 0.6) +
    geom_line(color = "#253494", linewidth = 1) +
    geom_point(color = "#253494", size = 2) +
    scale_x_continuous(breaks = 0:10) +
    labs(title = shock_title, subtitle = paste("Modelo:", model_title),
         x = "Horizonte (Años)", y = "Desviación del Cambio en la Tasa de Crecimiento (p.p.)") +
    theme_minimal(base_size = 14)
  
  print(irf_plot)
}


# 5) EJECUCIÓN DE TODOS LOS MODELOS
# -------------------------------------------------------------------------
plan(multisession, workers = parallel::detectCores() - 1)
cat(sprintf("\nIniciando cómputo en paralelo con %d workers... 🚀\n", availableCores() - 1))

# --- Modelos Base ---
run_analysis(list_of_datasets, "General (Todos los Países)", filter_expr = NULL, save_results = TRUE)
run_analysis(list_of_datasets, "Países de Altos Ingresos", filter_expr = "high_income == 1", save_results = TRUE)
run_analysis(list_of_datasets, "Países de Medios Ingresos", filter_expr = "middle_income == 1", save_results = TRUE)
run_analysis(list_of_datasets, "Países de Bajos Ingresos", filter_expr = "low_income == 1", save_results = TRUE)

# --- Modelos Económicos ---
run_analysis(list_of_datasets, "Países del G7", filter_expr = 'country %in% g7_countries', save_results = TRUE)
run_analysis(list_of_datasets, "Países de la OCDE", filter_expr = 'country %in% oecd_countries', save_results = TRUE)

# --- Modelos Geográficos ---
run_analysis(list_of_datasets, "América Latina y el Caribe", filter_expr = 'country %in% latam_caribbean', save_results = TRUE)
run_analysis(list_of_datasets, "Norteamérica", filter_expr = 'country %in% north_america', save_results = TRUE)
run_analysis(list_of_datasets, "Asia", filter_expr = 'country %in% asia', save_results = TRUE)
run_analysis(list_of_datasets, "Europa", filter_expr = 'country %in% europe', save_results = TRUE)
run_analysis(list_of_datasets, "África", filter_expr = 'country %in% africa', save_results = TRUE)
run_analysis(list_of_datasets, "Oceanía", filter_expr = 'country %in% oceania', save_results = TRUE)

plan(sequential)
cat("\n--- Fin del Script de Análisis Completo --- \n")