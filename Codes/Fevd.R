# =========================================================================
# SCRIPT FINAL DE FEVD CON 'bvartools'
# Versión: con derivación de primeras diferencias dentro del script.
# =========================================================================

# 1) INSTALACIÓN Y CARGA DE PAQUETES
# -------------------------------------------------------------------------
req_pkgs <- c("bvartools", "MCMCpack", "dplyr", "haven", "purrr", "ggplot2",
              "tidyr", "abind", "lubridate", "zoo", "rlang", "tibble")
install.packages(setdiff(req_pkgs, rownames(installed.packages())))

library(bvartools)
library(dplyr)
library(haven)
library(purrr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(zoo)
library(rlang)
library(tibble)

# 2) CARGA DE DATOS (NIVELES/LOGS) Y FILTRO DE AÑOS
# -------------------------------------------------------------------------
dir_path <- "C:/Users/joser/Desktop/Data Base/Imputed_Data_Stationary"
imputed_files <- list.files(dir_path, pattern = "\\.dta$", full.names = TRUE)
list_of_datasets <- map(imputed_files, haven::read_dta)
cat(sprintf("Se han cargado %d datasets. ✅\n\n", length(list_of_datasets)))

list_of_datasets <- map(list_of_datasets, function(df) {
  df %>% filter(year >= 1996 & year <= 2024)
})
cat("Todos los datasets han sido filtrados al período 1996-2024. ✅\n\n")

# 2.1) TRANSFORMACIÓN A PRIMERAS DIFERENCIAS (crea columnas 'd_*')
# -------------------------------------------------------------------------
# Variables base sobre las que queremos tomar primera diferencia
vars_to_transform <- c(
  "growth_gdp", "infl", "cbrate", "private_credit_gdp",
  "ln_reer", "ln1p_value_traded_gdp", "ln1p_mcap_gdp"
)

# Función auxiliar: asegura insumos y crea d_*
ensure_inputs_and_make_diffs <- function(df) {
  # Normaliza nombres simples
  names(df) <- gsub("\\s+", "_", names(df))
  names(df) <- gsub("\\.+", "_", names(df))
  
  # Si no existe ln_reer pero sí 'reer', créalo
  if (!"ln_reer" %in% names(df) && "reer" %in% names(df)) {
    df <- df %>%
      arrange(country, year) %>%
      mutate(ln_reer = log(reer))
  }
  
  # Si no existen los ln1p_* pero sí niveles, créalos
  if (!"ln1p_value_traded_gdp" %in% names(df)) {
    cand <- intersect(names(df), c("value_traded_gdp", "traded_value_gdp"))
    if (length(cand) >= 1) {
      base <- cand[1]
      df <- df %>%
        arrange(country, year) %>%
        mutate(ln1p_value_traded_gdp = log1p(.data[[base]]))
    }
  }
  if (!"ln1p_mcap_gdp" %in% names(df)) {
    cand <- intersect(names(df), c("mcap_gdp", "market_cap_gdp"))
    if (length(cand) >= 1) {
      base <- cand[1]
      df <- df %>%
        arrange(country, year) %>%
        mutate(ln1p_mcap_gdp = log1p(.data[[base]]))
    }
  }
  
  # Ahora, para cada variable en vars_to_transform que exista, crea su d_*
  df <- df %>%
    arrange(country, year) %>%
    group_by(country) %>%
    {
      g <- .
      for (v in vars_to_transform) {
        if (v %in% names(g)) {
          dv <- paste0("d_", v)
          g[[dv]] <- g[[v]] - dplyr::lag(g[[v]])
        }
      }
      g
    } %>%
    ungroup()
  
  df
}

# Aplica a todos los datasets
list_of_datasets <- map(list_of_datasets, ensure_inputs_and_make_diffs)

# (Opcional) Diagnóstico de faltantes tras crear d_*
endogenous_vars <- c(
  "d_growth_gdp", "d_infl", "d_cbrate", "d_private_credit_gdp",
  "d_ln_reer", "d_ln1p_value_traded_gdp", "d_ln1p_mcap_gdp"
)
diag_missing <- map2_dfr(imputed_files, list_of_datasets, function(path, df) {
  tibble(
    file = basename(path),
    missing = paste(setdiff(endogenous_vars, names(df)), collapse = ", ")
  )
})
print(diag_missing)

# 3) DEFINICIÓN DE GRUPOS DE PAÍSES
# -------------------------------------------------------------------------
g7_countries <- c("CAN", "FRA", "DEU", "ITA", "JPN", "GBR", "USA")
oecd_countries <- c("AUS", "AUT", "BEL", "CAN", "CHL", "COL", "CRI", "CZE", "DNK", "ESP", "FIN",
                    "FRA", "DEU", "GRC", "HUN", "IRL", "ISL", "ISR", "ITA", "JPN", "KOR", "LUX",
                    "MEX", "NLD", "NZL", "NOR", "POL", "PRT", "SVK", "SVN", "SWE", "CHE", "TUR",
                    "GBR", "USA")
latam_caribbean <- c("ARG", "BRA", "BRB", "CHL", "COL", "CRI", "ECU", "JAM", "MEX", "PAN", "PER", "PRY")
north_america <- c("CAN", "USA")
asia <- c("ARE", "ARM", "AZE", "BGD", "BHR", "CHN", "HKG", "IDN", "IND", "ISR", "JPN", "JOR",
          "KAZ", "KOR", "KWT", "LBN", "LKA", "MYS", "OMN", "PAK", "PHL", "PNG", "PSE", "QAT",
          "SAU", "SGP", "THA", "TUR", "VNM")
europe <- c("AUT", "BEL", "BGR", "BLR", "CHE", "CYP", "CZE", "DEU", "DNK", "ESP", "FIN", "FRA",
            "GBR", "GRC", "HRV", "HUN", "IRL", "ISL", "ITA", "LUX", "MLT", "NLD", "NOR", "POL",
            "PRT", "ROU", "RUS", "SRB", "SVK", "SVN", "SWE", "UKR")
africa <- c("BWA", "CIV", "DZA", "EGY", "GHA", "KEN", "MAR", "MUS", "NAM", "NGA", "RWA",
            "SWZ", "SYC", "TZA", "ZAF", "ZMB")
oceania <- c("AUS", "NZL")

# 4) PARÁMETROS DEL VAR Y FUNCIONES DE ANÁLISIS
# -------------------------------------------------------------------------
p <- 2

# --- Función principal que ejecuta el análisis FEVD ---
run_fevd_analysis <- function(datasets, model_title, filter_expr = NULL) {
  cat(paste0("\n\n--- CALCULANDO FEVD PARA: ", model_title, " ---\n"))
  
  list_of_all_fevds <- map(datasets, function(panel_data) {
    if (!is.null(filter_expr)) {
      panel_data <- panel_data %>% filter(!!parse_expr(filter_expr))
    }
    if (nrow(panel_data) == 0 || dplyr::n_distinct(panel_data$country) == 0) return(NULL)
    
    data_prepared_list <- panel_data %>%
      group_by(country) %>%
      filter(n() > 15) %>%  # al menos ~15 obs por país
      ungroup() %>%
      group_split(country)
    
    if (length(data_prepared_list) == 0) return(NULL)
    
    # Estimación por país
    country_fevds <- map(data_prepared_list, function(country_df) {
      
      # Prepara matriz de endógenas (todas en diferencias ya creadas arriba)
      aligned_data <- country_df %>%
        select(year, any_of(endogenous_vars)) %>%
        arrange(year)
      
      # Imputación forward para pequeños huecos y drop de NA de bordes de diff
      aligned_data <- aligned_data %>%
        mutate(across(all_of(intersect(endogenous_vars, names(.))),
                      ~ zoo::na.locf(.x, na.rm = FALSE))) %>%
        drop_na()
      
      # Si faltan columnas claves, salta país
      if (!all(endogenous_vars %in% names(aligned_data))) {
        message("Saltando país; faltan variables: ",
                paste(setdiff(endogenous_vars, names(aligned_data)), collapse = ", "))
        return(NULL)
      }
      
      if (nrow(aligned_data) < (p + 10)) return(NULL)
      
      # Asegura numérico
      X <- aligned_data %>%
        select(all_of(endogenous_vars)) %>%
        mutate(across(everything(), ~ as.numeric(.))) %>%
        as.matrix()
      
      country_ts <- ts(X)  # anual
      
      set.seed(123)
      model_spec <- gen_var(country_ts, p = p, deterministic = "const",
                            iterations = 500, burnin = 100)
      model_with_priors <- add_priors(
        model_spec,
        coef = list(minnesota = list(kappa0 = 0.5, kappa1 = 0.5, kappa2 = 0.5, kappa3 = 1))
      )
      posterior <- draw_posterior(model_with_priors)
      
      # FEVD de la variable objetivo (crecimiento)
      fevd_result <- fevd(posterior, response = "d_growth_gdp", n.ahead = 10)
      
      # Promedio por horizonte y shock
      as.data.frame.table(apply(fevd_result, 1:2, mean),
                          responseName = "variance_explained") %>%
        rename(horizon = Var1, shock_var = Var2)
    })
    
    purrr::compact(country_fevds)
  })
  
  pool_and_plot_fevd(list_of_all_fevds, model_title)
}

# --- Función para agrupar y graficar FEVDs ---
pool_and_plot_fevd <- function(all_fevds_list, model_title) {
  pooled_fevds <- all_fevds_list %>%
    unlist(recursive = FALSE) %>%
    bind_rows()
  
  if (nrow(pooled_fevds) == 0) {
    cat(paste("No hay resultados FEVD para graficar en el modelo:", model_title, "\n"))
    return(NULL)
  }
  
  plot_data <- pooled_fevds %>%
    mutate(horizon = as.numeric(horizon) - 1,
           shock_var = factor(shock_var, levels = endogenous_vars)) %>%
    group_by(horizon, shock_var) %>%
    summarise(mean_variance_explained = mean(variance_explained * 100, na.rm = TRUE),
              .groups = "drop")
  
  fevd_plot <- ggplot(plot_data, aes(x = horizon, y = mean_variance_explained, fill = shock_var)) +
    geom_area(stat = "identity", position = "stack", color = "white", alpha = 0.85) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    scale_x_continuous(breaks = seq(0, 10, by = 2)) +
    labs(
      title = "Descomposición de la Varianza del Error de Pronóstico (FEVD) para el Crecimiento",
      subtitle = paste("Modelo:", model_title),
      x = "Horizonte (Años)",
      y = "% de Varianza Explicada",
      fill = "Shock Originado en:"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
  
  print(fevd_plot)
}

# 5) EJECUCIÓN DE TODOS LOS MODELOS
# -------------------------------------------------------------------------
run_fevd_analysis(list_of_datasets, "General (Todos los Países)")
run_fevd_analysis(list_of_datasets, "Países de Altos Ingresos", "high_income == 1")
run_fevd_analysis(list_of_datasets, "Países de Medios Ingresos", "middle_income == 1")
run_fevd_analysis(list_of_datasets, "Países de Bajos Ingresos", "low_income == 1")
run_fevd_analysis(list_of_datasets, "Países del G7", 'country %in% g7_countries')
run_fevd_analysis(list_of_datasets, "Países de la OCDE", 'country %in% oecd_countries')
run_fevd_analysis(list_of_datasets, "América Latina y el Caribe", 'country %in% latam_caribbean')
run_fevd_analysis(list_of_datasets, "Norteamérica", 'country %in% north_america')
run_fevd_analysis(list_of_datasets, "Asia", 'country %in% asia')
run_fevd_analysis(list_of_datasets, "Europa", 'country %in% europe')
run_fevd_analysis(list_of_datasets, "África", 'country %in% africa')
run_fevd_analysis(list_of_datasets, "Oceanía", 'country %in% oceania')

cat("\n--- Fin del Script de Análisis de FEVD --- \n")
