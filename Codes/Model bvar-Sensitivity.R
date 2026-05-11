# =========================================================================
# SCRIPT DE ANÁLISIS DE SENSIBILIDAD - ORDEN DE CHOLESKY
# =========================================================================

# 1) INSTALACIÓN Y CARGA DE PAQUETES
# -------------------------------------------------------------------------
req_pkgs <- c("bvartools", "MCMCpack", "dplyr", "haven", "purrr", "ggplot2", 
              "tidyr", "abind", "lubridate", "future", "future.apply", "readxl", "zoo")
invisible(lapply(req_pkgs, function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}))

# 2) CARGA DE DATOS Y TRANSFORMACIÓN
# -------------------------------------------------------------------------
dir_path <- "C:/Users/joser/Downloads/Master-s_thesis_finance/Data Base/Imputed_Data_MICE_Complete"
imputed_files <- list.files(dir_path, pattern = "\\.dta$", full.names = TRUE)
list_of_datasets <- map(imputed_files, haven::read_dta)
cat(sprintf("Se han cargado %d datasets originales imputados. ✅\n", length(list_of_datasets)))

vars_to_transform <- c("growth_gdp", "infl", "cbrate", "private_credit_gdp", "ln_reer", 
                       "ln1p_value_traded_gdp", "ln1p_mcap_gdp")

list_of_datasets <- map(list_of_datasets, function(df) {
  df %>%
    group_by(country) %>%
    mutate(across(all_of(vars_to_transform), ~ .x - lag(.x), .names = "d_{.col}")) %>%
    ungroup() %>%
    filter(year >= 1996 & year <= 2024)
})
cat("Todos los datasets han sido transformados a primeras diferencias. ✅\n\n")

# 3) DEFINICIÓN DE ESCENARIOS DE ORDENAMIENTO DE CHOLESKY
# -------------------------------------------------------------------------
# Baseline: Rápidas -> Lentas (Financieras/Precios -> Real/Política)
baseline_vars <- c("d_private_credit_gdp", "d_infl", "d_ln_reer", 
                   "d_ln1p_value_traded_gdp", "d_ln1p_mcap_gdp", 
                   "d_growth_gdp", "d_cbrate")

# Alternativa 1: Lentas -> Rápidas (Invertido)
alt1_vars <- c("d_cbrate", "d_growth_gdp", "d_ln1p_mcap_gdp", 
               "d_ln1p_value_traded_gdp", "d_ln_reer", "d_infl", "d_private_credit_gdp")

# Alternativa 2: Shocks Bursátiles al Final
alt2_vars <- c("d_growth_gdp", "d_cbrate", "d_infl", "d_private_credit_gdp", 
               "d_ln_reer", "d_ln1p_value_traded_gdp", "d_ln1p_mcap_gdp")

scenarios <- list(
  "Baseline (Rápidas->Lentas)" = baseline_vars,
  "Alt 1 (Lentas->Rápidas)" = alt1_vars,
  "Alt 2 (Bursátiles al Final)" = alt2_vars
)

p <- 2

# 4) FUNCIÓN DE ESTIMACIÓN Y EXTRACCIÓN DE IRFS
# -------------------------------------------------------------------------
get_irfs_for_ordering <- function(datasets, endogenous_vars, scenario_name) {
  cat(paste0("\nEjecutando escenario: ", scenario_name, "...\n"))
  
  # Usaremos solo el primer dataset imputado para ahorrar tiempo computacional en la sensibilidad.
  # Si se desea, se puede iterar sobre los 5, pero para la estructura de sensibilidad, la media del primero es representativa.
  panel_data <- datasets[[1]] 
  
  data_prepared_list <- panel_data %>%
    group_by(country) %>%
    filter(n() > 15) %>%
    ungroup() %>%
    group_split(country)
  
  country_irfs <- imap(data_prepared_list, function(country_df, country_index) {
    aligned_data <- country_df %>%
      select(year, all_of(endogenous_vars)) %>%
      arrange(year) %>%
      mutate(across(all_of(endogenous_vars), ~ na.locf(.x, na.rm = FALSE))) %>%
      na.omit()
    
    if(nrow(aligned_data) < 15) return(NULL)
    
    country_ts <- ts(aligned_data %>% select(all_of(endogenous_vars)))
    
    set.seed(123)
    # Reducimos un poco las iteraciones para hacer el análisis de sensibilidad más rápido (e.g., 3000 iteraciones en total)
    # Si quieres exactamente el mismo rigor que el original, sube iterations a 5000 y burnin a 2000.
    model_spec <- gen_var(country_ts, p = p, deterministic = "const", iterations = 3000, burnin = 1000) 
    
    model_with_priors <- add_priors(model_spec, coef = list(minnesota = list(kappa0 = 0.5, kappa1 = 0.5, kappa2 = 0.5, kappa3 = 1)))
    posterior <- draw_posterior(model_with_priors)
    
    irf_mcap <- irf(posterior, impulse = "d_ln1p_mcap_gdp", response = "d_growth_gdp", n.ahead = 10, keep_draws = TRUE)
    irf_vtrd <- irf(posterior, impulse = "d_ln1p_value_traded_gdp", response = "d_growth_gdp", n.ahead = 10, keep_draws = TRUE)
    
    return(list(mcap = irf_mcap, vtrd = irf_vtrd))
  })
  
  return(compact(country_irfs))
}

# 5) EJECUCIÓN DE ESCENARIOS
# -------------------------------------------------------------------------
plan(multisession, workers = parallel::detectCores() - 1)
cat(sprintf("\nIniciando estimación en paralelo con %d workers... \n", availableCores() - 1))

results_all_scenarios <- list()
for (sc_name in names(scenarios)) {
  results_all_scenarios[[sc_name]] <- get_irfs_for_ordering(list_of_datasets, scenarios[[sc_name]], sc_name)
}

plan(sequential)

# 6) PROCESAMIENTO Y VISUALIZACIÓN CONJUNTA
# -------------------------------------------------------------------------

extract_pooled_irf_data <- function(irfs_list, shock_name, scenario_name) {
  # Validar que irfs_list no esté vacío
  if (length(irfs_list) == 0) {
    cat(sprintf("Advertencia: irfs_list vacío para escenario %s\n", scenario_name))
    return(NULL)
  }
  
  # Extraer los draws de cada país para el shock específico
  all_draws <- lapply(irfs_list, function(country_result) {
    # Verificar que country_result es una lista con el shock_name
    if (is.null(country_result) || !is.list(country_result)) {
      return(NULL)
    }
    
    # Obtener el IRF para el shock específico
    irf_object <- country_result[[shock_name]]
    
    if (is.null(irf_object)) {
      return(NULL)
    }
    
    # Extraer los draws de la matriz IRF
    # La estructura típica de un objeto IRF es una matriz [draws x horizontes]
    if (is.matrix(irf_object)) {
      return(irf_object)
    } else if (is.list(irf_object) && "irfs" %in% names(irf_object)) {
      return(irf_object$irfs)
    } else {
      return(NULL)
    }
  })
  
  # Eliminar NULLs
  all_draws <- compact(all_draws)
  
  # Validar que tenemos datos
  if (length(all_draws) == 0) {
    cat(sprintf("Advertencia: No se encontraron datos para shock '%s' en escenario '%s'\n", shock_name, scenario_name))
    return(NULL)
  }
  
  # Combinar todos los draws de todos los países
  pooled_draws <- do.call(rbind, all_draws)
  
  # Validar dimensiones
  if (is.null(pooled_draws) || nrow(pooled_draws) == 0) {
    cat(sprintf("Advertencia: pooled_draws vacío para shock '%s' en escenario '%s'\n", shock_name, scenario_name))
    return(NULL)
  }
  
  # Calcular cuantiles por horizonte (por columna)
  pooled_summary <- apply(pooled_draws, 2, function(h) {
    quantile(h, c(0.05, 0.16, 0.5, 0.84, 0.95), na.rm = TRUE)
  })
  
  # Convertir a data frame
  df <- as.data.frame(t(pooled_summary)) %>%
    rename(lower_90 = `5%`, lower_68 = `16%`, median = `50%`, upper_68 = `84%`, upper_90 = `95%`) %>%
    mutate(
      Horizon = 0:(n() - 1), 
      Scenario = scenario_name
    ) %>%
    select(Horizon, Scenario, lower_90, lower_68, median, upper_68, upper_90)
  
  return(df)
}

plot_sensitivity <- function(shock_name, title) {
  # Validar que shock_name sea válido
  valid_shocks <- c("mcap", "vtrd")
  if (!shock_name %in% valid_shocks) {
    cat(sprintf("Error: shock_name '%s' no válido. Use: %s\n", shock_name, paste(valid_shocks, collapse = ", ")))
    return(NULL)
  }
  
  plot_data_list <- list()
  
  # Extraer datos para cada escenario
  for (sc_name in names(scenarios)) {
    tryCatch({
      irf_data <- extract_pooled_irf_data(
        results_all_scenarios[[sc_name]], 
        shock_name, 
        sc_name
      )
      
      if (!is.null(irf_data)) {
        plot_data_list[[sc_name]] <- irf_data
        cat(sprintf("✓ Datos extraídos para %s - Escenario: %s\n", shock_name, sc_name))
      }
    }, error = function(e) {
      cat(sprintf("✗ Error al procesar %s - Escenario: %s\n  Detalle: %s\n", 
                  shock_name, sc_name, e$message))
    })
  }
  
  # Combinar todos los datos
  final_df <- bind_rows(plot_data_list)
  
  # Validar que tenemos datos para graficar
  if (is.null(final_df) || nrow(final_df) == 0) {
    cat(sprintf("\n⚠️  No hay datos disponibles para graficar el shock '%s'\n\n", shock_name))
    return(NULL)
  }
  
  cat(sprintf("Creando gráfico para: %s\n", title))
  
  # Separar datos del baseline
  baseline_df <- final_df %>% filter(Scenario == "Baseline (Rápidas->Lentas)")
  
  # Definir colores según el tipo de shock
  if (shock_name == "mcap") {
    base_color <- "#253494"
    alt1_color <- "#1b9e77"
    alt2_color <- "#d95f02"
  } else if (shock_name == "vtrd") {
    base_color <- "#b30000"
    alt1_color <- "#1b9e77"
    alt2_color <- "#d95f02"
  }
  
  # Crear gráfico
  p_plot <- ggplot() +
    # Bandas de confianza del Baseline
    geom_ribbon(
      data = baseline_df, 
      aes(x = Horizon, ymin = lower_90, ymax = upper_90), 
      fill = "grey80", 
      alpha = 0.5,
      name = "IC 90% Baseline"
    ) +
    geom_ribbon(
      data = baseline_df, 
      aes(x = Horizon, ymin = lower_68, ymax = upper_68), 
      fill = "grey60", 
      alpha = 0.7,
      name = "IC 68% Baseline"
    ) +
    
    # Línea horizontal de referencia
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
    
    # Líneas medianas de los escenarios
    geom_line(
      data = final_df, 
      aes(x = Horizon, y = median, color = Scenario, linetype = Scenario), 
      linewidth = 1.1
    ) +
    geom_point(
      data = final_df, 
      aes(x = Horizon, y = median, color = Scenario, shape = Scenario), 
      size = 2.5,
      stroke = 1
    ) +
    
    # Escalas
    scale_x_continuous(
      breaks = 0:10, 
      minor_breaks = NULL,
      limits = c(-0.3, 10.3)
    ) +
    scale_color_manual(
      values = c(
        "Baseline (Rápidas->Lentas)" = base_color, 
        "Alt 1 (Lentas->Rápidas)" = alt1_color, 
        "Alt 2 (Bursátiles al Final)" = alt2_color
      ),
      name = "Ordenamiento"
    ) +
    scale_linetype_manual(
      values = c(
        "Baseline (Rápidas->Lentas)" = "solid",
        "Alt 1 (Lentas->Rápidas)" = "dashed",
        "Alt 2 (Bursátiles al Final)" = "dotted"
      ),
      name = "Ordenamiento"
    ) +
    scale_shape_manual(
      values = c(
        "Baseline (Rápidas->Lentas)" = 19,
        "Alt 1 (Lentas->Rápidas)" = 17,
        "Alt 2 (Bursátiles al Final)" = 15
      ),
      name = "Ordenamiento"
    ) +
    
    # Etiquetas y tema
    labs(
      title = paste("Sensibilidad Cholesky:", title),
      subtitle = "Las bandas grises representan los intervalos de confianza (68% y 90%) del modelo Baseline.",
      x = "Horizonte (Años)", 
      y = "Respuesta Mediana (p.p.)",
      color = "Ordenamiento", 
      linetype = "Ordenamiento", 
      shape = "Ordenamiento"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      panel.grid.major = element_line(color = "grey90", size = 0.3),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(size = 12, color = "grey40"),
      axis.title = element_text(face = "bold", size = 13)
    )
  
  # Imprimir gráfico
  print(p_plot)
  
  cat(sprintf("✓ Gráfico completado\n\n"))
  
  return(invisible(p_plot))
}

# =========================================================================
# EJECUCIÓN
# =========================================================================

cat("\n========================================\n")
cat("Generando gráficos de sensibilidad...\n")
cat("========================================\n\n")

# Generar gráficos
plot_sensitivity("mcap", "Shock de Capitalización Bursátil sobre Crecimiento")
plot_sensitivity("vtrd", "Shock de Valor Negociado sobre Crecimiento")

cat("\n========================================\n")
cat("✓ Análisis de Sensibilidad Finalizado\n")
cat("========================================\n")