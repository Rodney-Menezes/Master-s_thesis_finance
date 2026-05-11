# =========================================================================
# HETEROGENEIDAD — IMPRIME GRÁFICO EN CONSOLA + PROBABILIDADES MCAP/VTRD
# Versión Corregida: Combina (Pools) todas las imputaciones MICE
# =========================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(ggplot2)
  library(tidyr)
  library(haven)
  library(grid)
})

# ---------------------- 1) Rutas y lectura -------------------------------
results_path    <- "C:/Users/joser/Desktop/TFM - Fin/Diagnostic"
file_to_analyze <- "irf_results_General__Todos_los_Países_.rds"
results_file    <- file.path(results_path, file_to_analyze)

cat("Cargando resultados desde:", results_file, "\n")
if (!file.exists(results_file)) stop("No se encontró: ", results_file)

### CORRECCIÓN METODOLÓGICA (PARTE A):
# Cargar la lista de listas (una por cada imputación MICE)
list_of_lists_by_imputation <- readRDS(results_file)
cat(sprintf("Resultados cargados para %d datasets de imputación. ✅\n\n", length(list_of_lists_by_imputation)))


# ------------------ 2) Datos base para clasificación ---------------------
dir_path_data  <- "C:/Users/joser/Desktop/TFM - Fin/Data Base/Imputed_Data_MICE_Complete"
data_file_name <- "panel_imputed_mice_complete_1.dta"
data_file_path <- file.path(dir_path_data, data_file_name)
if (!file.exists(data_file_path)) stop("No se encontró: ", data_file_path)
original_data <- haven::read_dta(data_file_path)

# Obtener la lista ÚNICA de países modelados
countries_modeled <- original_data %>%
  filter(year >= 1996, year <= 2024) %>%
  group_by(country) %>% filter(n() > 15) %>% ungroup() %>%
  pull(country) %>% unique() %>% sort()

n_countries <- length(countries_modeled)
n_imputations <- length(list_of_lists_by_imputation)


### CORRECCIÓN METODOLÓGICA (PARTE B):
# Desanidar la lista de listas en una sola lista larga (N_imputaciones * N_países)
list_of_all_irfs <- unlist(list_of_lists_by_imputation, recursive = FALSE)

# Crear un vector de nombres de países replicado para que coincida
all_country_names <- rep(countries_modeled, times = n_imputations)

# Verificación de seguridad
if (length(list_of_all_irfs) != length(all_country_names)) {
  warning("¡La longitud de los resultados no coincide con (países * imputaciones)!")
  nmin <- min(length(list_of_all_irfs), length(all_country_names))
  list_of_all_irfs    <- list_of_all_irfs[seq_len(nmin)]
  all_country_names <- all_country_names[seq_len(nmin)]
  cat("⚠️ Longitudes no coincidían; se alinearon por el mínimo común.\n")
}

names(list_of_all_irfs) <- all_country_names
cat(sprintf("Nombres de países asignados a %d resultados (países * imputaciones). ✅\n", length(list_of_all_irfs)))


country_info <- original_data %>%
  select(country, high_income, middle_income, low_income) %>%
  mutate(country = as.character(country)) %>%
  distinct(country, .keep_all = TRUE)

# --------- 3) Extracción robusta de componentes de IRF -------------------
# (Esta sección es robusta y no necesita cambios)
get_component_df <- function(irf_country, keys_priority) {
  for (k in keys_priority) {
    if (!is.null(irf_country[[k]])) {
      M <- irf_country[[k]]
      if (is.matrix(M) || is.data.frame(M)) return(as.data.frame(M))
    }
  }
  data.frame()
}
keys_mcap <- c("mcap","mcap_gdp","ln1p_mcap_gdp","d_ln1p_mcap_gdp","MCAP","MCAP_GDP")
keys_vtrd <- c("vtrd","value_traded","value_traded_gdp","ln1p_value_traded_gdp",
               "d_ln1p_value_traded_gdp","VTRD","VALUE_TRADED_GDP")

pooled_irfs <- purrr::imap_dfr(list_of_all_irfs, function(irf_country, cty) {
  mcap_df <- get_component_df(irf_country, keys_mcap)
  vtrd_df <- get_component_df(irf_country, keys_vtrd)
  
  if (nrow(mcap_df) > 0) {
    mcap_long <- mcap_df %>%
      mutate(draw_id = dplyr::row_number()) %>%
      pivot_longer(-draw_id, names_to = "horizon_chr", values_to = "response_mcap")
  } else {
    mcap_long <- tibble::tibble(draw_id = integer(0), horizon_chr = character(0), response_mcap = numeric(0))
  }
  
  if (nrow(vtrd_df) > 0) {
    vtrd_long <- vtrd_df %>%
      mutate(draw_id = dplyr::row_number()) %>%
      pivot_longer(-draw_id, names_to = "horizon_chr", values_to = "response_vtrd")
  } else {
    vtrd_long <- tibble::tibble(draw_id = integer(0), horizon_chr = character(0), response_vtrd = numeric(0))
  }
  
  full_join(mcap_long, vtrd_long, by = c("draw_id","horizon_chr")) %>%
    mutate(
      country = cty,
      horizon = { h <- as.integer(gsub("\\D", "", horizon_chr)); ifelse(is.na(h), seq_along(horizon_chr)-1L, h-1L) }
    ) %>%
    select(country, draw_id, horizon, response_mcap, response_vtrd)
})

full_summary_data <- pooled_irfs %>% left_join(country_info, by = "country")
cat("Datos IRF reformateados y unidos. ✅\n\n")

# ------------------- 4) Resúmenes por grupo (opcional) -------------------
# (mantengo por si quieres imprimirlos; se omiten aquí para brevedad)

# ------------------- 5) Impacto acumulado -----------------------

### CORRECCIÓN DE CONSISTENCIA:
# Ajustado a 1:3 para coincidir con las etiquetas de las Secciones 8 y 9
horizons_to_sum <- 1:3
cat(sprintf("Calculando impacto acumulado para horizontes %s. ✅\n", paste(horizons_to_sum, collapse = ", ")))


cumulative_draws <- full_summary_data %>%
  filter(horizon %in% horizons_to_sum) %>%
  group_by(country, draw_id, high_income, middle_income, low_income) %>%
  summarise(
    cumulative_mcap_draw = sum(response_mcap, na.rm = TRUE),
    cumulative_vtrd_draw = sum(response_vtrd, na.rm = TRUE),
    .groups = "drop"
  )

# -------- 6) Asegurar impresión del gráfico en consola (RStudio/GUI) ------
# (Esta función es robusta y no necesita cambios)
print_plot_in_console <- function(p, width=9, height=6) {
  # 1) Intento directo (RStudioGD normalmente basta)
  ok <- TRUE
  res <- try(print(p), silent = TRUE)
  if (inherits(res, "try-error")) ok <- FALSE
  
  # 2) Fallback: dibujar vía grid (fuerza refresco del canvas)
  if (!ok) {
    try({
      grid::grid.newpage()
      grid::grid.draw(ggplotGrob(p))
      ok <- TRUE
    }, silent = TRUE)
  }
  
  # 3) Último recurso: abrir device nativo y reimprimir
  if (!ok) {
    try({
      if (.Platform$OS.type == "windows") grDevices::windows(width=width, height=height)
      else if (capabilities("aqua"))     grDevices::quartz(width=width, height=height)
      else if (capabilities("X11"))      grDevices::x11(width=width, height=height)
      print(p)
      ok <- TRUE
    }, silent = TRUE)
  }
  
  if (ok) {
    flush.console()
    cat("✅ Gráfico impreso en el dispositivo gráfico activo.\n")
  } else {
    cat("⚠️ No se pudo imprimir el gráfico. Abre manualmente un device (windows()/quartz()/x11()) y vuelve a correr esta sección.\n")
  }
  invisible(ok)
}

# -------------------- 7) Gráfico de violín en consola ---------------------
cat("\n--- 2. VISUALIZACIÓN (GRÁFICO DE VIOLÍN - MCAP) ---\n\n")

violin_data <- cumulative_draws %>%
  group_by(country, high_income, middle_income, low_income) %>%
  summarise(median_cumulative_impact = median(cumulative_mcap_draw, na.rm = TRUE), .groups = "drop") %>%
  mutate(income_group = case_when(
    high_income == 1 ~ "Alto Ingreso",
    middle_income == 1 ~ "Medio Ingreso",
    low_income  == 1 ~ "Bajo Ingreso",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(income_group), is.finite(median_cumulative_impact))

if (nrow(violin_data) == 0) {
  cat("⚠️ No hay datos para el violín (revisa que existan response_mcap y clasificaciones).\n")
} else {
  violin_plot <- ggplot(violin_data,
                        aes(x = income_group, y = median_cumulative_impact, fill = income_group)) +
    geom_violin(trim = FALSE, alpha = 0.6) +
    geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      title = "Distribución del Impacto Acumulado por Grupo de Ingreso",
      subtitle = "Shock de Capitalización Bursátil sobre el Crecimiento",
      x = "Grupo de Ingreso", y = "Impacto Acumulado Mediano (p.p.)"
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none")
  
  print_plot_in_console(violin_plot)
}


# -------------------- 7b) Gráfico de violín para Valor Negociado (VTRD) ---------------------
cat("\n--- 2b. VISUALIZACIÓN (GRÁFICO DE VIOLÍN PARA VTRD) ---\n\n")

# Preparamos los datos específicos para el gráfico de VTRD
violin_data_vtrd <- cumulative_draws %>%
  group_by(country, high_income, middle_income, low_income) %>%
  summarise(median_cumulative_impact = median(cumulative_vtrd_draw, na.rm = TRUE), .groups = "drop") %>%
  mutate(income_group = case_when(
    high_income   == 1 ~ "Alto Ingreso",
    middle_income == 1 ~ "Medio Ingreso",
    low_income    == 1 ~ "Bajo Ingreso",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(income_group), is.finite(median_cumulative_impact))

# Verificamos si hay datos para graficar
if (nrow(violin_data_vtrd) == 0) {
  cat("⚠️ No hay datos para el violín de VTRD (revisa que existan response_vtrd y clasificaciones).\n")
} else {
  # Creamos el gráfico de violín para VTRD
  violin_plot_vtrd <- ggplot(violin_data_vtrd,
                             aes(x = income_group, y = median_cumulative_impact, fill = income_group)) +
    geom_violin(trim = FALSE, alpha = 0.6) +
    geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      title = "Distribución del Impacto Acumulado por Grupo de Ingreso",
      subtitle = "Shock de Valor Negociado sobre el Crecimiento",
      x = "Grupo de Ingreso", y = "Impacto Acumulado Mediano (p.p.)"
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none")
  
  # Imprimimos el gráfico en la consola
  print_plot_in_console(violin_plot_vtrd)
}

# --------------- 8) Pruebas bayesianas — MCAP y VTRD ---------------------
cat("\n--- 3. PRUEBAS BAYESIANAS ENTRE GRUPOS (MCAP y VTRD) ---\n\n")

compare_groups_bayesian <- function(draws_df, g1_filter, g2_filter, g1_name, g2_name, target = c("mcap","vtrd")) {
  target <- match.arg(target)
  col <- if (target == "mcap") "cumulative_mcap_draw" else "cumulative_vtrd_draw"
  
  g1 <- draws_df %>% filter({{ g1_filter }})
  g2 <- draws_df %>% filter({{ g2_filter }})
  if (nrow(g1) == 0 || nrow(g2) == 0) {
    cat(sprintf("No se pueden comparar %s vs %s (grupo vacío) para %s.\n", g1_name, g2_name, toupper(target)))
    return(NA_real_)
  }
  # Agrupamos por draw_id para obtener la distribución de la media del grupo
  a1 <- g1 %>% group_by(draw_id) %>% summarise(avg_impact = mean(.data[[col]], na.rm = TRUE), .groups = "drop")
  a2 <- g2 %>% group_by(draw_id) %>% summarise(avg_impact = mean(.data[[col]], na.rm = TRUE), .groups = "drop")
  
  # Combinamos las distribuciones
  comparison <- inner_join(a1, a2, by = "draw_id", suffix = c("_g1","_g2"))
  
  # Comparamos las distribuciones posteriores
  prob <- mean(comparison$avg_impact_g1 > comparison$avg_impact_g2, na.rm = TRUE)
  
  ### MEJORA: Etiqueta de horizonte dinámica
  cat(sprintf("• Probabilidad de que el impacto acumulado promedio (%s, %s) sea MAYOR en '%s' que en '%s': %.2f%%\n",
              toupper(target), 
              paste(horizons_to_sum, collapse = "–"), 
              g1_name, g2_name, 100*prob))
  invisible(prob)
}

# MCAP
compare_groups_bayesian(cumulative_draws, high_income == 1, low_income == 1, "Altos Ingresos", "Bajos Ingresos", "mcap")
compare_groups_bayesian(cumulative_draws, high_income == 1, middle_income == 1, "Altos Ingresos", "Medios Ingresos", "mcap")
compare_groups_bayesian(cumulative_draws, middle_income == 1, low_income == 1, "Medios Ingresos", "Bajos Ingresos", "mcap")

# VTRD
compare_groups_bayesian(cumulative_draws, high_income == 1, low_income == 1, "Altos Ingresos", "Bajos Ingresos", "vtrd")
compare_groups_bayesian(cumulative_draws, high_income == 1, middle_income == 1, "Altos Ingresos", "Medios Ingresos", "vtrd")
compare_groups_bayesian(cumulative_draws, middle_income == 1, low_income == 1, "Medios Ingresos", "Bajos Ingresos", "vtrd")

# -------- 9) P(Impacto acumulado > 0) — MCAP y VTRD (texto claro) --------
cat("\n--- 4. PROBABILIDAD DE IMPACTO POSITIVO (>0) ---\n\n")

prob_impacto_pos_mcap <- cumulative_draws %>%
  # Agrupamos por grupo de ingreso Y draw_id
  group_by(high_income, middle_income, low_income, draw_id) %>%
  # Calculamos la media del grupo para CADA draw
  summarise(avg = mean(cumulative_mcap_draw, na.rm = TRUE), .groups = "drop") %>%
  mutate(grupo = case_when(
    high_income == 1 ~ "Alto Ingreso",
    middle_income == 1 ~ "Medio Ingreso",
    low_income  == 1 ~ "Bajo Ingreso",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(grupo)) %>%
  # Ahora agrupamos solo por grupo
  group_by(grupo) %>%
  # Calculamos el porcentaje de draws donde la media del grupo fue > 0
  summarise(`P(impacto > 0) [%] (MCAP)` = round(mean(avg > 0, na.rm = TRUE)*100, 2), .groups = "drop")

cat(sprintf("Probabilidad de que el impacto acumulado sea POSITIVO por grupo (MCAP, horizontes %s):\n", paste(horizons_to_sum, collapse = "–")))
print(prob_impacto_pos_mcap)

prob_impacto_pos_vtrd <- cumulative_draws %>%
  group_by(high_income, middle_income, low_income, draw_id) %>%
  summarise(avg = mean(cumulative_vtrd_draw, na.rm = TRUE), .groups = "drop") %>%
  mutate(grupo = case_when(
    high_income == 1 ~ "Alto Ingreso",
    middle_income == 1 ~ "Medio Ingreso",
    low_income  == 1 ~ "Bajo Ingreso",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(grupo)) %>%
  group_by(grupo) %>%
  summarise(`P(impacto > 0) [%] (VTRD)` = round(mean(avg > 0, na.rm = TRUE)*100, 2), .groups = "drop")

cat(sprintf("\nProbabilidad de que el impacto acumulado sea POSITIVO por grupo (VTRD, horizontes %s):\n", paste(horizons_to_sum, collapse = "–")))
print(prob_impacto_pos_vtrd)

cat("\n--- FIN DEL SCRIPT ---\n")