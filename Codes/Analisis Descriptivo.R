# =========================================================================
# SCRIPT DE ANÁLISIS DESCRIPTIVO (USANDO TODAS LAS IMPUTACIONES MICE)
# - Carpeta: C:/Users/joser/Desktop/Data Base/Imputed_Data_MICE_Complete
# - Construye EXACTAMENTE las 7 d-variables del IRF desde niveles
# - Combina imputaciones con PROMEDIO por (country, year)
# - Filtro temporal 1996–2024
# - Tabla (gt) + Histogramas/Densidades + Matriz de Correlaciones
# =========================================================================

# 1) PAQUETES --------------------------------------------------------------
req_pkgs <- c("dplyr", "tidyr", "ggplot2", "haven", "gt", "ggcorrplot",
              "moments", "purrr", "stringr")
to_install <- req_pkgs[!sapply(req_pkgs, requireNamespace, quietly = TRUE)]
if (length(to_install)) install.packages(to_install)

library(dplyr)
library(tidyr)
library(ggplot2)
library(haven)
library(gt)
library(ggcorrplot)
library(moments)
library(purrr)
library(stringr)

# 2) DIRECTORIO Y ARCHIVOS -------------------------------------------------
dir_path <- "C:/Users/joser/Desktop/Data Base/Imputed_Data_MICE_Complete"

if (!dir.exists(dir_path)) {
  stop("No se encontró la carpeta MICE: ", dir_path)
}

imputed_files <- list.files(
  dir_path,
  pattern = "\\.dta$",
  full.names = TRUE
)

if (length(imputed_files) == 0) {
  stop("No se encontraron archivos .dta en: ", dir_path)
}

message("Archivos detectados:\n  - ", paste(basename(imputed_files), collapse = "\n  - "))

# 3) VARIABLES EN NIVELES Y ALIAS ------------------------------------------
# Canónicos en niveles (usados para construir diferencias):
vars_in_levels <- c(
  "growth_gdp", "infl", "cbrate", "private_credit_gdp",
  "ln_reer", "ln1p_value_traded_gdp", "ln1p_mcap_gdp"
)

# d-variables canónicas del IRF:
vars_irf_d <- c(
  "d_growth_gdp", "d_infl", "d_cbrate", "d_private_credit_gdp",
  "d_ln_reer", "d_ln1p_value_traded_gdp", "d_ln1p_mcap_gdp"
)

# Alias en NIVELES (por si tus archivos usan variantes)
aliases_levels <- list(
  growth_gdp = c("growth_gdp","gdp_growth","growthrgdp","rgdp_growth","gdpgr","growth"),
  infl = c("infl","inflation","pi","cpi_infl","cpi","inflac"),
  cbrate = c("cbrate","policy_rate","mpr","interest_rate","intrate","rate_policy"),
  private_credit_gdp = c("private_credit_gdp","privatecredit_gdp","priv_credit_gdp",
                         "pcredit_gdp","credit_private_gdp","credit_to_gdp_private"),
  ln_reer = c("ln_reer","lreer","lnreer","log_reer","logreer"),
  ln1p_value_traded_gdp = c("ln1p_value_traded_gdp","ln1p_vtr_gdp","ln1p_valuetraded_gdp",
                            "ln1p_vneg_gdp","ln1p_value_traded_to_gdp"),
  ln1p_mcap_gdp = c("ln1p_mcap_gdp","ln1p_mktcap_gdp","ln1p_market_cap_gdp",
                    "ln1p_cap_burs_gdp","ln1p_capitalization_gdp")
)

normalize_name <- function(x) gsub("[^a-z0-9]", "", tolower(x))
aliases_levels_norm <- lapply(aliases_levels, function(v) unique(normalize_name(v)))

# 4) HELPERS: MAPEO DE NOMBRES Y CONSTRUCCIÓN DE DIFERENCIAS ----------------
# Encuentra, para cada canónico en niveles, qué columna real usar en 'df'
map_levels_to_actual <- function(df) {
  nms <- names(df)
  nms_norm <- normalize_name(nms)
  
  map <- setNames(rep(NA_character_, length(vars_in_levels)), vars_in_levels)
  for (canon in names(aliases_levels_norm)) {
    # 1) Coincidencia exacta por normalización de alias conocidos
    found <- NA_character_
    for (cand in aliases_levels_norm[[canon]]) {
      hit <- which(nms_norm == cand)
      if (length(hit)) { found <- nms[hit[1]]; break }
    }
    # 2) Si no aparece, intenta el canónico exacto
    if (is.na(found)) {
      hit2 <- which(nms == canon)
      if (length(hit2)) found <- nms[hit2[1]]
    }
    map[canon] <- found
  }
  map
}

# Construye d-variables canónicas agrupando por country
build_dvars <- function(df) {
  # IDs mínimos
  if (!("country" %in% names(df))) {
    stop("Falta 'country' en el archivo para poder diferenciar por panel.")
  }
  if (!("year" %in% names(df))) {
    warning("No hay 'year' en el archivo; no se aplicará filtro temporal.")
  }
  
  # Mapeo de columnas en niveles
  level_map <- map_levels_to_actual(df)
  missing_levels <- names(level_map)[is.na(level_map)]
  if (length(missing_levels)) {
    stop("Faltan variables en niveles para construir diferencias: ",
         paste(missing_levels, collapse = ", "))
  }
  
  # Renombra temporalmente a canónicos para unificar
  df2 <- df %>% dplyr::rename(!!!setNames(unname(level_map), names(level_map)))
  
  # Diferencias por país
  df_diff <- df2 %>%
    group_by(country) %>%
    mutate(across(all_of(vars_in_levels), ~ .x - dplyr::lag(.x), .names = "d_{.col}")) %>%
    ungroup()
  
  # Filtro temporal si hay 'year'
  if ("year" %in% names(df_diff)) {
    df_diff <- df_diff %>% filter(year >= 1996 & year <= 2024)
  }
  
  # Selecciona solo IDs y las 7 d-variables canónicas (ya se llaman d_<canon>)
  id_cols <- intersect(c("country","year"), names(df_diff))
  out <- df_diff %>% select(all_of(id_cols), all_of(vars_irf_d))
  
  # Higiene numérica
  out <- out %>%
    mutate(across(all_of(vars_irf_d), ~ suppressWarnings(as.numeric(.x)))) %>%
    mutate(across(all_of(vars_irf_d), ~ ifelse(is.finite(.x), .x, NA_real_)))
  
  out
}

read_and_build <- function(path) {
  df <- haven::read_dta(path)
  out <- build_dvars(df)
  out$.__file__ <- basename(path)
  out$.imputation <- as.integer(stringr::str_extract(out$.__file__[1], "([0-9]+)"))
  out
}

# 5) PROCESA TODAS LAS IMPUTACIONES ----------------------------------------
data_list <- lapply(imputed_files, function(p) {
  tryCatch(
    read_and_build(p),
    error = function(e) stop("Archivo '", basename(p), "' con error: ", conditionMessage(e))
  )
})

# 6) COMBINACIÓN Y PROMEDIO POR (country, year) -----------------------------
data_stack <- bind_rows(data_list)

id_ok <- all(c("country","year") %in% names(data_stack))
if (!id_ok) {
  stop("Los archivos no contienen IDs completos ('country' y 'year') para promediar imputaciones.")
}

data_avg <- data_stack %>%
  group_by(country, year) %>%
  summarise(across(all_of(vars_irf_d), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

cat("\n--- Variables analizadas (exactamente las del IRF) ---\n")
print(vars_irf_d)

# 7) TABLA DE ESTADÍSTICAS DESCRIPTIVAS -----------------------------------
nice_labels <- c(
  d_growth_gdp = "Δ crecimiento PIB",
  d_infl = "Δ inflación",
  d_cbrate = "Δ tasa de política",
  d_private_credit_gdp = "Δ crédito privado/PIB",
  d_ln_reer = "Δ ln(REER)",
  d_ln1p_value_traded_gdp = "Δ ln(1+V.Neg/PIB)",
  d_ln1p_mcap_gdp = "Δ ln(1+Cap.Burs/PIB)"
)

descriptive_stats <- data_avg %>%
  select(all_of(vars_irf_d)) %>%
  summarise(across(
    everything(),
    list(
      Obs = ~sum(!is.na(.x)),
      Mean = ~mean(.x, na.rm = TRUE),
      SD = ~sd(.x, na.rm = TRUE),
      Median = ~median(.x, na.rm = TRUE),
      Min = ~min(.x, na.rm = TRUE),
      Max = ~max(.x, na.rm = TRUE),
      Skewness = ~moments::skewness(.x, na.rm = TRUE),
      Kurtosis = ~moments::kurtosis(.x, na.rm = TRUE)
    ),
    .names = "{.col}___{.fn}"
  )) %>%
  pivot_longer(everything(),
               names_to = c("Variable", ".value"),
               names_sep = "___") %>%
  mutate(Variable = factor(Variable, levels = vars_irf_d))

stats_table <- descriptive_stats %>%
  mutate(Variable = recode(as.character(Variable), !!!nice_labels)) %>%
  arrange(match(Variable, unname(nice_labels[vars_irf_d]))) %>%
  gt(rowname_col = "Variable") %>%
  tab_header(
    title = md("**Tabla 1. Estadísticas descriptivas (d-variables del IRF)**"),
    subtitle = "Fuente: Imputed_Data_MICE_Complete | Promedio imputacional (country, year)"
  ) %>%
  fmt_number(
    columns = c(Mean, SD, Median, Min, Max, Skewness, Kurtosis),
    decimals = 3
  ) %>%
  cols_label(
    Obs = "Observaciones",
    Mean = "Media",
    SD = "Desv.Est.",
    Median = "Mediana",
    Min = "Mín",
    Max = "Máx",
    Skewness = "Asimetría",
    Kurtosis = "Curtosis"
  ) %>%
  tab_options(
    table.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width = px(2)
  )

print(stats_table)

# 8) DISTRIBUCIONES: HISTOGRAMAS + DENSIDAD --------------------------------
data_long <- data_avg %>%
  select(all_of(vars_irf_d)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  mutate(variable = factor(variable, levels = vars_irf_d,
                           labels = unname(nice_labels[vars_irf_d])))

distribution_plot <- ggplot(data_long, aes(x = value)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "#2c7fb8", color = "white", alpha = 0.7) +
  geom_density(linewidth = 1) +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  labs(
    title = "Figura 1. Distribución de las variables del IRF (en diferencias)",
    subtitle = "Promedio imputacional por (country, year) | 1996–2024",
    x = "Valor",
    y = "Densidad"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5)
  )

print(distribution_plot)

# 9) MATRIZ DE CORRELACIONES ------------------------------------------------
corr_df <- data_avg %>% select(all_of(vars_irf_d))
corr_matrix <- cor(corr_df, use = "pairwise.complete.obs", method = "pearson")

correlation_plot <- ggcorrplot(
  corr_matrix,
  method = "circle",
  type = "lower",
  lab = TRUE,
  lab_size = 2.8,
  title = "Figura 2. Matriz de correlaciones (d-variables del IRF)",
  ggtheme = theme_minimal()
) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )

print(correlation_plot)

# =========================================================================
# FIN
# =========================================================================
