# ============================================================
# HEALTH CHECK / DATA QUALITY REPORT
# Base: panel_final.dta (country-year panel)
# - Carga y normaliza
# - Cobertura por variable / país / año
# - Duplicados clave
# - Faltantes agregados y mapas rápidos
# - Outliers (±5*IQR global y por país)
# - Correlaciones (heatmap y top pares)
# - Checks específicos (crecimiento, plausibilidad %)
# ============================================================

# 0) Paquetes
req <- c("dplyr","tidyr","haven","janitor","purrr","ggplot2","scales","tibble","forcats")
inst <- req[!sapply(req, requireNamespace, quietly = TRUE)]
if (length(inst)) install.packages(inst)
invisible(lapply(req, library, character.only = TRUE))

# 1) Ruta y carga
dir_path <- "C:/Users/joser/Desktop/Data Base"
path_dta <- file.path(dir_path, "panel_final.dta")
stopifnot(file.exists(path_dta))

df <- haven::read_dta(path_dta)

# 2) Normalizaciones mínimas
options(digits = 4)
df <- janitor::clean_names(df)
df <- haven::zap_labels(df)
stopifnot(all(c("country","year") %in% names(df)))
df <- df |>
  mutate(country = toupper(as.character(country)),
         year    = as.integer(year))

num_vars <- names(df)[sapply(df, is.numeric)]
num_vars <- setdiff(num_vars, "year")
chr_vars <- names(df)[sapply(df, is.character)]

is_dummy_vec <- function(x) {
  if (!is.numeric(x)) return(FALSE)
  ux <- unique(na.omit(x))
  length(ux) > 0 && all(ux %in% c(0,1))
}
dummy_cols <- names(df)[sapply(df, is_dummy_vec)]

sep <- function(txt) {
  cat("\n", paste0(rep("=", 72), collapse=""), "\n", txt, "\n",
      paste0(rep("-", 72), collapse=""), "\n", sep="")
}

# -------------------- A) INFO GENERAL --------------------
sep("A) INFO GENERAL")
cat("Observaciones:", nrow(df), " | Variables:", ncol(df), "\n")
cat("Países únicos:", dplyr::n_distinct(df$country), "\n")
cat("Rango de años:", min(df$year, na.rm=TRUE), "–", max(df$year, na.rm=TRUE), "\n")
cat("Numéricas:", length(num_vars), " | Carácter:", length(chr_vars),
    " | Dummies detectadas:", length(dummy_cols), "\n")
if ("growth_gdp" %in% names(df)) cat("✓ Detectada 'growth_gdp' (WDI: NY.GDP.MKTP.KD.ZG)\n")

# -------------------- B) FALTANTES POR VARIABLE ----------------
sep("B) TIPOS Y FALTANTES POR VARIABLE")
types_tbl <- tibble::tibble(
  variable = names(df),
  class    = purrr::map_chr(df, ~ paste(class(.x), collapse=", ")),
  n_non_na = purrr::map_int(df, ~ sum(!is.na(.x))),
  n_na     = purrr::map_int(df, ~ sum(is.na(.x))),
  pct_na   = round(100*n_na/nrow(df), 2)
) |>
  arrange(desc(pct_na), variable)
print(types_tbl, n = 50)

# Graf: % NA por variable (Top 30)
top_na <- types_tbl |> arrange(desc(pct_na)) |> head(30)
if (nrow(top_na) > 0) {
  ggplot(top_na, aes(x = reorder(variable, pct_na), y = pct_na)) +
    geom_col() + coord_flip() +
    labs(title = "% de NA por variable (Top 30)", x = "Variable", y = "% NA") +
    scale_y_continuous(labels = percent_format(scale = 1)) +
    theme_minimal()
}

# -------------------- C) CLAVE PANEL y COBERTURA ----------------
sep("C) CLAVE PANEL Y COBERTURA")
dupes_key <- df |> count(country, year, name = "n") |> filter(n > 1)
if (nrow(dupes_key) == 0) {
  cat("✔ Sin duplicados en country-year.\n")
} else {
  cat("✖ Duplicados en country-year. Muestra:\n")
  print(dupes_key, n = min(25, nrow(dupes_key)))
}

cat("\nCobertura por país (rango de años y nº años):\n")
coverage_country <- df |>
  summarise(year_min = min(year, na.rm=TRUE),
            year_max = max(year, na.rm=TRUE),
            n_years  = dplyr::n_distinct(year),
            .by = country) |>
  arrange(country)
print(coverage_country, n = 30)

cat("\nCobertura por año (# países con registros):\n")
coverage_year <- df |>
  summarise(n_countries = dplyr::n_distinct(country), .by = year) |>
  arrange(year)
print(coverage_year, n = nrow(coverage_year))

ggplot(coverage_year, aes(x = year, y = n_countries)) +
  geom_line() + geom_point(size = 1.1) +
  labs(title = "Cobertura por año (# países)", x = "Año", y = "# Países") +
  theme_minimal()

# -------------------- D) FALTANTES AGREGADOS --------------------
sep("D) FALTANTES AGREGADOS (PAÍS / AÑO)")
miss_by_country <- df |>
  summarise(across(all_of(num_vars), ~ sum(is.na(.x))), .by = country) |>
  mutate(total_na = rowSums(across(all_of(num_vars)))) |>
  arrange(desc(total_na))
cat("Top países por NA totales:\n")
print(miss_by_country |> head(25), n = 25)

miss_by_year <- df |>
  summarise(across(all_of(num_vars), ~ sum(is.na(.x))), .by = year) |>
  mutate(total_na = rowSums(across(all_of(num_vars)))) |>
  arrange(desc(total_na))
cat("\nTop años por NA totales:\n")
print(miss_by_year |> head(25), n = 25)

# Gráfico: % NA por país (Top 30 peor cobertura)
pct_na_country <- df |>
  summarise(across(all_of(num_vars), ~ mean(is.na(.x))), .by = country) |>
  mutate(pct_na = 100 * rowMeans(across(all_of(num_vars)))) |>
  arrange(desc(pct_na))
top30_cty <- head(pct_na_country, 30)
if (nrow(top30_cty) > 0) {
  ggplot(top30_cty, aes(x = reorder(country, pct_na), y = pct_na)) +
    geom_col() + coord_flip() +
    labs(title = "% NA promedio por país (Top 30 peor)", x = "País (ISO3)", y = "% NA") +
    scale_y_continuous(labels = percent_format(scale = 1)) +
    theme_minimal()
}

# -------------------- E) DESCRIPTIVOS BÁSICOS -------------------
sep("E) DESCRIPTIVOS (numéricos; percentiles robustos)")
quantiles <- c(0,.01,.05,.25,.5,.75,.95,.99,1)
desc_stats <- purrr::map_dfr(
  num_vars,
  function(v) {
    x <- df[[v]]; x_num <- x[!is.na(x)]
    if (length(x_num) == 0) {
      return(tibble(variable=v, n=length(x), n_non_na=0, n_na=length(x),
                    n_distinct=0, mean=NA_real_, sd=NA_real_,
                    min=NA_real_, p01=NA_real_, p05=NA_real_, p25=NA_real_,
                    p50=NA_real_, p75=NA_real_, p95=NA_real_, p99=NA_real_,
                    max=NA_real_, n_zeros=NA_real_, n_neg=NA_real_))
    }
    qs <- as.numeric(quantile(x_num, probs=quantiles, names=FALSE, type=7))
    tibble(
      variable=v,
      n=length(x), n_non_na=sum(!is.na(x)), n_na=sum(is.na(x)),
      n_distinct=dplyr::n_distinct(x_num),
      mean=mean(x_num), sd=sd(x_num),
      min=qs[1], p01=qs[2], p05=qs[3], p25=qs[4],
      p50=qs[5], p75=qs[6], p95=qs[7], p99=qs[8], max=qs[9],
      n_zeros=sum(x_num==0), n_neg=sum(x_num<0)
    )
  }
) |> arrange(variable)
print(desc_stats, n = 50)

# Histograma rápido de las 12 variables con mayor cobertura
top_cov <- desc_stats |>
  mutate(cov = n_non_na / n) |>
  arrange(desc(cov)) |>
  head(12) |>
  pull(variable)
for (v in top_cov) {
  if (v %in% names(df)) {
    p <- ggplot(df, aes(x = .data[[v]])) +
      geom_histogram(bins = 40) +
      labs(title = paste("Histograma -", v), x = v, y = "Frecuencia") +
      theme_minimal()
    print(p)
  }
}

# -------------------- F) OUTLIERS (±5*IQR global) ---------------
sep("F) OUTLIERS (±5*IQR por variable)")
outlier_counts <- purrr::map_dfr(
  num_vars,
  function(v) {
    if (v %in% dummy_cols) return(tibble(variable=v, thr_lo=NA_real_, thr_hi=NA_real_, n_out_below=NA_integer_, n_out_above=NA_integer_, pct_out=NA_real_))
    x <- df[[v]]; x <- x[!is.na(x)]
    if (length(x) < 5) {
      return(tibble(variable=v, thr_lo=NA_real_, thr_hi=NA_real_, n_out_below=NA_integer_, n_out_above=NA_integer_, pct_out=NA_real_))
    }
    q1 <- quantile(x, 0.25, names=FALSE, type=7)
    q3 <- quantile(x, 0.75, names=FALSE, type=7)
    iqr <- q3 - q1
    lo <- q1 - 5*iqr
    hi <- q3 + 5*iqr
    tibble(
      variable=v, thr_lo=lo, thr_hi=hi,
      n_out_below=sum(x < lo), n_out_above=sum(x > hi),
      pct_out=round(100*(sum(x<lo)+sum(x>hi))/length(x), 3)
    )
  }
) |> arrange(desc(pct_out))
print(outlier_counts, n = 50)

# Gráfico: % outliers (Top 20)
top_out <- outlier_counts |> filter(!is.na(pct_out)) |> head(20)
if (nrow(top_out) > 0) {
  ggplot(top_out, aes(x = reorder(variable, pct_out), y = pct_out)) +
    geom_col() + coord_flip() +
    labs(title = "% de outliers por variable (Top 20)", x = "Variable", y = "% outliers") +
    scale_y_continuous(labels = percent_format(scale = 1)) +
    theme_minimal()
}

# -------------------- G) OUTLIERS POR PAÍS ----------------------
sep("G) OUTLIERS POR PAÍS (±5*IQR dentro de país/variable)")
k_iqr <- 5
num_eval <- setdiff(num_vars, dummy_cols)

long <- df |>
  select(country, all_of(num_eval)) |>
  pivot_longer(-country, names_to = "variable", values_to = "value") |>
  filter(!is.na(value))

by_ctry_var <- long |>
  group_by(country, variable) |>
  summarise(
    n  = dplyr::n(),
    q1 = quantile(value, 0.25, type = 7, na.rm = TRUE),
    q3 = quantile(value, 0.75, type = 7, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    iqr    = q3 - q1,
    thr_lo = if_else(iqr > 0, q1 - k_iqr * iqr, -Inf),
    thr_hi = if_else(iqr > 0, q3 + k_iqr * iqr,  Inf)
  ) |>
  left_join(long, by = c("country", "variable")) |>
  mutate(is_out = if_else(is.finite(thr_lo) & is.finite(thr_hi),
                          value < thr_lo | value > thr_hi, FALSE)) |>
  group_by(country, variable) |>
  summarise(n = dplyr::first(n), n_out = sum(is_out, na.rm = TRUE), .groups = "drop")

out_by_country <- by_ctry_var |>
  group_by(country) |>
  summarise(
    n_cells     = sum(n),
    n_outliers  = sum(n_out),
    pct_outliers = round(100 * n_outliers / n_cells, 3),
    top_var      = variable[which.max(n_out)],
    top_var_out  = max(n_out),
    .groups = "drop"
  ) |>
  arrange(desc(pct_outliers), desc(n_outliers))
print(head(out_by_country, 25), n = 25)

top20 <- head(out_by_country, 20)
if (nrow(top20) > 0) {
  ggplot(top20, aes(x = reorder(country, pct_outliers), y = pct_outliers)) +
    geom_col() + coord_flip() +
    labs(title = "Outliers por país (Top 20)", x = "País (ISO3)", y = "% de outliers") +
    scale_y_continuous(labels = percent_format(scale = 1)) +
    theme_minimal()
}

# -------------------- H) CORRELACIONES --------------------------
sep("H) CORRELACIONES (pairwise; cobertura ≥60%)")
coverage_threshold <- 0.6
keep_for_corr <- desc_stats |>
  mutate(cov = n_non_na / n) |>
  filter(cov >= coverage_threshold, !(variable %in% dummy_cols)) |>
  pull(variable)

if (length(keep_for_corr) >= 2) {
  mat <- df |> select(all_of(keep_for_corr)) |> as.data.frame()
  suppressWarnings({ corr_mat <- stats::cor(mat, use = "pairwise.complete.obs") })
  corr_long <- as.data.frame(as.table(corr_mat), stringsAsFactors = FALSE) |>
    rename(var_i = Var1, var_j = Var2, corr = Freq) |>
    filter(var_i < var_j) |>
    mutate(abs_corr = abs(corr)) |>
    arrange(desc(abs_corr))
  cat("Top pares por |ρ| (cobertura ≥60%):\n")
  print(head(corr_long, 25), n = 25)
  
  if (ncol(mat) <= 60) {
    ggplot(corr_long, aes(var_i, var_j, fill = corr)) +
      geom_tile() +
      scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b", midpoint = 0, limits = c(-1,1)) +
      labs(title = "Matriz de correlaciones (vars con ≥60% cobertura)", x = NULL, y = NULL, fill = "ρ") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  } else {
    cat("Matriz grande (>", ncol(mat), " vars). Se omite heatmap.\n")
  }
} else {
  cat("No hay suficientes variables con cobertura ≥60% para correlación.\n")
}

# -------------------- I) CHECKS ESPECÍFICOS ---------------------
sep("I) CHECKS ESPECÍFICOS (crecimiento, plausibilidad %)")

# Crecimiento Δlog*100 de rgdp (si existe)
if ("rgdp" %in% names(df)) {
  growth <- df |>
    arrange(country, year) |>
    group_by(country) |>
    mutate(g_rgdp = 100 * (log(rgdp) - dplyr::lag(log(rgdp)))) |>
    ungroup()
  cat("Estadísticos de g_rgdp (Δlog*100):\n")
  gstats <- growth |> summarise(
    n_non_na = sum(!is.na(g_rgdp)),
    mean = mean(g_rgdp, na.rm = TRUE),
    sd   = sd(g_rgdp,   na.rm = TRUE),
    p01  = quantile(g_rgdp, .01, na.rm = TRUE),
    p05  = quantile(g_rgdp, .05, na.rm = TRUE),
    p50  = quantile(g_rgdp, .50, na.rm = TRUE),
    p95  = quantile(g_rgdp, .95, na.rm = TRUE),
    p99  = quantile(g_rgdp, .99, na.rm = TRUE)
  )
  print(gstats, n = Inf)
  ggplot(growth, aes(x = g_rgdp)) +
    geom_histogram(bins = 40) +
    labs(title = "Distribución: crecimiento del PIB real (Δlog*100)", x = "g_rgdp", y = "Frecuencia") +
    theme_minimal()
  
  # Comparación con growth_gdp si existe
  if ("growth_gdp" %in% names(df)) {
    comp <- growth |> mutate(diff_growth = g_rgdp - growth_gdp)
    cat("\nComparación g_rgdp vs growth_gdp (WDI):\n")
    print(comp |>
            summarise(
              n_pair     = sum(!is.na(g_rgdp) & !is.na(growth_gdp)),
              corr_pair  = cor(g_rgdp, growth_gdp, use = "pairwise.complete.obs"),
              mean_diff  = mean(diff_growth, na.rm = TRUE),
              p05_diff   = quantile(diff_growth, .05, na.rm = TRUE),
              p50_diff   = quantile(diff_growth, .50, na.rm = TRUE),
              p95_diff   = quantile(diff_growth, .95, na.rm = TRUE)
            ), n = Inf)
    ggplot(comp, aes(x = growth_gdp, y = g_rgdp)) +
      geom_point(alpha = 0.35) + geom_smooth(method = "lm", se = FALSE) +
      labs(title = "g_rgdp (Δlog*100) vs growth_gdp (WDI, % a/a)",
           x = "growth_gdp (WDI, % a/a)", y = "g_rgdp (Δlog*100)") +
      theme_minimal()
  }
}

# Plausibilidad en variables tipo % / tasas
pct_like <- intersect(
  c("unemp","infl","mcap_gdp","value_traded_gdp","turnover",
    "private_credit_gdp","govdebt_gdp","inv_gdp","ca_gdp","real_interest_rate","growth_gdp"),
  names(df)
)
if (length(pct_like)) {
  plaus <- purrr::map_dfr(
    pct_like,
    function(v) {
      x <- df[[v]]
      tibble(
        variable = v, n_non_na = sum(!is.na(x)),
        share_lt_m100 = mean(x < -100, na.rm = TRUE),
        share_lt_0    = mean(x < 0,    na.rm = TRUE),
        share_gt_100  = mean(x > 100,  na.rm = TRUE),
        share_gt_500  = mean(x > 500,  na.rm = TRUE)
      )
    }
  ) |> arrange(desc(share_gt_500), desc(share_gt_100), variable)
  cat("\nPlausibilidad (fracciones fuera de rango):\n")
  print(plaus, n = nrow(plaus))
}

# -------------------- J) RESUMEN EJECUTIVO ----------------------
sep("J) RESUMEN EJECUTIVO (diagnóstico de salud)")
# 1) Duplicados
if (nrow(dupes_key) == 0) cat("✔ Sin duplicados country-year.\n") else cat("✖ Hay duplicados en country-year.\n")
# 2) Variables con alta ausencia
altas_na <- types_tbl |> filter(pct_na >= 50)
cat("Variables con ≥50% de NA:", nrow(altas_na), "\n")
if (nrow(altas_na)) print(altas_na |> select(variable, pct_na) |> arrange(desc(pct_na)), n = 50)
# 3) Outliers
con_outliers <- outlier_counts |> filter(!is.na(pct_out) & pct_out > 0)
cat("Variables con outliers (±5*IQR):", nrow(con_outliers), "\n")
if (nrow(con_outliers)) print(con_outliers |> select(variable, pct_out) |> head(30), n = 30)
# 4) Correlaciones fuertes
if (exists("corr_long")) {
  muy_corr <- corr_long |> filter(abs_corr >= 0.9)
  cat("Pares con |ρ| ≥ 0.9:", nrow(muy_corr), "\n")
  if (nrow(muy_corr)) print(head(muy_corr, 30), n = 30)
}
cat("\nSugerencias:\n",
    "- Focalizar limpieza en variables con alta NA y países del Top NA.\n",
    "- Tratar outliers (winsorizar, recodificar episodios extremos identificados por país).\n",
    "- Considerar transformaciones (log/Δlog) y estandarizar unidades.\n",
    "- Asegurar consistencia entre growth_gdp y g_rgdp; documentar discrepancias.\n")








