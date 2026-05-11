# ============================================================
# Limpieza del panel (sobrescribe panel_final.*):
# - Drop: rgdp, cpi, srate, ltrate, listed_companies, turnover, reer_wdi
# - Filtro por cobertura en mcap_gdp y value_traded_gdp (umbral n>=7 en al menos UNA)
# - Dummies de crisis: NA -> 0 (y si no existen, se crean como 0)
# - Interpolación lineal (solo huecos internos <= 2 años; sin extrapolar) para el resto
# - Crea 'crisis' (con años específicos =1) y elimina dummies originales
# - Crea logs solicitados (SIN winsorización)
# - Elimina países específicos (si están)
# - Sobrescribe panel_final.dta / panel_final.xlsx
# - Imprime listado de países y conteo + variables/tipos
# ============================================================

# 0) Paquetes
req <- c("dplyr","readxl","haven","writexl","janitor","zoo","purrr","tibble")
inst <- req[!sapply(req, requireNamespace, quietly = TRUE)]
if (length(inst)) install.packages(inst)
invisible(lapply(req, library, character.only = TRUE))

# 1) Parámetros / rutas
dir_path  <- "C:/Users/joser/Desktop/Data Base"
path_dta  <- file.path(dir_path, "panel_final.dta")
path_xlsx <- file.path(dir_path, "panel_final.xlsx")

# Umbral de cobertura (n observaciones no-NA) requerido en cada variable
threshold_n <- 7  # si prefieres 5, cambia aquí a 5

# 2) Carga robusta
if (file.exists(path_dta)) {
  df <- haven::read_dta(path_dta)
} else if (file.exists(path_xlsx)) {
  df <- readxl::read_xlsx(path_xlsx)
} else {
  stop("No encuentro 'panel_final.dta' ni 'panel_final.xlsx' en: ", dir_path)
}

# 3) Normalizaciones mínimas
df <- janitor::clean_names(df)
df <- haven::zap_labels(df)
stopifnot(all(c("country","year") %in% names(df)))
df <- df |>
  mutate(country = toupper(as.character(country)),
         year    = as.integer(year))

# 4) Eliminar variables solicitadas (si existen)
drop_vars <- c("rgdp","cpi","srate","ltrate","listed_companies","turnover","reer_wdi")
present_to_drop <- intersect(drop_vars, names(df))
df <- dplyr::select(df, -dplyr::any_of(present_to_drop))
message("Columnas eliminadas: ", ifelse(length(present_to_drop)==0, "(ninguna encontrada)", 
                                        paste(present_to_drop, collapse = ", ")))

# 5) Filtrado de países por cobertura en mcap_gdp y value_traded_gdp
needed <- c("mcap_gdp","value_traded_gdp")
missing_needed <- setdiff(needed, names(df))
if (length(missing_needed)) {
  stop("Faltan en el panel las columnas necesarias para el filtro: ",
       paste(missing_needed, collapse = ", "))
}

coverage <- df |>
  summarise(
    n_mcap  = sum(!is.na(mcap_gdp)),
    n_vtrd  = sum(!is.na(value_traded_gdp)),
    .by = country
  )

# Mantener países que NO incumplen simultáneamente el umbral
# (es decir, se elimina solo si n_mcap < threshold_n Y n_vtrd < threshold_n)
keep_cty <- coverage |>
  filter(!(n_mcap < threshold_n & n_vtrd < threshold_n)) |>
  pull(country)

drop_cty <- setdiff(unique(df$country), keep_cty)

df_clean <- df |>
  filter(country %in% keep_cty) |>
  arrange(country, year)

# 6) Completar dummies de crisis con 0 (si no existen, crearlas como 0)
dummy_vars <- c("sov_debt_crisis","currency_crisis","banking_crisis")
for (dv in dummy_vars) {
  if (!dv %in% names(df_clean)) {
    df_clean[[dv]] <- 0L
  }
}

present_dummy <- intersect(dummy_vars, names(df_clean))
if (length(present_dummy)) {
  na_before_dummy <- sapply(df_clean[present_dummy], function(x) sum(is.na(x)))
  for (dv in present_dummy) {
    df_clean[[dv]][is.na(df_clean[[dv]])] <- 0L
    if (is.numeric(df_clean[[dv]])) {
      df_clean[[dv]] <- ifelse(df_clean[[dv]] >= 1, 1L, 0L)
    } else {
      df_clean[[dv]] <- as.integer(df_clean[[dv]])
    }
  }
  na_after_dummy <- sapply(df_clean[present_dummy], function(x) sum(is.na(x)))
  filled_dummy <- na_before_dummy - na_after_dummy
  msg <- tibble::tibble(variable = names(filled_dummy), na_rellenos = as.integer(filled_dummy))
  message("Dummies completadas/creadas (0/1):\n", paste(capture.output(print(msg, n=Inf)), collapse="\n"))
}

# 7) Interpolación razonable (lineal, interna, huecos <=2 años, sin extrapolar)
num_vars <- names(df_clean)[sapply(df_clean, is.numeric)]
num_interp <- setdiff(num_vars, c("year", present_dummy))  # no interpolar dummies ni 'year'

# Conteo de NAs antes
na_before <- sapply(df_clean[num_interp], function(x) sum(is.na(x), na.rm = TRUE))

df_interp <- df_clean |>
  group_by(country) |>
  mutate(
    across(
      all_of(num_interp),
      ~ zoo::na.approx(., x = year, maxgap = 2, na.rm = FALSE, rule = 1),
      .names = "{.col}"
    )
  ) |>
  ungroup()

# Conteo de NAs después
na_after <- sapply(df_interp[num_interp], function(x) sum(is.na(x), na.rm = TRUE))
filled <- na_before - na_after
rep_interp <- tibble::tibble(variable = names(filled),
                             na_antes = as.integer(na_before),
                             na_despues = as.integer(na_after),
                             na_rellenos = as.integer(filled)) |>
  arrange(desc(na_rellenos))
message("Resumen interpolación (top 15 por NA rellenados):\n",
        paste(capture.output(print(head(rep_interp, 15), n=15)), collapse="\n"))

# 8) Construir 'crisis', eliminar dummies originales
req_crisis <- c("sov_debt_crisis","currency_crisis","banking_crisis")
# Asegurar que existan (si no, ya se crearon arriba) y sean 0/1
for (dv in req_crisis) {
  if (!dv %in% names(df_interp)) df_interp[[dv]] <- 0L
  df_interp[[dv]] <- as.integer(df_interp[[dv]] >= 1)
}
crisis_years <- c(2000L, 2001L, 2002L, 2008L, 2009L, 2020L, 2021L, 2022L)
stopifnot("year" %in% names(df_interp))

df_interp <- df_interp |>
  mutate(
    crisis = as.integer(
      (coalesce(sov_debt_crisis, 0L) +
         coalesce(currency_crisis, 0L) +
         coalesce(banking_crisis, 0L)) > 0L
    ),
    crisis = if_else(year %in% crisis_years, 1L, crisis)
  ) |>
  select(-any_of(req_crisis))

# 9) Imprimir listado de países y conteo
cat("\n--- LISTADO DE PAÍSES EN EL PANEL ---\n")
countries_tbl <- df_interp |>
  distinct(country) |>
  arrange(country)
print(countries_tbl, n = nrow(countries_tbl))
cat("Total de países:", nrow(countries_tbl), "\n")

# 10) Imprimir variables y tipos
cat("\n--- VARIABLES Y TIPOS ---\n")
vars_tipos <- tibble::tibble(
  variable = names(df_interp),
  class    = purrr::map_chr(df_interp, ~ paste(class(.x), collapse = ", "))
)
print(vars_tipos, n = nrow(vars_tipos))

# 11) Guardar (sobrescribe)
haven::write_dta(df_interp, path_dta)
writexl::write_xlsx(df_interp, path_xlsx)
cat("\nGuardado (sobrescrito) en:\n - ", path_dta, "\n - ", path_xlsx, "\n")

# ============================================================
# BLOQUE: Eliminar países específicos y guardar
# ============================================================
if (!exists("df_interp")) {
  if (file.exists(path_dta)) {
    df_interp <- haven::read_dta(path_dta)
  } else if (file.exists(path_xlsx)) {
    df_interp <- readxl::read_xlsx(path_xlsx)
  } else {
    stop("No encuentro 'panel_final.dta' ni 'panel_final.xlsx' en: ", dir_path)
  }
  df_interp <- janitor::clean_names(df_interp)
}
stopifnot("country" %in% names(df_interp))
df_interp <- df_interp |>
  mutate(country = toupper(as.character(country)))

to_drop <- c("CYM","BMU","LVA","EST","VEN","IRN")
before_rows <- nrow(df_interp); before_cty <- dplyr::n_distinct(df_interp$country)
present_drop <- intersect(unique(df_interp$country), to_drop)

df_interp <- filter(df_interp, !country %in% to_drop)

after_rows <- nrow(df_interp);  after_cty  <- dplyr::n_distinct(df_interp$country)

cat("\n--- ELIMINACIÓN DE PAÍSES ---\n")
cat("Eliminados:", ifelse(length(present_drop)==0, "(ninguno en el panel)", paste(present_drop, collapse=", ")), "\n")
cat("Filas removidas:", before_rows - after_rows, "\n")
cat("Países antes:", before_cty, " | Países después:", after_cty, "\n")

haven::write_dta(df_interp, path_dta)
writexl::write_xlsx(df_interp, path_xlsx)
cat("Guardado en:\n - ", path_dta, "\n - ", path_xlsx, "\n")

# ============================================================
# BLOQUE: Logs seleccionados (SIN winsorización) y guardar
# ============================================================
safe_log <- function(x) ifelse(is.finite(x) & x > 0, log(x), NA_real_)
safe_log1p_pct <- function(x) {
  y <- x/100
  ifelse(is.finite(y) & (y > -1), log1p(y), NA_real_)
}

has_reer               <- "reer"               %in% names(df_interp)
has_inv_gdp            <- "inv_gdp"            %in% names(df_interp)
has_govdebt_gdp        <- "govdebt_gdp"        %in% names(df_interp)
has_mcap_gdp           <- "mcap_gdp"           %in% names(df_interp)
has_value_traded_gdp   <- "value_traded_gdp"   %in% names(df_interp)
has_private_credit_gdp <- "private_credit_gdp" %in% names(df_interp)
n_rows <- nrow(df_interp)

df_interp <- df_interp |>
  mutate(
    ln_reer                 = if (has_reer)               safe_log(reer)                      else rep(NA_real_, n_rows),
    ln1p_inv_gdp            = if (has_inv_gdp)            safe_log1p_pct(inv_gdp)            else rep(NA_real_, n_rows),
    ln1p_govdebt_gdp        = if (has_govdebt_gdp)        safe_log1p_pct(govdebt_gdp)        else rep(NA_real_, n_rows),
    ln1p_mcap_gdp           = if (has_mcap_gdp)           safe_log1p_pct(mcap_gdp)           else rep(NA_real_, n_rows),
    ln1p_value_traded_gdp   = if (has_value_traded_gdp)   safe_log1p_pct(value_traded_gdp)   else rep(NA_real_, n_rows),
    ln1p_private_credit_gdp = if (has_private_credit_gdp) safe_log1p_pct(private_credit_gdp) else rep(NA_real_, n_rows)
  )

haven::write_dta(df_interp, path_dta)
writexl::write_xlsx(df_interp, path_xlsx)
cat("\nGuardado con logs (sin winsorización) en:\n - ", path_dta, "\n - ", path_xlsx, "\n")

# ============================================================
# BLOQUE: Dummies de ingreso (high_income, middle_income, low_income)
# Clasificación basada en lista provista de países (ISO3)
# ============================================================
stopifnot("country" %in% names(df_interp))

all_list <- c(
  "ARE","ARG","ARM","AUS","AUT","AZE","BEL","BGD","BGR","BHR","BLR","BRA","BRB","BWA",
  "CAN","CHE","CHL","CHN","CIV","COL","CRI","CYP","CZE","DEU","DNK","DZA","ECU","EGY",
  "ESP","FIN","FRA","GBR","GHA","GRC","HKG","HRV","HUN","IDN","IND","IRL","ISL","ISR",
  "ITA","JAM","JOR","JPN","KAZ","KEN","KOR","KWT","LBN","LKA","LUX","MAR","MEX","MLT",
  "MUS","MYS","NAM","NGA","NLD","NOR","NZL","OMN","PAK","PAN","PER","PHL","PNG","POL",
  "PRT","PRY","PSE","QAT","ROU","RUS","RWA","SAU","SGP","SRB","SVK","SVN","SWE","SWZ",
  "SYC","THA","TUN","TUR","TZA","UKR","USA","VNM","ZAF","ZMB"
)

high_set <- c(
  "ARE","AUS","AUT","BEL","BHR","CAN","CHE","CHL","CYP","CZE","DEU","DNK","ESP","FIN","FRA",
  "GBR","GRC","HKG","HRV","HUN","IRL","ISL","ISR","ITA","JPN","KOR","KWT","LUX","MLT","MUS",
  "NLD","NOR","NZL","OMN","POL","PRT","QAT","ROU","SAU","SGP","SVK","SVN","SWE","SYC","USA"
)
low_set <- c("RWA","TZA","ZMB")
middle_set <- setdiff(all_list, union(high_set, low_set))

df_interp <- df_interp |>
  mutate(
    high_income   = as.integer(country %in% high_set),
    middle_income = as.integer(country %in% middle_set),
    low_income    = as.integer(country %in% low_set)
  )

cat("\n--- RESUMEN DUMMIES DE INGRESO ---\n")
grp_counts <- df_interp |>
  distinct(country, high_income, middle_income, low_income) |>
  summarise(
    n_high   = sum(high_income==1,   na.rm = TRUE),
    n_middle = sum(middle_income==1, na.rm = TRUE),
    n_low    = sum(low_income==1,    na.rm = TRUE)
  )
print(grp_counts, n = Inf)

haven::write_dta(df_interp, path_dta)
writexl::write_xlsx(df_interp, path_xlsx)
cat("\nGuardado con dummies de ingreso en:\n - ", path_dta, "\n - ", path_xlsx, "\n")
