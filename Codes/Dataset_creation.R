# ------------------------------------------------------------
# Panel (GMD + WDI: 8+ archivos) -> .dta y .xlsx (1970–2024)
# - Dummies de crisis preservadas (0/1) según GMD
# - 'country' = ISO3 (mayúscula)
# - Renombres WDI a nombres intuitivos (incluye growth_gdp)
# - Ajustado a tus nombres exactos en GMD: nGDP, rGDP, rGDP_pc, ..., SovDebtCrisis
#   (tras clean_names(): ngdp, rgdp, rgdp_pc, ..., sov_debt_crisis)
# ------------------------------------------------------------

# 0) Paquetes
req_pkgs <- c("readxl","dplyr","tidyr","stringr","janitor","purrr",
              "haven","readr","lubridate","writexl")
to_install <- req_pkgs[!sapply(req_pkgs, requireNamespace, quietly = TRUE)]
if (length(to_install)) install.packages(to_install)
invisible(lapply(req_pkgs, library, character.only = TRUE))

# 1) Ruta
dir_path <- "C:/Users/joser/Desktop/Data Base"

# 2) Variables objetivo (nombres tras clean_names())
#    GMD (macro + dummies de crisis)
vars_gmd_clean <- c(
  "rgdp","cpi","infl","cbrate","strate","ltrate",
  "reer","inv_gdp","ca_gdp","govdebt_gdp","unemp",
  "sov_debt_crisis","currency_crisis","banking_crisis"
)

#    WDI (incluye NY.GDP.MKTP.KD.ZG -> growth_gdp)
vars_wdi_keep <- c(
  "CM.MKT.LCAP.GD.ZS",
  "CM.MKT.LDOM.NO",
  "CM.MKT.TRAD.GD.ZS",
  "CM.MKT.TRNR",
  "FR.INR.RINR",
  "FS.AST.PRVT.GD.ZS","FD.AST.PRVT.GD.ZS",
  "PX.REX.REER",
  "NY.GDP.MKTP.KD.ZG"     # <- NUEVO: crecimiento PIB real (% a/a)
)

# 3) Utilitarios
.clean <- function(df) janitor::clean_names(df)

stata_sanitize_names <- function(nms) {
  out <- gsub("[^A-Za-z0-9_]", "_", nms)
  out <- ifelse(grepl("^[0-9]", out), paste0("v_", out), out)
  substr(out, 1, 32)
}

# Coerción robusta de dummies (conserva 1s; NAs permanecen NA)
coerce_dummy01 <- function(x) {
  if (is.logical(x)) return(as.integer(x))
  if (is.numeric(x)) return(ifelse(is.na(x), NA_integer_, ifelse(x >= 1, 1L, 0L)))
  if (is.character(x)) {
    y <- tolower(trimws(x))
    return(ifelse(is.na(y) | y=="", NA_integer_,
                  ifelse(y %in% c("1","true","sí","si","yes","y"), 1L,
                         ifelse(y %in% c("0","false","no","n"), 0L, NA_integer_))))
  }
  as.integer(!is.na(x) & x != 0)
}

# Detección de columna de año para filtrar rango
detect_year_col <- function(df) {
  nms <- names(df)
  if ("year" %in% nms) return("year")
  if ("anio" %in% nms) return("anio")
  numc <- nms[sapply(df, function(x) is.numeric(x) || is.integer(x))]
  if (length(numc)) {
    scores <- sapply(numc, function(cn) {
      v <- suppressWarnings(as.integer(df[[cn]])); v <- v[!is.na(v)]
      if (!length(v)) return(0)
      mean(v >= 1800 & v <= 2100)
    })
    if (max(scores) > 0.6) return(names(scores)[which.max(scores)])
  }
  stop("No se pudo identificar la columna de año.")
}

filter_years <- function(df, y_min = 1970, y_max = 2024) {
  ycol <- detect_year_col(df)
  df |>
    dplyr::mutate("{ycol}" := as.integer(.data[[ycol]])) |>
    dplyr::filter(.data[[ycol]] >= y_min, .data[[ycol]] <= y_max)
}

# Lectura WDI .xls (hoja "Data", años en columnas)
read_wdi_xls <- function(path_xls) {
  for (sk in 0:8) {
    df_try <- try(readxl::read_excel(path_xls, sheet = "Data", col_names = TRUE, skip = sk),
                  silent = TRUE)
    if (!inherits(df_try, "try-error")) {
      df <- .clean(df_try)
      if (!all(c("country_code","indicator_code") %in% names(df))) next
      yr_cols <- names(df)[grepl("^x?\\d{4}$", names(df))]
      if (!length(yr_cols)) next
      return(
        df |>
          tidyr::pivot_longer(all_of(yr_cols), names_to = "year", values_to = "value") |>
          mutate(year = readr::parse_number(year),
                 value = suppressWarnings(as.numeric(value))) |>
          select(country_name, country_code, indicator_name, indicator_code, year, value) |>
          filter(!is.na(year))
      )
    }
  }
  warning(sprintf("No se pudo leer correctamente: %s", basename(path_xls)))
  NULL
}

# 4) Importar GMD.xlsx (panel)
gmd_file <- file.path(dir_path, "GMD.xlsx")
stopifnot(file.exists(gmd_file))

gmd_raw <- readxl::read_excel(gmd_file, sheet = "data_final")
gmd <- .clean(gmd_raw)  # rGDP -> rgdp; SovDebtCrisis -> sov_debt_crisis

# IDs: 'iso3' (o 'id') y 'year'
iso_col <- dplyr::case_when(
  "iso3" %in% names(gmd) ~ "iso3",
  "id"   %in% names(gmd) ~ "id",
  TRUE ~ NA_character_
)
year_col <- if ("year" %in% names(gmd)) "year" else NA_character_

if (any(is.na(c(iso_col, year_col)))) {
  message("Columnas presentes en GMD (clean):"); print(names(gmd))
  stop("No se hallaron columnas ID 'iso3/id' y 'year' en GMD.xlsx::data_final.")
}

gmd <- gmd |>
  rename(iso3c = all_of(iso_col), year = all_of(year_col)) |>
  mutate(iso3c = toupper(as.character(iso3c)),
         year  = as.integer(year))

# Fallback para 'rgdp' si viniera con otro nombre tras clean_names
if (!"rgdp" %in% names(gmd)) {
  cand_rgdp <- intersect(c("r_gdp", "real_gdp"), names(gmd))
  if (length(cand_rgdp) >= 1) {
    gmd <- dplyr::rename(gmd, rgdp = dplyr::all_of(cand_rgdp[1]))
  }
}

# Dummies de crisis (sov_debt_crisis, currency_crisis, banking_crisis)
for (dv in c("sov_debt_crisis","currency_crisis","banking_crisis")) {
  if (dv %in% names(gmd)) gmd[[dv]] <- coerce_dummy01(gmd[[dv]])
}

# Selección de columnas GMD
gmd_sel <- gmd |>
  dplyr::select(any_of(c("iso3c","year", vars_gmd_clean)))

# 5) Importar WDI (API_*.xls) — ya capturará API_NY.GDP.MKTP.KD.ZG.xls
xls_files <- list.files(dir_path, pattern = "^API_.*\\.xls$", full.names = TRUE)
xls_files <- xls_files[!grepl("^~\\$", basename(xls_files))]

wdi_list <- purrr::map(xls_files, read_wdi_xls)
wdi_list <- wdi_list[!vapply(wdi_list, is.null, logical(1))]

if (length(wdi_list) == 0) {
  warning("No se cargaron archivos WDI .xls (hoja 'Data').")
  wdi_wide <- tibble::tibble(iso3c = character(), year = integer())
} else {
  wdi_long <- dplyr::bind_rows(wdi_list)
  
  # Mantener solo los indicadores de interés (incluye NY.GDP.MKTP.KD.ZG)
  wdi_long_f <- wdi_long |>
    mutate(indicator_code = dplyr::case_when(
      indicator_code == "FD.AST.PRVT.GD.ZS" ~ "FD.AST.PRVT.GD.ZS",
      indicator_code == "FS.AST.PRVT.GD.ZS" ~ "FS.AST.PRVT.GD.ZS",
      TRUE ~ indicator_code
    )) |>
    filter(indicator_code %in% vars_wdi_keep)
  
  # Consolidar país-año-indicador (primer no-NA)
  wdi_long_f <- wdi_long_f |>
    group_by(country_code, year, indicator_code) |>
    summarize(value = suppressWarnings(dplyr::first(na.omit(value))), .groups = "drop")
  
  # Ancho e ISO3 en mayúsculas
  wdi_wide <- wdi_long_f |>
    rename(iso3c = country_code) |>
    mutate(iso3c = toupper(as.character(iso3c))) |>
    tidyr::pivot_wider(names_from = indicator_code, values_from = value)
  
  # Coalesce SEGURO: private_credit_gdp (usa FD si existe, si no FS; si ninguna, NA)
  pc_cols <- intersect(c("FD.AST.PRVT.GD.ZS","FS.AST.PRVT.GD.ZS"), names(wdi_wide))
  if (length(pc_cols) == 2) {
    wdi_wide <- wdi_wide |>
      mutate(private_credit_gdp = dplyr::coalesce(.data[[pc_cols[1]]], .data[[pc_cols[2]]]))
  } else if (length(pc_cols) == 1) {
    wdi_wide <- wdi_wide |>
      mutate(private_credit_gdp = .data[[pc_cols[1]]])
  } else {
    wdi_wide <- wdi_wide |>
      mutate(private_credit_gdp = NA_real_)
  }
  
  # Renombres SEGUROS a nombres intuitivos (incluye growth_gdp)
  wdi_wide <- wdi_wide |>
    rename_with(~"mcap_gdp",            any_of("CM.MKT.LCAP.GD.ZS")) |>
    rename_with(~"listed_companies",    any_of("CM.MKT.LDOM.NO"))   |>
    rename_with(~"value_traded_gdp",    any_of("CM.MKT.TRAD.GD.ZS"))|>
    rename_with(~"turnover",            any_of("CM.MKT.TRNR"))      |>
    rename_with(~"real_interest_rate",  any_of("FR.INR.RINR"))      |>
    rename_with(~"reer_wdi",            any_of("PX.REX.REER"))      |>
    rename_with(~"growth_gdp",          any_of("NY.GDP.MKTP.KD.ZG"))|>  # <- NUEVO
    # Eliminar columnas originales FS/FD si existen
    select(-any_of(c("FD.AST.PRVT.GD.ZS","FS.AST.PRVT.GD.ZS")))
}

# 6) Unir GMD + WDI por iso3c-year
panel_all <- gmd_sel |>
  left_join(wdi_wide, by = c("iso3c","year")) |>
  # renombrar 'strate' -> 'srate' y 'iso3c' -> 'country'
  dplyr::rename(srate = dplyr::any_of("strate"),
                country = iso3c)

# 7) Filtrar 1970–2024
panel_all <- filter_years(panel_all, 1995, 2024)

# 8) Nombres válidos para Stata/Excel (mantiene 'country' y 'year')
orig_names  <- names(panel_all)
stata_names <- stata_sanitize_names(orig_names)
names(panel_all) <- stata_names

# Diccionario nombres (antes -> después)
readr::write_csv(
  tibble::tibble(original = orig_names, stata = stata_names),
  file.path(dir_path, "diccionario_nombres_stata.csv")
)

# 9) Exportar a Stata (.dta) y Excel (.xlsx)
out_stata <- file.path(dir_path, "panel_final.dta")
out_excel <- file.path(dir_path, "panel_final.xlsx")

haven::write_dta(panel_all, out_stata)
writexl::write_xlsx(panel_all, out_excel)

message("Exportado (1970–2024): ", out_stata)
message("Exportado (1970–2024): ", out_excel)






















