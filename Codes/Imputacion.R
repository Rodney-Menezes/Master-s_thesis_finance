# =========================================================================
# SCRIPT DE IMPUTACIÓN CON MICE (SOBRE EL PANEL DE DATOS COMPLETO)
#
# ESTRATEGIA:
# 1. Cargar el panel de datos original sin ninguna eliminación manual.
# 2. Configurar 'mice' de forma inteligente para que maneje la
#    multicolinealidad (ej. entre 'reer' y 'ln_reer') sin necesidad
#    de eliminar las variables.
# 3. Ejecutar la imputación múltiple sobre todos los países y variables.
# 4. Guardar los 5 datasets completos.
# =========================================================================

# 1) INSTALACIÓN Y CARGA DE PAQUETES
# -------------------------------------------------------------------------
req_pkgs <- c("mice", "dplyr", "haven", "writexl", "stringr")
to_install <- req_pkgs[!sapply(req_pkgs, requireNamespace, quietly = TRUE)]
if (length(to_install)) install.packages(to_install)

# Cargar librerías
invisible(lapply(req_pkgs, library, character.only = TRUE))

# 2) CONFIGURACIÓN DE RUTAS Y CARGA DE DATOS
# -------------------------------------------------------------------------
dir_path <- "C:/Users/joser/Desktop/Data Base"
input_file <- file.path(dir_path, "panel_final.dta")

output_dir <- file.path(dir_path, "Imputed_Data_MICE_Complete")
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
  message("Directorio creado para los resultados: ", output_dir)
}

# Cargar el panel de datos completo y original
panel_to_impute <- haven::read_dta(input_file)
cat("--- Panel de datos original cargado ---\n")
cat(sprintf("Dimensiones: %d filas, %d columnas\n", nrow(panel_to_impute), ncol(panel_to_impute)))
cat(sprintf("Número de países: %d\n\n", n_distinct(panel_to_impute$country)))


# 3) CONFIGURACIÓN INTELIGENTE DE MICE
# -------------------------------------------------------------------------
# 'mice' es más robusto, pero podemos ayudarlo a ser más eficiente y estable.

# Inicializar la configuración de mice para obtener la matriz de predictores
ini <- mice(panel_to_impute, maxit = 0)
pred_matrix <- ini$predictorMatrix
meth <- ini$method

# Variables que NO deben ser imputadas NI usadas como predictoras (identificadores)
vars_to_ignore <- c("country", "year")
pred_matrix[, vars_to_ignore] <- 0
meth[vars_to_ignore] <- ""

# MANEJO DE MULTICOLINEALIDAD:
# Para cada par de variable original/logarítmica (ej. 'reer'/'ln_reer'),
# le decimos a mice que NO use la versión original como PREDICTORA.
# Esto rompe la colinealidad perfecta, pero permite que la variable original
# SÍ sea imputada si tiene NAs.
log_vars <- names(panel_to_impute)[str_starts(names(panel_to_impute), "ln_") | str_starts(names(panel_to_impute), "ln1p_")]
original_vars_from_logs <- str_remove(log_vars, "^ln1p_") %>% str_remove("^ln_")
original_vars_to_exclude_as_predictors <- intersect(original_vars_from_logs, names(panel_to_impute))

if(length(original_vars_to_exclude_as_predictors) > 0){
  # Poner a 0 las COLUMNAS correspondientes en la matriz de predictores
  pred_matrix[, original_vars_to_exclude_as_predictors] <- 0
  cat("--- Configuración de predictores completada ---\n")
  cat("Para evitar multicolinealidad, las siguientes variables no se usarán como predictoras:\n")
  cat(paste(original_vars_to_exclude_as_predictors, collapse = ", "), "\n\n")
}


# 4) EJECUCIÓN DE LA IMPUTACIÓN CON MICE
# -------------------------------------------------------------------------
cat("--- Iniciando la imputación con MICE --- \n")
cat("Este proceso puede tardar varios minutos dependiendo de tus datos...\n")

set.seed(123) # Para reproducibilidad
mice_imputed <- mice(
  panel_to_impute,
  m = 5,
  predictorMatrix = pred_matrix,
  method = meth,
  printFlag = TRUE # Muestra el progreso de las iteraciones
)

cat("\n¡Imputación completada con éxito! ✅\n")


# 5) GUARDADO DE LOS DATASETS COMPLETOS
# -------------------------------------------------------------------------
for (i in 1:mice_imputed$m) {
  completed_df <- complete(mice_imputed, i)
  
  na_count <- sum(sapply(completed_df, function(x) sum(is.na(x))))
  cat(sprintf("\nVerificando dataset final %d: Total de NAs = %d\n", i, na_count))
  
  file_name_base <- paste0("panel_imputed_mice_complete_", i)
  path_dta <- file.path(output_dir, paste0(file_name_base, ".dta"))
  path_xlsx <- file.path(output_dir, paste0(file_name_base, ".xlsx"))
  
  # Guardar los archivos
  haven::write_dta(completed_df, path_dta)
  writexl::write_xlsx(completed_df, path_xlsx)
  
  cat(sprintf(" -> Guardado: %s\n", path_dta))
  cat(sprintf(" -> Guardado: %s\n", path_xlsx))
}

cat(sprintf("\n¡Proceso finalizado! Se han guardado %d datasets completos en la carpeta '%s'.\n",
            mice_imputed$m, basename(output_dir)))

# =========================================================================
# FIN DEL SCRIPT
# =========================================================================