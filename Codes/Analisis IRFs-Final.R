# =========================================================================
# SCRIPT DE DIAGNÓSTICO FINAL Y CORRECTO
#
# OBJETIVO:
# Utilizar las funciones nativas de 'bvartools' ('plot' y 'summary')
# para analizar correctamente los resultados de la estimación.
# =========================================================================

# 1) INSTALACIÓN Y CARGA DE PAQUETES
# -------------------------------------------------------------------------
req_pkgs <- c("bvartools", "dplyr", "purrr", "coda")
install.packages(setdiff(req_pkgs, installed.packages()))

library(bvartools)
library(dplyr)
library(purrr)
library(coda) # coda se usa para el cálculo del ESS

# 2) CARGA DE LOS RESULTADOS GUARDADOS
# -------------------------------------------------------------------------
# Apuntar a la carpeta donde están tus archivos .rds
tryCatch({
  setwd("C:/Users/joser/Desktop/TFM - Fin/Diagnostic")
}, error = function(e) {
  stop("No se pudo encontrar la carpeta de diagnóstico. Asegúrate de que la ruta es correcta.")
})

diagnostic_files <- list.files(pattern = "^diagnostic_results_.*\\.rds$")

if (length(diagnostic_files) == 0) {
  stop("No se encontraron archivos de diagnóstico '.rds' en la carpeta especificada.")
}

cat(sprintf("Se encontraron %d archivos de resultados para diagnosticar. ✅\n", length(diagnostic_files)))

# 3) BUCLE PRINCIPAL DE DIAGNÓSTICO
# -------------------------------------------------------------------------
# Itera sobre cada archivo de resultados y ejecuta las funciones de diagnóstico.
for (file in diagnostic_files) {
  
  model_title_clean <- gsub("diagnostic_results_|.rds", "", file) %>% gsub("_", " ", .)
  posterior_object <- readRDS(file)
  
  cat(paste0("\n\n========================================================\n"))
  cat(paste0("--- DIAGNÓSTICO PARA: ", model_title_clean, " ---\n"))
  cat(paste0("========================================================\n\n"))
  
  # --- a. Generar Gráficos de Diagnóstico Nativos de bvartools ---
  cat("Generando gráficos de diagnóstico (revisa la pestaña 'Plots')...\n")
  cat("Se generarán gráficos de traza para TODOS los coeficientes.\n")
  
  # Generamos los gráficos de traza, que son los más importantes para la convergencia
  # QUÉ BUSCAR: Un "gusano peludo" horizontal y estable.
  # Es una buena práctica envolverlo en tryCatch si un modelo es muy grande
  try(plot(posterior_object, type = "trace"), silent = TRUE)
  
  # --- b. Resumen Numérico y Diagnósticos de Convergencia ---
  cat("\n--- 1. Resumen Estadístico del Posterior (función nativa) ---\n")
  # summary() sobre un objeto 'bvar' da un resumen completo
  print(summary(posterior_object))
  
  
  ### CORRECCIÓN ###
  cat("\n--- 2. Cálculo del Tamaño Efectivo de la Muestra (ESS) ---\n")
  # Extraemos TODOS los coeficientes de la matriz A, no solo los primeros 5.
  
  all_coef_draws <- posterior_object$A
  
  # Convertimos a un objeto MCMC que 'coda' pueda leer
  mcmc_object_all <- as.mcmc(all_coef_draws)
  
  # Calculamos el ESS para CADA coeficiente
  ess_values <- effectiveSize(mcmc_object_all)
  
  cat("Resumen del ESS para TODOS los coeficientes de la Matriz A:\n")
  # Imprimir un resumen (Min, Mediana, Max) es mucho más útil
  print(summary(ess_values))
  
  cat(paste("\nValor MÍNIMO de ESS encontrado:", round(min(ess_values), 1), "\n"))
  cat("QUÉ BUSCAR: El ESS *MÍNIMO* idealmente debe ser > 400.\n")
  cat("Si el mínimo es bajo, la convergencia es pobre para ese parámetro.\n")
  ### FIN DE LA CORRECCIÓN ###
  
  
  # Pausa para que puedas ver los gráficos y resultados antes de continuar
  if (file != last(diagnostic_files)) {
    readline(prompt="Presiona [Enter] en la consola para continuar con el siguiente modelo...")
  }
}

cat("\n--- Fin del Script de Diagnóstico --- \n")