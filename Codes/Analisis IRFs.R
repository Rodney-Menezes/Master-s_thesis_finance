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
  cat("Se generarán gráficos de traza e histogramas para TODOS los coeficientes.\n")
  
  # plot() sobre un objeto 'bvar' genera los histogramas por defecto
  # plot(posterior_object, type = "hist") 
  
  # Generamos los gráficos de traza, que son los más importantes para la convergencia
  # QUÉ BUSCAR: Un "gusano peludo" horizontal y estable.
  plot(posterior_object, type = "trace")
  
  # --- b. Resumen Numérico y Diagnósticos de Convergencia ---
  cat("\n--- 1. Resumen Estadístico del Posterior (función nativa) ---\n")
  # summary() sobre un objeto 'bvar' da un resumen completo
  print(summary(posterior_object))
  
  cat("\n--- 2. Cálculo del Tamaño Efectivo de la Muestra (ESS) ---\n")
  # Extraemos la matriz A (ahora sabemos que no tiene nombres) y calculamos el ESS
  # para los primeros coeficientes como ejemplo
  
  # Tomamos los primeros 5 coeficientes de la matriz A como muestra representativa
  coef_draws_sample <- posterior_object$A[, 1:5]
  mcmc_object_sample <- as.mcmc(coef_draws_sample)
  
  ess_values <- effectiveSize(mcmc_object_sample)
  
  cat("ESS para una muestra de los primeros 5 coeficientes:\n")
  print(ess_values)
  cat("\nQUÉ BUSCAR: El 'Effective Sample Size' (ESS) idealmente debe ser > 400.\n")
  
  # Pausa para que puedas ver los gráficos y resultados antes de continuar
  if (file != last(diagnostic_files)) {
    readline(prompt="Presiona [Enter] en la consola para continuar con el siguiente modelo...")
  }
}

cat("\n--- Fin del Script de Diagnóstico --- \n")