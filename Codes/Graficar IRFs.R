# =========================================================================
# SCRIPT PARA RE-GENERAR GRÁFICOS DESDE ARCHIVOS .RDS
# NO se re-ejecutan los modelos BVAR.
# =========================================================================

# 1) PAQUETES NECESARIOS PARA GRAFICAR
# -------------------------------------------------------------------------
# Asegúrate de tenerlos instalados
# install.packages(c("ggplot2", "dplyr", "tidyr", "purrr"))

library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)


# 2) FUNCIÓN PARA AGRUPAR Y GRAFICAR (Copiada del script original)
# -------------------------------------------------------------------------
# Esta es la misma función de tu script, no necesita cambios.
pool_and_plot <- function(all_irfs_list, shock_name, model_title, shock_title) {
  
  # Desanida la lista de listas y extrae el shock específico
  all_draws <- all_irfs_list %>% 
    unlist(recursive = FALSE) %>% 
    compact() %>% 
    map(~ .x[[shock_name]])
  
  if (length(all_draws) == 0 || is.null(all_draws[[1]])) {
    cat(paste("No hay resultados válidos para graficar en el modelo:", model_title, "\n"))
    return(NULL)
  }
  
  # Combina todos los "draws" de MCMC de todos los países
  pooled_draws <- do.call(rbind, all_draws)
  if(nrow(pooled_draws) == 0) return(NULL)
  
  # Calcula los cuantiles para las bandas de credibilidad
  pooled_summary <- apply(pooled_draws, 2, function(h) {
    quantile(h, c(0.05, 0.16, 0.5, 0.84, 0.95), na.rm = TRUE)
  })
  
  # Prepara los datos para ggplot
  plot_data <- as.data.frame(t(pooled_summary)) %>%
    rename(
      lower_90 = `5%`, 
      lower_68 = `16%`, 
      median = `50%`, 
      upper_68 = `84%`, 
      upper_90 = `95%`
    ) %>%
    mutate(Horizon = 0:(n() - 1))
  
  # Define los colores (azul para mcap, rojo para vtrd)
  plot_color <- ifelse(shock_name == "mcap", "#253494", "#b30000")
  fill_color <- ifelse(shock_name == "mcap", "#2c7fb8", "#fb6a4a")
  
  # Crea el gráfico
  irf_plot <- ggplot(plot_data, aes(x = Horizon, y = median)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    geom_ribbon(aes(ymin = lower_90, ymax = upper_90), fill = fill_color, alpha = 0.3) +
    geom_ribbon(aes(ymin = lower_68, ymax = upper_68), fill = fill_color, alpha = 0.6) +
    geom_line(color = plot_color, linewidth = 1) +
    geom_point(color = plot_color, size = 2) +
    scale_x_continuous(breaks = 0:10) +
    labs(title = shock_title, subtitle = paste("Modelo:", model_title),
         x = "Horizonte (Años)", y = "Desviación del Cambio en la Tasa de Crecimiento (p.p.)") +
    theme_minimal(base_size = 14)
  
  # Esta línea imprime el gráfico en la consola de RStudio (o dispositivo gráfico)
  print(irf_plot)
  
  # --- (Opcional) Guardar el gráfico automáticamente ---
  # Si prefieres guardar cada gráfico como un archivo PNG, descomenta la línea de abajo.
  # safe_filename <- paste0("IRF_PLOT_", gsub("[^[:alnum:]_]", "", gsub(" ", "_", model_title)), "_", shock_name, ".png")
  # ggsave(safe_filename, irf_plot, width = 10, height = 7, dpi = 300)
  # cat(paste("Gráfico guardado en:", safe_filename, "\n"))
}


# 3) EJECUCIÓN: CARGAR .RDS Y GENERAR GRÁFICOS
# -------------------------------------------------------------------------

# ¡IMPORTANTE! Define la ruta donde guardaste los resultados .rds
results_path <- "C:/Users/joser/Desktop/TFM - Fin/Diagnostic"

# Define los títulos de los modelos (deben ser idénticos al script original)
model_titles <- c(
  "General (Todos los Países)",
  "Países de Altos Ingresos",
  "Países de Medios Ingresos",
  "Países de Bajos Ingresos",
  "Países del G7",
  "Países de la OCDE",
  "América Latina y el Caribe",
  "Norteamérica",
  "Asia",
  "Europa",
  "África",
  "Oceanía"
)

cat("--- Iniciando la generación de gráficos desde archivos .rds --- \n\n")

# Loop para cargar cada resultado y graficarlo
for (title in model_titles) {
  
  # Recrea el nombre de archivo exacto que generó el script original
  # (p.ej., "Países del G7" -> "irf_results_Pa_ses_del_G7.rds")
  safe_title_name <- gsub("[^[:alnum:]]", "_", title)
  file_name <- paste0("irf_results_", safe_title_name, ".rds")
  file_path <- file.path(results_path, file_name)
  
  cat(paste("Procesando modelo:", title, "\n"))
  cat(paste("Buscando archivo:", file_path, "\n"))
  
  # Comprueba si el archivo existe antes de intentar cargarlo
  if (file.exists(file_path)) {
    
    # Cargar el objeto .rds (este es el 'list_of_all_irfs' guardado)
    loaded_irfs <- readRDS(file_path)
    
    # Graficar Shock 1: mcap
    cat("Graficando shock 'mcap'...\n")
    pool_and_plot(loaded_irfs, "mcap", title, "Respuesta a Shock de Capitalización Bursátil")
    
    # Graficar Shock 2: vtrd
    cat("Graficando shock 'vtrd'...\n")
    pool_and_plot(loaded_irfs, "vtrd", title, "Respuesta a Shock de Valor Negociado")
    
    cat(paste("--- Modelo '", title, "' completado --- \n\n"))
    
  } else {
    cat(paste("!!! ADVERTENCIA: No se encontró el archivo:", file_name, "!!!\n"))
    cat("Asegúrate que la variable 'results_path' es correcta.\n\n")
  }
}

cat("--- Fin del script de gráficos --- \n")