# =========================================================================
# SCRIPT INDEPENDIENTE PARA LISTAR TODOS LOS PAÍSES ÚNICOS EN EL DATASET
# =========================================================================

# 1) Cargar los paquetes necesarios
# -------------------------------------------------------------------------
# 'haven' es necesario para leer archivos de Stata (.dta).
# 'dplyr' se usa para el operador pipe (%>%) que facilita la lectura del código.
if (!require(haven)) install.packages("haven")
if (!require(dplyr)) install.packages("dplyr")
library(haven)
library(dplyr)


# 2) Definir la ruta y cargar el archivo de datos
# -------------------------------------------------------------------------
# Se usa el primer dataset imputado, ya que todos contienen los mismos países.
dir_path <- "C:/Users/joser/Desktop/Data Base/Imputed_Data_MICE_Complete"
file_to_load <- file.path(dir_path, "panel_imputed_mice_complete_1.dta")

# Verificar si el archivo existe antes de intentar leerlo
if (!file.exists(file_to_load)) {
  stop("El archivo de datos no fue encontrado. Por favor, verifica que la ruta es correcta: ", file_to_load)
}

# Cargar el dataset en un objeto llamado 'panel_data'
panel_data <- haven::read_dta(file_to_load)


# 3) Extraer, ordenar e imprimir la lista de países
# -------------------------------------------------------------------------
# Se extraen los valores únicos de la columna 'country' y se ordenan alfabéticamente
lista_de_paises <- unique(panel_data$country) %>% sort()

# Imprimir los resultados de forma clara en la consola
cat("--- LISTA COMPLETA DE PAÍSES EN EL DATASET ---\n\n")
print(lista_de_paises)

cat("\n-------------------------------------------------\n")
cat(sprintf("Total de países únicos encontrados: %d\n", length(lista_de_paises)))
cat("-------------------------------------------------\n")

# =========================================================================
# FIN DEL SCRIPT
# =========================================================================