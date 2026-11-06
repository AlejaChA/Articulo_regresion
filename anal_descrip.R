###Instalacion de librerias###

#install.packages("tidyverse")
#install.packages("readxl")

### Librerías utilizadas ###

library(tidyverse)
library(readxl)

###Lectura de la base de datos###
url_datos <- "https://raw.githubusercontent.com/AlejaChA/Articulo_regresion/main/datos.xlsx"

# Descargar el archivo temporalmente en segundo plano
temp_file <- tempfile(fileext = ".xlsx")
download.file(url_datos, destfile = temp_file, mode = "wb")

# Leer la base de datos
datos <- read_excel(temp_file)

head(datos)
dim(datos)
summary(datos)

# Cambio de formato de la variable Anio
datos$Anio <- as.character(datos$Anio)

# Valores faltantes
print(colSums(is.na(datos)))

### Análisis descriptivo de variables numéricas ###
resumen_num <- datos %>%
  select(where(is.numeric)) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Valor") %>%
  group_by(Variable) %>%
  summarise(
    N        = sum(!is.na(Valor)),
    Media    = mean(Valor, na.rm = TRUE),
    Desv_Est = sd(Valor, na.rm = TRUE),
    Minimo   = min(Valor, na.rm = TRUE),
    Q1       = quantile(Valor, 0.25, na.rm = TRUE),
    Mediana  = median(Valor, na.rm = TRUE),
    Q3       = quantile(Valor, 0.75, na.rm = TRUE),
    Maximo   = max(Valor, na.rm = TRUE),
    IQR      = IQR(Valor, na.rm = TRUE),
    Skewness = (sum((Valor - mean(Valor))^3) / n()) / (sd(Valor)^3) # Asimetría a mano
  ) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

print(as.data.frame(resumen_num))

### Matriz de correlación ###
datos_num <- datos %>% select(where(is.numeric))
matriz_corr <- cor(datos_num, use = "complete.obs")
print(round(matriz_corr, 2))

### Visualización gráfica ###
# Histogramas de distribución
datos_num %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Valor") %>%
  ggplot(aes(x = Valor)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "black", alpha = 0.7) +
  facet_wrap(~ Variable, scales = "free") +
  theme_minimal() +
  labs(title = "Distribución de variables numéricas y rendimiento",
       x = "Valor", 
       y = "Frecuencia")

# Rendimiento por localidad y año
ggplot(datos, aes(x = Localidad, y = Rendimiento_Y, fill = Anio)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Rendimiento de híbridos por localidad y año",
       x = "Localidad",
       y = "Rendimiento (t/ha)")
