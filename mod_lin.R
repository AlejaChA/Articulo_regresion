# Paquetes necesarios
library(readxl)
library(tidyverse)
library(writexl)

# Descargar y leer los datos
url_datos <- "https://raw.githubusercontent.com/AlejaChA/Articulo_regresion/main/datos.xlsx"
archivo_temp <- tempfile(fileext = ".xlsx")
download.file(
  url = url_datos,
  destfile = archivo_temp,
  mode = "wb"
)
datos <- read_excel(archivo_temp)

# Revisar estructura
str(datos)

# Ajustar tipos de variables
datos$Genotipo <- as.factor(datos$Genotipo)
datos$Localidad <- as.factor(datos$Localidad)
datos$Anio <- as.integer(datos$Anio)
datos$Rendimiento_Y <- as.numeric(datos$Rendimiento_Y)

# Crear carpeta de resultados
if(!dir.exists("resultados")){
  dir.create("resultados")
}

# Calculo del indice ambiental
media_general <- mean(datos$Rendimiento_Y, na.rm = TRUE)
ambientes <- datos %>%
  group_by(Anio, Localidad) %>%
  summarise(
    Media_amb = mean(Rendimiento_Y, na.rm = TRUE),
    .groups = "drop"
  )
ambientes$Ij <- ambientes$Media_amb - media_general
ambientes$Condicion <- ifelse(
  ambientes$Ij < 0,
  "Desfavorable",
  "Favorable"
)

# Variable utilizada para el segundo segmento
promedio_positivos <- mean(
  ambientes$Ij[ambientes$Ij > 0]
)
ambientes$T_I <- ifelse(
  ambientes$Ij > 0,
  ambientes$Ij - promedio_positivos,
  0
)

# Incorporar Ij y T(Ij) a la base original
datos_modelo <- merge(
  datos,
  ambientes[, c(
    "Anio",
    "Localidad",
    "Ij",
    "T_I",
    "Condicion"
  )],
  by = c("Anio", "Localidad")
)

# Estadisticas descriptivas por genotipo
resumen_genotipos <- datos_modelo %>%
  group_by(Genotipo) %>%
  summarise(
    Rendimiento_general =
      mean(Rendimiento_Y, na.rm = TRUE),
    Desfavorable =
      mean(Rendimiento_Y[Ij < 0], na.rm = TRUE),
    Favorable =
      mean(Rendimiento_Y[Ij > 0], na.rm = TRUE),
    .groups = "drop"
  )

# Ajuste de regresiones por genotipo
genotipos <- unique(datos_modelo$Genotipo)
lista_modelos <- list()
for(i in seq_along(genotipos)){
  geno <- genotipos[i]
  datos_geno <- subset(
    datos_modelo,
    Genotipo == geno
  )
  modelo <- lm(
    Rendimiento_Y ~ Ij + T_I,
    data = datos_geno
  )
  coeficientes <- coef(modelo)
  fila <- data.frame(
    Genotipo = as.character(geno),
    b1 = coeficientes["Ij"],
    b2 = coeficientes["T_I"],
    b1_mas_b2 =
      coeficientes["Ij"] +
      coeficientes["T_I"],
    R2 =
      summary(modelo)$r.squared * 100
  )
  lista_modelos[[i]] <- fila
}
parametros_modelo <- bind_rows(lista_modelos)

# Combinar informacion
resultados_modelo <- left_join(
  resumen_genotipos,
  parametros_modelo,
  by = "Genotipo"
)
resultados_modelo <- resultados_modelo %>%
  arrange(desc(Rendimiento_general))

# Exportar tablas
write_xlsx(
  list(
    "Parametros modelo" = resultados_modelo,
    "Indices ambientales" = ambientes
  ),
  "resultados/resultados_modelo.xlsx"
)

# Predicciones para las curvas
lista_pred <- list()
for(i in seq_along(genotipos)){
  geno <- genotipos[i]
  datos_geno <- subset(
    datos_modelo,
    Genotipo == geno
  )
  modelo <- lm(
    Rendimiento_Y ~ Ij + T_I,
    data = datos_geno
  )
  grid <- data.frame(
    Ij = seq(
      min(datos_geno$Ij),
      max(datos_geno$Ij),
      length.out = 100
    )
  )
  grid$T_I <- ifelse(
    grid$Ij > 0,
    grid$Ij - promedio_positivos,
    0
  )
  grid$Predicho <- predict(
    modelo,
    newdata = grid
  )
  grid$Genotipo <- geno
  lista_pred[[i]] <- grid
}
predicciones <- bind_rows(lista_pred)

# Grafico 1
p1 <- ggplot() +
  geom_point(
    data = datos_modelo,
    aes(Ij, Rendimiento_Y),
    colour = "grey40"
  ) +
  geom_line(
    data = predicciones,
    aes(Ij, Predicho),
    colour = "blue"
  ) +
  geom_vline(
    xintercept = 0,
    linetype = 2,
    colour = "red"
  ) +
  facet_wrap(~Genotipo, scales = "free_y") +
  labs(
    title = "Regresion bisegmentada",
    x = "Indice ambiental",
    y = "Rendimiento"
  ) +
  theme_bw()
ggsave(
  "resultados/regresiones_bisegmentadas.png",
  p1,
  width = 12,
  height = 9,
  dpi = 300
)

# Grafico 2
p2 <- ggplot(
  ambientes,
  aes(
    reorder(
      paste(Localidad, Anio),
      Ij
    ),
    Ij,
    fill = Condicion
  )
) +
  geom_col() +
  geom_hline(
    yintercept = 0,
    linetype = 2
  ) +
  coord_flip() +
  theme_bw() +
  labs(
    title = "Indices ambientales",
    x = "Ambiente",
    y = "Indice ambiental"
  )
ggsave(
  "resultados/indices_ambientales.png",
  p2,
  width = 8,
  height = 6,
  dpi = 300
)

# Grafico 3
pendientes <- resultados_modelo %>%
  select(
    Genotipo,
    b1,
    b1_mas_b2
  )
colnames(pendientes) <- c(
  "Genotipo",
  "Desfavorables",
  "Favorables"
)
pendientes_long <- pivot_longer(
  pendientes,
  cols = -Genotipo,
  names_to = "Ambiente",
  values_to = "Pendiente"
)
p3 <- ggplot(
  pendientes_long,
  aes(
    Genotipo,
    Pendiente,
    fill = Ambiente
  )
) +
  geom_col(position = "dodge") +
  geom_hline(
    yintercept = 1,
    linetype = 2
  ) +
  theme_bw() +
  theme(
    axis.text.x =
      element_text(
        angle = 45,
        hjust = 1
      )
  ) +
  labs(
    title = "Pendientes por genotipo",
    x = "Genotipo",
    y = "Coeficiente"
  )
ggsave(
  "resultados/pendientes_por_genotipo.png",
  p3,
  width = 10,
  height = 6,
  dpi = 300
)
