#=========================================================================================
#                                   MINERÍA DE DATOS
#                           Reglas de Asociacion y Secuencias
#
#                         Autores: Adriel Starchevich y Elias Coradini
#                                       12/06/2025
#=========================================================================================

#========= Librerías necesarias ==================================
# Manipulación y transformación de datos
library(dplyr)           # Verbos como filter(), group_by(), summarise(), mutate()
library(tidyr)           # Para pivotear, separar o unir columnas (reshape de datos)
library(tibble)          # Alternativa moderna a data.frame, más limpia y legible

# Visualización de datos
library(ggplot2)         # Para crear gráficos personalizados
library(scales)          # Para formatear ejes (porcentajes, escalas logarítmicas, etc.)
library(waffle)          # Para crear gráficos tipo waffle (proporciones en cuadrícula)
library(RColorBrewer)    # Paletas de colores predefinidas para visualizaciones

# Minería de reglas de asociación y patrones secuenciales
library(arules)          # Para aplicar el algoritmo Apriori y generar reglas de asociación
library(arulesSequences) # Para análisis de secuencias ordenadas con el algoritmo cSPADE

# Manejo de texto
library(stringr)         # Funciones útiles para procesar strings: detectar, reemplazar, extraer

#=========================================================================================

#========= Cargar los datos ===================================
datos <- read.csv("./e-shop clothing 2008.csv", sep = ";")
#=========================================================================================

#========= Limpieza de los datos ===================================
# Renombramos columnas
colnames(datos) <- c(
  "year", "month", "day", "order", "country", "session_id",
  "main_category", "product_code", "colour", "location",
  "model_photography", "price", "price_above_avg", "page"
)

# COUNTRY
paises <- c(
  "1" = "Australia", "2" = "Austria", "3" = "Belgium", "4" = "British Virgin Islands",
  "5" = "Cayman Islands", "6" = "Christmas Island", "7" = "Croatia", "8" = "Cyprus",
  "9" = "Czech Republic", "10" = "Denmark", "11" = "Estonia", "12" = "unidentified",
  "13" = "Faroe Islands", "14" = "Finland", "15" = "France", "16" = "Germany",
  "17" = "Greece", "18" = "Hungary", "19" = "Iceland", "20" = "India",
  "21" = "Ireland", "22" = "Italy", "23" = "Latvia", "24" = "Lithuania",
  "25" = "Luxembourg", "26" = "Mexico", "27" = "Netherlands", "28" = "Norway",
  "29" = "Poland", "30" = "Portugal", "31" = "Romania", "32" = "Russia",
  "33" = "San Marino", "34" = "Slovakia", "35" = "Slovenia", "36" = "Spain",
  "37" = "Sweden", "38" = "Switzerland", "39" = "Ukraine", "40" = "UAE",
  "41" = "UK", "42" = "USA", "43" = "biz", "44" = "com", "45" = "int",
  "46" = "net", "47" = "org"
)
datos$country <- dplyr::recode(as.character(datos$country), !!!paises)

# MAIN CATEGORY
categorias <- c("1" = "trousers", "2" = "skirts", "3" = "blouses", "4" = "sale")
datos$main_category <- dplyr::recode(as.character(datos$main_category), !!!categorias)

# COLOUR
colores <- c(
  "1" = "beige", "2" = "black", "3" = "blue", "4" = "brown", "5" = "burgundy",
  "6" = "gray", "7" = "green", "8" = "navy blue", "9" = "many colors",
  "10" = "olive", "11" = "pink", "12" = "red", "13" = "violet", "14" = "white"
)
datos$colour <- dplyr::recode(as.character(datos$colour), !!!colores)

# LOCATION
ubicacion <- c(
  "1" = "top left", "2" = "top middle", "3" = "top right",
  "4" = "bottom left", "5" = "bottom middle", "6" = "bottom right"
)
datos$location <- dplyr::recode(as.character(datos$location), !!!ubicacion)

# MODEL PHOTOGRAPHY
datos$model_photography <- dplyr::recode(as.character(datos$model_photography),
                                         "1" = "en face", "2" = "profile")
# PRICE 2
datos$price_above_avg <- dplyr::recode(as.character(datos$price_above_avg),
                                       "1" = "yes", "2" = "no")

# Reordeno las columnas
datos <- datos %>%
  select(
    year, month, day, session_id, order, page,
    main_category, product_code, colour, price, price_above_avg,
    model_photography, location, country
  )

#=========================================================================================

#========= Exploracion de los datos ===================================

#--------- Exploración inicial de los datos
View(datos)

summary(datos)

str(datos)

dim(datos)

head(datos)

names(datos)

colSums(is.na(datos))

# Muestra la cantidad de valores únicos por columna
sapply(datos, function(x) length(unique(x)))

#--------- Clicks por página
datos %>%
  group_by(page) %>%
  summarise(clicks = n()) %>%
  ggplot(aes(x = factor(page), y = clicks)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Cantidad de clicks por página", x = "Página", y = "Clicks") +
  theme_minimal()

#--------- Distribución de precios
ggplot(datos, aes(x = price)) +
  geom_histogram(binwidth = 5, fill = "darkgreen", color = "white") +
  scale_x_continuous(breaks = seq(20, 80, by = 5)) +
  labs(
    title = "Distribución de precios",
    x = "Precio",
    y = "Frecuencia"
  ) +
  theme_minimal()


#--------- Distribución de clicks según la posición de la imagen en las diferentes páginas: por país
# Para Polonia
resumen_pagina_polonia <- datos %>%
  filter(country == "Poland") %>%
  group_by(page, location) %>% 
  summarise(cantidad_clics = n(), precio_promedio = mean(price, na.rm = TRUE), .groups = "drop")

ggplot(resumen_pagina_polonia, aes(x = reorder(location, cantidad_clics),
                                   y = cantidad_clics,
                                   fill = precio_promedio)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient(low = "#c7f0c1", high = "#006400", name = "Precio promedio") +
  labs(title = "Clicks por posición de imagen y página en Polonia",
       x = "Posición de imagen",
       y = "Cantidad de clicks") +
  facet_wrap(~ page, ncol = 1) +
  theme_minimal()

# Para Czech Republic
resumen_pagina_polonia <- datos %>%
  filter(country == "Czech Republic") %>%
  group_by(page, location) %>%
  summarise(
    cantidad_clics = n(),
    precio_promedio = mean(price, na.rm = TRUE),
    .groups = "drop")

ggplot(resumen_pagina_polonia, aes(x = reorder(location, cantidad_clics),
                                   y = cantidad_clics,
                                   fill = precio_promedio)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient(low = "#c7f0c1", high = "#006400", name = "Precio promedio") +
  labs(title = "Clicks por posición de imagen y página en República Checa",
       x = "Posición de imagen",
       y = "Cantidad de clicks") +
  facet_wrap(~ page, ncol = 1) +
  theme_minimal()

#---------- Mapa de calor de clicks por posición de imagen y página

posiciones_coord <- data.frame(
  location = 1:6,
  x = c(1, 2, 3, 1, 2, 3),  # coordenadas x para el gráfico
  y = c(2, 2, 2, 1, 1, 1)   # coordenadas y para el gráfico
)

# Función para generar el mapa de calor, según el país y título
crear_heatmap_corregido <- function(pais_nombre, titulo_pais) {
  # Mapeo de location a números
  mapeo_location <- c(
    "top left" = 1, "top middle" = 2, "top right" = 3,
    "bottom left" = 4, "bottom middle" = 5, "bottom right" = 6
  )
  
  # Preparar datos con mapeo correcto
  resumen_pais <- datos %>%
    filter(country == pais_nombre) %>%
    mutate(location_num = mapeo_location[location]) %>%
    filter(!is.na(location_num), location_num %in% 1:6) %>%
    group_by(page, location_num) %>%
    summarise(cantidad_clics = n(), .groups = "drop") %>%
    inner_join(posiciones_coord, by = c("location_num" = "location"))
  
  # Crea el gráfico
  ggplot(resumen_pais, aes(x = x, y = y, fill = cantidad_clics)) +
    geom_tile(color = "black", linewidth = 0.5) +
    geom_text(aes(label = cantidad_clics), color = "white", fontface = "bold") +
    coord_fixed() +
    scale_x_continuous(breaks = 1:3, labels = c("Izq", "Centro", "Der")) +
    scale_y_continuous(breaks = 1:2, labels = c("Abajo", "Arriba")) +
    scale_fill_viridis_c() +
    theme_minimal() +
    theme(legend.position = "right", panel.grid = element_blank()) +
    facet_wrap(~ page) +
    labs(title = paste(titulo_pais, "- Clicks por posición"),
         fill = "Cantidad\nde clicks")
}

# Generación de los gráficos para Polonia y República Checa
crear_heatmap_corregido("Poland", "Polonia")
crear_heatmap_corregido("Czech Republic", "República Checa")


#--------- Distribución de sesiones por numero de clicks

# Categoria principal por sesion
cat_sesion <- datos %>%
  group_by(session_id, main_category) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(session_id) %>%
  top_n(1, wt = n) %>%
  ungroup() %>%
  select(session_id, main_category)

# Unimos con los totales de clicks
sesiones_cat <- sesiones %>%
  left_join(cat_sesion, by = "session_id")

# Graficar
ggplot(sesiones_cat, aes(x = clicks_grupo, fill = main_category)) +
  geom_bar(position = "fill") +
  labs(title = "Clics por sesión según categoría principal",
       x = "Rango de clics por sesión",
       y = "Proporción de sesiones",
       fill = "Categoría") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#--

# Calcular proporciones por grupo
proporciones <- sesiones_cat %>%
  group_by(clicks_grupo, main_category) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(clicks_grupo) %>%
  mutate(proporcion = n / sum(n))#calculamos las proporciones

# Gráfico con proporciones y etiquetas
ggplot(proporciones, aes(x = clicks_grupo, y = proporcion, fill = main_category)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = percent(proporcion, accuracy = 0.1)),
            position = position_stack(vjust = 0.5),
            size = 3.05, color = "black") +
  labs(title = "Clics por sesión según categoría principal",
       x = "Rango de clics por sesión",
       y = "Proporción de sesiones",
       fill = "Categoría") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#=========================================================================================

#========= Clicks por sesión =====================
session_counts <- datos %>%
  group_by(session_id) %>%
  summarise(N = n()) %>%
  arrange(desc(N)) %>%
  slice_head(n = 300)

# Gráfico
ggplot(session_counts, aes(x = reorder(session_id, -N), y = N)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Cantidad de clicks por sesión", x = "ID de Sesión", y = "Cantidad de clicks") +
  theme_minimal() +
  theme(axis.text.x = element_blank())  # Oculta los labels

#--------- Distribución de sesiones - Otra forma de representar los clicks

# Clicks por sesión
sesiones <- datos %>%
  group_by(session_id) %>%
  summarise(total_clicks = n()) %>%
  ungroup()

# Agrupa en rangos de clics
sesiones <- sesiones %>%
  mutate(clicks_grupo = cut(
    total_clicks,
    breaks = c(0, 2, 5, 10, 20, 50, 100, Inf),
    labels = c("1–2", "3–5", "6–10", "11–20", "21–50", "51–100", "100+"),
    right = FALSE
  ))

# Calcula los conteos por grupo para agregar etiquetas
conteo <- sesiones %>%
  count(clicks_grupo)

# Gráfico
ggplot(conteo, aes(x = clicks_grupo, y = n)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black") +
  geom_text(aes(label = n), vjust = -0.5) +
  labs(title = "Distribución de sesiones por número de clicks",
       x = "Rango de clicks por sesión",
       y = "Cantidad de sesiones") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#========= Sesiones por país =====================

#--------- Sesiones únicas por país
sesiones_por_pais <- datos %>%
  group_by(country) %>%
  summarise(sesiones = n_distinct(session_id)) %>%
  arrange(desc(sesiones))

# Grafico
ggplot(sesiones_por_pais, aes(x = reorder(country, sesiones), y = sesiones)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Cantidad de sesiones únicas por país",
       x = "País",
       y = "Cantidad de sesiones") +
  theme_minimal()

#--------- Sesiones únicas por país con filtro de Polonia y mínimo de 20 sesiones
sesiones_por_pais <- datos %>%
  group_by(country) %>%
  summarise(sesiones = n_distinct(session_id)) %>%
  filter(country != "Poland" & sesiones > 20)

# Gráfico
ggplot(sesiones_por_pais, aes(x = reorder(country, sesiones), y = sesiones)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Cantidad de sesiones únicas por país",
       x = "País",
       y = "Cantidad de sesiones") +
  theme_minimal()


#--------- Porcentaje de sesiones únicas por país

# Agrupar, calcular porcentaje y consolidar países con <3%
waffle_vector <- datos %>%
  group_by(country) %>%
  summarise(sesiones = n_distinct(session_id), .groups = "drop") %>%
  mutate(
    country = case_when(
      country == "Czech Republic" ~ "República Checa",
      country == "Poland" ~ "Polonia",
      (sesiones / sum(sesiones)) * 100 < 3 ~ "Otros",
      TRUE ~ country  # Mantener el resto sin cambios
    )
  ) %>%
  group_by(country) %>%
  summarise(sesiones = sum(sesiones), .groups = "drop") %>%
  mutate(porcentaje = round(sesiones / sum(sesiones) * 100)) %>%
  arrange(desc(porcentaje)) %>%
  select(country, porcentaje) %>%
  deframe()

waffle_vector <- waffle_vector[c("Polonia", "República Checa", "Otros")]

# Gráfico waffle
waffle(waffle_vector,
       rows = 10,
       title = "Porcentaje de sesiones únicas por país",
       colors = brewer.pal(n = length(waffle_vector), name = "Set2"))


#========= Productos vistos por Sesión =====================
# Agrupa por producto y cuenta cuántas veces fue visto
productos_mas_vistos <- datos %>%
  group_by(product_code) %>%
  summarise(veces_visto = n(), .groups = "drop") %>%
  arrange(desc(veces_visto)) %>%
  slice_head(n = 20)

# Gráfico
ggplot(productos_mas_vistos, aes(x = reorder(product_code, veces_visto), y = veces_visto)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 productos más clickeados",
       x = "Código del producto",
       y = "Cantidad de clicks") +
  theme_minimal()


#========= Categoría de Producto por Sesión =====================
top_categorias <- datos %>%
  group_by(main_category) %>%
  summarise(N = n()) %>%
  arrange(desc(N)) %>%
  slice_head(n = 4) %>%
  rename(categoria = main_category)

# grafico de barras mostrando las categorías más clickeadas
# con limite en el eje x de 35 mil clicks en adelante
ggplot(top_categorias, aes(x = reorder(categoria, N), y = N)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip(ylim = c(35000, max(top_categorias$N))) +  # Limita en el eje Y
  labs(title = "Categorías más clickeadas",
       x = "Categoría",
       y = "Cantidad de clicks") +
  theme_minimal()

#=========================================================================================

#========= C) clicks de navegación a lo largo de los meses =====================
clics_por_mes <- datos %>%
  group_by(month) %>%
  summarise(cantidad_clics = n(), .groups = "drop") %>%
  mutate(nombre_mes = factor(month, 
                             levels = 4:8, 
                             labels = c("Abril", "Mayo", "Junio", "Julio", "Agosto")))

ggplot(clics_por_mes, aes(x = nombre_mes, y = cantidad_clics)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Evolución de los clicks de navegación por mes",
    x = "Mes",
    y = "Cantidad de clicks"
  ) +
  theme_minimal()
#=========================================================================================

#========= D) Convertir los productos por sesión a transacciones =====================
transacciones <- as(split(datos$product_code, datos$session_id), "transactions")

# Numero de transacciones
transacciones

# Resumen de las transacciones
summary(transacciones)

# Vemos las transacciones
inspect(transacciones[1:10])

#=========================================================================================

#========= E) Conjunto de itemsets frecuentes: soporte = 2%, minlen = 2 =====================
itemsets_frecuentes <- eclat(transacciones,
                             parameter = list(support = 0.02, minlen = 2), 
                             control = list(verbose=F))
# Numero de items frecuentes
itemsets_frecuentes

# Itemsets frecuentes ordenados por soporte
itemsets <- sort(itemsets_frecuentes, by= "support", decreasing = TRUE)

inspect(itemsets[1:10])

#=========================================================================================

#========= F) reglas de asociación: Polonia, en la categoría “blusas” =====================
polonia <- datos %>%
  filter(country == "Poland", main_category == "blouses")

trans_polonia <- as(split(polonia$product_code, polonia$session_id), "transactions")


# Itemset frecuentes con soporte mínimo de 2% y una confianza de 20%
itemsets_frec <- eclat(trans_polonia,
                             parameter = list(support = 0.02, minlen = 2), 
                             control = list(verbose=F))
# Numero de items frecuentes
itemsets_frec

itemsets <- sort(itemsets_frec, by = "support", decreasing = TRUE)

inspect(itemsets[1:6])

# Reglas con soporte mínimo de 2% y una confianza de 20%
reglas <- ruleInduction(itemsets_frec, 
                        transactions = trans_polonia,
                        confidence = 0.2)

# Numero de reglas frecuentes
reglas

itemsets <- sort(reglas, by = "lift", decreasing = TRUE)
inspect(itemsets[1:10])

# Cálculo de métricas
metricas <- interestMeasure(reglas, measure = c("coverage", "fishersExactTest", "lift"))

metricas

#=========================================================================================

#========= G) reglas de asociación: República Checa, en la categoría “blusas” =====================
czech_republic <- datos %>%
  filter(country == "Czech Republic", main_category == "blouses")

trans_czech_republic <- as(split(czech_republic$product_code, czech_republic$session_id), "transactions")


# Itemsets frecuentes con soporte mínimo de 4% y una confianza de 20%
itemsets_frec <- eclat(trans_czech_republic,
                       parameter = list(support = 0.04, minlen = 2), 
                       control = list(verbose=F))
# Numero de items frecuentes
itemsets_frec

itemsets <- sort(itemsets_frec, by = "support", decreasing = TRUE)
inspect(itemsets)

# Reglas con soporte mínimo de 4% y una confianza de 25%
reglas <- ruleInduction(itemsets_frec, 
                        transactions = trans_czech_republic,
                        confidence = 0.25)
# Numero de reglas frecuentes
reglas

itemsets <- sort(reglas, by = "lift", decreasing = TRUE)
inspect(itemsets)

# Cálculo de métricas
metricas <- interestMeasure(reglas, measure = c("coverage", "fishersExactTest", "lift"))

metricas

#=========================================================================================

#========= I) secuencias más frecuentes  =====================

# Filtro y selecciono solo las columnas necesarias
secuencias <- datos %>%
  select(session_id, order, product_code) %>%
  rename(sequenceID = session_id, eventID = order, item = product_code) %>%
  arrange(sequenceID, eventID)

# Guardo en CSV para despues cargarlo como transacciones
write.table(secuencias, "secuencias_tmp.csv", sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE)

# Cargo las secuencias como transacciones
trans_seq <- read_baskets("secuencias_tmp.csv", sep = ",", info = c("sequenceID", "eventID"))


inspect(trans_seq[1:5])

# Genero las secuencias con un soporte mínimo del 2%
seq1 <- cspade(trans_seq,
               parameter = list(support = 0.02),
               control = list(verbose=F))

# Filtro las secuencias con más de un elemento
seq1 <- subset(seq1, size(seq1) > 1)

# Ordeno las secuencias por soporte
seq1 <- sort(seq1, by = "support", decreasing = TRUE)

seq1
summary(seq1)
inspect(seq1[1:5])

# Lo guardo en una tabla para visualizar mejor
seq_tabla <- data.frame(
  secuencia = labels(seq1),
  soporte = quality(seq1)$support
)

seq_tabla

#=========================================================================================


