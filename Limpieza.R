#=========================================================================================
#                               TP-MINERÍA: Asociaciones y Secuencias
#                               Autores: Adriel Starchevich y Elias Coradini
#=========================================================================================
library(readxl)
library(dplyr)
library(ggplot2)
library(arules)
library(arulesSequences)
library(tidyr)
library(scales)# para etiquetas de porcentaje

#========= Cargar los datos ===================================
datos <- read.csv("./e-shop clothing 2008.csv", sep = ";")
#========= Limpieza de los datos ===================================
#Renombramos columnas
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

#========= Exploracion de los datos ===================================

#--------- Exploración inicial de los datos
View(datos)

summary(datos)

str(datos)

dim(datos)

head(datos)

names(datos)

colSums(is.na(datos))

sapply(datos, function(x) length(unique(x)))  # cantidad de valores únicos por columna

#--------- Sesiones únicas por país
sesiones_por_pais <- datos %>%
  group_by(country) %>%
  summarise(sesiones = n_distinct(session_id)) %>%
  arrange(desc(sesiones))

# Grafico de barras de sesiones por país
ggplot(sesiones_por_pais, aes(x = reorder(country, sesiones), y = sesiones)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Cantidad de sesiones únicas por país",
       x = "País",
       y = "Cantidad de sesiones") +
  theme_minimal()

#--------- Porcentaje de sesiones únicas por país
sesiones_por_pais <- datos %>%
  group_by(country) %>%
  summarise(sesiones = n_distinct(session_id)) %>%
  mutate(porcentaje = sesiones / sum(sesiones) * 100)

sesiones_agrupadas <- sesiones_por_pais %>%
  mutate(country = ifelse(porcentaje < 3, "Otros", country)) %>%
  group_by(country) %>%
  summarise(sesiones = sum(sesiones)) %>%
  mutate(porcentaje = sesiones / sum(sesiones) * 100) %>%
  arrange(desc(porcentaje))

# Gráfico
ggplot(sesiones_agrupadas, aes(x = reorder(country, porcentaje), y = porcentaje)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(round(porcentaje, 1), "%")), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(title = "Porcentaje de sesiones únicas por país",
       x = "País",
       y = "Porcentaje de sesiones (%)") +
  theme_minimal()

#--------- Clicks por página
datos %>%
  group_by(page) %>%
  summarise(clicks = n()) %>%
  ggplot(aes(x = factor(page), y = clicks)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Cantidad de clicks por página", x = "Página", y = "Clicks") +
  theme_minimal()

#--------- Boxplot del precio por categoría principal
ggplot(datos, aes(x = main_category, y = price)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Boxplot del precio por categoría principal",
       x = "Categoría principal",
       y = "Precio (USD)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

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

#--------- Distribución de sesiones
# Clics por sesión
sesiones <- datos %>%
  group_by(session_id) %>%
  summarise(total_clicks = n()) %>%
  ungroup()

# Calcular los conteos por grupo para agregar etiquetas
conteo <- sesiones %>%
  count(clicks_grupo)

# Agrupar en rangos de clics
sesiones <- sesiones %>%
  mutate(clicks_grupo = cut(
    total_clicks,
    breaks = c(0, 2, 5, 10, 20, 50, 100, Inf),
    labels = c("1–2", "3–5", "6–10", "11–20", "21–50", "51–100", "100+"),
    right = FALSE
  ))


# Gráfico con etiquetas
ggplot(conteo, aes(x = clicks_grupo, y = n)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black") +
  geom_text(aes(label = n), vjust = -0.5) +  # Agrega etiquetas arriba de las barras
  labs(title = "Distribución de sesiones por número de clicks",
       x = "Rango de clicks por sesión",
       y = "Cantidad de sesiones") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#--------- Distribución de sesiones por numero de clicks

#Categoria principal por sesion
cat_sesion <- datos %>%
  group_by(session_id, main_category) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(session_id) %>%
  top_n(1, wt = n) %>%
  ungroup() %>%
  select(session_id, main_category)

#Unimos con los totales de clicks
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


#=====================================

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


#========= Sesiones por país =====================
sesiones_por_pais <- datos %>%
  group_by(country) %>%
  summarise(sesiones = n_distinct(session_id)) %>%
  filter(country != "Poland" & sesiones > 20)

# Gráfico con barras horizontales
ggplot(sesiones_por_pais, aes(x = reorder(country, sesiones), y = sesiones)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Cantidad de sesiones únicas por país",
       x = "País",
       y = "Cantidad de sesiones") +
  theme_minimal()


#========= Productos vistos por Sesión =====================
productos_por_sesion <- datos %>%
  group_by(session_id, product_code)%>%
  summarise(veces_visto = n(), .groups = "drop")

productos_por_sesion %>%
  arrange(desc(veces_visto)) %>%
  head(10)


ggplot(filter(productos_por_sesion, session_id == 1), 
       aes(x = product_code, y = veces_visto)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Productos vistos en la sesión 1",
       x = "Código del producto",
       y = "Veces visto") +
  theme_minimal()

#========= Categoría de Producto por Sesión =====================
top_categorias <- datos %>%
  group_by(main_category) %>%
  summarise(N = n()) %>%
  arrange(desc(N)) %>%
  slice_head(n = 4) %>%
  rename(categoria = main_category)

# grafico de barras mostrando las categorías más clickeadas, con limite en el eje x de 30 mil clicks en adelante

ggplot(top_categorias, aes(x = reorder(categoria, N), y = N)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip(ylim = c(35000, max(top_categorias$N))) +  # Limita eje N en gráfico horizontal
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

# Vemos como se distribuyen las cantidades de items por transacción
frecuentes <- itemFrequency(transacciones)
frecuentes <- sort(frecuentes, decreasing = TRUE)
top10 <- frecuentes[1:10]

grises <- gray.colors(10, start = 0.3, end = 0.9)
barplot(top10, 
        col = grises,
        main = "Top 10 productos más frecuentes",
        ylab = "Frecuencia relativa",
        las = 2,
        cex.names = 0.7)

# Vemos la cantidad de transacciones por producto
boxplot(frecuentes, 
        main = "Distribución de frecuencias de ítems",
        ylab = "Frecuencia relativa",
        col = "lightblue")
#=========================================================================================

#========= E) Conjunto de itemsets frecuentes: soporte = 2%, minlen = 2 =====================
itemsets_frecuentes <- eclat(transacciones,
                             parameter = list(support = 0.02, minlen = 2), 
                             control = list(verbose=F))

itemsets <- sort(itemsets_frecuentes, by= "support", decreasing = TRUE)

inspect(head(itemsets, 6))

#=========================================================================================

#========= F) reglas de asociación: Polonia, en la categoría “blusas” =====================

polonia <- datos %>%
  filter(country == "Poland", main_category == "blouses")

trans_polonia <- as(split(polonia$product_code, polonia$session_id), "transactions")


# reglas con soporte mínimo de 2% y una confianza de 20%

itemsets_frec <- eclat(trans_polonia,
                             parameter = list(support = 0.02, minlen = 2), 
                             control = list(verbose=F))

itemsets_frec
itemsets <- sort(itemsets_frec[1:5], by = "support", decreasing = TRUE)
inspect(itemsets)

reglas <- ruleInduction(itemsets_frec, 
                        transactions = trans_polonia,
                        confidence = 0.2)
reglas

itemsets <- sort(reglas, by = "support", decreasing = TRUE)
inspect(itemsets[1:10])

metricas <- interestMeasure(reglas, measure = c("coverage", "fishersExactTest", "lift"))

metricas

#=========================================================================================

#========= G) reglas de asociación: República Checa, en la categoría “blusas” =====================
czech_republic <- datos %>%
  filter(country == "Czech Republic", main_category == "blouses")

trans_czech_republic <- as(split(czech_republic$product_code, czech_republic$session_id), "transactions")


# reglas con soporte mínimo de 4% y una confianza de 20%

itemsets_frec <- eclat(trans_czech_republic,
                       parameter = list(support = 0.04, minlen = 2), 
                       control = list(verbose=F))

itemsets_frec
itemsets <- sort(itemsets_frec, by = "support", decreasing = TRUE)
inspect(itemsets)

reglas <- ruleInduction(itemsets_frec, 
                        transactions = trans_czech_republic,
                        confidence = 0.25)
reglas

itemsets <- sort(reglas, by = "support", decreasing = TRUE)
inspect(itemsets)

metricas <- interestMeasure(reglas, measure = c("coverage", "fishersExactTest", "lift"))

metricas

#=========================================================================================

#========= I) secuencias más frecuentes  =====================

# Filtrar y seleccionar solo las columnas necesarias
# Preparar datos para análisis secuencial
secuencias <- datos %>%
  select(session_id, order, product_code) %>%
  rename(sequenceID = session_id, eventID = order, item = product_code) %>%
  # Asegurar que cada fila representa un item en un evento específico
  arrange(sequenceID, eventID)

# Guardar en CSV temporal para cargarlo como transacciones secuenciales
write.table(secuencias, "secuencias_tmp.csv", sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE)

trans_seq <- read_baskets("secuencias_tmp.csv", sep = ",", info = c("sequenceID", "eventID"))

inspect(trans_seq[1:5])

seq1 <- cspade(trans_seq,
               parameter = list(support = 0.02),
               control = list(verbose=F))

seq1 <- subset(seq1, size(seq1) > 1)

seq1 <- sort(seq1, by = "support", decreasing = TRUE)

seq1
summary(seq1)
inspect(seq1)


#=========================================================================================

#========= PRUEBAS =====================
trans_location <- as(split(datos$location, datos$session_id), "transactions")

# Reglas de asociación
rules_location <- apriori(trans_location,
                          parameter = list(supp = 0.05, conf = 0.80, minlen = 2))
rules_location

inspect(rules_location[1:10])
inspect(sort(rules_location, by = "support", decreasing = TRUE))


# Combinar productos y ubicaciones
datos$product_location <- paste(datos$product_code, datos$location, sep = "_")

transacciones <- as(split(datos$product_location, datos$session_id), "transactions")

# Reglas: encontrar asociaciones entre productos y posiciones
reglas <- apriori(transacciones, parameter = list(supp = 0.01, conf = 0.5, minlen = 2))
reglas
inspect(sort(reglas, by = "confidence", decreasing = TRUE))

#=========================================================================================























