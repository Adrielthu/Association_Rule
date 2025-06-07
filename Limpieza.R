library(readxl)
library(dplyr)
library(ggplot2)
library(arules)
library(arulesSequences)
library(tidyr)

#=========================================================================================
datos <- read.csv("./e-shop clothing 2008.csv", sep = ";")
#=========================================================================================
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

#============================ Exploracion de los datos ===================================

View(datos)

summary(datos)

str(datos)

dim(datos)

head(datos)

names(datos)

colSums(is.na(datos))

sapply(datos, function(x) length(unique(x)))  # cantidad de valores únicos por columna

# Grafico de barras de categorías principales
ggplot(datos, aes(x = main_category)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribución de categorías principales")

# Calcular sesiones únicas por país
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

# Grafico de distribución de precios
ggplot(datos, aes(x = price)) +
  geom_bar(bins = 50, fill = "darkgreen") +
  labs(title = "Distribución de precios")


# Agrupar los clics por posición de imagen
clics_por_posicion <- datos %>%
  group_by(location) %>%
  summarise(cantidad_clics = n()) %>%
  arrange(desc(cantidad_clics))

#top_categorias
top_categorias <- datos %>%
  group_by(main_category) %>%
  summarise(N = n()) %>%
  arrange(desc(N)) %>%
  slice_head(n = 10) %>%
  rename(categoria = main_category)

# Gráfico de barras horizontales ordenado por cantidad
ggplot(clics_por_posicion, aes(x = reorder(location, cantidad_clics), y = cantidad_clics)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  coord_flip(ylim = c(20000, max(top_categorias$N))) +
  labs(title = "Cantidad de clics por posición de imagen",
       x = "Posición de la imagen",
       y = "Cantidad de clics") +
  theme_minimal()


# Promedio de precios por posición de imagen
precio_promedio_por_posicion <- datos %>%
  group_by(location) %>%
  summarise(precio_promedio = mean(price, na.rm = TRUE)) %>%
  arrange(desc(precio_promedio))

# Visualización: gráfico de barras horizontales
ggplot(precio_promedio_por_posicion, aes(x = reorder(location, precio_promedio), y = precio_promedio)) +
  geom_bar(stat = "identity", fill = "darkgray") +
  coord_flip() +
  labs(title = "Precio promedio por posición de imagen",
       x = "Posición de imagen",
       y = "Precio promedio (USD)") +
  theme_minimal()

# grafico boxplot del precio
ggplot(datos, aes(x = main_category, y = price)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Boxplot del precio por categoría principal",
       x = "Categoría principal",
       y = "Precio (USD)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Calcular clics y precio promedio por posición
resumen_posicion <- datos %>%
  group_by(location) %>%
  summarise(
    cantidad_clics = n(),
    precio_promedio = mean(price, na.rm = TRUE)
  ) %>%
  arrange(desc(cantidad_clics))

# Gráfico
ggplot(resumen_posicion, aes(x = reorder(as.factor(location), cantidad_clics),
                             y = cantidad_clics,
                             fill = precio_promedio)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient(low = "#c7f0c1", high = "#006400", name = "Precio promedio") +
  labs(title = "Clics por posición de imagen",
       x = "Posición de imagen",
       y = "Cantidad de clics") +
  theme_minimal()


#Clics por sesión
sesiones <- datos %>%
  group_by(session_id) %>%
  summarise(total_clicks = n()) %>%
  ungroup()

#Agrupar en rangos de clics
sesiones <- sesiones %>%
  mutate(clicks_grupo = cut(
    total_clicks,
    breaks = c(0, 2, 5, 10, 20, 50, 100, Inf),
    labels = c("1–2", "3–5", "6–10", "11–20", "21–50", "51–100", "100+"),
    right = FALSE
  ))

head(sesiones)

#Grafico
ggplot(sesiones, aes(x = clicks_grupo)) +
  geom_bar(fill = "steelblue", color = "black") +
  labs(title = "Distribución de sesiones por número de clics",
       x = "Rango de clics por sesión",
       y = "Cantidad de sesiones") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


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
  labs(title = "Cantidad de clics por sesión", x = "ID de Sesión", y = "Cantidad de clics") +
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
  slice_head(n = 10) %>%
  rename(categoria = main_category)

# grafico de barras mostrando las categorías más clickeadas, con limite en el eje x de 30 mil clicks en adelante

ggplot(top_categorias, aes(x = reorder(categoria, N), y = N)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip(ylim = c(35000, max(top_categorias$N))) +  # Limita eje N en gráfico horizontal
  labs(title = "Top 10 categorías más clickeadas",
       x = "Categoría",
       y = "Cantidad de clics") +
  theme_minimal()

#=========================================================================================

#========= C) clicks de navegación a lo largo de los meses =====================
clics_por_mes <- datos %>%
  group_by(month) %>%
  summarise(cantidad_clics = n(), .groups = "drop")

ggplot(clics_por_mes, aes(x = factor(month), y = cantidad_clics)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Evolución de los clics de navegación por mes",
       x = "Mes",
       y = "Cantidad de clics") +
  theme_minimal()

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

#========= E) Conjunto de itemsets frecuentes: soporte = mediana, minlen = 2 =====================
soporte <- median(frecuentes)

itemsets_frecuentes <- eclat(transacciones,
                             parameter = list(support = soporte, minlen = 2), 
                             control = list(verbose=F))

itemsets <- sort(itemsets_frecuentes, by= "support", decreasing = TRUE)

inspect(head(itemsets, 5))

#itemsets frecuentes: soporte = 2%, minlen = 2
itemsets_frecuentes <- eclat(transacciones,
                             parameter = list(support = 0.02, minlen = 2), 
                             control = list(verbose=F))

itemsets <- sort(itemsets_frecuentes, by= "support", decreasing = TRUE)

inspect(head(itemsets, 5))

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
secuencias <- datos %>%
  select(session_id, order, product_code) %>%
  rename(sequenceID = session_id, eventID = order, item = product_code) %>%
  group_by(sequenceID, eventID) %>%
  summarise(items = paste(item, collapse = ","), .groups = "drop")

# Guardar en CSV temporal para cargarlo como transacciones secuenciales
write.table(secuencias, "secuencias_tmp.csv", sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE)

trans_seq <- read_baskets("secuencias_tmp.csv", sep = ",", info = c("sequenceID", "eventID"))

inspect(trans_seq[1:5])

seq1 <- cspade(trans_seq,
               parameter = list(support = 0.02),
               control = list(verbose=F))
seq1
summary(seq1)
inspect(seq1[1:10])


















#=========================================================================================
