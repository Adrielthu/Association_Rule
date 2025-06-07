library(readxl)
library(dplyr)
library(ggplot2)
library(arules)
library(arulesSequences)

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
datos$country <- recode(as.character(datos$country), !!!paises)

# MAIN CATEGORY
categorias <- c("1" = "trousers", "2" = "skirts", "3" = "blouses", "4" = "sale")
datos$main_category <- recode(as.character(datos$main_category), !!!categorias)

# COLOUR
colores <- c(
  "1" = "beige", "2" = "black", "3" = "blue", "4" = "brown", "5" = "burgundy",
  "6" = "gray", "7" = "green", "8" = "navy blue", "9" = "many colors",
  "10" = "olive", "11" = "pink", "12" = "red", "13" = "violet", "14" = "white"
)
datos$colour <- recode(as.character(datos$colour), !!!colores)

# LOCATION
ubicacion <- c(
  "1" = "top left", "2" = "top middle", "3" = "top right",
  "4" = "bottom left", "5" = "bottom middle", "6" = "bottom right"
)
datos$location <- recode(as.character(datos$location), !!!ubicacion)

# MODEL PHOTOGRAPHY
datos$model_photography <- recode(as.character(datos$model_photography),
                                   "1" = "en face", "2" = "profile")
# PRICE 2
datos$price_above_avg <- recode(as.character(datos$price_above_avg),
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

#histograma de order
ggplot(datos, aes(x = order)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(title = "Histograma de órdenes", x = "Orden", y = "Frecuencia") +
  theme_minimal()


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
  theme(axis.text.x = element_blank())  # Oculta los labels para no saturar


#========= Sesiones por país =====================
sesiones_por_pais <- datos %>%
  group_by(country) %>%
  summarise(sesiones = n_distinct(session_id))%>%
  filter(country != "Poland" & sesiones > 20)

head(sesiones_por_pais)

# Gráfico
ggplot(sesiones_por_pais, aes(x = as.factor(country), y = sesiones)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Cantidad de sesiones únicas por país",
       x = "País (código)",
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
#========= clicks de navegación a lo largo de los meses =====================
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
#========= Convertir los productos por sesión a transacciones =====================

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
#========= Conjunto de itemsets frecuentes: soporte = mediana, minlen = 2 =====================
soporte <- median(frecuentes)

itemsets_frecuentes <- eclat(transacciones,
                             parameter = list(support = soporte, minlen = 2), 
                             control = list(verbose=F))

itemsets <- sort(itemsets_frecuentes, by= "support", decreasing = TRUE)

inspect(head(itemsets, 5))

#========= itemsets frecuentes: soporte = 2%, minlen = 2 =====================
itemsets_frecuentes <- eclat(transacciones,
                             parameter = list(support = 0.02, minlen = 2), 
                             control = list(verbose=F))

itemsets <- sort(itemsets_frecuentes, by= "support", decreasing = TRUE)

inspect(head(itemsets, 5))

#=========================================================================================
#========= reglas de asociación: Polonia, en la categoría “blusas” =====================

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
#========= reglas de asociación: República Checa, en la categoría “blusas” =====================
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
#========= secuencias más frecuentes  =====================

seq1 <- cspade(zaki,
               # support = 0.1 -> set of 3917 sequences
               # support = 0.25 -> set of 3917 sequences
               # support = 0.4 -> set of 18 sequences
               # support = 0.7 -> set of 7 sequences
               parameter = list(support = 0.7),
               control = list(verbose=F))
seq1
summary(seq1)
inspect(seq1)

reglas1 <- ruleInduction(seq1,
                         confidence = 0.8)
reglas1
inspect(reglas1[1:10])

# Ejemplo 2
library(data.table)
transacciones <- read_baskets(con = "sequences.txt",
                              info = c("sequenceID","eventID","SIZE"))
transacciones
summary(transacciones)

inspect(transacciones[1:10])

aux0 <- as(transacciones, "data.frame")

seq1 <- cspade(transacciones,
               parameter = list(support = 0.002),
               control = list(verbose=F))

seq1
summary(seq1)
inspect(seq1[1:20])

reglas1 <- ruleInduction(seq1,
                         confidence = 0.26)
reglas1
inspect(reglas1[1:10])

as(sort(reglas1, decreasing = TRUE, by = "lift"),
   "data.frame")

arulesVIZ

















