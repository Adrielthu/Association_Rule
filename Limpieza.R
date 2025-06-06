library(readxl)
library(dplyr)
library(ggplot2)
library(data.table)
library(corrplot)

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

# Reordenar columnas si querés una estructura lógica
datos <- datos %>%
  select(
    year, month, day, session_id, order, page,
    main_category, product_code, colour, price, price_above_avg,
    model_photography, location, country
  )


head(datos)

#============================ Exploracion de los datos ===================================

View(datos)

summary(datos)

str(datos)

dim(datos)

head(datos)

names(datos)

colSums(is.na(datos))

#matriz de correlaciones
matriz_cor <- cor(datos[sapply(datos, is.numeric)], use = "complete.obs")
corrplot(matriz_cor, method = "color", type = "upper", tl.col = "black", tl.srt = 45)

#=========================================================================================

#========= Clicks por sesión =====================
session_counts <- datos %>%
  group_by(session_id) %>%
  summarise(N = n()) %>%
  arrange(desc(N)) %>%
  slice_head(n = 30)

# Gráfico
ggplot(session_counts, aes(x = reorder(session_id, -N), y = N)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Cantidad de clics por sesión", x = "ID de Sesión", y = "Cantidad de clics") +
  theme_minimal() +
  theme(axis.text.x = element_blank())  # Oculta los labels para no saturar

#========= Sesiones por país =====================
sesiones_por_pais <- datos %>%
  group_by(country) %>%
  summarise(sesiones = n_distinct(session_id))

head(sesiones_por_pais)

# Gráfico
ggplot(sesiones_por_pais, aes(x = as.factor(country), y = sesiones)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Cantidad de sesiones únicas por país",
       x = "País (código)",
       y = "Cantidad de sesiones") +
  theme_minimal()

#========= Productos por sesion =====================
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

#========= categoría de producto por sesión =====================
top_categorias <- datos %>%
  group_by(main_category) %>%
  summarise(N = n()) %>%
  arrange(desc(N)) %>%
  slice_head(n = 10) %>%
  rename(categoria = main_category)

ggplot(top_categorias, aes(x = reorder(categoria, N), y = N)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 categorías más clickeadas",
       x = "Categoría",
       y = "Cantidad de clics") +
  theme_minimal()

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

#=========   =====================












