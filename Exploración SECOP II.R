# ANÁLISIS DE TEXTO CON LOS DATOS DE SECOP II

# Autor: Juan Sebastián Numpaque Cano
# Contacto: jsnumpaquec@unal.edu.co
# Última modificación: 9 de Junio de 2019

# En caso de encontrar caracteres extraños, por favor dirígase a File -> Reopen with Encoding
# y realice la reapertura del documento en formato UTF-8.

# 1. CONFIGURACIÓN DEL ENTORNO ----

# Primero, limpiamos nuestro entorno de trabajo y cargamos las librarías necesarias
rm(list = ls())

library(rstudioapi) # Versión 1.1.456
library(dplyr) # Versión 0.8.0.1
library(ggplot2) # Versión 3.0.0
library(scales) # Versión 1.0.0
library(data.table)
library(tm)
library(wordcloud)

# Elegimos el directorio de trabajo donde se encuentran las bases de datos
# (en este caso, el mismo donde se encuentra el presente script)
actualdir<-getActiveDocumentContext()$path
setwd(dirname(actualdir))

# 2. FUNCIONES ----

# 2.1 Limpiar texto ----

preproc.text <- function(x){
  
  require(dplyr)
  require(tm)
  
  y <- x %>%
    tolower %>% # pasa a minúscula
    gsub("[^[:print:]]", " ", .) %>% # cambia caracteres no imprimibles por espacios
    gsub("[^[:lower:]^[:space:]]", " ", .) %>% # cambia puntuación y números por espacios
    removeWords(., stopwords(kind = "sp")) %>% # elimina stopwords
    gsub("[[:space:]]{1,}", " ", .) %>% # cambia espacios consecutivos por uno solo
    trimws # elimina espacios al inicio y al final de la cadena
  return(y)
  
}

# 2.3 Contar ngramas ----

# La función cuenta los ngramas presentes en un texto ----
contar_ngramas <- function(texto, n_palabras){
  
  require(NLP)
  
  # Tokeniza el texto por palabras
  texto <- as.character(texto)
  texto <- sapply(texto, function(x) strsplit(x, split = " ")[[1]])
  
  # Reescribe el texto como ngramas
  ngramas <- sapply(texto, function(x) NLP::ngrams(x, n_palabras))
  ngramas <- sapply(ngramas, function(x) sapply(x, function(y) paste(y, collapse = " ")))
  
  # Realiza una tabla de frecuencias para los ngramas
  df <- data.frame(sort(table(unlist(ngramas)), decreasing = TRUE))
  colnames(df) <- c("Palabra", "Frecuencia")
  
  # Retorna un data frame que contiene los n-gramas y su frecuencia en el texto
  df
}


# 3. LIMPIEZA DE DATOS ----

# Leemos el archivo con las órdenes y asignamos los tipos de variables adecuados a cada 

contratos <- fread("SECOP_II_-_Contratos.csv",
                   encoding = "UTF-8", 
                   colClasses = "character",
                   stringsAsFactors = FALSE)

# Seleccionamos algunas variables de interés
valores <- contratos %>%
  select("Nit Entidad", "Nombre Entidad", "Descripción del Proceso", "Tipo de Contrato", "Referencia del Contrato", "Estado Contrato",
         "Fecha de Inicio del Contrato",  "Fecha de Fin del Contrato",  "Fecha de Inicio de Ejecucion", "Fecha de Fin de Ejecucion",
         "Valor del Contrato", "Valor de pago adelantado",  "Valor Facturado", "Valor Pendiente de Pago", 
         "Valor Pagado", "Valor Amortizado", "Valor Pendiente de Amortizacion", "Valor Pendiente de Ejecucion",
         "Proveedor Adjudicado", "Tipo Documento Proveedor", "Documento Proveedor", "Departamento")

# Cambiamos los tipos de variable
valores$`Nit Entidad` <- as.numeric(valores$`Nit Entidad`)
valores$`Valor Amortizado` <- as.numeric(valores$`Valor Amortizado`)
valores$`Valor de pago adelantado` <- as.numeric(valores$`Valor de pago adelantado`)
valores$`Valor del Contrato` <- as.numeric(valores$`Valor del Contrato`)
valores$`Valor Facturado` <- as.numeric(valores$`Valor Facturado`)
valores$`Valor Pagado` <- as.numeric(valores$`Valor Pagado`)
valores$`Valor Pendiente de Amortizacion` <- as.numeric(valores$`Valor Pendiente de Amortizacion`)
valores$`Valor Pendiente de Ejecucion` <- as.numeric(valores$`Valor Pendiente de Ejecucion`)
valores$`Valor Pendiente de Pago` <- as.numeric(valores$`Valor Pendiente de Pago`)

valores$`Descripción del Proceso` <- preproc.text(valores$`Descripción del Proceso`)

# Extraemos año de inicio de los contratos
valores$inicio <- as.numeric(substr(valores$`Fecha de Inicio del Contrato`, 7, 10))

# Revisamos si el dataset contiene registros vacíos
sum(is.na(valores))
valores <- valores[complete.cases(valores),] # 340843 - 311643

# Revisamos si hay datos atípicos que puedan generar ruido en el análisis y los eliminamos
ggplot(valores, aes(y = `Valor del Contrato`, col = `Tipo de Contrato`)) +
  geom_boxplot()

# Calculamos el percentil 95 y verificamos qué porcentaje de datos serán tomados como outliers
valor_q_95 <- quantile(valores$`Valor del Contrato`,0.95)
sum(valores$`Valor del Contrato`>=valor_q_95)/nrow(valores)

contratos_atipicos <- valores[valores$`Valor del Contrato`>=valor_q_95,]
contratos_tipicos <- valores[valores$`Valor del Contrato`<valor_q_95,]

# Realizamos de nuevo el boxplot
ggplot(contratos_tipicos, aes(y = `Valor del Contrato`, col = `Tipo de Contrato`)) +
  geom_boxplot()

# 3. ANÁLISIS DESCRIPTIVO Y EXPLORATORIO ----

# Serie de tiempo, contratos registrados por año

rango_contratos <- range(contratos_tipicos$inicio)
contratos_por_anio <- data.frame(table(factor(contratos_tipicos$inicio, levels = rango_contratos[1]:rango_contratos[2])))
colnames(contratos_por_anio) <- c("Año", "Contratos iniciados")

ggplot(contratos_por_anio, aes(x = `Año`, y = `Contratos iniciados`)) +
  geom_point() + geom_line(group = 1)

rango_contratos <- c(2015, 2019)
contratos_por_anio <- data.frame(table(factor(contratos_tipicos$inicio, levels = rango_contratos[1]:rango_contratos[2])))
colnames(contratos_por_anio) <- c("Año", "Contratos iniciados")

ggplot(contratos_por_anio, aes(x = `Año`, y = `Contratos iniciados`)) +
  geom_point() + geom_line(group = 1)

# Tipos de contratos
tipos_contratos <- data.frame(table(contratos_tipicos$`Tipo de Contrato`))
colnames(tipos_contratos) <- c("Tipo de contrato", "Cantidad")

ggplot(tipos_contratos, aes(x = `Tipo de contrato`, y = `Cantidad`)) +
  geom_bar(stat = "identity", col= "steelblue4", fill = "steelblue2", alpha = 0.75) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

sum(contratos_tipicos$`Tipo de Contrato` == "Prestación de servicios")/nrow(contratos_tipicos)

# Correlación entre valores

ggplot(contratos_tipicos, aes(x = `Valor Pagado`, y = `Valor Pendiente de Pago`)) +
  geom_point(col = "dodgerblue2")

# Nube de palabras - contratos altos

mi_df <- contar_ngramas(contratos_atipicos$`Descripción del Proceso`, 1)

set.seed(1234)
X11()
wordcloud(words = mi_df$Palabra, freq = mi_df$Frecuencia, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# Nube de palabras - contratos normales

nuevas_stopwords <- c("prestación", "prestar", "servicio", "servicios", "contrat\\w+")
contratos_tipicos$`Descripción del Proceso` <- removeWords(contratos_tipicos$`Descripción del Proceso`, nuevas_stopwords)
contratos_tipicos$`Descripción del Proceso` <- gsub("[[:space:]]{1,}", " ", contratos_tipicos$`Descripción del Proceso`) 
contratos_tipicos$`Descripción del Proceso` <- trimws(contratos_tipicos$`Descripción del Proceso`)

mi_df <- contar_ngramas(contratos_tipicos$`Descripción del Proceso`, 1)

set.seed(123)
X11()
wordcloud(words = mi_df$Palabra, freq = mi_df$Frecuencia, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.15, 
          colors=brewer.pal(8, "Dark2"))
