---
title: "Análisis de Actividad Física"
output: 
  html_document:
    keep_md: true
date: "2026-02-13"
---

``` r
# 1. Cargar y limpiar datos
if(!file.exists("activity.csv")){
    unzip("activity.zip")
}
data <- read.csv("activity.csv")
data$date <- as.Date(data$date, format = "%Y-%m-%d")

# 2. Calcular pasos totales por día
pasos_por_dia <- aggregate(steps ~ date, data, sum, na.rm = TRUE)

# 3. Histograma
hist(pasos_por_dia$steps, 
     main = "Total de pasos dados por día", 
     xlab = "Número de pasos", 
     col = "green", 
     breaks = 20)
```

![](PA1_template_files/figure-html/análisis_inicial-1.png)<!-- -->

``` r
# 4. Media y Mediana
media_pasos <- mean(pasos_por_dia$steps)
mediana_pasos <- median(pasos_por_dia$steps)

# Mostrar resultados
media_pasos
```

```
## [1] 10766.19
```

``` r
mediana_pasos
```

```
## [1] 10765
```
## Patrón de actividad diaria promedio

``` r
# 1. Calcular el promedio de pasos por intervalo de 5 minutos
promedio_intervalo <- aggregate(steps ~ interval, data, mean, na.rm = TRUE)

# 2. Gráfico de serie temporal (línea azul)
plot(promedio_intervalo$interval, promedio_intervalo$steps, 
     type = "l", 
     col = "blue", 
     lwd = 2,
     main = "Promedio de pasos por intervalo de 5 min",
     xlab = "Intervalo", 
     ylab = "Promedio de pasos")
```

![](PA1_template_files/figure-html/serie_temporal-1.png)<!-- -->

``` r
# 3. Identificar el intervalo con el máximo de pasos
intervalo_max <- promedio_intervalo[which.max(promedio_intervalo$steps), ]
print(paste("El intervalo con el máximo promedio de pasos es el:", intervalo_max$interval))
```

```
## [1] "El intervalo con el máximo promedio de pasos es el: 835"
```

``` r
print(paste("Con un promedio de:", round(intervalo_max$steps, 2), "pasos"))
```

```
## [1] "Con un promedio de: 206.17 pasos"
```
## Imputación de valores faltantes

``` r
# 1. Calcular el número total de NA
total_nas <- sum(is.na(data$steps))
print(paste("Total de valores faltantes (NA):", total_nas))
```

```
## [1] "Total de valores faltantes (NA): 2304"
```

``` r
# 2. Estrategia: Llenar NA con el promedio de ese intervalo de 5 minutos
data_imputed <- data
for (i in 1:nrow(data_imputed)) {
    if (is.na(data_imputed$steps[i])) {
        interval_val <- data_imputed$interval[i]
        steps_val <- promedio_intervalo[promedio_intervalo$interval == interval_val, ]$steps
        data_imputed$steps[i] <- steps_val
    }
}

# 3. Nuevo histograma con datos imputados
pasos_por_dia_imputed <- aggregate(steps ~ date, data_imputed, sum)
hist(pasos_por_dia_imputed$steps, 
     main = "Total de pasos por día (Datos Imputados)", 
     xlab = "Número de pasos", 
     col = "orange", 
     breaks = 20)
```

![](PA1_template_files/figure-html/imputacion-1.png)<!-- -->

``` r
# 4. Nueva Media y Mediana
nueva_media <- mean(pasos_por_dia_imputed$steps)
nueva_mediana <- median(pasos_por_dia_imputed$steps)

print(paste("Nueva Media:", nueva_media))
```

```
## [1] "Nueva Media: 10766.1886792453"
```

``` r
print(paste("Nueva Mediana:", nueva_mediana))
```

```
## [1] "Nueva Mediana: 10766.1886792453"
```
## Diferencias en patrones de actividad entre semana y fin de semana

``` r
# 1. Crear la variable de día de la semana
data_imputed$date <- as.Date(data_imputed$date)
data_imputed$day <- weekdays(data_imputed$date)

# 2. Clasificar en "weekday" (día laborable) o "weekend" (fin de semana)
data_imputed$day_type <- ifelse(data_imputed$day %in% c("Saturday", "Sunday", "sábado", "domingo"), 
                               "weekend", "weekday")
data_imputed$day_type <- as.factor(data_imputed$day_type)

# 3. Calcular el promedio por intervalo y tipo de día
promedio_tipo_dia <- aggregate(steps ~ interval + day_type, data_imputed, mean)

# 4. Crear el gráfico de panel (usando el sistema lattice o ggplot2)
library(ggplot2)
g <- ggplot(promedio_tipo_dia, aes(x = interval, y = steps, color = day_type)) +
     geom_line() +
     facet_wrap(~ day_type, nrow = 2) +
     labs(title = "Promedio de pasos: Día laborable vs Fin de semana",
          x = "Intervalo", 
          y = "Número de pasos") +
     theme_minimal() +
     theme(legend.position = "none")

print(g)
```

![](PA1_template_files/figure-html/fin_de_semana-1.png)<!-- -->
