##### Analisis semanal de datos Covid2019
##### Scripts usados en el artículo:
##### Un anílisis semanal de los casos confirmados a la COVID-19 en Cuba: primeros 70 días. 
##### Medisur, 18(3).  2020
##### http://www.medisur.sld.cu/index.php/medisur/article/view/4722

library(tidyverse)
library(ggExtra)

### Datos Covid
cov2 <- read.csv("covid19-casos-15.06.2020.csv", 
                 encoding = "UTF-8")

### Crear dataframes y selecciones

# Extraer subconjunto de cov2, columnas prov y mun,  haciendo un conteo por municipio 
sel_lineas <- cov2 %>% group_by(fecha_confirmacion) %>% 
  summarise(n = n()) %>% subset(n > 5)


## Para gr�fico circular
## Extraer subconjunto de cov2, columnas prov y mun,  haciendo un conteo por municipio 
cov3 <- cov2[,5:6] %>% group_by(provincia, municipio) %>% 
  summarise(n = n()) %>% subset(n > 5, na.rm = TRUE)
data <- data.frame(
  individual = cov3$municipio,
  group = cov3$provincia,
  value = cov3$n
)

###

### Gr�ficos circulares

## Variante 1. todos los municipios sin agrupar por provincia
## Sin agrupar por provincia
# Set a number of 'empty bar'
empty_bar <- 10
# Add lines to the initial dataset
to_add <- matrix(NA, empty_bar, ncol(data))
colnames(to_add) <- colnames(data)
data <- rbind(data, to_add)
data$id <- seq(1, nrow(data))
# Get the name and the y position of each label
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)
# Make the plot
p <- ggplot(data, aes(x=as.factor(id), y=value, fill=data$group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(stat="identity", show.legend = FALSE) +
  ylim(-100,120) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar(start = 0) + 
  geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 
p

## Variante 2. Organizada por provincia sin label ni escala
# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 4
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each=empty_bar)
data <- rbind(data, to_add)
data <- data %>% arrange(group)
data$id <- seq(1, nrow(data))
# Get the name and the y position of each label
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)
# Make the plot
p <- ggplot(data, aes(x=as.factor(id), y=value, fill=as.factor(id))) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(stat="identity", alpha=0.5) +
  ylim(-100,220) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 
p

## Variante 3. Con label y escala
# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 2
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each=empty_bar)
data <- rbind(data, to_add)
data <- data %>% arrange(group)
data$id <- seq(1, nrow(data))
# Get the name and the y position of each label
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)
# prepare a data frame for base lines
base_data <- data %>% 
  group_by(group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))
# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]
# Make the plot
p <- ggplot(data, aes(x=as.factor(id), y=value, fill=as.factor(id))) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(data$id),4), y = c(20, 40, 60, 80), label = c("20", "40", "60", "80") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
  ylim(-100,250) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -18, label=group),  colour = "black", alpha=0.8, size=2, fontface="bold", inherit.aes = FALSE)

p

### Barras

## Extraer un subconjunto de un dataframe
subset_data <- cov2 %>%  # asignar a una variavle el resultado del paso del dataframe
  subset(.$provincia == "Cienfuegos") %>% write_csv("ss2.csv") # filtrar variable
#  group_by(sexo) %>%  # Agrupar por una variable categ�rica
#  summarize(avg = mean(edad)) %>% # aplicar alguna funci�n a la agrupaci�n para que agrupe
#  mutate(avg = round(avg, 1)) # Redondea a 1 lugar el ave calculado

# graficar la selecci�n con ggplot , flip coordenadas
ggplot(subset_data, aes(y = municipio)) + 
  geom_bar(aes(fill = sexo), position = position_stack(reverse = TRUE)) +
  theme(legend.position = "top")

#Barras con las x en vertical, cada barra rellenada con las provincias
ggplot(cov2, aes(fecha_confirmacion, fill = provincia)) +
  geom_bar()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) #Aqu� se giran las x

#Barras simples
ggplot(cov2, aes(as_date(fecha_confirmacion))) +
  geom_bar()

# Uso de la variable calculada 'count' y con el valor de los datos en las barras
ggplot(cov2, aes(provincia)) + # con geom_bar se puede usar solo x o y y el agrega el count
  geom_bar(aes(fill = provincia), show.legend = FALSE) + # con colores por municipio
# coord_polar(start = 0) + # Para hacerlo circular 
  geom_text(aes(label = stat(count)), stat = 'count', nudge_y = 1.8)+ # Con etiquetas de datos en las baras
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) #Aqu� se giran las x

### Puntos

cov4 <- cov2[,5:6] %>% group_by(mun = cov2$municipio, fecha = cov2$fecha_confirmacion) %>% 
  summarise(n = n()) %>% subset(n > 5)
ggplot(cov4, aes(municipio, n)) +
  geom_point() +  theme(axis.text.x = element_text(angle = 90, hjust = 1)) #Aqu� se giran las x

cov6 <- cov2 %>% mutate(dia_semana = as_date(cov2$fecha_confirmacion))  %>% 
  group_by(dia_semana) %>% summarise( n = n())
ggplot(cov6, aes(dia_semana, n)) +
  geom_point() +  
  geom_smooth() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) #Aqu� se giran las x

# Puntos por sexo y geom_smooth
cov10 <- cov2 %>% mutate(dia_semana = as_date(cov2$fecha_confirmacion))  %>% 
  group_by(dia_semana, sexo) %>% summarise( n = n())
# Convertir fecha_confirmacion a tipo fecha y listo
ggplot(cov10, aes(as_date(dia_semana), n )) +
  geom_point(aes(fill=sexo, color = sexo)) +
  geom_smooth(aes(group=sexo, color = sexo)) +
#  facet_wrap(~sexo, ncol = 1, nrow = 2) +
  labs(title="Casos confirmados por sexo - l�nea de regresi�n",
       x = "D�as",
       y = "Casos")  # title and caption
  
### Lineas

# lineas por sexo y fecha de confirmacion
ggplot(cov2, aes(fecha_confirmacion, colour = sexo)) + 
  geom_line(aes(group = sexo), stat = "count") +
  geom_text(aes(label = stat(count)), stat = "count", nudge_y = 1.8) + # Con etiquetas de datos en las baras
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) #Aqu� se giran las x

# Linea general
ggplot(cov2, aes(x = as_date(fecha_confirmacion))) +
  geom_line(stat = "count", color = "blue") 
 # geom_line(aes(x = as_date(fecha_ingreso)), stat = "count", color = "red")
# Con acumulado
con_Acumulado <- cov2  %>% 
  group_by(fecha = cov2$fecha_confirmacion) %>% 
  summarise(n = n()) 
con_Acumulado$acum <- cumsum(con_Acumulado$n/10)
ggplot(con_Acumulado, aes(fecha)) +
  geom_line(aes(y = n), color = "blue", size = 2) +
  geom_line(aes(y = acum), color = "red", size = 2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


### Cajas

ggplot(subset_data, mapping = aes(x = municipio, y = edad, fill=municipio)) + 
  geom_boxplot() +
  labs(title="Diagrama de cajas: Casos por municipio y edad", 
       subtitle = "Otra cosa de prueba",
       caption="fuente: github covid19 Cuba", 
       y="Edad",
       x="Municipios",
       color=NULL,
       family = "Helvetica") +  # title and caption
    geom_jitter()


### Medias moviles

cov5 <- subset_data %>% mutate(dia_semana = as_date(fecha_confirmacion))  %>% 
  group_by(dia_semana) %>% summarise( n = n()) 
cov4 <- cov2 %>% mutate(dia_semana = as_date(fecha_confirmacion))  %>% 
  group_by(dia_semana) %>% summarise( n = n()) 

#Cienfuegos
plot(cov5$n, main='Medias m�viles - Cienfuegos', xlab='D�a', ylab='Tasa')
lines(cov5$n, col="black", lwd=2)
lines(zoo::rollmean(cov5$n, 4), col="red", lwd=2)
#lines(rollmean(cov5$n, 4,align="right"), col="blue", lwd=2)
legend("topleft", c("Original", "Media m�vil centrada",
                    "Media m�vil no centrada"),
       lwd=c(1,2,2), col=c("black", "red", "blue"))
grid()


#Cuba
plot(cov4$n, main='Media movil - Cuba', xlab='D�as', ylab='Tasa')
lines(cov4$n, col="black", lwd=2)
lines(zoo::rollmean(cov4$n, 4), col="red", lwd=2)
#lines(rollmean(cov4$n, 4,align="right"), col="blue", lwd=2)
legend("topleft", c("Original", "Media m�vil centrada"), #"Media m�vil no centrada"),
       lwd=c(1,2,2), col=c("black", "red", "blue"))
grid()


###  SERIES DE TIEMPO

library(lubridate)
cov6 <- cov2 %>% mutate(dia_semana = as_date(cov2$fecha_confirmacion))  %>% 
  group_by(dia_semana) %>% summarise( n = n()) # %>%   write_csv("C:/Users/Admin/Desktop/Busquedas/Covid/casosXdia.csv")
cov6 <- cov6[3:length(cov6$dia_semana),]
cov6 <- cov6[1:65,]
# Para editarlo y agregar una nueva observaci�n
write_csv(cov6, "C:/Users/Admin/Desktop/Busquedas/Covid/casosXdia.csv")
cov6 <- read.csv("C:/Users/Admin/Desktop/Busquedas/Covid/casosXdia.csv")

# Buscar per�odos
spectrum(cov7, log = "no")
spectrum(cov7, method = "ar")
spectrum(cov7, method = "pgram")
spectrum(cov7, spans = c(3,3))
spectrum(cov7, spans = c(5,7), log = "dB", ci = 0.8)

# Comprobar normalidad de las diferencias de manera visual
hist(diff(cov6$n), probability = TRUE)
plot(diff(cov6$n, 1), type = "l")

# Crear serie de tiempo y descomponer
# La forma de especificar esta frecuencia es declarando la variable
# en donde se encuentran los datos como un objeto "ts"con frecuencia (52, 12, 4, 1), 
# es decir, semanal, mensual, trimestral o anual, respectivamente.
#cov7 <- ts(cov6$n, start = c(2020, 15), frequency = 52)
cov7 <- ts(cov6$n, frequency = 7)
cov8 <- ts(cov6$n, frequency = 30)
cov8
plot(cov8)
s1 <- stl(cov7, s.window = "periodic")
plot(s1)

fit4 <- arima(cov7, order = c(1,0,1))
mod1 <- arima(cov7,c(1,1,0),c(0,1,1))
decompose(cov8)
plot(decompose(cov7,"multiplicative"))

par(mfcol = c(1, 1)) # Poner dos gr�ficas en linea
acf(cov8) 
pacf(cov8)

par(mfcol = c(1, 1)) # Poner 1 gr�ficas en linea
x1 <- diff(cov8)
plot(x1, main="Pasajeros en L�neas A�reas. Diferencias Log.")
x2 <- diff(cov8)
plot(x2, main="Pasajeros en L�neas A�reas. Diferencias Log.")
mod1 <- arima(x,c(1,1,0),c(0,1,1))

fit1 <- arima(cov7, order = c(1,0,0))
fit2 <- arima(cov7, order = c(3,0,0))
fit3 <- arima(cov7, order = c(1,0,1))
predict(fit3, n.ahead = 1)
 
#plotear cada semana con lineas desde el 16
par(mfcol = c(2, 2))
plot(cov6[1:7,]$n, type = "l", col = "red", main = "Semanas 1", xlab = "Lunes - Domingo", ylab = "Casos", size = 4)
plot(cov6[8:14,]$n, type = "l", col = "blue", main = "Semanas 2", xlab = "Lunes - Domingo", ylab = "Casos")
plot(cov6[15:21,]$n, type = "l", col = "violet", main = "Semanas 3", xlab = "Lunes - Domingo", ylab = "Casos")
plot(cov6[22:28,]$n, type = "l", col = "darkseagreen4", main = "Semanas 4", xlab = "Lunes - Domingo", ylab = "Casos")
plot(cov6[29:35,]$n, type = "l", col = "red", main = "Semanas 5", xlab = "Lunes - Domingo", ylab = "Casos")
plot(cov6[36:42,]$n, type = "l", col = "blue", main = "Semanas 6", xlab = "Lunes - Domingo", ylab = "Casos")
plot(cov6[43:49,]$n, type = "l", col = "violet", main = "Semanas 7", xlab = "Lunes - Domingo", ylab = "Casos")
plot(cov6[50:56,]$n, type = "l", col = "darkseagreen4", main = "Semanas 8", xlab = "Lunes - Domingo", ylab = "Casos")

### ANALISIS SEMANAL OKK

# data frame de las semanas con casos desde el 11/03
semanas <- c(rep("Sem 01", 2), rep("Sem 02", 7), rep("Sem 03", 7), rep("Sem 04", 7),
             rep("Sem 05", 7), rep("Sem 06", 7), rep("Sem 07", 7), rep("Sem 08", 7),
             rep("Sem 09", 7), rep("Sem 10", 7), rep("Sem 11", 7), rep("Sem 12", 7),
             rep("Sem 13", 7), rep("Sem 14", 7))

#semConCasos <- data.frame(cov6[1:65,], semanas)
semConCasos <- data.frame(cov6, semanas)
semConCasos$weekday <- wday(semConCasos$dia_semana, week_start = 1)

para_tabla <- semConCasos %>% group_by(semanas) %>% summarise(N = sum(n),
                                                Media = mean(n),
                                                Mediana = median(n), 
                                                SD = sd(n),
                                                CV = round((SD/Media)*100, 1) ) 
  write_excel_csv("C:/Users/Admin/Desktop/Busquedas/Covid/tablaResumen.csv")
para_tabla 

library(kableExtra)
kable(para_tabla)
kable(para_tabla, "latex", booktabs = T) %>%
  kable_styling(latex_options = "striped") %>% as_image("my_latex_table")

# graficar las semanas en cuadros separados
ggplot(semConCasos, aes(as_date(semConCasos$dia_semana), n, color = semanas)) +
  geom_line(size = 2) +
  xlab('Fecha') + ylab('Cantidad de casos confirmados') +
  theme_my_style_mini() +
  labs(title="Casos confirmados por d�a desde el 11-03-2020 al 14-06-2020",
       subtitle = "Dividido las semanas por colores",
       x = "D�as",
       y = "Casos")  
# Con facetas
ggplot(semConCasos, aes(weekday, n, color = semanas)) +
  geom_line(size = 2) +
  xlab('Fecha') + ylab('Cantidad de casos confirmados') +
  theme_my_style() +
 #       legend.text = element_text(reorder(semanas, semConCasos$dia_semana))) +
  labs(title="Casos confirmados por d�a desde el 11-03-2020 al 14-06-2020",
       subtitle = "Dividido las semanas por colores",
       #       caption="fuente: posgradosvirtuales.ucf.edu.cu"
       x = "D�as",
       y = "Casos") +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7), labels = c("Lu", "Ma", "Mi", "Ju", "Vi", "Sa", "Do")) +
  facet_wrap(~semanas, scales = "free_y", ncol = 2)
  
# boxplot por semanas en la misma linea general
ggplot(semConCasos, aes(as_date(dia_semana), n, color = semanas)) +
  geom_boxplot(aes(group=semanas)) +
  xlab('Fecha') + ylab('Cantidad de casos confirmados') +
  theme(legend.position = "bottom") +
  labs(title="Casos confirmados por d�a desde el lun 11-03-2020 al dom 14-06-2020",
       subtitle = "Dividido en 13 semanas  con diagrama de cajas",
       x = "D�as",
       y = "Casos")    # title and caption
  theme_my_style_mini()

library(lubridate)

#crear dataframe con una col por semana y una col con d�a de la semana
semanasEnCols <- data.frame(#"DS" = c("Lun", "Mar", "Mie", "Jue", "Vie", "Sab", "Dom"),
                       "DS" = c(1, 2, 3, 4, 5, 6, 7),
                       "Sem1" = cov6[1:7,]$n, 
                       "Sem2" = cov6[8:14,]$n,
                       "Sem3" = cov6[15:21,]$n,
                       "Sem4" = cov6[22:28,]$n,
                       "Sem5" = cov6[29:35,]$n,
                       "Sem6" = cov6[36:42,]$n,
                       "Sem7" = cov6[43:49,]$n,
                       "Sem8" = cov6[50:56,]$n)

#plotear todas las semanas superpuestas
ggplot(semanasEnCols, aes(DS)) +
  geom_line(aes(y = Sem1), size = 2) +  geom_text(aes(y = Sem1, label = Sem1)) +
  geom_line(aes(y = Sem2), size = 2) + geom_text(aes(y = Sem2, label = Sem2)) +
  geom_line(aes(y = Sem3), size = 2) +  geom_text(aes(y = Sem3, label = Sem3)) +
  geom_line(aes(y = Sem4), size = 2) + geom_text(aes(y = Sem4, label = Sem4)) +
  geom_line(aes(y = Sem5), size = 2) + geom_text(aes(y = Sem5, label = Sem5)) +
  geom_line(aes(y = Sem6), size = 2) + geom_text(aes(y = Sem6, label = Sem6)) +
  geom_line(aes(y = Sem7), size = 2) +   geom_text(aes(y = Sem7, label = Sem7)) +
  geom_line(aes(y = Sem8), size = 2) + geom_text(aes(y = Sem8, label = Sem8)) +
  xlab('Lunes - Domingo') + ylab('Cantidad de casos confirmados') +
  theme(legend.position = "bottom")+
  facet_wrap(~DS) #, nrow = 4, ncol = 1)
#  geom_text(aes(label = stat(count)), stat = "count", nudge_y = 1.8)

#plotear 2 periodos de 30 dias
cov9 <- read.csv("C:/Users/Admin/Desktop/Busquedas/Covid/casosXdia.csv")
per30 <- c(rep("Dia1-30", 30), rep("Dia31-60", 30))
# Primero pivotear a lo largo manteniendo el factor del cluster
unir <- data.frame(cov9, per30)

# Plotear los 2 tramos en facetas y en una misma gr�fica
ggplot(unir, aes(as_date(dia_semana), n, color = per30)) +
  geom_point() +
  geom_smooth() + # method = "gam", formula = y ~ s(x, bs = "cs")) +
#  facet_wrap(~per30, nrow = 2, ncol = 1) +
  labs(title="Casos confirmados",
       subtitle = "Dividido en dos per�odos de 30 d�as - con l�nea de regresi�n",
       x = "D�as",
       y = "Cantidad de casos confirmados")    # title and caption

ggplot(unir, aes(as_date(dia_semana), n, color = per30)) +
  geom_line(size = 2) +
  labs(title="Casos confirmados",
       subtitle = "Dividido en dos per�odos de 30 d�as",
       x = "D�as",
       y = "Cantidad de casos confirmados")    # title and caption


## Medias m�viles
plot(cov6$n, type = "l")
st.1 <- stats::filter(cov6$n, filter = rep(1/5, 5))
lines(st.1, col = "red")
st.2 <- stats::filter(cov6$n, filter = rep(1/25, 25))
lines(st.2, col = "blue")

## OK OK Grafica con animaci�n

cov4 <- cov2[,5:6] %>% 
  group_by(pro = cov2$provincia, fecha = cov2$fecha_confirmacion) %>% 
  summarise(n = n()) %>% mutate(acum = cumsum(n)) 
ggplot(cov4, aes(x = pro, y = acum, fill = pro)) +
  #  geom_col(aes(y =  acum, width = 0.9)) +#, alpha = 0.8, color = NA)
  geom_col(width = 0.8, position="identity", show.legend = FALSE) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  labs(title="Acumulados por provincia",
       x = "Acumulado",
       y = "Provincias") +
  coord_flip() +
  transition_reveal(as_date(fecha))

## Animaci�n con intercambio de barras por rank y acumulado

# Arreglar nombres en base 
#cov2 <- cov2 %>% mutate(provincia = recode(provincia, 
#                                           "Camagüey" = "Camaguey", 
#                                           "Ciego de �\u0081vila" = "Ciego de �vila", 
#                                           "Guantánamo" = "Guant�namo") ) 
#                                           "Hol*" = "Holgu�n", 
#                                           "Pinar del R�o" = "Pinar del R�o", 
#                                           "Sancti Sp�ritus" = "Sancti Sp�ritus"))

#cov2 <- cov2 %>%
#  mutate(provincia = str_replace(cov2$provincia, "Holgu�-n", "Holgu�n"))

# Crear secuencia de fechas
f <- as.Date("2020-03-11") # desde
t <- as.Date("2020-06-07") # Hasta
sec_fecha <- seq.Date(f,t, 1) # Crear secuencia
# Crear base con todas las provincias, con todas las fechas y 0 en cada d�a
prov_fecha <- tibble(provincia = c(rep("Artemisa", 89), rep("Camag�ey", 89),  rep("Ciego de �vila", 89), 
                                   rep("Cienfuegos", 89) , rep("Granma", 89),  rep("Guant�namo", 89), 
                                   rep("Holgu�n", 89) , rep("Isla de la Juventud", 89), rep("La Habana", 89) ,
                                   rep("Las Tunas", 89), rep("Matanzas", 89) , rep("Mayabeque", 89), 
                                   rep("Pinar del R�o", 89),  rep("Sancti Sp�ritus", 89),
                                   rep("Santiago de Cuba", 89), rep("Villa Clara", 89)),
                     fecha = c(rep(sec_fecha, 16)))
                     #n = rep(0, 1104))

cov4 <- cov2 %>% group_by(provincia, fecha = cov2$fecha_confirmacion) %>% 
  summarise(n = n())
cov4$fecha <- as.Date(cov4$fecha)

prov_fecha_n <- full_join(prov_fecha,cov4, by=c("provincia", "fecha"))
prov_fecha_n <- prov_fecha_n %>% mutate(n = ifelse(is.na(n), 0, n))

cov5 <- prov_fecha_n %>% 
  group_by(provincia) %>% 
  mutate(acum = cumsum(n)) 

otro <- cov5 %>%
  group_by(fecha) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = min_rank(-acum) * 1,
         Value_rel = acum/acum[rank==1],
         Value_lbl = paste0(" ",acum)) %>%
  # filter(rank <=10) %>%
  ungroup()

p <- ggplot(otro, aes(rank, group = provincia, 
                      fill = as.factor(provincia), color = as.factor(provincia))) +
  geom_tile(aes(y =acum/2,
                height = acum,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(provincia, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=acum,label = Value_lbl, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  labs(title='Acumulados de casos confirmados de Covid por provincias en Cuba',
       subtitle='Acumulados del d�a: {closest_state}', x = "", y = "Acumulado",
       caption = "Fuente: covid19cubadata.github.io") +
  theme(plot.title = element_text(hjust = 0, size = 22),
        axis.ticks.y = element_blank(),  # These relate to the axes post-flip
        axis.text.y  = element_blank(),  # These relate to the axes post-flip
        plot.margin = margin(1,1,1,4, "cm")) +
  transition_states(fecha, transition_length = 4, state_length = 1) +
  ease_aes('cubic-in-out')

animate(p, 200, fps = 25, duration = 20, 
        width = 800, height = 600, end_pause = 10)
animate(p, renderer = av_renderer() )



semConCasos %>% group_by(semanas) %>% summarise(casos = sum(n))
