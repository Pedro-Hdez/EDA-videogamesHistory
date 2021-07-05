# ********** Librerías necesarias **********
# Para la graficación
library(ggplot2) 
library(cowplot)

# Para la manipulación de los datos
library(dplyr)



# ********** Cargando los datos **********
# Cargamos los datos
data <- read.csv("vgsales.csv", header=T, sep=",")

# Mostramos las columnas que contiene el archivo
colnames(data)


# Existen varios títulos con años de estreno no especificados, los eliminamos.
data <- data[data$Year != "N/A", ]

# Mostramos un resumen de la base de datos
summary(data)


# ******** Los videojuegos más vendidos de la historia a nivel mundial ********
# Obtenemos un la sumatoria de las ventas a nivel global por cada
# título en cada año y almacenamos esta información en una lista con las
# columnas: Ventas, Nombre
ventas_globales <- aggregate(list(Ventas = data$Global_Sales),
                             list(Nombre = data$Name), sum)

# Ordenamos los titulos de acuerdo al número de ventas de forma decreciente
# y tomamos los primeros diez
ventas_globales <- ventas_globales[order(ventas_globales$Ventas,
                                         decreasing = T), ]
ventas_globales <- head(ventas_globales, n=10)

# Construimos una gráfica de barras para mostrar el resultado
a <- ggplot(data=ventas_globales, mapping = aes(x = Nombre, y = Ventas)) +
  geom_bar(stat = "identity", 
           mapping = aes(fill = Nombre, color = Nombre), 
           size = 2, alpha = .6) +
  geom_label(mapping = aes(label = Ventas), 
             size = 7, 
             fontface = "bold") +
  xlab("") +
  ylab("Ventas a nivel mundial (Millones)") +
  ggtitle("Los 10 videojuegos más vendidos de la historia (1980-2016)") + 
  theme(legend.position = "none",
        plot.title = element_text(size = 22, face = "bold", hjust = .5),
        axis.text.x = element_text(size = 12, face = "bold", angle = 20),
        axis.text.y = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 20))

png(file="fig1.png", width=1696, height=734)
plot(a)
dev.off()



## ********** Los videojuegos más vendidos de la historia por región **********

# Norte América
ventas_norteamerica <- aggregate(list(Ventas = data$NA_Sales), 
                                 list(Nombre = data$Name), sum)
ventas_norteamerica <- ventas_norteamerica[order(ventas_norteamerica$Ventas, 
                                                 decreasing = T), ]
ventas_norteamerica <- head(ventas_norteamerica, n=10)

# Europa
ventas_europa <- aggregate(list(Ventas = data$EU_Sales), 
                           list(Nombre = data$Name), sum)
ventas_europa <- ventas_europa[order(ventas_europa$Ventas, decreasing = T), ]
ventas_europa <- head(ventas_europa, n=10)

# Japón
ventas_japon <- aggregate(list(Ventas = data$JP_Sales), 
                          list(Nombre = data$Name), sum)
ventas_japon <- ventas_japon[order(ventas_japon$Ventas, decreasing = T), ]
ventas_japon <- head(ventas_japon, n=10)

norteamerica <- ggplot(data=ventas_norteamerica, 
                       mapping = aes(x = Nombre, y = Ventas)) +
  geom_bar(stat = "identity", 
           mapping = aes(fill = Nombre, color = Nombre), 
           size = 3, 
           alpha = 1) +
  geom_label(mapping = aes(label = Ventas), 
             size = 7, 
             fontface = "bold") +
  xlab("") +
  ylab("Ventas (Millones)") +
  ggtitle("Los 10 videojuegos más vendidos en Norteamérica (1980-2016)")+ 
  theme(
    plot.title = element_text(size = 22, face = "bold", hjust = .5),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(size = 20))

europa <- ggplot(data=ventas_europa, mapping = aes(x = Nombre, y = Ventas)) +
  geom_bar(stat = "identity", 
           mapping = aes(fill = Nombre, color = Nombre), 
           size = 3, 
           alpha = 1) +
  geom_label(mapping = aes(label = Ventas), 
             size = 7, 
             fontface = "bold") +
  xlab("") +
  ylab("Ventas (Millones)") +
  ggtitle("Los 10 videojuegos más vendidos en Europa (1980-2016)") + 
  theme(
    plot.title = element_text(size = 22, face = "bold", hjust = .5),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(size = 20))

japon <- ggplot(data=ventas_japon, mapping = aes(x = Nombre, y = Ventas)) +
  geom_bar(stat = "identity", 
           mapping = aes(fill = Nombre, color = Nombre), 
           size = 3, 
           alpha = 1) +
  geom_label(mapping = aes(label = Ventas), 
             size = 7, 
             fontface = "bold") +
  xlab("") +
  ylab("Ventas (Millones)") +
  ggtitle("Los 10 videojuegos más vendidos en Japón (1980-2016)") + 
  theme(
    plot.title = element_text(size = 22, face = "bold", hjust = .5),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(size = 20))

png(file="fig2.png", width=1800, height=3000)
plot_grid(norteamerica, europa, japon, nrow = 3, ncol = 1)
dev.off()

# Creamos vectores con los nombres y número de títulos vendidos de cada región
Region = c("Norteamérica", "Japón", "Europa", "Otros")
Ventas = c(sum(data$NA_Sales),sum(data$JP_Sales), sum(data$EU_Sales),
           sum(data$Other_Sales))

# Creamos un Dataframe
n_ventas = tibble(Region, Ventas)

# Calculamos el porcentaje de ventas para cada región
n_ventas$fraccion = n_ventas$Ventas / sum(n_ventas$Ventas)

# Calculamos el porcentaje acumulado para posicionar las etiquetas en 
# rectángulos
n_ventas$ymax <- cumsum(n_ventas$fraccion)

# Calculamos las coordenadas inferiores de cada rectángulo
n_ventas$ymin <- c(0, head(n_ventas$ymax, n=-1))

# Calculamos la posición de cada etiqueta
n_ventas$labelPosition <- (n_ventas$ymax + n_ventas$ymin) / 2

# Creamos una etiqueta para cada región
n_ventas$label <- paste0(n_ventas$Region, ": ", n_ventas$Ventas)

# Creamos la gráfica
png(file="fig3.png", width=800, height=800)
ggplot(n_ventas, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Region)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=7, color="black") +
  scale_fill_brewer(palette=7) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none") +
  xlab("") +
  ylab("Ventas (Millones)") +
  ggtitle("Millones de copias vendidas en cada región (1980-2016)")+
  theme(
    plot.title = element_text(size = 22, face = "bold", hjust = .5))
dev.off()



# ********** Los publishers con mayores ventas **********
# Ventas globales
publishers = aggregate(list(VentasGlobales = data$Global_Sales), 
                       list(Publisher = data$Publisher), sum)
publishers = publishers[order(publishers$VentasGlobales, decreasing = T), ]
publishers = head(publishers, n=20)

pub_global <- ggplot(data=publishers, mapping=aes(x=Publisher, 
                                                  y=VentasGlobales))+
  geom_bar(stat = "identity", 
           mapping = aes(fill = Publisher, color = Publisher), 
           size =1, 
           alpha = 1) +
  geom_label(mapping = aes(label = VentasGlobales), 
             size = 7, 
             fontface = "bold") +
  xlab("") +
  ylab("Ventas (Millones)") +
  ggtitle("Los 20 publishers con más ventas (1980-2016)") + 
  theme(legend.position = "none",
        plot.title = element_text(size = 25, face = "bold", hjust = .5),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 17, face = "bold"),
        axis.title.x = element_text(size = 20))+
  coord_flip()

png(file="fig4.png", width=1500, height=750)
plot(pub_global)
dev.off()


# ********** Ventas en los mercados principales **********
# Obtenemos las ventas en Norteamérica
ventasNA = aggregate(list(VentasNA = data$NA_Sales), 
                     list(Publisher = data$Publisher), sum)

# Adjuntamos las ventas de Norteamérica
publishers = merge(publishers, ventasNA)

# Obtenemos las ventas en Japón
ventasJP = aggregate(list(VentasJP = data$JP_Sales), 
                     list(Publisher = data$Publisher), sum)

# Adjuntamos las ventas de Japón
publishers = merge(publishers, ventasJP)

# Obtenemos las ventas en Europa
ventasEU = aggregate(list(VentasEU = data$EU_Sales), 
                     list(Publisher = data$Publisher), sum)

# Adjuntamos las ventas de Europa
publishers = merge(publishers, ventasEU)

publishers$VentasGlobales = NULL

colors <- c("Europa" = "blue", "Japón" = "orange", "Norteamérica" = "black")

png(file="fig5.png", width=1500, height=1000)
ggplot(data = publishers) +
  geom_bar(stat = "identity", 
           mapping = aes(x = Publisher, y = VentasEU, color="Europa"), 
           alpha = 0, size = 2) + 
  geom_bar(stat = "identity",
           mapping = aes(x = Publisher, y = VentasJP, color="Japón"), 
           alpha = 0, size = 2) + 
  geom_bar(stat = "identity", 
           mapping = aes(x = Publisher, y = VentasNA, color="Norteamérica"), 
           alpha = 0, size = 2) +
  theme( plot.title = element_text(size = 22, face = "bold", hjust = .5),
         axis.text.x = element_text(size = 12, face = "bold"),
         axis.text.y = element_text(size = 10, face = "bold"),
         axis.title.y = element_text(size = 17), 
         axis.title.x = element_text(size = 17),
         legend.position = "bottom",
         legend.title = element_text(color = "white"),
         legend.text = element_text(size = 16, face = "bold"),
  ) +
  labs(
    color = "Legend") +
  scale_color_manual(values = colors)+
  xlab("") +
  ylab("Ventas (millones)") +
  ggtitle("Ventas por región de los 20 mejores publishers (1980-2016)") + 
  theme(
    plot.title = element_text(size = 25, face = "bold", hjust = .5),
    axis.text.y = element_text(size = 17, face = "bold"),
    axis.title.x = element_text(size = 20))+
  coord_flip()
dev.off()

# ********** Los géneros con mayores ventas **********

# Global
g_Global <- aggregate(list(Ventas=data$Global_Sales), 
                      list(Genero= data$Genre), sum)
g_Global <- head(g_Global[order(g_Global$Ventas, decreasing = T), ], n=10)

# Norteamérica
g_NA <- aggregate(list(Ventas=data$NA_Sales), 
                  list(Genero= data$Genre), sum)
g_NA <- head(g_NA[order(g_NA$Ventas, decreasing = T), ], n=10)

# Japón
g_JP <- aggregate(list(Ventas=data$JP_Sales), list(Genero= data$Genre), sum)
g_JP <- head(g_JP[order(g_JP$Ventas, decreasing = T), ], n=10)

# Europa
g_EU <- aggregate(list(Ventas=data$EU_Sales), 
                  list(Genero= data$Genre), sum)
g_EU <- head(g_EU[order(g_EU$Ventas, decreasing = T), ], n=10)

# Otros
g_Otros <- aggregate(list(Ventas=data$Other_Sales), 
                     list(Genero= data$Genre), sum)
g_Otros <- head(g_Otros[order(g_Otros$Ventas, decreasing = T), ], n=10)

global <- ggplot(data = g_Global, mapping = aes(x = Genero, y = Ventas)) +
  geom_segment(aes(xend=Genero, yend=0, color = Genero), 
               size = 2.3, alpha = .8) +
  geom_point(mapping = aes(fill = Genero), size = 7, shape = 21) +
  geom_line(group = 1, size = 1.1, linetype = 10, color = "red") +
  xlab("") +
  ylab("") +
  ggtitle("Géneros con mayores ventas en el mundo (1980-2016)") +
  theme(plot.title = element_text(size = 25, face = "bold", hjust = .5),
        axis.title.x = element_text(size = 16, hjust = .5, 
                                    face = "italic"),
        axis.title.y = element_text(size = 16, hjust = .5, 
                                    face = "italic"),
        axis.text.x = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 15, face = "bold"),
        legend.position = "none")

norteamerica <- ggplot(data = g_NA, mapping = aes(x = Genero, y = Ventas)) +
  geom_segment(aes(xend=Genero, yend=0, color = Genero), size = 2.3, 
               alpha = .8) +
  geom_point(mapping = aes(fill = Genero), size = 7, shape = 21) +
  geom_line(group = 1, size = 1.1, linetype = 10, color = "red") +
  xlab("") +
  ylab("") +
  ggtitle("Géneros con mayores ventas en Norteamérica (1980-2016)") +
  theme(plot.title = element_text(size = 25, face = "bold", hjust = .5),
        axis.title.x = element_text(size = 16, hjust = .5, 
                                    face = "italic"),
        axis.title.y = element_text(size = 16, hjust = .5, 
                                    face = "italic"),
        axis.text.x = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 15, face = "bold"),
        legend.position = "none")

japon <- ggplot(data = g_JP, mapping = aes(x = Genero, y = Ventas)) +
  geom_segment(aes(xend=Genero, yend=0, color = Genero), size = 2.3, 
               alpha = .8) +
  geom_point(mapping = aes(fill = Genero), size = 7, shape = 21) +
  geom_line(group = 1, size = 1.1, linetype = 10, color = "red") +
  xlab("") +
  ylab("") +
  ggtitle("Géneros con mayores ventas en Japón (1980-2016)") +
  theme(plot.title = element_text(size = 25, face = "bold", hjust = .5),
        axis.title.x = element_text(size = 16, hjust = .5, 
                                    face = "italic"),
        axis.title.y = element_text(size = 16, hjust = .5, 
                                    face = "italic"),
        axis.text.x = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 15, face = "bold"),
        legend.position = "none")

europa <- ggplot(data = g_EU, mapping = aes(x = Genero, y = Ventas)) +
  geom_segment(aes(xend=Genero, yend=0, color = Genero), size = 2.3, 
               alpha = .8) +
  geom_point(mapping = aes(fill = Genero), size = 7, shape = 21) +
  geom_line(group = 1, size = 1.1, linetype = 10, color = "red") +
  xlab("") +
  ylab("") +
  ggtitle("Géneros con mayores ventas en Europa (1980-2016)") +
  theme(plot.title = element_text(size = 25, face = "bold", hjust = .5),
        axis.title.x = element_text(size = 16, hjust = .5, 
                                    face = "italic"),
        axis.title.y = element_text(size = 16, hjust = .5, 
                                    face = "italic"),
        axis.text.x = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 15, face = "bold"),
        legend.position = "none")

otros <- ggplot(data = g_Otros, mapping = aes(x = Genero, y = Ventas)) +
  geom_segment(aes(xend=Genero, yend=0, color = Genero), size = 2.3, 
               alpha = .8) +
  geom_point(mapping = aes(fill = Genero), size = 7, shape = 21) +
  geom_line(group = 1, size = 1.1, linetype = 10, color = "red") +
  xlab("") +
  ylab("") +
  ggtitle("Géneros con mayores ventas en el resto el mundo (1980-2016)")+
  theme(plot.title = element_text(size = 25, face = "bold", hjust = .5),
        axis.title.x = element_text(size = 16, hjust = .5, 
                                    face = "italic"),
        axis.title.y = element_text(size = 16, hjust = .5, 
                                    face = "italic"),
        axis.text.x = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 15, face = "bold"),
        legend.position = "none")

png(file="fig6.png", width=1500, height=1400)
plot_grid(global, norteamerica, japon, europa, otros, nrow = 5, ncol = 1)
dev.off()



# ********** No. de ventas de videojuegos a lo largo de la historia **********
df_global <- aggregate(list(Ventas=data$Global_Sales), 
                       list(Anio=data$Year), sum)
df_global <- df_global[order(df_global$Anio), ]

anios = 1980:2020
ventas <- ggplot(data = df_global, mapping = aes(x = Anio, y = Ventas)) +
  geom_line(size = 1, linetype = 2, color = "blue") +
  geom_label(mapping = aes(label=Ventas), fill = "dark blue", size = 2, 
             color = "white", fontface = "bold", alpha = .7) +
  ylab("Ventas (Millones)") +
  ggtitle("Número de ventas de videojuegos (1980-2020)") +
  theme(legend.position = "none",
        plot.title = element_text(size = 22, face = "bold", hjust = .5),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 15, face = "bold"),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20))+
  scale_x_continuous("Año", labels = as.character(anios), 
                     breaks = anios)

png(file="fig7.png", width=1800, height=1000)
plot_grid(ventas)
dev.off()
