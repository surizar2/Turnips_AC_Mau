
# Turnip prices: data cleaning --------------------------------------------

rm(list = ls())

library(tidyverse)
library(readr)
library(readxl)
library(hrbrthemes)
library(plotly)

# -------------------------------------------------------------------------

data <- read_excel("Turnips NH.xlsx")
data <- data[-1, ]

participantes <- vector()
NombresCol <- colnames(data) 
NumParticipantes <- ((length(NombresCol)-1)/2)
for (i in 1:((length(NombresCol)-1)/2)) {
  participantes <- c(participantes,NombresCol[2*i])
  
}


NombresFinal <- paste(
  rep(participantes, each = 2),
  c("am","pm"),
  sep = "_"
)

NombresFinal <-  c("Fecha", NombresFinal)

colnames(data) <-  NombresFinal

Fechas <- as.POSIXct(
  paste(data$Fecha[1], "09:00"),
  format = "%d/%m/%Y %H:%M")
for(i in 1:length(data$Fecha))
{
  FechaTemp <- as.POSIXct(
    paste(data$Fecha[i], "09:00"),
    format = "%d/%m/%Y %H:%M"
  )
  FechaTemp2 <- FechaTemp + 12*3600
  Fechas <- c(Fechas,FechaTemp,FechaTemp2)
}
Fechas <- Fechas[-1]


data2 <- tibble(
  Fecha = Fechas
)

for (i in 1:length(participantes)) {
  data2[1:(length(Fechas)/2), participantes[i]] <- data[, 2*i]
  data2[(length(Fechas)/2 + 1):length(Fechas), participantes[i]] <- data[, 2*i + 1]
}


for (i in 1:length(participantes)) {
  data2[[participantes[i]]] <-  as.numeric(data2[[participantes[i]]])
}

write.csv(data2, "turnip.csv")


# -------------------------------------------------------------------------

data3 <- pivot_longer(
  data2,
  participantes, 
  names_to = "participantes",
  values_to = "precio"
)

data3 <- drop_na(data3)

data3 %>% ggplot() + 
  geom_histogram(aes(precio, color = participantes, fill = participantes ))


data3 %>% ggplot() +
  geom_histogram(aes(precio))
p <- data3 %>% ggplot() +
  geom_histogram(aes(precio))

plotly::ggplotly(p)


# Bootstrap ---------------------------------------------------------------
nSample <- 100000

median <- vector(mode = "numeric", length = nSample + 1)
median[1] <- median(data3$precio)

for (i in 2:length(median)) {
  median[i] <- median(sample(data3$precio, length(data3$precio), replace = T)) 
}


ggplot() + geom_histogram(aes(median))
p1 <- ggplot() + geom_histogram(aes(median),color="red",fill="blue")
plotly::ggplotly(p1)

