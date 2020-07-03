rm(list = ls())
library(seasonal)
path <- "C:/Users/jesus.lopezp/Desktop/Victor_Guerrero/"
igae <- read.csv(paste(path, "IGAE-1993-2020.csv", sep = ""), row.names = 1)
bym <- read.csv(paste(path, "ByM-2001-2020.csv", sep = ""), row.names = 1)
viajeros <- read.csv(paste(path, "Viajeros-1980-2020.csv", sep = ""), 
                     row.names = 1)
date_start <- "2001/01"
date_ends <- "2018/12"
dates <- row.names(igae)[which(row.names(igae) == date_start):which(row.names(igae) == date_ends)]

igae <- 
  igae[which(row.names(igae) == date_start):which(row.names(igae) == date_ends),]
igae <- ts(igae, frequency = 12, 
           start = as.numeric(c(substr(date_start,1,4),
                                substr(date_start,6,7))))
bym <- 
  bym[which(row.names(bym) == date_start):which(row.names(bym) == date_ends),]

bym <- ts(bym, frequency = 12, 
           start = as.numeric(c(substr(date_start,1,4),
                                substr(date_start,6,7))))
viajeros <- 
  viajeros[which(row.names(viajeros) == date_start):which(row.names(viajeros) == date_ends),]
dates_viajeros <- row.names(viajeros)
viajeros <- ts(viajeros, frequency = 12, 
          start = as.numeric(c(substr(date_start,1,4),
                               substr(date_start,6,7))))

# Modelo para IGAE
# setup model # as of 25/11/19
# TOTAL
arima_mod <- "(0 1 [1 4 7])(0 1 1)"
tl <- "log"
td <- "tdnolpyear"
lp <- "lpyear"
ss <- "Easter[4]"
#ao <- c("AO1995.Oct")
ls <- c("LS1995.Feb", "LS2009.Jan")
ls <- "LS2009.Jan"
FE <- "s3x3x5x3x5x3x5x5x3x5x3x3"

#run model 
modelo_inegi <- 
  seas(igae, transform.function = tl,
       regression.variables = c(td, lp, ss, ls), 
       arima.model = arima_mod,
       x11.seasonalma = FE,
       seats.noadmiss = "no", x11 = "", outlier = NULL,
       regression.aictest = NULL)

igae_sa <- series(modelo_inegi,"d11")
igae_trend <- series(modelo_inegi,"d12")
igae_irr <- series(modelo_inegi,"d13")
igae_seas <- series(modelo_inegi,"d10")

par(mfrow = c(2,2))
ts.plot(cbind(igae, igae_sa), col = c(4,2), 
        main = "IGAE")
ts.plot(igae_trend, main = "Componente Tendencia-Ciclo")
ts.plot(igae_seas, main = "Componente Estacional")
ts.plot(igae_irr, main = "Componente Irregular")

# export
igae_df <- data.frame(Period = dates, IGAE = igae, IGAE_SA = igae_sa,
                 Tendencia = igae_trend, FE = igae_seas,
                 Irregular = igae_irr)

write.csv(igae_df, paste(path, "igae_ae.csv", sep = ""), 
          row.names = FALSE)

# Modelo para Billetes y Monedas
lambda = 0.25
bym_model <- seas(bym, transform.function = "none",
                  transform.power = lambda)
bym_sa <- series(bym_model,"s11")
bym_trend <- series(bym_model, "s12")
bym_irr <- series(bym_model, "s13")
bym_seas <- series(bym_model, "s10")


par(mfrow = c(2,2))
ts.plot(cbind(bym, bym_sa), col = c(4,2), 
        main = "Billetes y Monedas en circulación")
ts.plot(bym_trend, main = "Componente Tendencia-Ciclo")
ts.plot(bym_seas, main = "Componente Estacional")
ts.plot(bym_irr, main = "Componente Irregular")

# export
bym_df <- data.frame(Period = dates, ByM = bym, ByM_SA = bym_sa,
                      Tendencia = bym_trend, FE = bym_seas,
                      Irregular = bym_irr)

write.csv(bym_df, paste(path, "bym_ae.csv", sep = ""), 
          row.names = FALSE)

