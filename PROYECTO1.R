# leyendo cvs del proyecto
bp <- read.csv(file="/home/jonathan/disco_d/proyectos/proyecto_final_bs/Anexo/bloodPressure.csv", header=TRUE, sep=",")
gl <- read.csv(file="/home/jonathan/disco_d/proyectos/proyecto_final_bs/Anexo/Glucose.csv", header=TRUE, sep=",")
ox <- read.csv(file="/home/jonathan/disco_d/proyectos/proyecto_final_bs/Anexo/Oximetry.csv", header=TRUE, sep=",")
wh <- read.csv(file="/home/jonathan/disco_d/proyectos/proyecto_final_bs/Anexo/Weight_Height.csv", header=TRUE, sep=",")

#revisando la informacion

head(bp)

str(bp)
#revisando la informacion
summary(bp)

"
AL REVISAR ME DOY CUENTA QUE HAY VARIABLES QUE TIENEN VALOR 0, SE SUPONE QUE LOS VALORES DEBEN SER MAYORES A 0

Patient          Systolic       Diastolic      AvBloodPressure   HeartRate               Date      
Min.   :   1.0   Min.   :  0.0   Min.   :  0.00   Min.   :  0.0   Min.   :  0   2/1/2015 20:55:    5  
1st Qu.: 420.0   1st Qu.: 99.0   1st Qu.: 63.00   1st Qu.: 92.0   1st Qu.: 67   2/1/2015 9:55 :    5  
Median : 730.0   Median :113.0   Median : 74.00   Median :108.0   Median : 80   2/5/2015 14:58:    5  
Mean   : 720.2   Mean   :112.8   Mean   : 73.38   Mean   :110.5   Mean   : 80   2/9/2015 10:49:    5  
3rd Qu.:1073.0   3rd Qu.:127.0   3rd Qu.: 84.00   3rd Qu.:129.0   3rd Qu.: 93   2/1/2015 12:45:    4  
Max.   :1453.0   Max.   :187.0   Max.   :118.00   Max.   :149.0   Max.   :183   2/1/2015 19:25:    4  
(Other)       :10139  
"

#revisando el tipo de variable de los datos
class(bp$Patient)
class(bp$Systolic)
class(bp$Diastolic)
class(bp$AvBloodPressure)
class(bp$HeartRate)
class(bp$Date)

#convirtiendo la variable Date a tipo data class

#bp$Date <- as.Date(bp$Date, format = "%d/%m/%y")

#contar los valores faltantes
sapply(bp, FUN = function(X) sum(is.na(X)))

"No hay valores faltantes

Patient        Systolic       Diastolic AvBloodPressure       HeartRate            Date 
              0               0               0               0               0               0 
"

#graficando los datos:

install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)

"#REVISAR SI HAY OUTLIERS"

boxplot(bp$Systolic, horizontal= TRUE)
"se encontraton outliers, se van a eliminar..."
boxplot.stats(bp$Systolic) 
"outliers para (bp$Systolic)
$out
[1] 187 178   0 173"
"se usa subset para eliminar outliers bp$Systolic"
bp2 <- subset(bp, bp$Systolic < 170 & bp$Systolic > 0)
boxplot(bp2$Systolic, horizontal= TRUE)
View(bp2)

"#se repite el proceso para las otras variables"


"#Diastolic"
boxplot(bp2$Diastolic, horizontal= TRUE)
"se encontraton outliers, se van a eliminar..."
boxplot.stats(bp2$Diastolic) 
"se usa subset para eliminar outliers bp$Diastolic"
bp3 <- subset(bp2, bp2$Diastolic < 115)
boxplot(bp3$Diastolic, horizontal= TRUE)

"#AvBloodPressure"
"#se repite el proceso para las otras variables"
boxplot(bp3$AvBloodPressure, horizontal= TRUE)
"se encontraton outliers, se van a eliminar..."
boxplot.stats(bp3$AvBloodPressure) 
"no se encontraton outliers para (bp$AvBloodPressure)"


"HeartRate"
boxplot(bp3$HeartRate, horizontal= TRUE)
"se encontraton outliers, se van a eliminar..."
boxplot.stats(bp3$HeartRate) 
"se usa subset para eliminar outliers bp$Diastolic"
bp4 <- subset(bp3, bp3$HeartRate < 140)
boxplot(bp4$HeartRate, horizontal= TRUE)


boxplot(bp4$Systolic, bp4$Diastolic, bp4$AvBloodPressure, bp4$HeartRate)
View(bp4)

summary(bp4)


#LIMPIEZA DE DATOS  DE LOS OTROS CSV  Glucose.
View(gl)
head(gl)
tail(gl)
str(gl)

sapply(gl, FUN = function(X) sum(is.na(X)))

boxplot(gl$Glucose, horizontal = TRUE)

boxplot.stats(gl$Glucose)
"se encuentra outliers"
"$out
  [1] 200 201 200 201 200 200 201 200 200 200 201 201 201 201 201 200 201 200 201 201 200 201
 [23] 200 201 200 200 200 200 200 200 201 200 201 200 200 200 201 201 200 200 200 200 201 201
 [45] 201 200 200 200 200 200 201 201 201 201 201 201 201 201 201 200 201 201 200 201 200 201
 [67] 201 201 200 201 201 200 200 200 200 200 200 201 200 200 200 200 200 200 200 201 201 200
 [89] 201 201 201 200 201 201 200 200 200 201 201 201 200 200 200 200 201 200 201 200 200 200
[111] 201 201 201 200 200 201 200 201 201 200 201 201 200 200 200 200 201 201 200 201 200 201
[133] 201 207 200 200 201 200 201 201 200 200 200 201 201 200 201 200 201 200 200 201 201 200
[155] 201 200 200 200 200 201"

gl2 <- subset(gl, gl$Glucose < 195)
boxplot(gl2$Glucose, horizontal= TRUE)
View(gl2)
summary(gl2)


"#OXIGEMETRY"
View(ox)
head(ox)
tail(ox)
str(ox)
summary(ox)
sapply(ox, FUN = function(X) sum(is.na(X)))

boxplot(ox$SpO2, horizontal = TRUE)
boxplot(ox$HeartRate, horizontal = TRUE)


boxplot.stats(ox$HeartRate)
"se encuentra outliers"

"$out
 [1] 172 175 151 137 190 190 190 190 154 145 139 187 146 204 139 155 164 217 144 167 155 143"

ox2 <- subset(ox, ox$HeartRate < 130)#se eliminan los que no cumplen con la condicion
boxplot(ox2$HeartRate, horizontal= TRUE)
View(ox2)
summary(ox2)
#sd(ox2$SpO2)
#hist(ox2$HeartRate)
#hist(ox2$HeartRate,main="HearRate",col="#009966",border="#007744",xlab="HeartRate",ylab="Nro de frecuencias")


#weight and height
View(wh)
head(wh)
tail(wh)
str(wh)
summary(wh)
sapply(wh, FUN = function(X) sum(is.na(X)))

boxplot(wh$Weight, horizontal = TRUE)
boxplot.stats(wh$Weight)
"se encuentra outliers"

wh2 <- subset(wh, wh$Weight < 150 & wh$Weight > 16)
boxplot(wh2$Weight, horizontal= TRUE)
summary(wh2)


boxplot(wh2$Height, horizontal = TRUE)
boxplot.stats(wh2$Height)

boxplot(wh2$IMC, horizontal = TRUE)
boxplot.stats(wh2$IMC)

wh3 <- subset(wh2, wh2$IMC < 45)
boxplot(wh3$IMC, horizontal = TRUE)
View(wh3)

plot(wh3$Height ~ wh3$Weight)
plot(wh3$Weight ~ wh3$Height)

abline(lm(wh3$Weight ~ wh3$Height))
pairs(~wh3$Weight + wh3$Height + wh3$IMC) #interesante

install.packages("psych")
library(psych)
pairs.panels(wh3[c(2,3,4)])


#UNIENDO LAS BASES DE DATOS EN UNA SOLA
#bp4,gl2,ox2,wh3

install.packages("dplyr")
install.packages("Rcpp")
install.packages("pkgconfig")
library(dplyr)
library(Rcpp)
library(pkgconfig)

#SE NOTAN QUE LOS DATOS TIENEN EL ID Y LA FECHA EN COMUN ASI QUE SE VA A TRATAR
#DE UNIR LAS BD EN UNA SOLA USANDO ESTAS DOS VARIABLES

#SE DECIDE QUITAR LA HORA DE LAS BD

install.packages(lubridate)
library(lubridate)

#SE CREA UNA FUNCION PARA PROCESAR LAS FECHAS DE CADA BASE DE DATOS
format_date <- function(time){
  time1 <- trimws(x = time) #quitar espacios en la variable
  time2 <- as.character(time1) #se convierte character, antes era factor
  sec <- ':00' #no es necesario pero yo agregue los segundos.
  time3 <- paste(time2, sec, sep = "") #y pegue las dos cadenas
  time4 <- parse_date_time(time3, orders = "mdY HMS") #convierto a un formato legible por R
  time5 <- format(time4, "%Y-%m-%d")
}

"bp4
gl2
ox2
wh3"

#guardando valores
bp.d <- bp4
gl.d <- gl2
ox.d <- ox2
wh.d <- wh3

bp.d$Date <- format_date(bp4$Date) #proceso para bp
gl.d$Date <- format_date(gl2$Date) #proceso para gl
ox.d$Date <- format_date(ox2$Date) #proceso para ox
wh.d$Date <- format_date(wh3$Date) #proceso para wh

names(bp.d)

#AGREGAR COLUMNAS paciente + date

bp.f <- mutate(bp.d, id = paste(bp.d$Patient,bp.d$Date , sep=""))
gl.f <- mutate(gl.d, id = paste(gl.d$Patient,gl.d$Date , sep=""))
ox.f <- mutate(ox.d, id = paste(ox.d$Patient,ox.d$Date , sep=""))
wh.f <- mutate(wh.d, id = paste(wh.d$Patient,wh.d$Date , sep=""))

names(ox.f)[3] = "HeartRate2"
names(ox.f)

#write.csv(bp.f, "E:/proyectos/proyecto_final_bs/Anexo/bp.csv", row.names = FALSE)
#write.csv(gl.f, "E:/proyectos/proyecto_final_bs/Anexo/gl.csv", row.names = FALSE)
#write.csv(ox.f, "E:/proyectos/proyecto_final_bs/Anexo/ox.csv", row.names = FALSE)
#write.csv(wh.f, "E:/proyectos/proyecto_final_bs/Anexo/wh.csv", row.names = FALSE)

#PRUEBAS MERGE
# sort by mpg
bp.f1  <- bp.f[order(bp.f$Patient),]
head(bp.f1)
tail(bp.f1)
bp.f2 <- bp.f1[0:50,]
View(bp.f2)

gl.f1  <- gl.f[order(gl.f$Patient),]
gl.f2 <- gl.f1[0:50,]
View(gl.f2)

bp.gl <- merge(bp.f2, gl.f2, by="id")
bp.gl2 <- merge(gl.f2, bp.f2, by="id")
View(bp.gl2)


dim(bp.gl %>%filter(Patient.x == 8))
dim(bp.gl2 %>%filter(Patient.x == 8))
dim(bp.f2 %>%filter(Patient == 8))
dim(gl.f2 %>%filter(Patient == 8))


length(bp.f2$Patient['8',])

#END PRUEBAS MERGE



biometric <- merge(bp.f, gl.f, by = "id")


biometric2 <- Reduce(function(x, y) merge(x, y, by="id"), list(bp.f, gl.f,ox.f, wh.f ))
summary(biometric)

View(filter(biometric, Patient.x =="8"))

View(biometric2)
View(biometric)
#cor(select(bp4, Systolic, Diastolic, AvBloodPressure, HeartRate))

#filter(bp4, Systolic==89)


str(bp4)


if(x > 0){
  print("Non-negative number")
} else {
  print("Negative number")
}
