# OLA_01_4 
# OPGAVE 4.1 - 4.4

#kør packmann før du går videre
pacman::p_load(pacman, dplyr, tidyverse, ggplot2, tidyr)
install.packages("pacman")
# 4.1 Funktion med med påvirkning af udfald. Der er 4 gange større chance for at slå en 6´er.
# Sandsynligheden er 40/100 for en 6´er imod 10/100 for tal 1:5,

#Terning med 6 sider og påvirkning af udfald
roll <- function() {
  die <- 1:6
  dice <- sample(
    die,
    size = 1,
    replace = TRUE,
    prob = c(40 / 60, 10 / 60, 10 / 60, 10 / 60, 10 / 60, 40 / 60)
  )
  sum(dice)
}

#kør funktion
roll()

#funktionen roll replikeres 10.000 gange
rolls <- replicate(10000, roll())

#viser et plot
qplot(rolls, binwidth = 1)

#viser forselingen i en tabel
fordeling <- table(rolls)
fordeling

#terninger
roll_sum <- function() {
  die <- 1:6
  dice <- sample(die, size = 2, replace = TRUE)
  sum(dice)
}

roll_sum()

#antal kast
rolls <- replicate(1000000, roll_sum())
qplot(rolls, binwidth = 1)

#Opgave 4.2
#
#
roll_4_2 <- function(){
  die <- 1:6
  dice <- sample(die,size = 2, replace = TRUE,
                 prob = c(4/12,1/12,1/12,1/12,1/12,4/12))
  sum(dice)
}
vektor_4_2 <- replicate(10000,roll_4_2())
qplot(vektor_4_2,binwidth =1)

#Opgave 4.3
#Sandsynligheden for at slå 2,4 og 5 sættes til 0. Derfor vil terningen vise kun slå 1,3 eller 6.

#Terning som slår 1,3 eller 6 
roll_4_3 <- function() {
  die <- 1:6
  dice <- sample(
    die,
    size = 2,
    replace = TRUE,
    prob = c(1, 0, 1, 0, 0, 1)
  )
  sum(dice)
}
#kør
roll_4_3()
#repliker
vektor_4_3 <- replicate(10000, roll_4_3())
#viser plot
qplot(vektor_4_3, binwidth = 1)

#Opgave 4.4
#Hvad bliver summen af de to vektorer (integers) fra opgave 4.2 og 4.3 ganget med hinanden?
#Se nedenstående for eksempel på fremgangsmåde:

#vektor 4.2 ganget og vektor 4.3 samles i en matrix, en kolonne tilføjes og der ganges ud
sum_af_vektorer <- data.frame(
  "Opg. 4.2" = c(vektor_4_2),
  "Opg. 4.3" = c(vektor_4_3),
  "Sum: 4.2*4.3" = c(vektor_4_3 * vektor_4_2)
)
#summen af kolonner
colSums(sum_af_vektorer)

fordeling_4_2 <- table(vektor_4_2)
fordeling_4_2

fordeling_4_3 <- table(vektor_4_3)
fordeling_4_3

sum_af_vektorer_1 <- data.frame(
  "Opg. 4.2" = c(fordeling_4_2),
  "Opg. 4.3" = c(fordeling_4_3),
)
