# OLA_01_2 
# OPGAVE 2.1 - 2.4
install.packages("pacman")
pacman :: p_load(pacman,rio,tidyverse)

# Opgave 2.1

#Rens af data library(readr)
EDC_renset_data <- read_csv2("~/Desktop/OLA1/EDC_renset_data.csv") #husk at tilpasse din sti til filen
View(EDC_renset_data)
#De ovenstående koder er til at importere dataen fra CSV-fil til R. 

Ny=EDC_renset_data[-c(6), -c(11, 15, 19, 20, 21, 22, 23, 24)]
# I linje 6 er alle kolonner med udelukkende NA-værdier fjernet. 
# Dette datasæt er kaldt "Ny"
Ny1=na.omit(Ny)
# I linje 8 er alle NA-værdier fjernet, dette datasæt er kaldt "Ny1"

# Viser beskrivende statistik på datasæt 
# (På alle variabler i det rensede datasæt)
Ny1 %>% summary()


# Opgave 2.2

cor(Ny1$Boligareal, Ny1$pris)
# Denne kode er få at komme frem til korrelationen mellem kvadratmeter og pris. 

plot(Ny1$Boligareal, Ny1$pris, 
     main = "Scatterplot", 
     xlab = "Kvadratmeterpris", ylab = "Boligens pris", 
     col="dark green" ) 
plot(Ny1$pris, Ny1$pris, 
     main = "Scatterplot", 
     xlab = "Boligens pris", ylab = "Boligens pris", 
     col="dark blue" )
plot(Ny1$Pris_m_, Ny1$Liggetid, 
     main = "Scatterplot", 
     xlab = "Pris pr. kvadratmeter", ylab = "Liggetid", 
     col="dark red" )
# Linje 45-57 er scatterplots brugt i opgave 2.2, for at illustrere hhv. positiv korrelation,
# svag positiv korrelation og negativ korrelation. Der er både givet navn og farve

# Opgave 2.3

model.linreg1 <- lm( Ny1$Pris_m_ ~ Ny1$Grundareal, data = Ny1 )
summary(model.linreg1)
model.linreg2 <- lm( Ny1$Pris_m_ ~ Ny1$Vgtet_areal, data = Ny1 )
summary(model.linreg2)
model.linreg3 <- lm( Ny1$Pris_m_ ~ Ny1$Liggetid, data = Ny1 )
summary(model.linreg3)
model.linreg4 <- lm( Ny1$Pris_m_ ~ Ny1$Boligareal, data = Ny1 )
summary(model.linreg4)
model.linreg5 <- lm( Ny1$Pris_m_ ~ Ny1$Ejerudgifter_pr__md_, data = Ny1 )
summary(model.linreg5)
# I linje 1-10 er der lavet simple lineære regressioner på de 5 variable der er valgt, 
# til opgave 2.3. Der ses i koden, hvilken variabel der passer til den enkelte ligning
# Efter hver lm-funktion, er der benyttet summary for at se hvilke værdier,
# der er kommet ud af den lineære regression. 
# Hver enkelt ligning er kaldt model.linreg 1-5. 

Matr=Ny1[, -c(1, 2, 3, 4, 8, 9, 10, 14, 15, 16, 17)]
# Linje 17 er der lavet et nyt datasæt, hvor alle uønskede variable er fjernet (udover de 5 vi bruger)
# Det nye datasæt der skal benyttes til korrelationsmatrix er kaldt "Matr"

Korrelationsmatrix=cor(Matr)
#For at lave en korrelationsmatrix sættes koden op som i linje 21. Her er hele datasættet indsat
# Hvis man kun skulle lave korrelation mellem to variable, ses eksempel på dette i linje 24.
cor(Ny1$Boligareal, Ny1$pris)

plot(Matr$Pris_m_, Matr$Boligareal)
plot(Matr$Pris_m_, Matr$Grundareal)
plot(Matr$Pris_m_, Matr$Vgtet_areal)
plot(Matr$Pris_m_, Matr$Boligareal)
plot(Matr$Pris_m_, Matr$Ejerudgifter_pr__md_)
# I linje 26-30 er der lavet simplificerede plots, for at se hvor den mest rette linje er i et plot.
# Disse er der screendumps af i docs OLA 1.

Matrny=Matr[,-c(5)]
d=cor(Matrny)
# Linje 35-35 er der lavet en ny dataframe, hvor pris pr. m2 er fjernet. 
# Dette fordi variablerne i opgave 2.3 skal sættes overfor hinanden og se om der er korrelation. 
# Den nye dataframe er kaldt "Matrny" og den nye korrelationsmatrix er kaldt "d"

confint(model.linreg6)
#Denne funktion viser signifikansniveau og konfidensinterval i konsollen. 