# OLA_01 
# OPGAVE 3.1 - 3.4

# Kør packman inden du går videre
pacman :: p_load(pacman, dplyr,tidyverse, ggplot2)

# Opgave 3,1
# Lav et script (en funktion) i R-studio, der kan slå med 25.000 terninger.Hvor mange 5’ere har jeres terning slået? 
# Og hvad er sandsynligheden,givet jeres resultatet med de 25.000 terninger, for, at jeres script slår en 5’er?

# Her rulles med 1 terning med 6 sider
roll_sum <- function() {
  die <- 1:6
  dice <- sample(die, size = 1, replace = TRUE)
  sum(dice)
}

# Her replikeres roll_sum 25.000 gange
rolls <- replicate(25000, roll_sum())
qplot(rolls, binwidth = 1)

# En tabel over slag fra 1 til 6
fordeling <- table(rolls)
fordeling

# Terninger
roll_sum <- function() {
  die <- 1:6
  dice <- sample(die, size = 6, replace = TRUE)
  sum(dice)
}

# 3.3 Brug jeres script fra 3.2 og slå nu 1.000.000 gange med de terninger. Lav igen et barplot og sammenlign med jeres plot fra 3.2. 
# 10.000 kast
rolls <- replicate(10000, roll_sum())
qplot(rolls, binwidth = 1)

fordeling <- table(rolls)
fordeling


sample(rolls(1), 10)

# 3.4 Lav et script i R-studio, der viser en tilfældigt opstillet række af tallene 1, 2, 3, 5, 6.
# Lav en matrix med to kolonner og fem rækker, hvor den første kolonne skal være tallene 2 til 6
# og den anden kolonne skal være jeres tilfældige række af tallene 1, 2, 3, 5, 6.
# (hint: Google funktionen cbind(), der sætter lige lange kolonner sammen i en matrix).

# "tal" Giver en tilfældig rækkefølge af tallene 1,2,3,5,6
tal <- sample(x = c(1:3, 5:6), size = 5)

# En række med tal fra 2 til 6.
dataframe1 <- data.frame(x1 = c(2:6))

# En række med tal fra "tal"
dataframe2 <- data.frame(z1 = c(tal))

# Kombiner de to dataframes
datakombi <- cbind(dataframe1, dataframe2)



                      
