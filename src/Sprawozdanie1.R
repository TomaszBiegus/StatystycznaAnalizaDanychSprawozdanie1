library(magrittr)
library(data.table)
library(ggplot2)
library(dplyr)


HOMA_DF <- read.csv('../HOMA.csv', sep = ';', dec = ',')
HOMA_DT <- data.table(HOMA_DF)



# 1. Przegląd danych - "sanity checks":
HOMA_DT[WIEK < 20 | WIEK > 40]
# Ujemny wiek, prawdopodobnie minus znalazł się tam przez przypadek gdyż wartość bezwzględna mieści się w przedziale [20, 40] lat.
# Zatem obsługa tego przypadku polega na zmianie znaku na dodatni z -34 na 34 lata.
HOMA_DT[WIEK < 0, WIEK:=-WIEK]
HOMA_DT[FAT.ALL.P > 100 | FAT.A.P > 100 | FAT.G.P > 100 | FAT.ALL.P < 0 | FAT.A.P < 0 | FAT.G.P < 0]
# Wszystkie wartości procentowe tłuszczu zawierają się w poprawnym przedziale [0,100].
length(HOMA_DT[BMI < 18 | BMI > 25]$BMI)
setdiff(HOMA_DT, HOMA_DT[!(BMI < 18 | BMI > 25)])
# 11 pacjentów ma BMI spoza przedziału [18, 25] a tylko tacy powinni brać udział w badaniu. W związku z tym wiersze te zostały usunięte i nie są brane pod uwagę w toku dalszej analizy.
HOMA_DT <- HOMA_DT[BMI >= 18 & BMI <= 25]

HOMA_DT[WHR<0]
HOMA_DT[TROJGLICERYDY < 0]
HOMA_DT[LDL.HDL < 0]
# Wartości WHR, stężenia trójglicerydów i stosunku LDL/HDL wszystkie są nieujemne tak jak być musi w rzeczywistości.

sapply(HOMA_DT, class)


?complete.cases
num_complete_cases <- sum(complete.cases(HOMA_DT))
num_rows <- nrow(HOMA_DT)
num_incomplete_cases <- num_rows - num_complete_cases
num_incomplete_cases
# Po wcześniejszych operacjach pozostało 265 obserwacji, z czego 259 kompletnych wierszy. Liczba 6 wierszy z brakami stanowi 6/259 = 2.3% całego zbioru danych. Przy takiej ilości możemy po prostu odrzucić wiersze z brakującymi danymi zamiast próbować je uzupełniać. Tak też robimy.

complete.cases(HOMA_DT)
HOMA_DT <- HOMA_DT[complete.cases(HOMA_DT)]


# 2.
summary(HOMA_DT)
colnames(HOMA_DT)
boxplot(HOMA_DT$WIEK)
boxplot(HOMA_DT$BMI)
boxplot(HOMA_DT$WHR)
boxplot(HOMA_DT$TROJGLICERYDY)
boxplot(HOMA_DT$HOMA)
boxplot(HOMA_DT$LDL.HDL)
boxplot(HOMA_DT$FAT.ALL.P)
boxplot(HOMA_DT$FAT.A.P)
boxplot(HOMA_DT$FAT.G.P)


# 3.
set.seed(100)
TEST_SAMPLE <- sample_n(HOMA_DT, 10)
is.data.table(HOMA_DT)
is.data.table(TEST_SAMPLE)
HOMA_DT <- setdiff(HOMA_DT, TEST_SAMPLE)


# 4.
colnames(HOMA_DT)

fit <- lm(HOMA ~ WIEK + BMI + WHR + TROJGLICERYDY + LDL.HDL + FAT.ALL.P + FAT.A.P + FAT.G.P, data = HOMA_DT)
summary(fit)

coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table
vcov(fit) # covariance matrix for model parameters
influence(fit) # regression diagnostics 

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit)


