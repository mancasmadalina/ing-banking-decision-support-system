#install.packages("readxl") 

library(readxl)
conturi <- read_excel("C:/Users/Admin/Desktop/SSD_Banca_Proiect.xlsx", sheet = "Conturi")
tranzactii <- read_excel("C:/Users/Admin/Desktop/SSD_Banca_Proiect.xlsx", sheet = "Tranzactii")
credite <- read_excel("C:/Users/Admin/Desktop/SSD_Banca_Proiect.xlsx", sheet = "Credite")

par(mfrow = c(2, 2))  

hist(conturi$sold,
     main = "Distribuția soldurilor",
     xlab = "Sold",
     ylab = "Frecvență",
     col= "lightyellow")

hist(tranzactii$suma,
     main = "Distribuția sumelor tranzacțiilor",
     xlab = "Sumă tranzacție",
     ylab = "Frecvență",
     col="lightblue")

hist(credite$suma_credit,
     main = "Distribuția sumelor creditelor",
     xlab = "Sumă credit",
     ylab = "Frecvență",
     col="mediumpurple")

hist(credite$rata_lunara,
     main = "Distribuția ratelor lunare",
     xlab = "Rată lunară",
     ylab = "Frecvență",
     col="aquamarine")

par(mfrow = c(2, 2))


boxplot(conturi$sold,
        main = "Solduri",
        ylab = "Sold",
        col = "lightyellow")

boxplot(tranzactii$suma,
        main = "Sume tranzacții",
        ylab = "Sumă tranzacție",
        col = "lightblue")

boxplot(credite$suma_credit,
        main = "Sume credite",
        ylab = "Sumă credit",
        col = "mediumpurple")


boxplot(credite$rata_lunara,
        main = "Rate lunare",
        ylab = "Rată lunară",
        col = "aquamarine")


# 2.2 Prognoza unor indicatori micro- sau macro-economici
# Calcul valorile actuale
sold_total <- sum(conturi$sold)
credit_total <- sum(credite$suma_credit)
rata_lunara_total <- sum(credite$rata_lunara)
sold_total
credit_total
rata_lunara_total
# Creare scenarii
scenarii <- data.frame(
  Indicator = c("Sold total", "Suma totală credite", "Rată lunară totală"),
  Actual = c(sold_total, credit_total, rata_lunara_total),
  Optimist = c(sold_total * 1.10, credit_total, rata_lunara_total),    # sold +10%, credite și rate neschimbate
  Pesimist = c(sold_total * 0.90, credit_total * 1.05, rata_lunara_total * 1.10)  # sold -10%, credite +5%, rate +10%
)

print(scenarii)

# Vizualizare grafică
library(ggplot2)
scenarii_long <- reshape2::melt(scenarii, id.vars = "Indicator", variable.name = "Scenariu", value.name = "Valoare")

ggplot(scenarii_long, aes(x = Indicator, y = Valoare, fill = Scenariu)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Prognoză indicatori financiari pe scenarii",
       y = "Valoare totală", x = "Indicator") +
  scale_fill_manual(values = c("Actual" = "lightblue1", "Optimist" = "green2", "Pesimist" = "red2")) +
  geom_text(aes(label = round(Valoare, 0)), position = position_dodge(width = 0.9), vjust = -0.5) +
  theme_minimal()

# 2.3 Rezolvarea unei probleme decizionale economice în cadrul respectivei companii

# Instalare și încărcare pachet TOPSIS
if(!require(topsis)) install.packages("topsis")
library(topsis)

# Crearea matricii decizionale: fiecare rând reprezintă un scenariu
matrice_decizie <- matrix(c(
  sold_total, credit_total, rata_lunara_total,    # Scenariu Actual
  sold_total * 1.10, credit_total, rata_lunara_total,  # Scenariu Optimist
  sold_total * 0.90, credit_total * 1.05, rata_lunara_total * 1.10 # Scenariu Pesimist
), nrow = 3, byrow = TRUE)

colnames(matrice_decizie) <- c("Sold total", "Suma totală credite", "Rată lunară totală")
rownames(matrice_decizie) <- c("Actual", "Optimist", "Pesimist")
matrice_decizie
# Ponderile criteriilor (alege după cât de important consideri fiecare indicator)
ponderi <- c(0.4, 0.3, 0.3)
ponderi
# Impactul: "+" dacă mai mult este mai bine, "-" dacă mai mult este mai rău
impact <- c("+", "-", "-")  # Sold mare = ok, Credite mari = negativ, Rată mare = negativ
impact
# Aplicarea metodei TOPSIS
rezultate_topsis <- topsis(matrice_decizie, ponderi, impact)
rezultate_topsis
# Crearea unui tabel final cu scorul și clasamentul
clasament <- data.frame(
  Scenariu = rownames(matrice_decizie),
  Scor_Performanta = rezultate_topsis$score,
  Rank = rank(-rezultate_topsis$score)  # cel mai mare scor = 1
)

print(clasament[order(clasament$Rank), ])
