library(dplyr)
library(moments)
library(lmtest)
library(np)


#Import danych z pliku
Zawarte_malzenstwa_2017 <- read.csv("Zawarte_malzenstwa_2017.csv", header=TRUE, sep=";")
Pracujacy_na_1000_ludnosci_2017 <- read.csv("Pracujacy_na_1000_ludnosci_2017.csv", header=TRUE, sep=";")
Gestosc_zaludnienia_na_1_km_kwadratowy_2017 <- read.csv("Gestosc_zaludnienia_na_1_km_kwadratowy_2017.csv", header=TRUE, sep=";")

#Wybranie danych dotyczcych powaitów
Y_data <- filter(Y_Urodzenia_zywe_2018, grepl('^Powiat', Nazwa))
X1_data <- filter(Zawarte_malzenstwa_2017, grepl('^Powiat', Nazwa))
X2_data <- filter(Pracujacy_na_1000_ludnosci_2017, grepl('^Powiat', Nazwa))
X3_data <- filter(Gestosc_zaludnienia_na_1_km_kwadratowy_2017, grepl('^Powiat', Nazwa))

#Zebranie danych w jednej tabeli
dane <- data.frame(Nazwa=Y_data$Nazwa, urodzenia_zywe_na_1000=Y_data$urodzenia.Å¼ywe.na.1000.ludnoÅ.ci.2018...., Zawarte_malzenstwa=X1_data$ogÃ³Å.em.2017...., Pracujacy_na_1000=X2_data$ogÃ³Å.em.2017..osoba., Gestosc_zaludnienia=X3_data$ludnoÅ.Ä..na.1.km2.2017..osoba.)


# ----- Czesc 1 - opis zmiennych ------------------------- 

#Usuwanie czesci outlierow

#W niektorych przypadkach np dla malzenstw usuniecie wiekszosci outlierow 
#skutkuje znaczna utrata danych dlatego usunieto tylko ich czesc 

#1)Urodzenia zywe na 1000

#podglad na wykresie pudelkowym
boxplot(dane$urodzenia_zywe_na_1000)

max(dane$urodzenia_zywe_na_1000)
Dane <- dane
Dane <- filter(Dane, urodzenia_zywe_na_1000 < 16.52)


#WYKRES PUDELKOWY DLA URODZEN
boxplot(Dane$urodzenia_zywe_na_1000,main='Urodzenia zywe na 100 mieszkancow - 2018')


#2)Zawarte malzenstwa

#podglad na wykresie pudelkowym
boxplot(dane$Zawarte_malzenstwa)
View(sort(dane$Zawarte_malzenstwa))

Dane <- filter(Dane, Zawarte_malzenstwa < 2500)

#WYKRES PUDELKOWY DLA MALZENSTW
boxplot(Dane$Zawarte_malzenstwa,main='Zwarte malzenstwa - 2017')


#3) Pracujacy na 1000

#podglad na wykresie pudelkowym
boxplot(dane$Pracujacy_na_1000)

Dane <- filter(Dane, Pracujacy_na_1000 < 500)


#WYKRES PUDELKOWY DLA PRACUJACYCH
boxplot(Dane$Pracujacy_na_1000,main='Pracujacy na 1000 mieszkancow - 2017')


#4) Gestosc zaludnienia

#podglad na wykresie pudelkowym
boxplot(Dane$Gestosc_zaludnienia)

Dane <- filter(Dane, Gestosc_zaludnienia < 2500)

#WYKRES PUDELKOWY DLA GESTOSCI ZALUDNIENIA
boxplot(Dane$Gestosc_zaludnienia,main='Gestosc zaludnienia na 1km^2 - 2017')


#STATYSTYKI OPISOWE-------------------------------------------
#srednia,kwartyle, mediana, minimalna i maksymalna wartosc

#ZYWE URODZENIA NA 1000
summary(Dane$urodzenia_zywe_na_1000)

#ZAWARTE MALZENSTWA
summary(Dane$Zawarte_malzenstwa)

#PRACUJACY NA 1000 LUDNOSCI
summary(Dane$Pracujacy_na_1000)

#GESTOSC ZALUDNIENIA NA 1 KM KWADRATOWY
summary(Dane$Gestosc_zaludnienia)


#odchylenie standardowe

#ZYWE URODZENIA NA 1000
sd(Dane$urodzenia_zywe_na_1000)

#ZAWARTE MALZENSTWA
sd(Dane$Zawarte_malzenstwa)

#PRACUJACY NA 1000 LUDNOSCI
sd(Dane$Pracujacy_na_1000)

#GESTOSC ZALUDNIENIA NA 1 KM KWADRATOWY
sd(Dane$Gestosc_zaludnienia)

#WNIOSKI - Wsrod badanych czynnikow wartosci dotyczace gestosci zaludnienia na 1 km kwadratowy sa najbardziej zroznicowana zmienna wzgledem sredniej. 
#Najmniej zroznicowane dane dotycza zywych urodzen na 1000 mieszkancow


#wariancja

#ZYWE URODZENIA NA 1000
var(Dane$urodzenia_zywe_na_1000)

#ZAWARTE MALZENSTWA
var(Dane$Zawarte_malzenstwa)

#PRACUJACY NA 1000 LUDNOSCI
var(Dane$Pracujacy_na_1000)

#GESTOSC ZALUDNIENIA NA 1 KM KWADRATOWY
var(Dane$Gestosc_zaludnienia)



#kurtoza

#ZYWE URODZENIA NA 1000
kurtosis(Dane$urodzenia_zywe_na_1000)

#ZAWARTE MALZENSTWA
kurtosis(Dane$Zawarte_malzenstwa)

#PRACUJACY NA 1000 LUDNOSCI
kurtosis(Dane$Pracujacy_na_1000)

#GESTOSC ZALUDNIENIA NA 1 KM KWADRATOWY
kurtosis(Dane$Gestosc_zaludnienia)

#WNIOSKI - Dla danych dotyczacych urodzeñ mamy do czynienia z kurtoza 3.04 - oznacza to bardzo male, nieznaczne wyostrzenie rozkladu powyzej normalnosci, normalna koncentracje danych.
#Dla danych dotyczacych malzenstw kurtoza wynosi 11.78 - jest to ogromne wyostrzenie powyzej normalnosci i bardzo duza koncentracja danych
#Dla danych dotyczacych l. pracujacych osob mamy do czynienia z kurtoza 5.25 - oznacza to wyostrzenie rozkladu powyzej normalnosci, duza koncentracje danych.
#Natomiast dla dancyh dotycacych gestosci zaludnienia kurtoza wynosi 7.58 - w tym przypadku mamy bardzo duze wyostrzenie rozkladu powyzej normalnosci, duza koncentracje danych.


#skosnosc
#ZYWE URODZENIA NA 1000
skewness(Dane$urodzenia_zywe_na_1000)

#ZAWARTE MALZENSTWA
skewness(Dane$Zawarte_malzenstwa)

#PRACUJACY NA 1000 LUDNOSCI
skewness(Dane$Pracujacy_na_1000)

#GESTOSC ZALUDNIENIA NA 1 KM KWADRATOWY
skewness(Dane$Gestosc_zaludnienia)

#WNIOSKI -Wszystkie badane zmienne posiadaja wspolczynnik skosnosci dodatni, co 
#œwiadczy o prawostronnej asymetrii rozk³adu (maja rozklad dodatnio skoœny). Dane dotyczace zmiennej objasnianej (urodzenia) 
#maja najmniejsza asymetrie, najbardziej zblizona do zera (bliskie zera swiadczylyby o braku asymetrii rozkladu probki).
#Dane dotyczace Zawartych malzenstw i gestosci zaludnienia charakteryzuja sie najwieksza i bardzo silna asymetria


#HISTOGRAMY DLA ZM. OBJASNIAJACYCH I ZM. OBJASNIANEJ----------------------------------------------------------------------

#ZYWE URODZENIA NA 1000
hist(Dane$urodzenia_zywe_na_1000, main="Histogram - Urodzenia ¿ywe na 1000 mieszkañców - 2018",ylab="Czestosc",xlab = "Urodzenia ¿ywe na 1000 mieszkañców",col = "blue")

#ZAWARTE MALZENSTWA
hist(Dane$Zawarte_malzenstwa, main="Histogram - Zawarte ma¿eñstwa - 2017",ylab="Czestosc",xlab = "Liczba zawartych mal¿eñstw",col = "pink")

#PRACUJACY NA 1000 LUDNOSCI
hist(Dane$Pracujacy_na_1000,main="Histogram - Pracujacy na 1000 mieszkañców - 2017",ylab="Czestosc",xlab = "Liczba osob pracujacych na 1000 mieszkañców",col = "purple")

#GESTOSC ZALUDNIENIA NA 1 KM KWADRATOWY
hist(Dane$Gestosc_zaludnienia,main="Histogram - Gestosæ zaludnienia 1 km^2 - 2017",ylab="Czestosc",xlab = "Gestoæ zaludnienia na 1 km^2" ,col = "green")

#BADANIE NORMALNOSCI ROZKLADU ZMIENNYCH-----------------------------------------------------------------------------------------------------------------s
#test Shapiro- Wilka 

#H0: Probka pochodzi z populacji o rozkladzie normalnym
#H1: Probka pochodzi z populacji o rozkladzie innym niz normalny


#ZYWE URODZENIA NA 1000
shapiro.test(Dane$urodzenia_zywe_na_1000)
#WNIOSKI - Odrzucic Hipoteze Zerowa na rzecz Hipotezy alternatywnej H1
#Probka pochodzi z populacji o rozkladzie innym niz normalny


#ZAWARTE MALZENSTWA
shapiro.test(Dane$Zawarte_malzenstwa)
#WNIOSKI - Odrzucic Hipoteze Zerowa na rzecz Hipotezy alternatywnej H1
#Probka pochodzi z populacji o rozkladzie innym niz normalny


#PRACUJACY NA 1000 LUDNOSCI
shapiro.test(Dane$Pracujacy_na_1000)
#WNIOSKI - Odrzucic Hipoteze Zerowa na rzecz Hipotezy alternatywnej H1
#Probka pochodzi z populacji o rozkladzie innym niz normalny


#GESTOSC ZALUDNIENIA NA 1 KM KWADRATOWY
shapiro.test(Dane$Gestosc_zaludnienia)
#WNIOSKI - Odrzucic Hipoteze Zerowa na rzecz Hipotezy alternatywnej H1
#Probka pochodzi z populacji o rozkladzie innym niz normalny


#ZMIENNA OBJASNIANA - LICZBA URODZEN

Urodzenia <- c(Dane$urodzenia_zywe_na_1000)


kernels <- eval(formals(density.default)$kernel)
h.f <- sapply(kernels, function(k)density(kern = k, give.Rkern = TRUE))
(h.f <- (h.f["gaussian"] / h.f)^ .2)
bw <- bw.SJ(Urodzenia)
plot(density(Urodzenia, bw = bw, n = 2^13))


for(i in 2:length(kernels)) {
  lines(density(Urodzenia, bw = bw, adjust = h.f[i], kern = kernels[i],
                n = 2^13), col = i)
}
legend(55, 0.035, legend = kernels, col = seq(kernels), lty = 1)
points(Urodzenia,0*Urodzenia,pch="|",cex=3) 

dane <- Dane


# ----- Czesc 2 - MNK -------------------------

#pierwsza proba estymacji MNK
model_x1 <- lm(formula = urodzenia_zywe_na_1000 ~ Zawarte_malzenstwa, data=dane)
summary(model_x1)
plot(dane$Zawarte_malzenstwa, dane$urodzenia_zywe_na_1000)
abline(model_x1)
grid()

model_x2 <- lm(formula = urodzenia_zywe_na_1000 ~ Pracujacy_na_1000, data=dane)
summary(model_x2)
plot(dane$Pracujacy_na_1000, dane$urodzenia_zywe_na_1000)
abline(model_x2)
grid()

model_x3 <- lm(formula = urodzenia_zywe_na_1000 ~ Gestosc_zaludnienia, data=dane)
summary(model_x3)
plot(dane$Gestosc_zaludnienia, dane$urodzenia_zywe_na_1000)
abline(model_x3)
grid()

#Usuniecie optymalnej ilosci wartosci odstajacych
boxplot(dane$urodzenia_zywe_na_1000)
boxplot(dane$Zawarte_malzenstwa)
boxplot(dane$Pracujacy_na_1000)
boxplot(dane$Gestosc_zaludnienia)


dane <- filter(dane, urodzenia_zywe_na_1000 < 13)
dane <- filter(dane, Zawarte_malzenstwa < 800)
dane <- filter(dane, Pracujacy_na_1000 < 300)
dane <- filter(dane, Gestosc_zaludnienia < 200)

boxplot(dane$urodzenia_zywe_na_1000)
boxplot(dane$Zawarte_malzenstwa)
boxplot(dane$Pracujacy_na_1000)
boxplot(dane$Gestosc_zaludnienia)

#Estymacja MNK:

model_x1 <- lm(formula = urodzenia_zywe_na_1000 ~ Zawarte_malzenstwa, data=dane)
summary(model_x1)
plot(dane$Zawarte_malzenstwa, dane$urodzenia_zywe_na_1000, main="MNK - Zawarte ma¿eñstwa - 2017")
abline(model_x1)
grid()

qqnorm(model_x1$residuals, main="Rozklad reszt - model 1")
shapiro.test(model_x1$residuals)
#Nie odrzucamy H0 o normalnosci rozkladu reszt

model_x2 <- lm(formula = urodzenia_zywe_na_1000 ~ Pracujacy_na_1000, data=dane)
summary(model_x2)
plot(dane$Pracujacy_na_1000, dane$urodzenia_zywe_na_1000, main="MNK - Pracujcy na 1000 ludnoci - 2017")
abline(model_x2)
grid()

qqnorm(model_x2$residuals, main ="Rozklad reszt - model 2")
shapiro.test(model_x2$residuals)
#Nie odrzucamy H0 o normalnym rozkadzie reszt

model_x3 <- lm(formula = urodzenia_zywe_na_1000 ~ Gestosc_zaludnienia, data=dane)
summary(model_x3)
plot(dane$Gestosc_zaludnienia, dane$urodzenia_zywe_na_1000, main="MNK - Gestosc zaludnienia na 1km^2 - 2017")
abline(model_x3)
grid()

qqnorm(model_x3$residuals, main="Rozklad reszt - model 3")
shapiro.test(model_x3$residuals)
#Nie odrzucamy H0 o normalnym rozkadzie reszt


#W kazdym przypadku wyraz wolny jest istotny
#Metoda prob i bledow nie udalo sie uzyskac lepszych miar dopasowania modelu


#Testy zalozen MNK:

#model 1

#W obu testach: H0 - model jest liniowy
raintest(model_x1)
resettest(model_x1, power=2, type="regressor")
#Z obu testów wynika, ¿e nale¿y przyjæ hipotez o liniowoci modelu

#test Goldfeld-Quandt'a: H0 - rownosc wariancji
gqtest(model_x1)
#Wariancja skladnika losowego nie jest stala

#Test Durbina-Watsona: H0 - autokorelacja skladnika losowego = 0
dwtest(model_x1)
#Odrzucamy H0 - w modelu wystpuje autokorelacja rzedu I skladnika losowego

#test Breusch-Godfrey dla autokorelacji rzedu II: H0 - nie wystepuje autokorelacja skladnika losowego do rzedu 2
bgtest(model_x1, order=2)
#Odrzucamy H0 - wystepuje autokorelacja skladnika losowego rzedu 1 i 2

#WNIOSKI: Model nie spelnia zalo¿eñ koniecznych do stosowania MNK (m.in. tych o staej wariancji skladnika losowego i o braku autokorelacji skladnika losowego) wiec ta metoda nie powinna byc stosowana do estymacji parametrow modelu


#model 2

#W obu testach: H0 - model jest liniowy
raintest(model_x2)
resettest(model_x2, power=2, type="regressor")
#Z obu testów wynika, ¿e nale¿y przyjac hipoteze o liniowoci modelu

#test Goldfeld-Quandt'a: H0 - rownosc wariancji
gqtest(model_x2)
#Nie ma podstaw do odrzucenia H0, mozemy przyjac wariancje skladnika losowego za stala

#Test Durbina-Watsona: H0 - autokorelacja skdnika losowego = 0
dwtest(model_x2)
#Odrzucamy H0 - w modelu wystpuje autokorelacja rzedu I skladnika losowego

#test Breusch-Godfrey dla autokorelacji rzedu II: H0 - nie wystepuje autokorelacja skladnika losowego do rzedu 2
bgtest(model_x2, order=2)
#Odrzucamy H0 - wystepuje autkorelacja skladnika losowego rzedu 1 i 2

#WNIOSKI: Model nie spelnia zalo¿eñ koniecznych do stosowania MNK (o braku autokorelacji skladnika losowego) wiec ta metoda nie powinna byc stosowana do estymacji parametrow modelu


#model 3

#W obu testach: H0 - model jest liniowy
raintest(model_x3)
resettest(model_x3, power=2, type="regressor")
#Z obu testów wynika, ¿e nale¿y przyjac hipoteze o liniowoci modelu

#test Goldfeld-Quandt'a: H0 - rownosc wariancji
gqtest(model_x3)
#Odrzucamy H0 - wariancja skladnika losowego nie jest stala

#Test Durbina-Watsona: H0 - autokorelacja skdnika losowego = 0
dwtest(model_x3)
#Odrzucamy H0 - w modelu wystpuje autokorelacja rzedu I skladnika losowego

#test Breusch-Godfrey dla autokorelacji rzedu II: H0 - nie wystepuje autokorelacja skladnika losowego do rzedu 2
bgtest(model_x3, order=2)
#Odrzucamy H0 - wystepuje autokorelacja skladnika losowego rzedu 1 i 2

#WNIOSKI: Model nie spelnia zalo¿eñ koniecznych do stosowania MNK (o braku autokorelacji skladnika losowego) wiec ta metoda nie powinna byc stosowana do estymacji parametrow modelu


#Najwiekszym R^2 charakteryzowal sie model:
summary(model_x1)



# ----- Czesc 3 - Regresja nieparametryczna ------------------------- 

regresja1 <- npreg(txdat=dane$Zawarte_malzenstwa, tydat=dane$urodzenia_zywe_na_1000, bws=2)
plot(dane$Zawarte_malzenstwa, dane$urodzenia_zywe_na_1000)
plot(regresja1 ,plot.errors.method = "asymptotic", ylim=c(-5,25), ylab="Urodzenia zywe na 1000 mieszkancow", xlab="Zawarte malzenstwa")
summary(regresja1)

regresja2 <- npreg(txdat=dane$Pracujacy_na_1000, tydat=dane$urodzenia_zywe_na_1000, bws=0.7)
plot(regresja2,plot.errors.method = "asymptotic", ylim=c(-5,25), ylab="Urodzenia zywe na 1000 mieszkancow", xlab="Pracujacy na 1000 mieszkancow")
summary(regresja2)

regresja3 <- npreg(txdat=dane$Gestosc_zaludnienia, tydat=dane$urodzenia_zywe_na_1000, bws=0.7)
plot(regresja3,plot.errors.method = "asymptotic", ylim=c(-5,25), ylab="Urodzenia zywe na 1000 mieszkancow", xlab="Gestosc zaludnienia")
summary(regresja3)

#k-krotny sprawdzian krzy¿owy:

#Porownywane beda wlasnosci modelu 1 (Dla zwartych malzenstw)

#Regresja nieparametryczna

#--- 1 probka --

bw = npregbw(formula=dane$urodzenia_zywe_na_1000~dane$Zawarte_malzenstwa, data = dane[c(55:270),], 
             regtype="lc", nmulti=2)
reg = npreg(bw,exdat=dane[1:54,3])
m_hat=reg$mean
y=dane[1:54,2]
bledy1=y-m_hat
bledy1


#--- 2 probka --

bw = npregbw(formula=dane$urodzenia_zywe_na_1000~dane$Zawarte_malzenstwa, data = dane[c(1:54,163:270),], regtype="lc", nmulti=2)
reg=npreg(bw,exdat=dane[55:108,3])
m_hat=reg$mean
y=dane[55:108,2]
bledy2=y-m_hat


#--- 3 probka --

bw = npregbw(formula=dane$urodzenia_zywe_na_1000~dane$Zawarte_malzenstwa, data = dane[c(1:108,163:270),], regtype="lc", nmulti=2)
reg=npreg(bw,exdat=dane[109:162,3])
m_hat=reg$mean
y=dane[109:162,2]
bledy3=y-m_hat

#--- 4 probka --

bw = npregbw(formula=dane$urodzenia_zywe_na_1000~dane$Zawarte_malzenstwa, data = dane[c(1:163,217:270),], regtype="lc", nmulti=2)
reg=npreg(bw,exdat=dane[163:216,3])
m_hat=reg$mean
y=dane[163:216,2]
bledy4=y-m_hat

#--- 5 probka --

bw = npregbw(formula=dane$urodzenia_zywe_na_1000~dane$Zawarte_malzenstwa, data = dane[c(1:216),], regtype="lc", nmulti=2)
reg=npreg(bw,exdat=dane[217:270,3])
m_hat=reg$mean
y=dane[217:270,2]
bledy5=y-m_hat

#Œredni blad bezwzgledny (MAE)
bledy_razem_nieparametryczna=c(mean(abs(bledy1)), mean(abs(bledy2)), mean(abs(bledy3)), mean(abs(bledy4)), mean(abs(bledy5)))
bledy_razem_nieparametryczna

#MNK

mnk <- lm(formula = urodzenia_zywe_na_1000 ~ Zawarte_malzenstwa, data=dane[55:270,])
b = coef(mnk)
b0 = b[1]
b1 = b[2]
m_hat=b0+dane[1:54,3]*b1
m_hat
y=dane[1:54,2]
bledy1=y-m_hat

mnk <- lm(formula = urodzenia_zywe_na_1000 ~ Zawarte_malzenstwa, data=dane[c(1:54,163:270),])
b = coef(mnk)
b0 = b[1]
b1 = b[2]
m_hat=b0+dane[55:108,3]*b1
m_hat
y=dane[55:108,2]
bledy2=y-m_hat

mnk <- lm(formula = urodzenia_zywe_na_1000 ~ Zawarte_malzenstwa, data=dane[c(1:108,163:270),])
b = coef(mnk)
b0 = b[1]
b1 = b[2]
m_hat=b0+dane[109:162,3]*b1
m_hat
y=dane[109:162,2]
bledy3=y-m_hat

mnk <- lm(formula = urodzenia_zywe_na_1000 ~ Zawarte_malzenstwa, data=dane[c(1:163,217:270),])
b = coef(mnk)
b0 = b[1]
b1 = b[2]
m_hat=b0+dane[163:216,3]*b1
m_hat
y=dane[163:216,2]
bledy4=y-m_hat

mnk <- lm(formula = urodzenia_zywe_na_1000 ~ Zawarte_malzenstwa, data=dane[1:216,])
b = coef(mnk)
b0 = b[1]
b1 = b[2]
m_hat=b0+dane[217:270,3]*b1
m_hat
y=dane[217:270,2]
bledy5=y-m_hat

#Œredni blad bezwzgledny (MAE)
bledy_razem_parametryczna=c(mean(abs(bledy1)), mean(abs(bledy2)), mean(abs(bledy3)), mean(abs(bledy4)), mean(abs(bledy5)))
bledy_razem_parametryczna



