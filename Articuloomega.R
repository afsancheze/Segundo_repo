library(readxl)
library(psych)
library(Scale)
library(psy)
library(apaTables)
library(GPArotation)
library(lavaan)
library(semPlot)
library(GPArotation)


install.packages("ctv") #this downloads the task view package
library(ctv) #this activates the ctv package
install.views("Psychometrics")

?sample


It1<-sample(1:4,30,replace=T)
It2<-sample(1:4,30,replace=T)
It3<-sample(1:4,30,replace=T)
It4<-sample(1:4,30,replace=T)

Datos<-cbind.data.frame(It1,It2,It3,It4)

describe(Datos)
omega(Datos)
describe(Datos3)
omega

?ItemAnalysis
EscalaG<-Scale(Datos) #Transforma los datos a otro formato#
AnalisisG<-ItemAnalysis(EscalaG, nfactors=1, method="polychoric") #Realiza cálculos análisis ítems politómicos#
ReportTable(AnalisisG) #Resumen Anáisis ítems#
AnalisisG$valid$bartlett #Test Bartlett#
AnalisisG$rely$alpha #Resumen Alfa#
AnalisisG$valid$kmo #Test Adecuación muestral KMO#
print.reliability(AnalisisG$rely) #Resumen análisis Fiabilidad#
print(AnalisisG)
GetScores(AnalisisI)

#Omega####

?omega
?rnorm()
rnorm(80,mean = 2,sd=1)

Omega<-omega(Datos, nfactors = 1, poly = TRUE,plot=TRUE, digits = 3) #Omega a partir de matríx de correlaciones Policóricas#
print(Omega) #Resutado Omega#
Omega$schmid$orthog #Resultado Análisis Factorial
Omega$alpha
Omega$omega.tot




library(readxl)
Datos500n3cat <- read_excel("~/Desktop/Artículo Omega/Simulación_Datos.xlsx", 
                               sheet = "n500-3")
describe(Datos50n3cat)
Omega<-omega(Datos50n3cat)
Omega$alpha
Omega$omega_h
Omega$omega.to
alpha(Datos50n3cat)
a$alpha.drop
EscalaG<-Scale(Datos30n3cat) #Transforma los datos a otro formato#
AnalisisG<-ItemAnalysis(EscalaG, nfactors=1, method="polychoric") #Realiza cálculos análisis ítems politómicos#
ReportTable(AnalisisG) #Resumen Anáisis ítems#
AnalisisG$rely$alpha #Resumen Alfa#
print.reliability(AnalisisG$rely)

set.seed(17)
r9 <- sim.hierarchical(n=500,raw=TRUE)$observed
r5<-sim.hierarchical(n=500,raw=TRUE)$observed

?sim.item()
q<-sim.item(nvar = 30, nsub = 30, circum = FALSE, xloading = 0.6, categorical = T, low = 1, high = 3, 
         truncate = T)

q<-sim.item(nvar = 4, nsub = 500, circum = F,
         gloading = 0.6, xbias = 0, ybias = 0, categorical = T, low = 1, high = 5, cutpoint = 5)
q<-as.data.frame(q)
?sim.hierarchical()

round(cor(r9),2)



q<-sim.item(nvar = 4, nsub = 50, circum = FALSE, xbias = 0, ybias = 0, categorical = TRUE, low = 1, high = 4, 
         truncate = FALSE, cutpoint = 100)
q<-as.data.frame(q)





