# Les librairies 
rm(list = ls())
library(Rcmdr)
library(dplyr)
library(ggplot2)
library(plyr)
library(MASS)
library(stargazer)
library(readxl)
library(fastDummies)
library(colorspace, pos=26)
library(pander)
# Lecture de la base 
setwd("C:/Users/lenovo/Downloads/PROJET TUTEURE")
data<-read_excel("Base_de_donnees up-work.xlsx")
ncol(data)
colnames(data)
str(data)
#Recodage des variables en format correcte
data$Prix<-as.numeric(data$Prix)
data$job_successe<-as.numeric(data$job_successe)
data$Top_Rated<-as.factor(data$Top_Rated)
data$Rating<-as.factor(data$Rating)
data$Agence<-as.factor(data$Agence)
data$Pays<-as.factor(data$Pays)
str(data)
summary(data)
table(data$Pays)
#Trairtement des nan
##comptage des valeurs Na dans chaque colonne en pourcentage
#with(na_count, Hist(na_count, breaks="Sturges", col="darkgray"))
na_count <-sapply(data, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count);na_count
#agg(data,prop = F,numbers =T)
data1 <- data[,c('Total_heure','job_successe')]
data1 =apply(data1,2,function(x){
  x[is.na(x)]=mean(x,na.rm = T)
  return(x)
})
data1 = round(data1,0)
data1 <- data.frame(data1)
data$Total_heure = data1$Total_heure
data$job_successe = data1$job_successe
# suprimer les lignes qui comprend les valeurs NA dans la table 'data'
data <- na.omit(data)

#Creation de la variable salaire_total pour mesurer le revenue
data$salaire_total = data$Total_heure * data$Prix
summary(data$salaire_total)

# Analyse descriptive et la visualisation 
## La distribution de pareto
hist(data$Total_jobs)
hist(data$Total_heure)
hist(data$Prix)

## Description des variables non numerique
#Pour savoir les pays qui ont plus de freelanceurs dans la plateforme.
table_pays <- table(data$Pays);table_pays
plot(table_pays,main = "La distribution de l'origine des freelancers", col = "darkgray")

#TOP RATED 
with(data, piechart(Top_Rated, xlab="", ylab="", main="Top_Rated", 
                    col=rainbow_hcl(2), scale="percent"))
#Rating
with(data, piechart(Rating, xlab="", ylab="", main="Rating", 
                    col=rainbow_hcl(2), scale="percent"))
#Agence 
with(data, piechart(Agence, xlab="", ylab="", main="agence", 
                    col=rainbow_hcl(2), scale="percent"))


## Description des variables numerique globale.
summary(data[c("Total_jobs", "Prix", "job_successe", "Total_heure")])

##Le plot qui s'affiche les quantiles de job_successe en groupe des freelancer qui sont dans l'agence 
##et ne sont pas dans l'agence 
with(data, qqPlot(job_successe, dist="norm", id=list(method="y", n=2, 
                                                     labels=rownames(data)), groups=Agence))
## quantiles des pris par angences
with(data, qqPlot(Prix, dist="norm", id=list(method="y", n=2, 
                                             labels=rownames(data)), groups=Agence))
## quantiles des points total heures 
with(data, qqPlot(Total_heure, dist="norm", id=list(method="y", n=2, 
                                                    labels=rownames(data)), groups=Agence))
## Total jobs
with(data, qqPlot(Total_jobs, dist="norm", id=list(method="y", n=2, 
                                                   labels=rownames(data)), groups=Agence))

#data2 => la base qui a les valeurs aberrantes
data_abe = data
#Les index les lignes qui a des valeurs aberantes dans les 3 variables(Total_jobs,Total_heure,
#job_successe)
Boxplot( ~ Total_jobs, data=data, id=list(method="y"))#7
Boxplot( ~ Total_heure, data=data, id=list(method="y"))
Boxplot( ~ job_successe, data=data, id=list(method="y"))
Boxplot( ~ salaire_total, data = data,id=list(methode="y"))

attach(data)
hist(job_successe)
hist(salaire_total)
#suppression des valeurs aberrantes
data = data[-c(427,149,388,195,113,22,7,106,134,100,469,291,446,416,317,388,195,78),]
summary(data)

#
data_agence = data[which(data$Agence==1),]
data_agence$catego_salaire <- ifelse(data_agence$salaire_total>10000,"1","0")
data_agence$catego_jobsuc <- ifelse(data_agence$job_successe>90, "1","0")
colnames(data_agence)
data_agence <- data_agence[,!colnames(data_agence) %in% c('job_successe','salaire_total','Agence')]
data_agence$catego_salaire<- as.factor(data_agence$catego_salaire)
data_agence$catego_jobsuc<-as.factor(data_agence$catego_jobsuc)
summary(data_agence)

data_nonag = data[which(data$Agence==0),]
data_nonag$catego_salaire <- ifelse(data_nonag$salaire_total>1000, "1","0")
data_nonag$catego_jobsuc <- ifelse(data_nonag$job_successe>90, "1","0")
data_nonag <- data_nonag[,!colnames(data_nonag) %in% c('job_successe','salaire_total','Agence')]
data_nonag$catego_salaire <- as.factor(data_nonag$catego_salaire)
data_nonag$catego_jobsuc <- as.factor(data_nonag$catego_jobsuc)
summary(data_nonag)


cor(data[,c("Total_jobs", "Prix", "job_successe", "Total_heure")])
# nous trouvons que il y a une forte relation corelee entre les variables 
# : 'total.jobs' et  'Total.heure' (0.285), aussi que la corelation d'entre le prix et le taux de reussite 
#    et d'entre le prix et le heure total sont presque pareil. 
# pour savoir les infos detaillement, nous allons faire les testS de correlation.
attach(data)
cor.test(job_successe, Prix, alternative="two.sided", 
         method="pearson")
cor.test(Total_heure, Prix, alternative="two.sided", 
         method="pearson")
cor.test(Total_jobs,Total_heure, alternative="two.sided", 
         method="pearson")


# les hypotheses:
## 1. Job success donne des influences positive frelancers sur sa situation de Top rated 

## 2. Le prix peut donner des influences negatives pour notre variable expliquee, et ce gere d'influences
## est plus forte pour les freelancers qui ne travaillent pas dans les agences, si le prix est eleve, dans 
## quelque mesures il va influencer le nombre de demande de travail pour un freelancer. Les freelancer qui 
## n'ont pas bcp de opportunite de travaille 

## (robustesse : 1.les gens qui travaillent dans l'agence prix >10,000, 2. ___ prix <=10,000 
##               3.les gens qui ne travaillent pas dans l'agence prix >1,000, ___prix <= 1,000 )


# modele  
#===================================================================================================
model_general<- glm(Top_Rated ~ Total_heure + Total_jobs + Prix + job_successe + Rating +Agence, 
             family=binomial(logit), data=data)
vif(model_general)
summary(model_general)
exp(coef(model_general))  # Exponentiated coefficients (odds ratios)

#choisir les variables avec le methode "forward"
stepwise(model_general, direction='forward', criterion='BIC')
## les varibales expliquees choisis sont : job_successe, Total.jobs, Total.heure. 
## mais les coefficients sont negatives mais sauf le coefficient de job_successe les autres sont tres 
## proche de 0.
#===================================================================================================
#robust
summary(data_agence)
Modele_ag1 <- glm(Top_Rated ~ Total_heure + Total_jobs + catego_jobsuc + catego_salaire + Prix + Rating,
                  family=binomial(probit), data=data_agence)
vif(Modele_ag1)
summary(Modele_ag1)
odd_ratio_ag1 = exp(coef(Modele_ag1));odd_ratio_ag1

Modele_ag2 <- glm(Top_Rated ~ Total_heure + Total_jobs + catego_jobsuc + catego_salaire + Prix,
                  family=binomial(probit), data=data_agence)
summary(Modele_ag2)
odd_ratio_ag2 = exp(coef(Modele_ag2));odd_ratio_ag2

Modele_ag3 <- glm(Top_Rated ~ Total_heure + catego_jobsuc + catego_salaire + Rating,
                  family=binomial(probit), data=data_agence)
summary(Modele_ag3)
odd_ratio_ag3 = exp(coef(Modele_ag3));odd_ratio_ag3

Modele_ag4 <- glm(Top_Rated ~ Total_heure + catego_jobsuc + catego_salaire + Prix ,
                  family=binomial(probit), data=data_agence)
summary(Modele_ag4)
odd_ratio_ag4 = exp(coef(Modele_ag4));odd_ratio_ag4


Modele_ag5 <- glm(Top_Rated ~ Total_heure + Total_jobs + catego_jobsuc + catego_salaire,
                  family=binomial(probit), data=data_agence)
summary(Modele_ag5)
odd_ratio_ag5 = exp(coef(Modele_ag5));odd_ratio_ag5

#================================================================================================


Modele_nonag1 <- glm(Top_Rated ~ Total_heure + Total_jobs + catego_jobsuc + catego_salaire + Prix + Rating, 
                     family=binomial(probit), data=data_nonag)
summary(Modele_nonag1)
odd_ratio_noag1 = exp(coef(Modele_nonag1));odd_ratio_noag1

Modele_nonag2 <- glm(Top_Rated ~ Total_heure + Total_jobs + catego_salaire + Prix + Rating,
                     family=binomial(probit), data=data_nonag)
summary(Modele_nonag2)
odd_ratio_noag2 = exp(coef(Modele_nonag2));odd_ratio_noag2

Modele_nonag3 <- glm(Top_Rated ~ Total_heure + Total_jobs + catego_jobsuc + catego_salaire + Rating,
                     family=binomial(probit), data=data_nonag)
summary(Modele_nonag3)
odd_ratio_noag3 = exp(coef(Modele_nonag3));odd_ratio_noag3

Modele_nonag4 <- glm(Top_Rated ~ Total_heure + Total_jobs + catego_jobsuc + catego_salaire + Prix,
                     family=binomial(probit), data=data_nonag)
summary(Modele_nonag4)
odd_ratio_noag4 = exp(coef(Modele_nonag4));odd_ratio_noag4

Modele_nonag5 <- glm(Top_Rated ~ Total_heure + Total_jobs + catego_salaire + Prix, 
                     family=binomial(probit), data=data_nonag)
summary(Modele_nonag5)
odd_ratio_noag5 = exp(coef(Modele_nonag5));odd_ratio_noag5

stargazer(Modele_ag1,Modele_ag2,Modele_ag3,Modele_ag4,Modele_ag5,
          title="Robustesse avec agences", align = F, type = "html", out ="Robuste-agence.html")
stargazer(Modele_nonag1,Modele_nonag2,Modele_nonag3,Modele_nonag4,Modele_nonag5,
          title="Robustesse sans agences", align = F, type = "html", out ="Robuste-non-agence.html")
stargazer(odd_ratio_ag1,odd_ratio_ag2, odd_ratio_ag3, odd_ratio_ag4, odd_ratio_ag5, 
          title="Les odds-ratio sans agences", align = F, type = "html", out ="oddsratio-agence.html")
stargazer(odd_ratio_noag1,odd_ratio_noag2, odd_ratio_noag3, odd_ratio_noag4, odd_ratio_noag5, 
          title="Les odds-ratio sans agences", align = F, type = "html", out ="oddsratio-non-agence.html")



test_jP
