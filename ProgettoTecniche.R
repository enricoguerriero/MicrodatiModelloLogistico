library(ggplot2)
library(ggfortify)
library(ggcorrplot)
library(dplyr)

setwd("C:/Users/enric/Desktop/Progetto Tecniche")

AVQ_Microdati_2020 <- read.delim("C:/Users/enric/Desktop/Progetto Tecniche/AVQ_Microdati_2020.txt") 

data <- AVQ_Microdati_2020

weight <- data$COEFIN
# has subject ever ever smoked? 1,2yes, 3no
# is subject currently smoking? 1yes, 2,3no

#regmf 999 na
#cittmi 9 na
#sporco 5 na
#colmp 5 na
#inqar 5 na
#rumore 5 na
#crim 5 na
nas <- c()
for (i in 1:length(data[1,])){
  
  if (names(data)[i]=="REGMF"){
    for(j in 1:length(data$REGMF)){
      if (! is.na(data$REGMF[j])){
        if (data$REGMF[j]==999){
          data$REGMF[j] = NA
        }
      }
    }
  }
  if (names(data)[i]=="CITTMi"){
    for(j in 1:length(data$CITTMi)){
      if (! is.na(data$CITTMi[j])){
        if (data$CITTMi[j]==9){
          data$CITTMi[j] = NA
        }
      }
    }
  }
  if (names(data)[i]=="SPORCO"){
    for(j in 1:length(data$SPORCO)){
      if (! is.na(data$SPORCO[j])){
        if (data$SPORCO[j]==5){
          data$SPORCO[j] = NA
        }
      }
    }
  }
  if (names(data)[i]=="COLMP"){
    for(j in 1:length(data$COLMP)){
      if (! is.na(data$COLMP[j])){
        if (data$COLMP[j]==5){
          data$COLMP[j] = NA
        }
      }
    }
  }
  if (names(data)[i]=="INQAR"){
    for(j in 1:length(data$INQAR)){
      if (! is.na(data$INQAR[j])){
        if (data$INQAR[j]==5){
          data$INQAR[j] = NA
        }
      }
    }
  }
  if (names(data)[i]=="RUMORE"){
    for(j in 1:length(data$RUMORE)){
      if (! is.na(data$RUMORE[j])){
        if (data$RUMORE[j]==5){
          data$RUMORE[j] = NA
        }
      }
    }
  }
  if (names(data)[i]=="CRIM"){
    for(j in 1:length(data$CRIM)){
      if (! is.na(data$CRIM[j])){
        if (data$CRIM[j]==5){
          data$CRIM[j] = NA
        }
      }
    }
  }
  
  nas <- c(nas, sum(is.na(data[,i])))
  
}


ggplot(aes(x=nas), data=data.frame("nas"=nas)) + geom_histogram(col="black", fill="lightblue") + theme_bw()
ggplot(aes(x=nas), data=data.frame("nas"=nas)) + geom_bar(col="black", fill="blue") + theme_bw()

mean(nas < 100)

sum(is.na(data$FUMO))  


for (i in 1:length(data$FUMO)){
  if(! is.na(data$FUMO[i])){
    if(data$FUMO[i]==2 | data$FUMO[i]==1){
      data$FUMO[i] <- 1
    }
    if(data$FUMO[i]==3){
      data$FUMO[i] <- 0
    }
  }
}

for (i in 1:length(data$FUMO)){
  if(! is.na(data$FUMO[i])){
    if(data$FUMO[i]==2 | data$FUMO[i]==3){
      data$FUMO[i] <- 0
    }
  }
}
data$FUMO
ggplot(aes(x=FUMO), data=data) + geom_bar(col="black", fill="blue") 


y <- data$FUMO

#removing nas from data, need to keep the most columns, trying 100, 1000, 10000 as 
# first tresholds

# 10000 -> 9263 rows, 333 columns
# 1000 -> 37781 rows, 130 columns
# 100 -> 42719 rows, 115 columns

lim <- 1750

data <- data[, nas<lim]

data$FUMO <- y
data$weight <- weight

#remove rows that have one or more na
data <- na.omit(data)

weight <- data$weight / sum(data$weight)

#cols remaining
length(data[1,])
#rows remaining
length(data[,1])

head(data)
names(data)

# factor sesso, regmf, cittmi, parchi, godab, bic
keep <- c("NCOMP", "ETAMi", "SESSO", "REGMF", 
          "CITTMi", "SALUTE", "SENELE", "SPORCO",
          "INQAR", "RUMORE", "CRIM", "PARCHI",
          "STANZEM", "GODAB", "AVVOC", "BIC",
          "LIBFAM", "SITE", "RISEC", "FUMO")

data <- data[, keep] 
head(data)
keep %in% names(data)

data$SESSO <- factor(data$SESSO)
data$REGMF <- factor(data$REGMF)
data$CITTMi <- factor(data$CITTMi)
data$PARCHI <- factor(data$PARCHI)
data$BIC <- factor(data$BIC)
data$GODAB <- factor(data$GODAB)


# try regression with everything else and then STEPAIC 
library(MASS)

model <- glm(FUMO ~ ., data=data, family=binomial())
summary(model)
par(mfrow=c(2,2))
plot(model)
par(mfrow=c(1,1))
model1 <- stepAIC(model, direction="both", data=data)
summary(model1)

# quantitative variables
keep2 <- c("NCOMP", "ETAMi", "SALUTE", "SENELE",
           "SPORCO", "INQAR", "RUMORE",
           "CRIM", "STANZEM", "AVVOC", "LIBFAM",
           "SITE", "RISEC", "FUMO")
head(df)
df <- data[, keep2]

ggcorrplot(cor(df)) 

data.pca <- prcomp(df)
summary(data.pca)
autoplot(data.pca, col="ETAMi", data=df)

data.pca$rotation[,1:4]

modello3 <- glm(FUMO ~ ETAMi + AVVOC + SESSO
                + RISEC + SENELE + SPORCO + LIBFAM,
                family=binomial(), data=data)
summary(modello3)


ggplot(aes(x=ETAMi, y=FUMO), data=data) 
  + geom_point() + geom_smooth() + theme_bw()



###################### parte di suk ##################################


newdata = data
newdata$CRIM = rep(0,length(data$CRIM))
newdata$CRIM[data$CRIM <= 2] = 1
head(newdata)
names(newdata)

ggplot(aes(x=CRIM), data=newdata) + geom_bar(col="black", fill="blue")


data.pure <- AVQ_Microdati_2020

#Tolgo solo na in criminalità e confronto con dati del modello
par(mfrow=c(1,2))
data.crim = data.pure[!is.na(data.pure$CRIM),]
sum(is.na(data.crim$CRIM))
data.crim.2 = data.crim
data.crim.2$CRIM = rep(0,length(data.crim$CRIM))
data.crim.2$CRIM[data.crim$CRIM <= 2] = 1
hist(data.crim.2$CRIM,main="Tutte le Misurazioni",xlab="")
hist(newdata$CRIM,main="Misurazioni nel Modello",xlab="")

crim.mod1 = glm(CRIM ~ .,data = newdata,family=binomial())
summary(crim.mod1)

crim.aic = stepAIC(crim.mod1,direction="both",data=newdata)
summary(crim.aic)

keep.crim = c("CITTMi","SALUTE","SPORCO","INQAR","RUMORE","PARCHI","GODAB","SITE","RISEC","AVVOC","CRIM")
keep.crim.df = c("SALUTE","SPORCO","INQAR","RUMORE","SITE","RISEC","AVVOC","CRIM")

df.crim = newdata[,keep.crim.df]
head(df.crim)

ggcorrplot(cor(df.crim))


data.pca <- prcomp(df.crim)
summary(data.pca)
autoplot(data.pca, col="CRIM",
         data=df.crim, theme="dark")




crim.mod2 = glm(CRIM ~ ., family=binomial(), data=newdata[,keep.crim])
summary(crim.mod2)

newdata.final = newdata[,keep.crim]
head(newdata.final)

smooth.print = function(x,name){
  vnorm = rnorm(length(x),sd=0.01)
  sal.norm = as.double(x) + vnorm
  ggplot(aes(x=sal.norm, y=CRIM), data=newdata.final) + geom_point() + geom_smooth(col=2) + theme_bw() +
    xlab(name) + ylab("Criminalità Percepita")
}

smooth.print(newdata.final$SALUTE,"Insanità")
