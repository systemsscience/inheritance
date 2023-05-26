##############################################################################

# Install and load libraries

#install.packages(c("vtable","plyr","lme4","clusterSEs","DescTools")) # Uncomment this line to install libraries

library(vtable) # for st() summary statistics function
library(plyr) # for ddply() summarize function and other functions
library(lme4) # for lmer() regression function
library(clusterSEs) # for estimating clustered standard errors
library(DescTools) # for Gini() function

##############################################################################

# Set working directory
setwd("~/Inheritance") # Change this to the appropriate working directory

# Load datasets

A <- read.csv("tuva_darkhad_households_anonymized.csv",stringsAsFactors=F)
names(A)
head(A)
dim(A)

C <- read.csv("tuva_darkhad_camps_anonymized.csv",stringsAsFactors=F)
names(C)
head(C)
dim(C)

Z <- read.csv("tuva_darkhad_parent_offspring_dyads_anonymized.csv",stringsAsFactors=F)
names(Z)
head(Z)
dim(Z)

##############################################################################

# Summarize variables for Supplementary Table SI1

#####
# For Supplementary Table SI1 - descriptive statistics of camps. 

# numerical variables we want to summarize
cols1 <-c("GrassQuality","Inherited")
st(C[,cols1])

# categorical variables we want to summarize
cols2 <- c("Season","Terrain","InheritedLine")
st(C[,cols2])

#####
# For Supplementary Table SI1 - descriptive statistics of households. 

names(A)
# numerical variables we want to summarize
cols1 <-c("Ages","Bodo","Cattle","Horses","SmallStock","Yaks","Camels","MaterialWealth","MeanCampInheritance","Wages","Education","HayBought")
st(A[,cols1])

##############################################################################

# Summarize variables by site for Supplementary Table SI2

#####

# For Supplementary Table SI2 - descriptive statistics of camps by site.

# numerical variables we want to summarize
cols1 <-c("Site","GrassQuality","Inherited")
C1 <- C[,cols1]
st(C1,group="Site") 

# categorical variables we want to summarize
cols2 <- c("Site","Season","Terrain","InheritedLine")
C2 <- C[,cols2]
st(C2,group="Site") 

#####

# For Supplementary Table SI2 - descriptive statistics of households by site.
cols1 <-c("Site","Ages","Bodo","Cattle","Horses","SmallStock","Yaks","Camels","MaterialWealth","MeanCampInheritance","Wages","Education","HayBought")
A1 <- A[,cols1]
st(A1,group="Site") 

##############################################################################

# Simple cross-tab summary used for Supplementary Table SI3 - frequency of camps by season and terrain
ddply(C,.(Season,Terrain),summarize,N=length(Season))

##############################################################################

# Inheritance of camps

# Mean probability of inheritance:
mean(C$Inherited[which(!is.na(C$Inherited))])

##############################################################################

# For Figure 2 - Inheritance by season and terrain

#####################

# Camps by season - MEANS

C$Season <- factor(C$Season,c("Summer","Fall","Winter","Spring"))
summary(res <- glm(Inherited ~ Season-1,family=binomial,data=C))
predict(res,type="response",newdata=data.frame(Season=c("Summer","Fall","Winter","Spring")))
CI <- cluster.bs.glm(res, dat=C, ~ anonymized_id, ci.level = 0.95, boot.reps = 1000, cluster.se = T)
Ci <- matrix(unlist(CI[2]),4,2)
Ta <- data.frame(low=Ci[,1],mean=res$coefficients,high=Ci[,2])

#####################

# Camps by season - CONTRASTS

C$Season <- factor(C$Season,c("Summer","Fall","Winter","Spring"))
summary(res <- glm(Inherited ~ Season,family=binomial,data=C))
predict(res,type="response",newdata=data.frame(Season=c("Summer","Fall","Winter","Spring")))
CI <- cluster.bs.glm(res, dat=C, ~ anonymized_id, ci.level = 0.95, boot.reps = 1000, cluster.se = T)

######

C$Season <- factor(C$Season,c("Fall","Winter","Spring","Summer"))
summary(res <- glm(Inherited ~ Season,family=binomial,data=C))
predict(res,type="response",newdata=data.frame(Season=c("Summer","Fall","Winter","Spring")))
CI <- cluster.bs.glm(res, dat=C, ~ anonymized_id, ci.level = 0.95, boot.reps = 1000, cluster.se = T)

######

C$Season <- factor(C$Season,c("Winter","Spring","Summer","Fall"))
summary(res <- glm(Inherited ~ Season,family=binomial,data=C))
predict(res,type="response",newdata=data.frame(Season=c("Summer","Fall","Winter","Spring")))
CI <- cluster.bs.glm(res, dat=C, ~ anonymized_id, ci.level = 0.95, boot.reps = 1000, cluster.se = T)

######

C$Season <- factor(C$Season,c("Spring","Summer","Fall","Winter"))
summary(res <- glm(Inherited ~ Season,family=binomial,data=C))
predict(res,type="response",newdata=data.frame(Season=c("Summer","Fall","Winter","Spring")))
CI <- cluster.bs.glm(res, dat=C, ~ anonymized_id, ci.level = 0.95, boot.reps = 1000, cluster.se = T)

#####################

# For Figure 2
# Camps by terrain type - MEANS

C$Terrain <- factor(C$Terrain,c("Steppe","River","Mountain"))
summary(res <- glm(Inherited ~ Terrain-1,family=binomial,data=C))
predict(res,type="response",newdata=data.frame(Terrain=c("Steppe","River","Mountain")))
CI <- cluster.bs.glm(res, dat=C, ~ anonymized_id, ci.level = 0.95, boot.reps = 1000, cluster.se = T)
Ci <- matrix(unlist(CI[2]),3,2)
Ta <- data.frame(low=Ci[,1],mean=res$coefficients,high=Ci[,2])
Ta

#####################

# Camps by terrain type - CONTRASTS

C$Terrain <- factor(C$Terrain,c("Steppe","River","Mountain"))
summary(res <- glm(Inherited ~ Terrain,family=binomial,data=C))
predict(res,type="response",newdata=data.frame(Terrain=c("Steppe","River","Mountain")))
CI <- cluster.bs.glm(res, dat=C, ~ anonymized_id, ci.level = 0.95, boot.reps = 1000, cluster.se = T)

######

C$Terrain <- factor(C$Terrain,c("River","Mountain","Steppe"))
summary(res <- glm(Inherited ~ Terrain,family=binomial,data=C))
predict(res,type="response",newdata=data.frame(Terrain=c("Steppe","River","Mountain")))
CI <- cluster.bs.glm(res, dat=C, ~ anonymized_id, ci.level = 0.95, boot.reps = 1000, cluster.se = T)

######

C$Terrain <- factor(C$Terrain,c("Mountain","Steppe","River"))
summary(res <- glm(Inherited ~ Terrain,family=binomial,data=C))
predict(res,type="response",newdata=data.frame(Terrain=c("Steppe","River","Mountain")))
CI <- cluster.bs.glm(res, dat=C, ~ anonymized_id, ci.level = 0.95, boot.reps = 1000, cluster.se = T)

##############################################################################

# Make Figure 2 - Camp inheritance by season and terrain figure

dev.new(width=8,height=5)
par(mar=c(5.1,5.1,2.1,2.1))
mu <- c(0.851,0.841,0.815,0.714,NA,0.840,0.836,0.739)
bot <- 0.6
plot(0,0,col="white",ylim=c(bot,1),xlim=c(0.5,length(mu)+0.5),yaxs="i",xaxt="n",xlab=NA,ylab="Probability of Inheritance")
axis(1,at=c(1:4,6:8),labels=c("Fall","Winter","Spring","Summer","Mountain","River","Steppe"),tick=T)
axis(1,at=c(2.5,7),labels=c("Season","Terrain"),line=2,tick=F)
wi <- 0.2
above <- 0.025
for(i in 1:length(mu)) {
	polygon(c(i-wi,i+wi,i+wi,i-wi),c(bot,bot,mu[i],mu[i]),col="gray")
	text(i,mu[i]+above,round(mu[i],3),cex=0.8)
}
abline(0.7993,0,lty=2)
abline(v=5)
yi <- 0.94
lines(c(1,4),c(yi,yi))
lines(c(1,1),c(yi,yi-0.01))
lines(c(4,4),c(yi,yi-0.01))
text(2.5,yi+0.01,"*",cex=1.5)
yi <- 0.91
lines(c(2,4),c(yi,yi))
lines(c(2,2),c(yi,yi-0.01))
lines(c(4,4),c(yi,yi-0.01))
text(3,yi+0.01,"*",cex=1.5)
yi <- 0.91
lines(c(6,8),c(yi,yi))
lines(c(6,6),c(yi,yi-0.01))
lines(c(8,8),c(yi,yi-0.01))
text(7,yi+0.01,"*",cex=1.5)

#########################################################################

# Make Figure 3 - Inheritance by gender figure

dev.new(width=8,height=3)
par(mar=c(1.1,1.1,0.1,0.1))
plot(0,0,ylim=c(0,1.6),xlim=c(-0.25,1),col="white",xlab="",ylab="",axes=F)
wi <- 0.3
lab <- 0.2
to1 <- 1.2
ba <- -0.16
text(ba,to1-0.5*wi,"All Camps",font=2)
pst <- 0
nxt <- pst + 0.527
polygon(c(pst,nxt,nxt,pst),c(to1,to1,to1-wi,to1-wi),col="gray1")
text(0.5*(pst+nxt),to1-0.5*wi,"52.7%",col="white")
text(0.5*(pst+nxt),to1+ lab,"Inherited from\nmale's kin")
pst <- nxt
nxt <- pst + 0.272
polygon(c(pst,nxt,nxt,pst),c(to1,to1,to1-wi,to1-wi),col="gray40")
text(0.5*(pst+nxt),to1-0.5*wi,"27.2%",col="white")
text(0.5*(pst+nxt),to1+ lab,"Inherited from\nfemale's kin")
pst <- nxt
nxt <- pst + 0.116
polygon(c(pst,nxt,nxt,pst),c(to1,to1,to1-wi,to1-wi),col="gray55")
text(0.5*(pst+nxt),to1-0.5*wi,"11.6%",col="white")
text(0.5*(pst+nxt),to1+ lab,"Self-\nclaimed")
pst <- nxt
nxt <- pst + 0.085
polygon(c(pst,nxt,nxt,pst),c(to1,to1,to1-wi,to1-wi),col="gray65")
text(0.5*(pst+nxt),to1-0.5*wi,"8.5%",col="white")
text(0.5*(pst+nxt),to1+ lab,"New\ncamp")
polygon(c(0,1,1,0),c(to1,to1,to1-wi,to1-wi))
to1 <- wi
text(ba,to1-0.5*wi,"Inherited Camps",font=2)
pst <- 0
nxt <- pst + 0.527
polygon(c(pst,nxt,nxt,pst),c(to1,to1,to1-wi,to1-wi),col="gray1")
text(0.5*(pst+nxt),to1-0.5*wi,"66.0%",col="white")
text(0.5*(pst+nxt),to1+ lab,"Inherited from\nmale's kin")
pst <- nxt
nxt <- pst + 0.272
polygon(c(pst,nxt,nxt,pst),c(to1,to1,to1-wi,to1-wi),col="gray40")
text(0.5*(pst+nxt),to1-0.5*wi,"34.0%",col="white")
text(0.5*(pst+nxt),to1+ lab,"Inherited from\nfemale's kin")
nxt <- 0.527 + 0.272
polygon(c(0, nxt, nxt,0),c(to1,to1,to1-wi,to1-wi))

#######################################################################

# Figure 4 - Grass quality by inheritance

nrow(subset(C, !is.na(GrassQuality) & !is.na(Inherited)))
summary(res <- lm(scale(GrassQuality) ~ Inherited,C))
nd <- data.frame(Inherited=c(0,1))
ys <- predict(res, newdata=nd,interval="confidence")
ys

dev.new(width=4,height=4)
par(mar=c(5.1,5.1,2.1,2.1))

plot(c(-1,1),ys[,1],col='white',xlim=c(-2,2),ylim=c(-1,1),pch=16,xaxt="n",xlab="", ylab="Z(Grass quality)")
axis(1,c(-1,1),c("Not inherited","Inherited"))
abline(0,0,lty=2)
xwid <- 0.2
polygon(c(-1-xwid,-1-xwid,-1+xwid,-1+xwid),c(0,ys[1,1],ys[1,1],0),col="gray")
polygon(c(1-xwid,1-xwid,1+xwid,1+xwid),c(0,ys[2,1],ys[2,1],0),col="gray")


yi <- 0.6
vi <- 0.1
lines(c(-1,1),c(yi,yi))
lines(c(-1,-1),c(yi,yi-vi))
lines(c(1,1),c(yi,yi-vi))
text(0,yi+vi*1.5,"*",cex=1.5)

text(-1,-0.7,round(ys[1,1],3),cex=0.8)
text(1,0.3,round(ys[2,1],3),cex=0.8)

#######################################################################

# For Table 2 - Intergenerational transmission coefficient

summary(res <- lmer(log(F2_Bodo) ~ log(F1_Bodo) + (1| F1_anonymized_id), data=Z))
confint(res)

#######################################################################

# For Table 2 - Gini coefficient

names(A)
ls <-A$Bodo[!is.na(A$Bodo)]
Gini(ls,conf.level=0.95,R=10000)
length(ls)


##############################################################################

# Table 3 - Predictors of Bodo wealth 
names(A)
summary(res <- lm(log(Bodo)~log(MaterialWealth + 1) ,A))
confint(res)
summary(res <- lm(log(Bodo)~log(Education + 1),data=A))
confint(res)
summary(res <- lm(log(Bodo )~log(Wages + 1) ,A))
confint(res)
summary(res <- lm(log(Bodo )~scale(Ages,scale=F) ,A))
confint(res)
summary(res <- lm(log(Bodo )~MeanCampInheritance ,A))
confint(res)
summary(res <- lm(log(Bodo )~log(MaterialWealth + 1) + log(Education + 1) + log(Wages + 1) + scale(Ages,scale=F) +  MeanCampInheritance,A))
confint(res)

