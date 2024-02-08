dados <- read.csv('C:/Users/tonny/Downloads/dialise.csv')
head(dados)
require(survival)
require(AdequacyModel)

attach(dados)

dados$hip=as.numeric(dados$causa=="hip")
dados$out=as.numeric(dados$causa=="out")



table(idade,tempo)

summary(dados$idade)
dados$faixas<-cut(dados$idade,breaks = c(0,2,30,60,98),include.lowest = T,labels = c("0 - 1","1 - 30","30 - 60","60 - 98"))

attach(dados)
head(dados)
hist(tempo,xlab="Tempo de sobrevivencia em dias", ylab="Frequencia de pacientes",
     main="Distribuição do tempo de sobrevivência 
dos pacientes",ylim=c(0,2500))

KM<-survfit(Surv(tempo,status)~1,conf.int=T)
summary(KM)
plot(KM,conf.int=F, xlab="Tempo", ylab="S(t)", mark.time = T,col=c(1,2,2)
     ,main="Função de Sobrevivência")

plot(KM,conf.int=F, fun="cumhaz", mark.time = T,xlab="Tempo", ylab="H(t)"
     ,main="Função de risco acumulada",col=c(1,2,2))

summary(KM)

###S(t) por grupo###

KMfaixa<-survfit(Surv(tempo,status)~faixas, conf.int=F)
plot(KMfaixa,conf.int=F, xlab="Tempo", ylab="S(t)", col=c(1,2,3,4),mark.time = F
     ,main="Idade")
legend(5,0.5,lty=c(1,1,1,1),col=c(1,2,3,4),c("0 a 1 ano","2 a 30 anos","30 a 60 anos","60 anos ou mais"), bty="n",cex=1)
text(5,0,"p-valor = 2e-16",cex=0.6)

survdiff(Surv(tempo,status)~faixas, rho=0)

KMg<-survfit(Surv(tempo,status)~grande, conf.int=F)
plot(KMg,conf.int=F, xlab="Tempo", ylab="S(t)", col=c(2:3),mark.time = F
     ,main="Número de salas de diálise na unidade de tratamento")
legend(5,0.5,lty=c(1,1,1),col=c(2,3),c("uma ou duas salas","três salas ou mais"), bty="n",cex=1)
text(5,0,"p-valor = 0.0002",cex=0.6)

survdiff(Surv(tempo,status)~grande, rho=0)

KMcausa<-survfit(Surv(tempo,status)~causa, conf.int=F)
plot(KMcausa,conf.int=F, xlab="Tempo", ylab="S(t)", col=c(2:6),mark.time = F
     ,main="Causa da insuficiência renal")
legend(5,0.5,lty=c(1,1,1,1,1),col=c(2:6),c("hipertensão","diabetes","renal",
                                           "congênita","outras"), bty="n",cex=0.8)
text(5,0,"p-valor = 2e-13",cex=0.6)

survdiff(Surv(tempo,status)~causa, rho=1)

KMcdiab<-survfit(Surv(tempo,status)~cdiab, conf.int=F)
plot(KMcdiab,conf.int=F, xlab="Tempo", ylab="S(t)", col=c(2:3),mark.time = F
     ,main="Diabetes")
legend(5,0.5,lty=c(1,1,1),col=c(2,3),c("Não","Sim"), bty="n",cex=1)
text(5,0,"p-valor = 2e-15",cex=0.6)

survdiff(Surv(tempo,status)~cdiab, rho=0)

KMcrim<-survfit(Surv(tempo,status)~crim, conf.int=F)
plot(KMcrim,conf.int=F, xlab="Tempo", ylab="S(t)", col=c(2:3),mark.time = F
     ,main="Causas renais")
legend(5,0.5,lty=c(1,1,1),col=c(2,3),c("Não","Sim"), bty="n",cex=1)
text(5,0,"p-valor = 0.02",cex=0.6)

survdiff(Surv(tempo,status)~crim, rho=0)

KMcongenita<-survfit(Surv(tempo,status)~congenita, conf.int=F)
plot(KMcongenita,conf.int=F, xlab="Tempo", ylab="S(t)", col=c(2:3),mark.time = F
     ,main="Causas congênitas")
legend(5,0.5,lty=c(1,1,1),col=c(2,3),c("Não","Sim"), bty="n",cex=1)
text(5,0,"p-valor = 0.002",cex=0.6)

survdiff(Surv(tempo,status)~congenita, rho=0)

KMhip<-survfit(Surv(tempo,status)~hip, conf.int=F)
plot(KMhip,conf.int=F, xlab="Tempo", ylab="S(t)", col=c(2:3),mark.time = F
     ,main="Hipertensão")
legend(5,0.5,lty=c(1,1,1),col=c(2,3),c("Não","Sim"), bty="n",cex=1)
text(5,0,"p-valor = 0.03",cex=0.6)

survdiff(Surv(tempo,status)~hip, rho=0)

KMout<-survfit(Surv(tempo,status)~out, conf.int=F)
plot(KMout,conf.int=F, xlab="Tempo", ylab="S(t)", col=c(2:3),mark.time = F
     ,main="Outras causas")
legend(5,0.5,lty=c(1,1,1),col=c(2,3),c("Não","Sim"), bty="n",cex=1)
text(5,0,"p-valor = 0.3",cex=0.6)

survdiff(Surv(tempo,status)~out, rho=1)

TTT(tempo, col="red", lwd=2, grid=TRUE, lty=2)
title("Tempo total em teste (TTT)")

dad2<-data.frame(tempo,cdiab)
tempo1<-dad2[dad2$cdiab == "0",]
TTT(tempo1$tempo, col="red", lwd=2.5, grid=TRUE, lty=2)
tempo2<-dad2[dad2$cdiab == "1",]
TTT(tempo2$tempo, col="red", lwd=2.5, grid=TRUE, lty=2)


n=length(tempo)
mexp<-survreg(Surv(tempo,status)~1, dist="exponential")
mexp
summary(mexp)

alpha<-exp(mexp$coefficients[1])
alpha

mwe<-survreg(Surv(tempo,status)~1, dist="weibull")
mwe
summary(mwe)

alphaw<-exp(mwe$coefficients[1])
alphaw

gamaw<-1/mwe$scale
gamaw


pws<-2
AICws<-(-2*mwe$loglik[1])+(2*pws)  

AICcws<-AICws + ((2*pws*(pws+1))/(n-pws-1))

BICws<-(-2*mwe$loglik[1]) + pws*log(n)

medidasw<-cbind(AICws,AICcws,BICws)
medidasw

#### Dist. Log-normal ##

mlognorm<-survreg(Surv(tempo,status)~1, dist='lognorm')
mlognorm

mi<-mlognorm$coefficients[1]
sigma<-mlognorm$scale

pexp <- 1
AICexp <- (-2*mexp$loglik[1])+(2*pexp)
AICcexp<-AICexp + ((2*pexp*(pexp+1))/(n-pexp-1))

BIClns<-(-2*mexp$loglik[1]) + pexp*log(n)

medidasexp <- cbind(AICexp,AICcexp,BIClns)
medidasexp

plns<-2
AIClns<-(-2*mlognorm$loglik[1])+(2*plns)

AICclns<-AIClns + ((2*pws*(pws+1))/(n-plns-1))

BIClns<-(-2*mlognorm$loglik[1]) + plns*log(n)

medidasln<-cbind(AIClns,AICclns,BIClns)
medidasln

#### Dist. Log-logistica ##

mloglogi<-survreg(Surv(tempo,status)~1, dist='loglogistic')
summary(mloglogi)

alphall<-exp(mloglogi$coefficients[1])
alphall

gamall<- 1/mloglogi$scale
gamall

plls<-2
AIClls<-(-2*mloglogi$loglik[1])+(2*plls)

AICclls<-AIClls + ((2*pws*(pws+1))/(n-plls-1))

BIClls<-(-2*mloglogi$loglik[1]) + plls*log(n)

medidasll<-cbind(AIClls,AICclls,BIClls)
medidasll

dframe <- data.frame(t(medidasexp),t(medidasw),t(medidasll),t(medidasln))
fix(dframe)
View(dframe)

km<-survfit(Surv(tempo,status)~1)
time<-km$time 

skm<-km$surv

sexp <- exp(-time/alpha)

swe<-exp(-(time/alphaw)^gamaw)

slognorm<-pnorm((-log(time)+mi)/sigma)

sloglogi<-1/(1+(time/alphall)^gamall) 

plot(km,conf.int=F, xlab="Tempos", ylab="S(t)",mark.time = F)
lines(c(0,time),c(1,sexp),lty=1,col=2)
legend(10,0.6,lty=c(1,1),col=c(1,2),c("Kaplan-Meier","Exponencial"),bty="n",cex=0.8)

lines(c(0,time),c(1,swe),lty=1,col=5)
legend(10,0.4,lty=c(1,1),col=c(5),c("Weibull"),bty="n",cex=0.8)
lines(c(0,time),c(1,slognorm),lty=1,col=4)
legend(10,0.3,lty=1,col=4,c("Log-Normal"),bty="n",cex=0.8)
lines(c(0,time),c(1,sloglogi),lty=1,col="green")
legend(10,0.2,lty=1,col="green",c("Log-Logisitca"),bty="n",cex=0.8)
title("Ajuste dos modelos")

#######################################################################

#Modelo de Regressao - normal#
y<-log(tempo)
mod<-survreg(Surv(y,status)~1, dist='gaussian')  
summary(mod)

## Selecao de covariavel ##

mod1<-survreg(Surv(y,status)~hip, dist='gaussian')  
summary(mod1)

mod2<-survreg(Surv(y,status)~grande, dist='gaussian')  
summary(mod2)

mod3<-survreg(Surv(y,status)~cdiab, dist='gaussian')  
summary(mod3)

mod4<-survreg(Surv(y,status)~crim, dist='gaussian')  
summary(mod4)

mod5<-survreg(Surv(y,status)~congenita, dist='gaussian')  
summary(mod5)

mod6<-survreg(Surv(y,status)~out, dist='gaussian')  
summary(mod6)

#Passo 2 - modelo com todas as covariaveis significativas no passo 1
mod12345<-survreg(Surv(y,status)~grande+cdiab+crim+congenita+hip, dist='gaussian')  
summary(mod12345)

mod1234<-survreg(Surv(y,status)~grande+cdiab+congenita+crim, dist='gaussian')  
summary(mod1234)

mod123<-survreg(Surv(y,status)~grande+cdiab+congenita, dist='gaussian')  
summary(mod123)

#Passo 3 - retirar covs significativas uma a uma, para verificar se realemente sao significativas
mod12<-survreg(Surv(y,status)~grande+cdiab, dist='gaussian')  
summary(mod12)

mod13<-survreg(Surv(y,status)~grande+congenita, dist='gaussian')  
summary(mod13)


mod23<-survreg(Surv(y,status)~cdiab+congenita, dist='gaussian')  
summary(mod23)

#Modelo Final : modelo 4 - aqui pode-se verificar inclusao de termos de interacao dupla

mod123_1<-survreg(Surv(y,status)~grande+cdiab+congenita+grande*congenita, dist='gaussian')  
summary(mod123_1)


############################################3

model.matrix(mod123_1)
x2 <- model.matrix(mod123_1)[,2]
x3 <- model.matrix(mod123_1)[,3]
x4 <- model.matrix(mod123_1)[,4]
x5 <- model.matrix(mod123_1)[,5]

mod123_1$coeff[1]

mi <- mod123_1$coeff[1] + mod123_1$coeff[2]*x2 + mod123_1$coeff[3]*x3 +
  mod123_1$coeff[4]*x4 + mod123_1$coeff[5]*x5

mip <- mod123_1$linear.predictors

Smod <- 1-pnorm((y-mip)/mod123_1$scale)
ei <- (-log(Smod))

KMew <- survfit(Surv(ei,status)~1,conf.int=F)
te <- KMew$time
ste <- KMew$surv
sexp <- exp(-te)
summary(KMew)

plot(ste,sexp, xlab="S(ei):Kaplan-Meier", ylab="S(ei):Exponencial padrao")
abline(0,1,col="red",lty=1,lwd=2)
title()
plot(KMew,conf.int=F, xlab="Resíduos de Cox-Snell", ylab="Sobrevivencia estimada")
lines(te,sexp,lty=2, col=2)
legend(0.1,0.5,lty=c(1,2), col=c(1,2),c("Kaplan-Meier", "Exponencial padrao"), cex=0.8, bty="n")

##########

martingal <- status-ei

par(mfrow=c(1,2))
plot(y,martingal,xlab="log(tempo)", ylab="Residuo Martingal",pch=censura+1)
plot(rank(y),martingal,xlab="rank das observacoes", ylab="Residuo Martingal",pch=censura+1)
title("Martingal")
par(mfrow=c(2,2))
plot(cdiab,martingal,xlab="Diabetes", ylab="Residuo Martingal",pch=censura+1)
plot(congenita,martingal,xlab="Congênitas", ylab="Residuo Martingal",pch=censura+1)
plot(crim,martingal,xlab="Renais", ylab="Residuo Martingal",pch=censura+1)
plot(factor(hip),martingal,xlab="Hipertensão", ylab="Residuo Martingal",pch=censura+1)

###########################
par(mfrow=c(1,2))
devw<-(martingal/abs(martingal))*(-2*(martingal+status*log(status-martingal)))^(1/2)
plot(y,devw,xlab="log(tempo)", ylab="Residuo Deviance",pch=censura+1)
plot(rank(y),devw,xlab="rank das observacoes", ylab="Residuo Deviance",pch=censura+1)
par(mfrow=c(1,1))

fb2 <- basehaz(modcox,centered=F)
modcox <- coxph(Surv(tempo,status)~idade)
summary(modcox)
beta1 <- modcox$coefficients[1]

temp2 <- fb2$time
h02 <- fb2$hazard
s02 <- exp(-h02)

sg0 <- s02^exp(beta1*0)
sg1 <- s02^exp(beta1*30)
sg2 <- s02^exp(beta1*60)
sg3 <- s02^exp(beta1*90)

plot(temp2,sg1,type='l',xlab="Tempo",ylab="Função de Sobrevivência Estimada",lwd=1,ylim=c(0,1))
lines(temp2,sg0,col=2,lty=1)
lines(temp2,sg2,,col=3,lty=1)
lines(temp2,sg3,,col=4,lty=1)

legend(0,0.5,lty=c(1,1,1,1),bty="n",col=c(2,1,3,4),c("0 anos","30 anos","60 anos","90 anos"))
title("Idade")
