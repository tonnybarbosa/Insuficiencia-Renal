require(survival)

dados2<-read.table(file.choose(), header = TRUE,sep=" ")

head(dados2)
t<-dados2$tempo
n=length(t)
hist(t,xlab="Tempo de sobrevivencia em dias", ylab="Frequencia de pacientes",main="")

censura<-dados2$status
linha<-factor(dados2$linha)

propatraso<-dados2$propatraso
comprimido<-dados2$comprimdia

KM<-survfit(Surv(t,censura)~1, conf.int=F)
summary(KM)
plot(KM,conf.int=F, xlab="Tempo", ylab="S(t)")

KMe<-survfit(Surv(t,censura)~linha, conf.int=F)
summary(KMe)
plot(KMe,conf.int=F, xlab="Tempo", ylab="S(t)",lty=c(2,3,4),col=c(2,3,4))
legend("topright",lty=c(2,3,4), cex=0.75,c("Falha1","Falha2","Falha3"),col=c(2,3,4))


survdiff(Surv(t,censura)~linha, rho=1)

#Funcao risco acumulada
plot(KM,conf.int=F, fun="cumhaz", xlab="Tempo", ylab="H(t)")

#TTT plot#
require(AdequacyModel)
TTT(t, col="red", lwd=2.5, grid=TRUE, lty=2)

dad2<-data.frame(t,linha)
tempo1<-dad2[dad2$linha == "1",]
TTT(tempo1$t, col="red", lwd=2.5, grid=TRUE, lty=2)
tempo2<-dad2[dad2$linha == "2",]
TTT(tempo2$t, col="red", lwd=2.5, grid=TRUE, lty=2)
tempo3<-dad2[dad2$linha == "3",]
TTT(tempo3$t, col="red", lwd=2.5, grid=TRUE, lty=2)

