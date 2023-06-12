library("compositions")
library("here")

# data from unpublished article by Christine-Thomas et al (2023)
load(here("data-raw/data.ILR.Reg.RDATA"))
#############


#les 15 bins originaux
#Bin1=[-6,14.4]; Bin2=[14.4,22.1], Bin3=[22.1,25.1],Bin4=[25.1,27.0],Bin5=[27.0,28.3],Bin6=[28.3,29.3]
#Bin7=[29.3,30.1], Bin8=[30.1,30.8], Bin 9=[30.8,31.4], BIn10=[31.4,32.0], BIn11=[32.0,32.7], Bin12=[32.7,33.7]
#Bin13=[33.7,35.4], Bin14=[35.4,44.5], Bin15=[44.5,45]

#modele avec 3 bins
#Bin1=[-6,25.1], Bin2=[25.1,35.4], Bin3=[35.4,45]
nv1=data.ILR.Reg$s1+data.ILR.Reg$s2+data.ILR.Reg$s3
nv2=data.ILR.Reg$s4+data.ILR.Reg$s5+data.ILR.Reg$s6+ data.ILR.Reg$s7+data.ILR.Reg$s8+data.ILR.Reg$s9+ data.ILR.Reg$s10+data.ILR.Reg$s11+data.ILR.Reg$s12+data.ILR.Reg$s13
nv3=data.ILR.Reg$s14+data.ILR.Reg$s15
data.ILR.Reg$nv1=nv1
data.ILR.Reg$nv2=nv2
data.ILR.Reg$nv3=nv3
data.ILR.Reg$histc=clo(data.ILR.Reg,parts=c(28,29,30))
data.ILR.Reg$year_num=as.numeric(data.ILR.Reg$year)
colnames(data.ILR.Reg$histc)=c("Faibles","Moyennes","Fortes")


rice_yields <- data.ILR.Reg

# select
remove_cols <- c(
  "year", "year_conti",
  paste0("s", 1:15), paste0("nv", 1:3),
  colnames(data.ILR.Reg)[5:10])
rice_yields[remove_cols] <- NULL

# re-code
reg_sa <- rice_yields$Region == " Southeastern Area"
rice_yields$Region[reg_sa] <- "Southeastern Area"
rice_yields$Region <- factor(rice_yields$Region)
rice_yields$province <- factor(rice_yields$province)

# rename
rename_cols <- c(
  "province" = "PROVINCE",
  "Region" = "REGION",
  "year_num" = "YEAR",
  "YIELDF" = "YIELD",
  "Precipitation" = "PRECIPITATION",
  "histc" = "TEMPERATURES")
rice_yields <- rice_yields[,names(rename_cols)]
names(rice_yields) <- rename_cols
colnames(rice_yields$TEMPERATURES) <- c("LOW","MIDDLE","HIGH")

# export
usethis::use_data(rice_yields,overwrite = TRUE)

stop("Do not run explortation code...")
#exploratoire
library(ggtern)
library(RColorBrewer)
library(classInt)
library(plotfunctions)
pdf("climate_rice_triangle.pdf", width = 6, height = 6) # enregistre la figure en pdf dans ton workin directory qui est : getwd()
par(oma = c(0, 0, 0, 0), mar = c(2.5, 1, 4, 2)) # g?re les marges  : 2.5 = bas, 1 = gauche, 3 = haut, 2 = droite

A <- c(0, 0)
B <- c(1, 0)
C <- c(0.5, sqrt(3)/2)

my.palette <- brewer.pal(n = 9, name = "OrRd")
breaks_ilr1 <- classIntervals(data.ILR.Reg$YIELDF[1:100], n = 9,
                              style = "quantile", intervalClosure = "left")
simplex_x <- data.ILR.Reg$histc[, 1] * A[1] +
  data.ILR.Reg$histc[, 2] * B[1] + data.ILR.Reg$histc[, 3] * C[1]
simplex_y <- data.ILR.Reg$histc[, 1] * A[2] +
  data.ILR.Reg$histc[, 2] * B[2] + data.ILR.Reg$histc[, 3] * C[2]

plot(rbind(A, B, C), xaxt = "n", yaxt = "n", frame = F,
     type = "n", xlab = "", ylab = "", asp = 1,
     ylim = c(-0.1, sqrt(3)/2), cex.main = 2)

text(c(0, 1, 1/2+0.07), c(0-0.06, -0.06, sqrt(3)/2-0.05),
     c("Faibles", "Moyennes", "Fortes"), pos = 3, cex = 0.8)
lines(c(0, 1), c(0, 0), col = "black")
lines(c(0, 0.5), c(0, sqrt(3)/2), col = "black")
lines(c(1, 0.5), c(0, sqrt(3)/2), col = "black")
points(simplex_x, simplex_y, pch = 16,
       col = my.palette[findCols(breaks_ilr1)], cex = 1)

lines(c(0.5, 0.5), c(0, 0.2886751), lty = 2, col = "darkgrey")
lines(c(0.25, 0.5), c(0.4330127, 0.2886751), lty = 2, col = "darkgrey")
lines(c(0.75, 0.5), c(0.4330127, 0.2886751), lty = 2, col = "darkgrey")

gradientLegend(data.ILR.Reg$YIELDF[1:100],col=my.palette[sort(findCols(breaks_ilr1))],
               nCol = 9, dec = 1, length =0.4, depth = 0.1, inside = TRUE,
               fit.margin = TRUE)

dev.off()


#regression
mod_0=lm(YIELDF~year_num+ Precipitation+ReSoutheasternArea+ReMekongDelta+ReMidNorthMoutain+ReNorCoastaCentral+ReCentralHighlands,data=data.ILR.Reg)

mod_compo=lm(YIELDF~ilr(histc),data=data.ILR.Reg)
summary(mod_compo)
ilrBase(D=3)# 1er ilr oppose tmax low ? tmax medium et
#2eme ilr oppose tmax low et medium ? tmax High
anova(mod_compo)
b=ilrInv(coef(mod_compo)[-1],orig=data.ILR.Reg$histc)
b=acomp(b[1:3])
a = mod_compo$coefficients[1]
#plot de la droite de regression: enlever les droites ortho

plot(acomp(data.ILR.Reg$histc))
straight(mean(acomp(data.ILR.Reg$histc)),b,lwd=2,col="red",lty=2)
myY=pretty(data.ILR.Reg$YIELDF)
myY=seq(3,5,length.out=7)
refX= mean(acomp(data.ILR.Reg$histc))+ ((myY-a)/norm(b)^2)*b
plot(refX,add=TRUE,pch=19)
orthoComp=function(x) { ilrInv(ilr(x) %*%
                                 matrix(c(0, 1, -1, 0), ncol = 2)) }
mygreyscale = grey((0:nrow(refX))/nrow(refX))
for (i in 1:nrow(refX)) {straight(acomp(refX[i, ]), orthoComp(b),
             col = mygreyscale[i],lwd = 2)}
##ellipses de confiance
colnames(data.ILR.Reg$histc)=c("Low","Medium","High")
varb=ilrvar2clr(vcov(mod_compo)[-1,-1])
par(mar=c(3,3,0,1))
names(b)=colnames(data.ILR.Reg$histc)
scaleB=4
plot(scaleB*b)
plot(0*b,add=TRUE,pch=20)
alpha=0.05
rF=sqrt(qf(1-alpha,nrow(varb)-1,mod_compo$df))
ellipses(scaleB*b,scaleB^2*varb,rF)
#meme chose mais avec year_num et precipitation
#le plot de la droite de regression est etrange
mod_compo2=lm(YIELDF~ilr(histc)+year_num+ Precipitation,data=data.ILR.Reg)
summary(mod_compo2)
anova(mod_compo2)
a2 = mod_compo2$coefficients[1]
b2=ilrInv(mod_compo2$coefficients[2:3],orig=data.ILR.Reg$histc)
plot(acomp(data.ILR.Reg$histc))
straight(mean(acomp(data.ILR.Reg$histc)),b2,lwd=2,col="red",lty=2)
myY=pretty(data.ILR.Reg$YIELDF)
refX= mean(acomp(data.ILR.Reg$histc))+ ((myY-a2)/norm(b2)^2)*b2
plot(refX,add=TRUE,pch=19)
orthoComp=function(x) { ilrInv(ilr(x) %*%
                                 matrix(c(0, 1, -1, 0), ncol = 2)) }
mygreyscale = grey((0:nrow(refX))/nrow(refX))
for (i in 1:nrow(refX)) {straight(acomp(refX[i, ]), orthoComp(b2),
                                  col = mygreyscale[i],lwd = 2)}
##ellipses de confiance avec mod-comp2
colnames(data.ILR.Reg$histc)=c("Low","Medium","High")
varb2=ilrvar2clr(vcov(mod_compo2)[2:3,2:3])
par(mar=c(3,3,0,1))
names(b)=colnames(data.ILR.Reg$histc)
scaleB=4
plot(scaleB*b2)
plot(0*b2,add=TRUE,pch=20)
alpha=0.05
rF=sqrt(qf(1-alpha,nrow(varb2)-1,mod_compo2$df))
ellipses(scaleB*b2,scaleB^2*varb2,rF)

#meme chose mais avec en plus l'effet region
mod_compo3=lm(YIELDF~ilr(histc)+year_num+ Precipitation+ReSoutheasternArea+ReMekongDelta+ReMidNorthMoutain+ReNorCoastaCentral+ReCentralHighlands,data=data.ILR.Reg)
summary(mod_compo3)
anova(mod_compo3)
a3 = mod_compo3$coefficients[1]
b3=ilrInv(mod_compo3$coefficients[2:3],orig=data.ILR.Reg$histc)
#plot de la droite de régression pour mod_compo3
plot(acomp(data.ILR.Reg$histc))
straight(mean(acomp(data.ILR.Reg$histc)),b3,lwd=2,col="red",lty=2)
myY=pretty(data.ILR.Reg$YIELDF)
refX= mean(acomp(data.ILR.Reg$histc))+ ((myY-a3)/norm(b3)^2)*b3
plot(refX,add=TRUE,pch=19)
orthoComp=function(x) { ilrInv(ilr(x) %*%
                                 matrix(c(0, 1, -1, 0), ncol = 2)) }
mygreyscale = grey((0:nrow(refX))/nrow(refX))
for (i in 1:nrow(refX)) {straight(acomp(refX[i, ]), orthoComp(b3),
                                  col = mygreyscale[i],lwd = 2)}
##ellipses de confiance avec mod-comp3
#colnames(data.ILR.Reg$histc)=c("Low","Medium","High")
varb3=ilrvar2clr(vcov(mod_compo3)[2:3,2:3])
par(mar=c(3,3,0,1))
names(b3)=colnames(data.ILR.Reg$histc)
scaleB=4
plot(scaleB*b3)
plot(0*b3,add=TRUE,pch=20)
alpha=0.05
rF=sqrt(qf(1-alpha,nrow(varb3)-1,mod_compo3$df))
ellipses(scaleB*b3,scaleB^2*varb3,rF)
#figure 5.13 page 141, plot observed against predicted
#pour mod_compo2
opar=par(mar=c(3,3,0,0))
predicted=predict(mod_compo2)
plot(predicted,data.ILR.Reg$YIELDF,cex=0.2)
abline(0,1)
par(opar)
#pour mod_compo3
opar=par(mar=c(3,3,0,0))
predicted3=predict(mod_compo3)
plot(predicted3,data.ILR.Reg$YIELDF,cex=0.2)
abline(0,1)
par(opar)
#qualit? du mod?le autre que R carre
cor(data.ILR.Reg$YIELDF,predicted3,method = "spearman")
#barplot des clr de b
barplot(as.numeric(compositions::clr(b3)),names.arg=colnames(data.ILR.Reg$histc))

#et si on permute l'ordre des variables pour la table d'anova
#etrange: ca change l'anova (attendu) mais aussi les coefs des ilr ??
mod_compo3perm=lm(YIELDF~year_num+ Precipitation+ReSoutheasternArea+ReMekongDelta+ReMidNorthMoutain+ReNorCoastaCentral+ReCentralHighlands+ilr(histc),data=data.ILR.Reg)
summary(mod_compo3perm)
anova(mod_compo3perm)
anova(mod_0,mod_compo3)

#avec tous les 15 bins  ?
data.ILR.Reg$histc15=clo(data.ILR.Reg,parts=c(11,12,13,14,15,16,17,18,19,20,21,22,23,24,25))#ne marche pas ??
data.ILR.Reg$parts15=data.ILR.Reg[,11:25]
data.ILR.Reg$histc15=clo(data.ILR.Reg$parts15)

mod_compo15=lm(YIELDF~ilr(histc15)+year_num+ Precipitation+ReSoutheasternArea+ReMekongDelta+ReMidNorthMoutain+ReNorCoastaCentral+ReCentralHighlands,data=data.ILR.Reg)
summary(mod_compo15)
anova(mod_compo15)
a15 = mod_compo15$coefficients[1]
b15=ilrInv(mod_compo15$coefficients[2:16],orig=data.ILR.Reg$histc15)
barplot(as.numeric(compositions::clr(b15)))


##avec 4 bins, pas concluant
data.ILR.Reg$nw1=data.ILR.Reg$s1+data.ILR.Reg$s2+data.ILR.Reg$s3 + data.ILR.Reg$s4+data.ILR.Reg$s5+data.ILR.Reg$s6+ data.ILR.Reg$s7
data.ILR.Reg$nw2=data.ILR.Reg$s8+ data.ILR.Reg$s9
data.ILR.Reg$nw3=data.ILR.Reg$s10+data.ILR.Reg$s11+data.ILR.Reg$s12+data.ILR.Reg$s13
data.ILR.Reg$nw4=data.ILR.Reg$s14+data.ILR.Reg$s15


data.ILR.Reg$parts4=data.ILR.Reg[,31:34]
data.ILR.Reg$histc4=clo(data.ILR.Reg$parts4)

mod_compo4=lm(YIELDF~ilr(histc4)+year_num+ Precipitation+ReSoutheasternArea+ReMekongDelta+ReMidNorthMoutain+ReNorCoastaCentral+ReCentralHighlands,data=data.ILR.Reg)
summary(mod_compo4)
anova(mod_compo4)
a4 = mod_compo4$coefficients[1]
b4=ilrInv(mod_compo4$coefficients[2:4],orig=data.ILR.Reg$histc4)
barplot(as.numeric(compositions::clr(b4)))

#comparaison de modèles
mod_simple=lm(YIELDF~year_num+ Precipitation+ReSoutheasternArea+ReMekongDelta+ReMidNorthMoutain+ReNorCoastaCentral+ReCentralHighlands,data=data.ILR.Reg)
