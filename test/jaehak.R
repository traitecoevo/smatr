 
morph <- read.csv("test/morph.csv")

library(smatr)

# Jae-hak problem plot
allo3<-sma(Mtibtar~Weight*Sex,log="xy",data=morph)
summary(allo3)

plot(allo3)                  ## Allometric line plot 

#>> this one
plot(allo3,which="residual",centered=F)
abline(h=0)

# same problem
morph$logMtibtar <- log10(morph$Mtibtar)
morph$logWeight <- log10(morph$Weight)
allo4<-sma(logMtibtar~logWeight*Sex,data=morph)
plot(allo4, "resid")

# same problem when not centering
obj <- allo3
Y <- fitted.sma(obj, type = "residuals")
X <- fitted.sma(obj, type = "fitted", centered=FALSE)
plot(X,Y)

