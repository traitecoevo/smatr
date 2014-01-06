load(file="test/sample.RData") # data
mSMA<- sma(var ~ size * group, robust=TRUE, data=data)

dResid.sma<- data.frame() # manual results

# Calculate residuals manually for each group
for (g in 1:length(mSMA$groups)){
  dG<- data[data$group %in% mSMA$groups[g],]
  coeficients<- coef(mSMA)[g,]
  dG$resid<- dG$var - (coeficients$elevation + dG$size * coeficients$slope)
  
  dResid.sma<- rbind(dResid.sma, data.frame(species=dG$species, resid=dG$resid))
}

# Check results
table(na.omit(dResid.sma$resid) %in% resid(mSMA))
# Many FALSES which means that manual residuals are different from resid.sma() whithout taking in to account the order of the values

plot(sort(na.omit(dResid.sma$resid)), sort(resid(mSMA)))
abline(0,1, col="red")
#All points should be in the red line



gn <- table(data$group)
data2 <- subset(data, group %in% names(gn)[gn > 3])
sma1 <- sma(var ~ size*group, data=data2)
sma2 <- sma(var ~ size*group, data=data2, robust=T)

cdf <- as.data.frame(coef(sma2))
cdf$group <- rownames(cdf)
data2$R1 <- resid(sma2)


data2 <- merge(data2, cdf, by="group", sort=FALSE)
data2$R2 <- with(data2, var - (elevation + slope*size))


plot(data2$R, resid(sma2))


sma3 <- sma(var ~ size, data=data2)








