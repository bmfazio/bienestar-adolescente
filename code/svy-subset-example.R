data(fpc)

# Forma incorrecta de seleccionar subconjunto de datos:
dfpc2<-svydesign(id=~psuid,strat=~stratid,weight=~weight,data=fpc[fpc$x>4,],nest=TRUE)
# Forma correcta de hacerlo:
dfpc<-svydesign(id=~psuid,strat=~stratid,weight=~weight,data=fpc,nest=TRUE)
dsub<-subset(dfpc,x>4)

# Impacta en la estimacion de la varianza
svymean(~x,design=dfpc2)
svymean(~x,design=dsub)

# Formas correctas equivalentes:
svyby(~x, ~I(x>4), design = dfpc, svymean)
summary(svyglm(x~I(x>4)+0, design = dfpc))