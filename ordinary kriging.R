k <- gstat(formula=OZDLYAV~1, locations=aq, model=fve)

kp<- predict(k,g)
spplot(kp)

ok<- brick(kp)

ok<- mask(ok,ca)
names(ok)<-c('prediction','variance')
plot(ok)

#compare with other methodes
library(gstat)
idm<- gstat(formula =OZDLYAV~1, locations=aq )
idp<-interpolate(r,idm)
idp<-mask(idp,ca)
plot(idp)
