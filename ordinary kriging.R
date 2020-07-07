k <- gstat(formula=OZDLYAV~1, locations=aq, model=fve)

kp<- predict(k,g)
spplot(kp)

ok<- brick(kp)

ok<- mask(ok,ca)
names(ok)<-c('prediction','variance')
plot(ok)
