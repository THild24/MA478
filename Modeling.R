library(INLA)


formula1 <- observation ~ Unemployment + Wealth + ym_prop + month_number + 
  f(Block, model = "iid", hyper = list(prec=list(prior="loggamma", param=c(1,10^-5))))

model1 <- inla(formula1, family="poisson", 
               data=data_long, offset=log(Population),
               control.compute=list(dic=TRUE, config=TRUE, return.marginals.predictor=TRUE))
summary(model1)

formula2 <- observation ~ Wealth + ym_prop + month_number + 
  f(Block, model = "iid", hyper = list(prec=list(prior="loggamma", param=c(1,10^-5))))

model2 <-  inla(formula2, family="poisson", 
                data=data_long, offset=log(Population),
                control.compute=list(dic=TRUE, config=TRUE, return.marginals.predictor=TRUE))
summary(model2)

formula3 <- observation ~ Unemployment + Wealth + ym_prop + month_number + temp +
  f(Block, model = "iid", hyper = list(prec=list(prior="loggamma", param=c(1,10^-5))))
    
model3 <- inla(formula3, family="poisson", 
               data=data_long, offset=log(Population),
               control.compute=list(dic=TRUE, config=TRUE, return.marginals.predictor=TRUE))
summary(model3)

N.mat=readMM("neighborhood.mtx")

pairs<-triu(N.mat)
spat<-N.mat
neigh.hood.mat<-matrix(0,nrow=nrow(N.mat),ncol=ncol(N.mat))
diag(neigh.hood.mat)<-apply(N.mat,1,sum)

pairs<-summary(pairs)[summary(pairs)$x==1,]

model4 <- observation ~ Unemployment + Wealth + ym_prop + month_number + temp +
  f(Block, model = "bym", graph = loc.adj, hyper = list(prec=list(prior="loggamma", param=c(1,10^-5))))

summary(model4)

formula5 <- observation ~ Unemployment + Wealth + ym_prop + temp +
  f(Block, model = "iid", hyper = list(prec=list(prior="loggamma", param=c(1,10^-5)))) + 
  f(month_number, model = "ar1", hyper = list(prec=list(prior="loggamma", param=c(1,10^-5))))
model5 <- inla(formula5, family="poisson", 
               data=data_long, offset=log(Population),
               control.compute=list(dic=TRUE, config=TRUE, return.marginals.predictor=TRUE))
summary(model5)

formula6 <- observation ~ Unemployment + Wealth + temp +
  f(Block, model = "iid", hyper = list(prec=list(prior="loggamma", param=c(1,10^-5)))) + 
  f(month_number, model = "ar1", hyper = list(prec=list(prior="loggamma", param=c(1,10^-5))))

model6 <- inla(formula6, family="poisson", 
               data=data_long, offset=log(YoungMales),
               control.compute=list(dic=TRUE, config=TRUE, return.marginals.predictor=TRUE))
summary(model6)
