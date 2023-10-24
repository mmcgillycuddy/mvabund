library(mvabund)

data(spider)
spiddat <- log(spider$abund+1)
spiddat <- mvabund(spiddat)
spidx <- as.matrix(spider$x)

fit <- manylm( spiddat ~ (spidx[,"moss"]) ) 
fit0 <- manylm( spiddat ~ 1 )
an <- anova(fit, fit0, nBoot = 50, keep.boot = T)
an
# p-value given 
an$table[2,4]  == 1/ (50 + 1)

#Calculate from keep boot
str(an)

# p = (#exceeding observed stat + 1)/(#nboot+1)

(sum(an$bootStat > an$table[2,3] ) + 1 )/(50 + 1)
  


set.seed(99)
data(iris)
Y = with(iris, cbind(Sepal.Length,Sepal.Width,Petal.Length,Petal.Width))
iris_mlm=manylm(Y~Species,data=iris)
iris_an <- anova(iris_mlm, nBoot = 100, keep.boot = T)

iris_an$bootStat
# p-value given 
iris_an$table[2,4]  == 1/ (100 + 1)

(sum(iris_an$bootStat > iris_an$table[2,3] ) + 1 )/(100 + 1)



data(spider)
spiddat <- log(spider$abund+1)
spiddat <- mvabund(spiddat)
spidx <- as.matrix(spider$x)
fit <- manylm( spiddat ~ (spidx) ) 
fit0 <- manylm( spiddat ~ (spidx[,1:5]) )
spidan <-anova(fit, fit0)

pan_spid <-pboot <- rep(NA, length(nboots))
for(i in 1:length(nboots)){
  spidan <-anova(fit, fit0, nBoot = nboots[i], keep.boot = T)
  pan_spid[i] <- spidan$table[[4]][2]
  pboot[i] <- (sum(spidan$bootStat > spidan$table[2,3] ) + 1 )/(nboots[i] + 1) #bug in code (should be + 1 instead of +2)
  
}
pboot == pan_spid 

