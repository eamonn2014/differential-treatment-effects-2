
#https://stackoverflow.com/questions/57944333/linear-regression-simulation
#https://stackoverflow.com/questions/26955334/how-to-create-a-factor-interaction-variable-in-r-why-cant-i-just-multiply

# N=4000 # population size
# n=4000 # sample size   
# 
# race=sample(as.factor(c(rep("white",.8*N), rep("minority",.2*N))),n, replace=T)
# 
# location=sample(as.factor(c(rep("A",.25*N), rep("B",.25*N), rep("C",.25*N), rep("D",.25*N))),n, replace=T)
# 
# (X = as.matrix(model.matrix(~race*location))) # take a look .. nrow columns -> nrow effects
# 
# colnames(X) # show effect names
# # Choose effects
# int <- 12 # Intercept
# race.effects <- c(5) # 1 df -> one effect
# location.effects <- c(3,4,5) # 3 df -> three effects
# interaction.effects <- c(15, 20, 4) # 1*3 df -> three Interaction effects, not necessarily multiplicative
# all.effects <- c(int, race.effects, location.effects, interaction.effects)
# sigma <- 3
# res <- rnorm(n, 0, sigma) # Residuals
# y <- as.numeric(as.matrix(X) %*% as.matrix(all.effects) + res) # multiply data
# lm1 <- lm(y ~ race*location)
# summary(lm1)


rm(list=ls())
require(rms) 
library(forestplot)
p2 <- function(x) {formatC(x, format="f", digits=2)}
set.seed(123)
N=10000 # population size
N=400
#~~~~~~~~~~~~~~~~~
age      <- sample(18:65, N, replace=TRUE) 

#~~~~~~~~~~~~~~~~~
x<-rexp(51)
x<- exp(-.45*1:51)
FASSS      <- sample(0:50, N, prob = (sort(x, decreasing =T)/sum(x)), replace=TRUE) 

#~~~~~~~~~~~~~~~~~~
x<- exp(-.45*1:10)
deg.chg       <- sample(0:9, N, prob = (sort(x, decreasing =T)/sum(x)), replace=TRUE) 

#~~~~~~~~~~~~~~~~~~~
berlin.spine=sample(as.factor(c(rep("yes",.27*N), 
                       rep("no",.63*N)))
           ,N, replace=T)

#~~~~~~~~~~~~~~~~~~~
berlin.spj =sample(as.factor(c(rep("yes",.3*N), 
                                rep("no",.7*N)))
                    ,N, replace=T)

#~~~~~~~~~~~~~~~~~~~~
trt=sample(as.factor(c(rep("placebo",.5*N), 
                       rep("drug",.5*N)))
           ,N, replace=T)

#~~~~~~~~~~~~~~~~~~~~
sex=sample(as.factor(c(rep("male",.5*N), 
                       rep("female",.5*N)))
           ,N, replace=T)

#~~~~~~~~~~~~~~~~~~~~
smoking=sample(as.factor(c(rep("Never",.33*N), 
                           rep("quitter",.33*N),
                           rep("smoker",.33*N)
                        )),N, replace=T)

#~~~~~~~~~~~~~~~~~~~~
back=sample(as.factor(c(    rep("No changes",.125*N),
                            rep("inflam changes",.125*N), 
                            rep("post inflam chgs",.125*N), 
                            rep("inflam and post inflam changes",.125*N),
                            rep("inflam and deg changes",.125*N),
                            rep("post inflam and deg changes",.125*N), 
                            rep("infla and post inflam changes & deg changes",.125*N), 
                            rep("deg changes",.125*N)   
                            ))
            ,N,   
   replace=T)
#~~~~~~~~~~~~~~~~~~~~
 
#~~~~~~~~~~~~~~~~~~~~~~true effects on the log scale

intercept <-            -3
trt.effects <-          1                # we expect exp(1) OR that is OR= 2.7183, large effect
sex.effects <-          log(1)           # we expect no effect, OR=1
back.effects <-         rep( log(2), 7)  # we expect coefficients each to have OR=2
smoking.effects <-      rep( log(1), 2)  # we expect coefficients each to have OR=1
age.effects <-          1/(65-18)        # we expect unit change effect of 1.0215
berlin.spine.effects <- log(1)           # we expect no effect, OR=1
berlin.spj.effects <- - log(1)           # we expect no effect, OR=1
deg.chg.effects <-      1/(10)           # we expect OR of exp(0.1) , OR 1.105
FASSS.effects <-        1/(51)           # we expect unit change effect of  1.0198

# interaction effects
sex.int.effects <-     log(1)             # we expect no effect, OR=1
back.int.effects <-    rep( log(1.1), 7)  # we expect  OR=1.1 
smoking.int.effects <- rep( log(2), 2)    # we expect  OR=2, big effect
age.int.effects <-     log(1)             # we expect no int effect, OR=1 
berlin.spine.effects <-log(1)             # we expect no int effect, OR=1 
berlin.spj.effects <-  log(1)             # we expect no int effect, OR=1 
deg.chg.int.effects <- 1/(10)             #  we expect OR of exp(0.1) per unit change
FASSS.int.effects <-   1/(51)             # we expect unit change effect of  1.0198
  

#~~~~~~~~~~~~~~~~~~~~~~
all.effects <- c(intercept ,
                 trt.effects ,
                 sex.effects ,
                 back.effects ,
                 smoking.effects ,
                 age.effects ,
                 FASSS.effects ,
                 deg.chg.effects ,
                 berlin.spine.effects ,
                 berlin.spj.effects ,
                 
                 sex.int.effects ,
                 back.int.effects ,
                 smoking.int.effects,
                 age.int.effects,
                 FASSS.int.effects,
                 deg.chg.int.effects ,
                 berlin.spine.effects ,
                 berlin.spj.effects )

all.effects2 <- c(intercept ,
                 trt.effects ,
                 sex.effects ,
                 back.effects ,
                 smoking.effects ,
                 age.effects ,
                 FASSS.effects ,
                 deg.chg.effects ,
                 berlin.spine.effects ,
                 berlin.spj.effects 
                 )


# trt interacts with everything
X = as.matrix(model.matrix(~trt*(sex+back+smoking+age+FASSS+deg.chg+berlin.spine+berlin.spj))) # take a look .. nrow columns -> nrow effects
lp <- as.numeric(as.matrix(X) %*% as.matrix(all.effects)) # mult

# no interaction
X1 = as.matrix(model.matrix(~trt+(sex+back+smoking+age+FASSS+deg.chg+berlin.spine+berlin.spj))) # take a look .. nrow columns -> nrow effects
lp <- as.numeric(as.matrix(X1) %*% as.matrix(all.effects2)) # mult



colnames(X) # show effec
 
y <- ifelse(runif(N) < plogis(lp), 1, 0)   # one liner

da <- cbind.data.frame(y,trt ,sex,back,smoking,age,FASSS,deg.chg,berlin.spine,berlin.spj)  


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 9 unobserved parameters
label(da$age)                <- 'Age'                       # labels for forest plots
label(da$trt)                <- 'Treatment'
label(da$back)               <- 'axialPsA & spine SIJ deg chgs'
label(da$smoking)            <- 'Smoking'
label(da$berlin.spine)       <- 'Berlin Spine'
label(da$berlin.spj)         <- 'Berlin SPJ'
label(da$deg.chg)            <- 'deg. change'
label(da$FASSS)              <- "FASSS"
label(da$sex)                <- 'Sex'
#
dd <<- datadist(da)
options(datadist="dd")

# RUN REGRESSONS

 
A<-lrm(y~   trt * (sex+ back + smoking + age + berlin.spine + berlin.spj + deg.chg + FASSS ),da, y=TRUE ,x=TRUE)  # all interact with trt
B<-lrm(y~  (trt * smoking) + sex+ back + age + berlin.spine + berlin.spj + deg.chg + FASSS,  da, y=TRUE, x=TRUE)  # smoking * trt only
C<-lrm(y~   trt +  sex+ back + smoking + age + berlin.spine + berlin.spj + deg.chg + FASSS,  da, y=TRUE, x=TRUE)  # main effect


lrtest( A, B)

lrtest( A, C)

lrtest( B, C)




par(mfrow=c(1,2)) 

par(oma=c(3,6,1,1)) 

options(digits=1)

plot( 
  
  summary(A, smoking="quitter", age, back="inflam and deg changes",  berlin.spine="no", 
          berlin.spj="no", deg.ch=1, FASSS=1 , sex="female", trt="placebo", 
          est.all=FALSE, vnames=c( "labels")),
  
  log=TRUE, xlim=c(log(.01),log(40)),
  q=c(  0.95 ), at=c(.02,0.05,.1,.2,.5,1,2,4,8,20), lwd=3, pch=17,
  col=   rgb(red=.4,green=.1,blue=.5,alpha=c(.5,.3,.2)),
  col.points='black', cex=1, main= "Odds Ratio (placebo)", cex.main=1.8
)

plot(
  
  summary(A, smoking="quitter", age, back="inflam and deg changes",  berlin.spine="no", 
          berlin.spj="no", deg.chg=1, FASSS=1, sex="female", trt="drug", 
          est.all=FALSE, vnames=c( "labels")),
  
  log=TRUE, xlim=c(log(.01),log(40)),
  q=c(  0.95 ), at=c(.02,0.05,.1,.2,.5,1,2,4,8,20), lwd=3, pch=17,
  col=   rgb(red=.4,green=.1,blue=.5,alpha=c(.5,.3,.2)),
  col.points='black', cex=1, main= "Odds Ratio (drug)", cex.main=1.8
)
 

par(mfrow=c(1,1))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# trt contrasts, 
options(digits=6)
z=contrast(A, list(sex=c("male","female"),  trt="drug"), 
            list(sex=c("male","female"),  trt="placebo"))
print(z, fun=exp)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# relevel, center and read treatment effect from regression table!

# reference levels 
da$back <-    relevel(factor(da$back) ,    ref="inflam and deg changes")
da$smoking <- relevel(factor(da$smoking) , ref="quitter")
da$trt <- relevel(factor(da$trt) , ref="placebo")
# center the variables
da$age <-      da$age     - median(da$age,     na.rm=TRUE)
da$deg.chg <-  da$deg.chg - median(da$deg.chg, na.rm=TRUE)
da$FASSS <-    da$FASSS   - median(da$FASSS,   na.rm=TRUE)

update(A)
da$sex <-    relevel(factor(da$sex) , ref="male")
update(A)


# get the trt effects for levels of smoking
z <- contrast(A, list(smoking= dput(names(table(da$smoking))),  trt="drug"), 
                 list(smoking= dput(names(table(da$smoking))),  trt="placebo"))

print(z, fun=exp)

zz <- summary(A, smoking="quitter",  
              trt=c("placebo","drug"), 
              est.all=FALSE, vnames=c( "labels"), antilog = T)
s1 <- as.vector(zz[dim(zz)[1],c(4,6,7)])



# get the trt effects for levels of back
z <- contrast(A, list(back= dput(names(table(da$back))),  trt="drug"), 
                 list(back= dput(names(table(da$back))),  trt="placebo"))
print(z, fun=exp)







#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

d <- addmargins(table(da$back, da$y))
d1 <- as.vector(d[,3])



# summary(A, back="No changes", 
#         trt=c("placebo","drug"), est.all=FALSE, vnames=c( "labels"), antilog = T)
# 

zz <- summary(A, back="No changes",  trt=c("placebo","drug"), est.all=FALSE, vnames=c( "labels"), antilog = F)
s1 <- as.vector(zz[dim(zz)[1],c(4,6,7)])

zz <- summary(A, back="infla and post inflam changes & deg changes",  trt=c("placebo","drug"), est.all=FALSE, vnames=c( "labels"), antilog = F)
s2 <- as.vector(zz[dim(zz)[1],c(4,6,7)])

zz <- summary(A, back="inflam and deg changes",  trt=c("placebo","drug"), est.all=FALSE, vnames=c( "labels"), antilog = F)
s3 <- as.vector(zz[dim(zz)[1],c(4,6,7)])

zz <- summary(A, back="inflam and post inflam changes",  trt=c("placebo","drug"), est.all=FALSE, vnames=c( "labels"), antilog = F)
s4 <- as.vector(zz[dim(zz)[1],c(4,6,7)])

zz <- summary(A, back="inflam changes",  trt=c("placebo","drug"), est.all=FALSE, vnames=c( "labels"), antilog = F)
s5 <- as.vector(zz[dim(zz)[1],c(4,6,7)])

zz <- summary(A, back="post inflam and deg changes",  trt=c("placebo","drug"), est.all=FALSE, vnames=c( "labels"), antilog = F)
s6 <- as.vector(zz[dim(zz)[1],c(4,6,7)])

zz <- summary(A, back="infla and post inflam changes & deg changes",  trt=c("placebo","drug"), est.all=FALSE, vnames=c( "labels"), antilog = F)
s7 <- as.vector(zz[dim(zz)[1],c(4,6,7)]) 

zz <- summary(A, back="deg changes",  trt=c("placebo","drug"), est.all=FALSE, vnames=c( "labels"), antilog = F)
s8 <- as.vector(zz[dim(zz)[1],c(4,6,7)])

zz <- summary(C,   trt=c("placebo","drug"), est.all=FALSE, vnames=c( "labels"), antilog = F)
m <- as.vector(zz[dim(zz)[1],c(4,6,7)])






vars <- c("Overall",
          levels(da$back)
          
)





# co ordinates


df <- data.frame(study= vars,
                 
                 index= 1:(length(vars)),
                 
                 effect=c(   
                   
                   s1[1],s2[1],s3[1],s4[1],s5[1],  s6[1],s7[1],s8[1] , m[1] ),
                 
                 lower= c(  
                   
                   s1[2],s2[2],s3[2],s4[2],s5[2],  s6[2],s7[2],s8[2] ,m[2] ),
                 
                 upper= c(
                   
                   s1[3],s2[3],s3[3],s4[3],s5[3],  s6[3],s7[3],s8[3]  ,m[3])
                 
)

# ignore above and start df again


 study=c(as.character(z$back), "overall")
 effect=c(as.numeric(z$Contrast),m[1])
 lower=c(as.numeric(z$Lower), m[2])
 upper=c(as.numeric(z$Upper), m[3])
 df <- data.frame(study, effect, lower, upper)
 




effect <- exp(df$effect)
lower  <- exp(df$lower)
upper  <- exp(df$upper)

effect <-append(effect, c(NA,NA) , after=0)
lower  <-append(lower, c(NA,NA) , after=0)
upper  <-append(upper, c(NA,NA) , after=0)

effect <-append(effect, c(NA) , after=length(effect)-1)
lower  <-append(lower, c(NA) ,  after=length(lower)-1)
upper  <-append(upper, c(NA) ,  after=length(upper)-1)


# library(forestplot)
cochrane_from_rmeta <- 
  structure(list(
    mean  = effect, 
    lower = lower, 
    upper = upper),
    .Names = c("mean", "lower", "upper"), 
    row.names = c (NA,12L), 
    class = "data.frame")


# text

txts <- cochrane_from_rmeta
txts$txts  <- paste0(p2(txts$mean), " (",p2(txts$lower),", ",p2(txts$upper),")") 
ors <- txts$txts
ors[1] <- NA
ors[2] <-  "Odds ratio & 95%CI" 
ors[length(ors)-1] <- NA


tabletext <- cbind(
  
  c("", 
    "Level", 
    levels(da$back),
    NA, 
    "Overall Treatment effect"),
  
  c(NA, "N", 
    d1[1:length(d1)-1],
    NA, 
    d1[length(d1)]),
  
  c(ors)
)

xticks <- 2^seq(-8, 8, by=1)

dev.off()  #RESET PLOT WINDOW
forestplot(tabletext, boxsize = 0.2,
           
           txt_gp = fpTxtGp(label = list(gpar(fontfamily = ""),
                                         gpar(fontfamily = "",
                                              col = "#660000")),
                            ticks = gpar(fontfamily = "", cex = .7),
                            xlab  = gpar(fontfamily = "", cex = 1)),
           
           title = c("Interaction of XXX with treatment, interaction p-value=XXX"),
           graph.pos = 2,
           cochrane_from_rmeta,new_page = TRUE,
           is.summary = c(TRUE,TRUE,rep(FALSE,9),TRUE),
           clip = c(0.1,2.5), 
           xlog = TRUE, 
           xticks = xticks ,
           xlab = "Adjusted odds ratio",
           col = fpColors(box = "royalblue",
                          line = "darkblue",
                          summary = "royalblue"))



# remove overall
options(scipen = 999)    
forestplot(tabletext[-(dim(tabletext)[1]),], boxsize = 0.2, 
         
           txt_gp = fpTxtGp(label = list(gpar(fontfamily = ""),
                                         gpar(fontfamily = "",
                                              col = "#660000")),
                            ticks = gpar(fontfamily = "", cex = .7),
                            xlab  = gpar(fontfamily = "", cex = 1)),
           
           title = c("Interaction of XXX with treatment, interaction p-value=XXX"),
           graph.pos = 2,
           cochrane_from_rmeta[-(dim(cochrane_from_rmeta)[1]),],
           
           new_page = TRUE,
           is.summary = c(TRUE,TRUE,rep(FALSE,9)),
           clip = c(0.1,2.5), 
           xlog = TRUE, 
           xticks = xticks ,
           xlab = "Adjusted odds ratio",
           col = fpColors(box = "royalblue",
                          line = "darkblue",
                          summary = "royalblue"))


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

































