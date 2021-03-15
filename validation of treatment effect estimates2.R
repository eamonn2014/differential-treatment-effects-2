# add this to the app
# plotting interactions alot of code.
# we have defaulted to truth as no interaction 
# need to add adjusted for information to graph.

rm(list=ls())
set.seed(1234)
library(rms)          # automatically engages rms(Hmisc)
library(tidyverse)
library(reshape)
library(rms)
library(ggplot2)
require(gridExtra)


## convenience functions
p0 <- function(x) {formatC(x, format="f", digits=1)}
p1 <- function(x) {formatC(x, format="f", digits=1)}
p2 <- function(x) {formatC(x, format="f", digits=2)}
p3 <- function(x) {formatC(x, format="f", digits=3)}
p4 <- function(x) {formatC(x, format="f", digits=4)}
p5 <- function(x) {formatC(x, format="f", digits=5)}

logit <-     function(p) log(1/(1/p-1))
expit <-     function(x) 1/(1/exp(x) + 1)
inv_logit <- function(logit) exp(logit) / (1 + exp(logit))
is.even <-   function(x){ x %% 2 == 0 }                       # function to id. odd maybe useful
options(width=200)


#Simulate logistic regression and focus on one factor's interaction with treatment and how we should present the results

# select the design to simulate!!!!!!!!
Design = "Treatment interacts with all variables" 
#Design = "Treatment interacts with smoking only" 
#Design = "No-interaction logit-additive model"

# coefficients on log odds scale

intercept <- -3
n=10000
v1 = 1 
v2 = 1/(65-18)
v3 = 0.4
v4 = 0
v5 = 1/3
v6 = -.5/10
v7 = 0.25/30
v8 = -.1/10
v9 = -1/50
v10 = log(2) ################!!!  OR2
v11 = -log(1)
v12 = log(0.5)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
randomi <- runif(n)    # this determines Y=1 later, 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

trt.coef       <-  v1     # log odds ratio so 1 -> 2.718, so 1 is LARGE
age.coef       <-  v2     # log odds of 1 over the age range
smoke.coef     <-  v3     # this is odds of 1.5
bmi.coef       <-  v4     # this is an odds of 1..50:50
covar3.coef    <-  v5     # log odds 1 over range of 3
covar1.coef    <-  v6     # log odds -.05 per unit change
vas.coef       <-  v7     # log odds .008 per unit change. log odds .25 over 30 units odds 1.27
time.coef      <-  v8     # log odds -.01 per year, log odds -.1 over 10 years or odds .90
covar2.coef    <-  v9     # log odds 0.02 per unit, log odds 1 over 50 units or odds 2.7
fact1.coef     <-  v10    # log odds 0.693 per change in binary, or odds of 2   !!!!!!!!!!!!!!!
binary2.coef   <-  v11    # log odds 0 per change in binary, or odds of 1  
sex.coef       <-  v12    # log odds -0.693 per change in binary, or odds of .5  

# made up data structure 
trt      <- sample(1:3,   n, replace=TRUE)      # trt 3 levels
age      <- sample(18:65, n, replace=TRUE)      # continuous
bmi      <- sample(1:3,   n, replace=TRUE)      # assume 3 equal groups
smoking  <- sample(1:3,   n, replace=TRUE)      # categorical assume 3 equal groups
covar3   <- round(runif(n,0,3),2)
covar1   <- round(runif(n,0,10),2)
vas      <- sample(1:30, n, replace=TRUE)
time     <- round(runif(n,0,10),2)              # years
covar2   <- sample(1:50, n, replace=TRUE)
fact1    <- sample(0:1,  n, replace=TRUE)
binary2  <- sample(0:1,  n, replace=TRUE)
sex      <- sample(0:1,  n, replace=TRUE)


# Linear predictor



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~linear predictor
if ( ( Design) == "Treatment interacts with all variables" )  {
  
  lp = intercept + trt*trt.coef*(smoking*smoke.coef   +   age*age.coef  + bmi*bmi.coef   + covar3*covar3.coef +
                                   covar1*covar1.coef +   vas*vas.coef  + time*time.coef + covar2*covar2.coef +
                                   fact1*fact1.coef   +
                                   binary2*binary2.coef + sex*sex.coef) 
  
  
}   else if ( ( Design ) == "Treatment interacts with smoking only" ) {    
  
  # truth  only smoking interacts  with trt
  lp = intercept + (trt*trt.coef*smoking*smoke.coef)   +   age*age.coef   + bmi*bmi.coef + covar3*covar3.coef +
    covar1*covar1.coef  + vas*vas.coef   + time*time.coef + covar2*covar2.coef + fact1*fact1.coef +
    binary2*binary2.coef + sex*sex.coef
  
  
}   else if ( ( Design) == "No-interaction logit-additive model" ) {  
  
  # truth no interactions
  lp = intercept + trt*trt.coef + smoking*smoke.coef + age*age.coef  + bmi*bmi.coef  + covar3*covar3.coef  +
    covar1*covar1.coef + vas*vas.coef  + time*time.coef + covar2*covar2.coef + fact1*fact1.coef +
    binary2*binary2.coef + sex*sex.coef
}

y <- ifelse(randomi < plogis(lp), 1, 0)   # one liner  randomi object is necessary for R shiny only



# Dataset creation Stick the linear predictor and covariates together


datx <- data.frame(cbind(y,  trt ,smoking,  age, covar3, covar1, vas, time, covar2, fact1, binary2, sex, bmi))

da <-  datx 

da$trt <-     factor(da$trt)
da$smoking <- factor(da$smoking)
da$fact1 <-   factor(da$fact1)
da$binary2 <- factor(da$binary2)
da$sex <-     factor(da$sex)
da$bmi <-     factor(da$bmi)

# labels for forest plots
label(da$age)                <- 'Age'                       
label(da$trt)                <- 'Treatment'
label(da$bmi)                <- 'Body Mass Index'
label(da$smoking)            <- 'Smoking'
label(da$covar3)             <- 'Biomarker'
label(da$covar1)             <- 'Blood score'
label(da$vas)                <- 'Visual analogue score'
label(da$time)               <- 'Time since diagnosis'
label(da$covar2)             <- 'Fitness score'
label(da$fact1)              <- "History"
label(da$binary2)            <- "Employed"
label(da$sex)                <- 'Sex'

dd <<- datadist(da)
options(datadist="dd")

# Regression Run logistic regression

A<-lrm(y~   trt * (smoking  + age  + bmi + covar3 + covar1 + vas + time + covar2 + fact1 + binary2 +sex),da, y=TRUE ,x=TRUE)  # all interact with trt
B<-lrm(y~  (trt *  smoking) + age  + bmi + covar3 + covar1 + vas + time + covar2 + fact1 + binary2 +sex, da, y=TRUE, x=TRUE)  # smoking * trt only
C<-lrm(y~   trt +  smoking  + age +  bmi + covar3 + covar1 + vas + time + covar2 + fact1 + binary2 +sex, da, y=TRUE, x=TRUE)  # main effect




## treatment effects # relvel or use contrast


v0. <-  1
v1. <-  40
v2. <-  1.3
v3. <-  5
v4. <-  17
v5. <-  4
v6. <-  20
v7. <-  0
v8. <-  0
v9. <-  0
v10. <- 1

## need trt levels

#i <- as.numeric(unlist(strsplit(input$tlevz,",")))

M <- 1
N <- 2


# need to enter the order of the infep factor levels 
#i <- as.numeric(unlist(strsplit(input$ilevz,",")))


M1 <- 0 
N1 <- 21



options(digits=6)

 
 
##################GOOD TO HERE#################################################

# inputs
# covariate settings of other variables
# levels of indep var
# trt levels of interest

v0. <- 1 # smoking
v1. <- 0 # age
v2. <- 0 # biomarker  
v3. <- 0 # blood      
v4. <- 0 # vas       
v5. <- 0 # time     
v6. <- 0 # fitness    
v7. <- 0 # history
v8. <- 0 # employed   
v9. <- 0 # sex  
v10. <- 1 # BMI       


#function to return contrast/double/anova p


































#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function to get treatment effects for interactions, double diff and anova p value for 
# interaction chunk
# M and N are the treatment levels , 
# M1 and N1 are the independent variable levels

statz <- function( v="fact1", M=1,  N=2,   M1=0 , N1=1)  {
  
  
 if (v %in% "smoking") {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
  k1 <- rms::contrast(A,
                      
                      list(fact1=v10.,
                           smoking=c(M1,N1), age=v1., covar3=v2., covar1=v3., vas=v4., time=v5.,
                           covar2=v6., binary2=v8., sex=v9., bmi=v10.,
                           trt=N),
                      
                      list(fact1=v10.,
                           smoking=c(M1,N1), age=v1., covar3=v2., covar1=v3., vas=v4., time=v5.,
                           covar2=v6.,  binary2=v8., sex=v9., bmi=v10.,
                           trt=M) 
  )
  
  
  double <- contrast(A, 
                     list(trt=N,  smoking=N1
                     ),
                     
                     list(trt=M,  smoking=N1 
                     ),
                     
                     list(trt=N,  smoking=M1
                     ),
                     
                     list(trt=M,  smoking=M1 
                     ), 
                     conf.int=.95)
  
  
  # lets get the interaction p-value
  x <- anova(A, india=FALSE )
  pv <- x[grep("* smoking", rownames(x)),"P"]
  pvalue <- x[grep("* smoking", rownames(x)),]
  M=M;N=N;M1=M1;N1=N1;v=v;
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  


} else if (v %in% "fact1") {

  k1 <- rms::contrast(A,   
                      list(fact1=c(M1,N1),  
                           smoking=v0.,  age=v1., covar3=v2., covar1=v3., vas=v4., time =v5.,
                           covar2=v6., binary2=v8., sex=v9. , bmi=v10., 
                           trt=c(N)),
                      
                      list(fact1=c(M1,N1),    
                           smoking=v0.,  age=v1., covar3=v2., covar1=v3., vas=v4., time =v5.,
                           covar2=v6., binary2=v8., sex=v9. , bmi=v10., 
                           trt=c(M))
                      
  )
  
  double <- contrast(A, 
                     list(trt=N,  fact1=N1
                     ),
                     
                     list(trt=M,  fact1=N1 
                     ),
                     
                     list(trt=N,  fact1=M1
                     ),
                     
                     list(trt=M,  fact1=M1 
                     ), 
                     conf.int=.95)


    # lets get the interaction p-value
    x <- anova(A, india=FALSE )
    pv <- x[grep("* fact1", rownames(x)),"P"]
    pvalue <- x[grep("* fact1", rownames(x)),]
    M=M;N=N;M1=M1;N1=N1;v=v;
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
} else if (v %in% "age") {
  
  
  k1 <- rms::contrast(A,
                      
                      list(fact1=v10.,
                           smoking=v0., age=c(M1,N1), covar3=v2., covar1=v3., vas=v4., time=v5.,
                           covar2=v6., binary2=v8., sex=v9., bmi=v10.,
                           trt=N),
                      
                      list(fact1=v10.,
                           smoking=v0., age=c(M1,N1), covar3=v2., covar1=v3., vas=v4., time=v5.,
                           covar2=v6.,  binary2=v8., sex=v9., bmi=v10.,
                           trt=M) 
  )
  
  
  double <- contrast(A, 
                     list(trt=N,  age=N1
                     ),
                     
                     list(trt=M,  age=N1 
                     ),
                     
                     list(trt=N,  age=M1
                     ),
                     
                     list(trt=M,  age=M1 
                     ), 
                     conf.int=.95)
  
  
  # lets get the interaction p-value
  x <- anova(A, india=FALSE )
  pv <- x[grep("* age", rownames(x)),"P"]
  pvalue <- x[grep("* age", rownames(x)),]
  M=M;N=N;M1=M1;N1=N1;v=v;

  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
}   else if (v %in% "bmi") {
  
  k1 <- rms::contrast(A,
                      
                      list(fact1=v10.,
                           smoking=v0., age=v1., covar3=v2., covar1=v3., vas=v4., time=v5.,
                           covar2=v6., binary2=v8., sex=v9., bmi=c(M1,N1),
                           trt=N),
                      
                      list(fact1=v10.,
                           smoking=v0., age=v1., covar3=v2., covar1=v3., vas=v4., time=v5.,
                           covar2=v6.,  binary2=v8., sex=v9., bmi=c(M1,N1),
                           trt=M) 
  )
  
  
  double <- contrast(A, 
                     list(trt=N,  bmi=N1
                     ),
                     
                     list(trt=M,  bmi=N1 
                     ),
                     
                     list(trt=N,  bmi=M1
                     ),
                     
                     list(trt=M,  bmi=M1 
                     ), 
                     conf.int=.95)
  
  
  # lets get the interaction p-value
  x <- anova(A, india=FALSE )
  pv <- x[grep("* bmi", rownames(x)),"P"]
  pvalue <- x[grep("* bmi", rownames(x)),]
  M=M;N=N;M1=M1;N1=N1;v=v;
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
} else if (v %in% "covar3") {
  
  
  k1 <- rms::contrast(A,
                      
                      list(fact1=v10.,
                           smoking=v0., age=v1., covar3=c(M1,N1), covar1=v3., vas=v4., time=v5.,
                           covar2=v6., binary2=v8., sex=v9., bmi=v10.,
                           trt=N),
                      
                      list(fact1=v10.,
                           smoking=v0., age=v1., covar3=c(M1,N1), covar1=v3., vas=v4., time=v5.,
                           covar2=v6.,  binary2=v8., sex=v9., bmi=v10.,
                           trt=M) 
  )
  
  
  double <- contrast(A, 
                     list(trt=N,  covar3=N1
                     ),
                     
                     list(trt=M,  covar3=N1 
                     ),
                     
                     list(trt=N,  covar3=M1
                     ),
                     
                     list(trt=M,  covar3=M1 
                     ), 
                     conf.int=.95)
  
  
  # lets get the interaction p-value
  x <- anova(A, india=FALSE )
  pv <- x[grep("* covar3", rownames(x)),"P"]
  pvalue <- x[grep("* covar3", rownames(x)),]
  
  M=M;N=N;M1=M1;N1=N1;v=v;
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
} else if (v %in% "covar1") {
  
  
  k1 <- rms::contrast(A,
                      
                      list(fact1=v10.,
                           smoking=v0., age=v1., covar1=c(M1,N1), covar3=v2., vas=v4., time=v5.,
                           covar2=v6., binary2=v8., sex=v9., bmi=v10.,
                           trt=N),
                      
                      list(fact1=v10.,
                           smoking=v0., age=v1., covar1=c(M1,N1), covar3=v2., vas=v4., time=v5.,
                           covar2=v6.,  binary2=v8., sex=v9., bmi=v10.,
                           trt=M) 
  )
  
  
  double <- contrast(A, 
                     list(trt=N,  covar1=N1
                     ),
                     
                     list(trt=M,  covar1=N1 
                     ),
                     
                     list(trt=N,  covar1=M1
                     ),
                     
                     list(trt=M,  covar1=M1 
                     ), 
                     conf.int=.95)
  
  
  # lets get the interaction p-value
  x <- anova(A, india=FALSE )
  pv <- x[grep("* covar1", rownames(x)),"P"]
  pvalue <- x[grep("* covar1", rownames(x)),]
  M=M;N=N;M1=M1;N1=N1;v=v;
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
} else if (v %in% "vas") {
  
  
  k1 <- rms::contrast(A,
                      
                      list(fact1=v10.,
                           smoking=v0., age=v1., covar3=v2., covar1=v3., vas=c(M1,N1), time=v5.,
                           covar2=v6., binary2=v8., sex=v9., bmi=v10.,
                           trt=N),
                      
                      list(fact1=v10.,
                           smoking=v0., age=v1., covar3=v2., covar1=v3., vas=c(M1,N1), time=v5.,
                           covar2=v6.,  binary2=v8., sex=v9., bmi=v10.,
                           trt=M) 
  )
  
  double <- contrast(A, 
                     list(trt=N,  vas=N1
                     ),
                     
                     list(trt=M,  vas=N1 
                     ),
                     
                     list(trt=N,  vas=M1
                     ),
                     
                     list(trt=M,  vas=M1 
                     ), 
                     conf.int=.95)
  
  
  # lets get the interaction p-value
  x <- anova(A, india=FALSE )
  pv <- x[grep("* vas", rownames(x)),"P"]
  pvalue <- x[grep("* vas", rownames(x)),]
  M=M;N=N;M1=M1;N1=N1;v=v;
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
} else if (v %in% "covar2") {
  
  
  
  k1 <- rms::contrast(A,
                      
                      list(fact1=v10.,
                           smoking=v0., age=v1., covar3=v2., covar1=v3., vas=v4., time=v5.,
                           covar2=c(M1,N1), binary2=v8., sex=v9., bmi=v10.,
                           trt=N),
                      
                      list(fact1=v10.,
                           smoking=v0., age=v1., covar3=v2., covar1=v3., vas=v4., time=v5.,
                           covar2=c(M1,N1),  binary2=v8., sex=v9., bmi=v10.,
                           trt=M) 
  )
  
  double <- contrast(A, 
                     list(trt=N,  covar2=N1
                     ),
                     
                     list(trt=M,  covar2=N1 
                     ),
                     
                     list(trt=N,  covar2=M1
                     ),
                     
                     list(trt=M,  covar2=M1 
                     ), 
                     conf.int=.95)
  
  
  # lets get the interaction p-value
  x <- anova(A, india=FALSE )
  pv <- x[grep("* covar2", rownames(x)),"P"]
  
  pvalue <- x[grep("* covar2", rownames(x)),]
  M=M;N=N;M1=M1;N1=N1;v=v;
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
} else if (v %in% "time") {
  
  
  
  k1 <- rms::contrast(A,
                      
                      list(fact1=v10.,
                           smoking=v0., age=v1., covar3=v2., covar1=v3., vas=v4., time=c(M1,N1),
                           covar2=v6., binary2=v8., sex=v9., bmi=v10.,
                           trt=N),
                      
                      list(fact1=v10.,
                           smoking=v0., age=v1., covar3=v2., covar1=v3., vas=v4., time=c(M1,N1),
                           covar2=v6.,  binary2=v8., sex=v9., bmi=v10.,
                           trt=M) 
  )
  

  
  double <- contrast(A, 
                     list(trt=N,  time=N1
                     ),
                     
                     list(trt=M,  time=N1 
                     ),
                     
                     list(trt=N,  time=M1
                     ),
                     
                     list(trt=M,  time=M1 
                     ), 
                     conf.int=.95)
  
  
  # lets get the interaction p-value
  x <- anova(A, india=FALSE )
  pv <- x[grep("* time", rownames(x)),"P"]
  pvalue <- x[grep("* time", rownames(x)),]
  M=M;N=N;M1=M1;N1=N1;v=v;
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
}   else if (v %in% "sex") {
  
  
  k1 <- rms::contrast(A,
                      
                      list(fact1=v10.,
                           smoking=v0., age=v1., covar3=v2., covar1=v3., vas=v4., time=v5.,
                           covar2=v6., binary2=v8., sex=c(M1,N1), bmi=v10.,
                           trt=N),
                      
                      list(fact1=v10.,
                           smoking=v0., age=v1., covar3=v2., covar1=v3., vas=v4., time=v5.,
                           covar2=v6.,  binary2=v8., sex=c(M1,N1), bmi=v10.,
                           trt=M) 
  )
  
  
  
  double <- contrast(A, 
                     list(trt=N,  sex=N1
                     ),
                     
                     list(trt=M,  sex=N1 
                     ),
                     
                     list(trt=N,  sex=M1
                     ),
                     
                     list(trt=M,  sex=M1 
                     ), 
                     conf.int=.95)
  
  
  # lets get the interaction p-value
  x <- anova(A, india=FALSE )
  pv <- x[grep("* sex", rownames(x)),"P"]
  pvalue <- x[grep("* sex", rownames(x)),]
  M=M;N=N;M1=M1;N1=N1;v=v;
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
}   else if (v %in% "binary2") {
  
  
  k1 <- rms::contrast(A,
                      
                      list(fact1=v10.,
                           smoking=v0., age=v1., covar3=v2., covar1=v3., vas=v4., time=v5.,
                           covar2=v6., binary2=c(M1,N1), sex=v9., bmi=v10.,
                           trt=N),
                      
                      list(fact1=v10.,
                           smoking=v0., age=v1., covar3=v2., covar1=v3., vas=v4., time=v5.,
                           covar2=v6.,  binary2=c(M1,N1), sex=v9., bmi=v10.,
                           trt=M) 
  )
  
  double <- contrast(A, 
                     list(trt=N,  binary2=N1
                     ),
                     
                     list(trt=M,  binary2=N1 
                     ),
                     
                     list(trt=N,  binary2=M1
                     ),
                     
                     list(trt=M,  binary2=M1 
                     ), 
                     conf.int=.95)
  
  
  # lets get the interaction p-value
  x <- anova(A, india=FALSE )
  pv <- x[grep("* binary2", rownames(x)),"P"]
  pvalue <- x[grep("* binary2", rownames(x)),]
  M=M;N=N;M1=M1;N1=N1;v=v;
  
}
  return(list(k1=k1, double=double, pv=pv, pvalue=pvalue, M=M, N=N, M1=M1, N1=N1,v=v))
  print(k1)
}
  
# end function 
# function to get treatment effects for interactions, double diff and anova p value for 
# interaction chunk
# M and N are the treatment levels , 
# M1 and N1 are the independent variable levels
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  


  
#### now let us write a plot function 
  
  
  
###########################

i.plot <- function( 
  res =statz(v , M ,  N ,   M1  ,  N1 )  
) {
  
  # original is on log scale
  zz <- res$k1
  Scorex=as.vector(zz$Contrast)
  lbx =  as.vector(zz$Lower)
  ubx =  as.vector(zz$Upper)
  
  M=res$M
  N=res$N
  vx=res$v
  effect = names(res$k1)[1]
  
  # create a data set
  df.plot <- data.frame(factor.=c(res$M1,res$N1 ),
                        x=paste0("Treatment ",N," - Treatment ",M), 
                        Score=exp(Scorex),
                        lb = exp(lbx),
                        ub =exp(ubx)
  )
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #https://stackoverflow.com/questions/11094822/numbers-in-geometric-progression
  v <- 2^seq(-8, 8, by=1)
  #https://stackoverflow.com/questions/5046026/print-number-as-reduced-fraction-in-r
  v2 <- as.character(MASS::fractions(v)) # labels for axis, mix of fractions and numeric
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
   # get the y axis the same whether 0,1 1,0 ... good idea?
   if (res$M1 < res$N1) {
  
   df.plot$factor. = factor(df.plot$factor., levels = c(res$M1,res$N1 ))

   } else {
     
     df.plot$factor. = factor(df.plot$factor., levels = c(res$N1,res$M1 ))
     
     
   }
  
    # capture interaction effect to present on graph
 # interaction. <- max(Scorex[2],Scorex[1]) -  min(Scorex[2],Scorex[1])  # difference on log odds scale, could use abs(diff(Scorex))
  interaction. <-  res$double$Contrast[[1]]
  
  gp <- ggplot(df.plot, aes(x=factor., y=log(Score), fill="black", group=x))
  gg <- gp + #geom_line(aes(linetype=x), size=.6) + 
    geom_point(aes(shape=x), size=4,  color="blue") + 
    geom_errorbar(aes(ymax=log(ub), ymin=log(lb)), width=0.1, size=1, color="blue") +
    theme(legend.position="none") + ylab("Odds Ratio (OR > 1 better outcomes) ") + xlab( vx) +
    
    theme_bw() +
    
    # tick labels and axis labels
    theme(axis.text.x=element_text(size=14),
          axis.title.x=element_text(size=14,face="bold")) +
    
    theme(axis.text.y=element_text(size=14),
          axis.title.y=element_text(size=14,face="bold")) +
    
    theme(plot.title = element_text(size = 16, face = "bold")) +
    
    theme(legend.position="none") +
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    # line of no effect
    geom_hline(yintercept=log(1), linetype="dashed", color = "blue") +
    
    
    scale_y_continuous(
      breaks= log(v)  ,  
      limits = c(log(min(v)),log(max(v))),  
      label=     v2  # created earlier
    ) +
    
    coord_flip() +
    
    geom_text(aes(   
      y=log(40),
      label = paste0(p3(Score),", 95%CI (" ,p3(lb),", ",p3(ub), ")"), 
      vjust=-1.0), size=5.8, color='black') +
    
    ggtitle( paste0("Adjusted odds ratio for treatment effect (Treatment ",N," - Treatment ",M,") for ",vx,", interaction p-value ",p5(res$pv) ) )
  
  gg <- gg + labs(caption = c(paste0("The p-value tests for the necessity of the interaction printed in orange, it is the result of a hypothesis test assessing the interaction with treatment alone."))) + 
    
    #theme(plot.caption = element_text(hjust=c(1, 0), size = 14, face = "bold")) 
    theme(plot.caption = element_text( size = 14, face = "bold")) 
  # p + theme(
  #     plot.title = element_text(color = "red", size = 12, face = "bold"),
  #     plot.subtitle = element_text(color = "blue"),
  #     plot.caption = element_text(color = "green", face = "italic")
  # )
#
#if (exp(res$double$Contrast) < 1 & (res$M1 > res$N1 )) {
  
##  if   ((res$M1 > res$N1 ) | (  exp(res$double$Contrast) < 1     ) )    { 
    
  
   #if( diff(res$k1$Contrast)>0  ) {
     
  #   if( diff(log(df.plot$Score))<0  ) {
    
  # Add arrows
  
  i <- gg + geom_segment(
    x = 1.5, y =  Scorex[1],  #y start of arrow
    xend = 1.5, yend =  Scorex[2],  # end of arrow yend 
    lineend = "round", # See available arrow types in example above
    linejoin = "round",
    size = .5, 
    arrow = arrow(length = unit(0.2, "cm")),
    colour = "#EC7014" # Also accepts "red", "blue' etc
  ) 
  
##  } else {
  
  # i <- gg  + geom_segment(
  #   xend = 1.5, yend =  Scorex[1],
  #   x = 1.5, y =  Scorex[2],
  #   lineend = "round", # See available arrow types in example above
  #   linejoin = "round",
  #   size = .5, 
  #   arrow = arrow(length = unit(0.2, "cm")),
  #   colour = "#EC7014" # Also accepts "red", "blue' etc
  # )   
  
  
 ##}
  
  # now add text , we exponentiate the dif of the log odds ratios and show the interaction form both points of view
  
  k <- i + geom_text( aes(
    x = 1.4, #y = (Scorex[1]+Scorex[2])/2,
    y=log(75),
    label = paste0("Interaction multiplication factor:\n ",p3(exp(res$double$Contrast)),", 95%CI (" ,p3(exp(res$double$Lower)),", ",p3(exp(res$double$Upper)), ")"), 
    group = NULL,
    vjust = -1, #.3
    hjust = .7 #1
  ), size=5.8 , color="#EC7014") 
  
}

# trouble shooting
  res1 =statz(v="fact1",     M= 1,  N=2,   M1 =1 ,  N1 = 0)   
  res2 =statz(v="fact1",     M= 1,  N=2,   M1 =0 ,  N1 = 1)  
  res3 =statz(v="fact1",     M= 2,  N=1,   M1 =1 ,  N1 = 0)  
  res4 =statz(v="fact1",     M= 2,  N=1,   M1 =0 ,  N1 = 1)   
 
(z <- i.plot(  res =statz(v="fact1",     M= 1,  N=2,   M1 =1 ,  N1 = 0)  ))  #g  <1 L 
(z <- i.plot(  res =statz(v="fact1",     M= 1,  N=2,   M1 =0 ,  N1 = 1)  ))  #   >1 L 

(z <- i.plot(  res =statz(v="fact1",     M= 2,  N=1,   M1 =1 ,  N1 = 0)  ))  #g >1 R
(z <- i.plot(  res =statz(v="fact1",     M= 2,  N=1,   M1 =0 ,  N1 = 1)  ))  #g  <1 L



(z <- i.plot(  res =statz(v="fact1",     M= 2,  N=3,   M1 =0 ,  N1 = 1)  ))

(z <- i.plot(  res =statz(v="fact1",     M= 1,  N=3,   M1 =0 ,  N1 = 1)  ))
(z <- i.plot(res <- statz(v="age",       M= 1,  N=3,   M1 =10 , N1 = 15)  ))

res <- statz(v="smoking",   M= 3,  N=1,   M1 =1 ,  N1 = 2) 
(z <- i.plot(res <- statz(v="smoking",   M= 3,  N=1,   M1 =1 ,  N1 = 2)  ))

(z <- i.plot(res <- statz(v="sex",   M= 1,  N=3,   M1 =0 ,  N1 = 1)  ))
(z <- i.plot(res <- statz(v="vas",   M= 1,  N=3,   M1 =5 ,  N1 = 10)  ))

 