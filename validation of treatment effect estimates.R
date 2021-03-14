
# we have defaulted to truth as no interaction 


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
Design = "Treatment interacts with smoking only" 
Design = "No-interaction logit-additive model"

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


M1 <- 1 
N1 <- 2



options(digits=6)

k1 <- rms::contrast(A,
                    
                    list(fact1=c(M1,N1),
                         smoking=v0., age=v1., covar3=v2., covar1=v3., vas=v4., time=v5.,
                         covar2=v6., binary2=v8., sex=v9., bmi=v10.,
                         trt=M),
                    
                    list(fact1=c(M1,N1),
                         smoking=v0., age=v1., covar3=v2., covar1=v3., vas=v4., time=v5.,
                         covar2=v6.,  binary2=v8., sex=v9., bmi=v10.,
                         trt=N) 
)

k1

# lets get the interaction p-value
x <- anova(A, india=FALSE )
pv <- x[grep("* fact1", rownames(x)),"P"]


# get covatiates in model, use the main effects model to do this, put in  a vector
x <- gsub("\\+","",unlist( as.character(formula(C))[3]))
x <- as.vector(str_split(x, "  "))
x <- unlist(x)
x <- gsub("^trt$", "", x)
x <- x[x != ""]
x
 

x1 <- sapply(da, is.factor)
xf <- names(da)[x1]


x1 <- sapply(da, is.numeric)
xn <- names(da)[x1]

##################################

#The levels of fact1 (trt2-trt1)


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

#set to default regression null values, to match regression table
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

# label(da$covar3)             <- 'Biomarker'
# label(da$covar1)             <- 'Blood score'
# label(da$covar2)             <- 'Fitness score'
# label(da$fact1)              <- "History"
# label(da$binary2)            <- "Employed"
 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~
#Q. find the trt2 - tr1 effect for levels of fact1, and as a byproduct quote the interaction
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#RELEVEL~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# method number 1 
#1 look at trt effect, when all vars are set to zero or baseline level for factors
da$fact1 <- relevel(da$fact1, ref="0")  # no need for this , but no harm
A<-lrm(y~   trt * (smoking  + age  + bmi + covar3 + covar1 + vas + time + covar2 + fact1 + binary2 +sex),da, y=TRUE ,x=TRUE)  # all interact with trt
A

# pull out the trt2 main effect
AC <- as.data.frame(A$coefficients)
AC <- AC[str_match('trt=2', rownames(AC)),] 
AC0 <- AC[!is.na(AC)]
AC0    # trt efect in level 0
  

# relevel fact 1 and re-look at trt effect
da$fact1 <- relevel(da$fact1, ref="1")
A<-lrm(y~   trt * (smoking  + age  + bmi + covar3 + covar1 + vas + time + covar2 + fact1 + binary2 +sex),da, y=TRUE ,x=TRUE)  # all interact with trt
A

# pull out the trt2 main effect
AC <- as.data.frame(A$coefficients)
AC <- AC[str_match('trt=2', rownames(AC)),] 
AC1 <- AC[!is.na(AC)]
AC1   # trt efect in level 1

AC1-AC0  # here is the interaction!
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#SUMMARY.RMS~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# method 2 .

s <- summary(A, trt=1,  smoking=v0., age=v1., bmi=v10., covar3=v2., covar1=v3., vas=v4., time =v5., covar2=v6., 
             fact1=v7., binary2=v8., sex=v9.)
#print(s )
s[grep("trt", rownames(s)),] # look at 2v1 


s <- summary(A, trt=1,  smoking=v0., age=v1., bmi=v10., covar3=v2., covar1=v3., vas=v4., time =v5., covar2=v6., 
             fact1=1, binary2=v8., sex=v9.)
#print(s )
s[grep("trt", rownames(s)),] # look at 2v1

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~contrast function~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 # this will give AC1 and AC0 above
 k1 <- rms::contrast(A,  # smoking 2v1 
                     
                     list(fact1=c(1,0),  
                          smoking=v0.,  age=v1., covar3=v2., covar1=v3., vas=v4., time =v5.,
                          covar2=v6., binary2=v8., sex=v9. , bmi=v10., 
                          trt=c(2)),
                     
                     list(fact1=c(1,0),   #this will match trt 2 effect
                          smoking=v0.,  age=v1., covar3=v2., covar1=v3., vas=v4., time =v5.,
                          covar2=v6., binary2=v8., sex=v9. , bmi=v10., 
                          trt=c(1))
                     
                      )
 k1
 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
 # show interactions
 da$fact1 <- relevel(da$fact1, ref="0")
 A<-lrm(y~   trt * (smoking  + age  + bmi + covar3 + covar1 + vas + time + covar2 + fact1 + binary2 +sex),da, y=TRUE ,x=TRUE)  # all interact with trt
 A
 
 Ai <- as.data.frame(A$coefficients)
 Ai[grep('trt=2 \\* fact1=1', rownames(Ai)),] 
 
 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
 # the interaction, double difference!
 
 da$fact1 <- relevel(da$fact1, ref="0")
 A<-lrm(y~   trt * (smoking  + age  + bmi + covar3 + covar1 + vas + time + covar2 + fact1 + binary2 +sex),da, y=TRUE ,x=TRUE)  # all interact with trt
 A
 
 double <- contrast(A, 
                    
                    list(trt=2,  fact1=1,
                         smoking=v0.,  age=v1., covar3=v2., covar1=v3., vas=v4., time =v5.,
                         covar2=v6., binary2=v8., sex=v9. , bmi=v10.
                         ),
                    
                    list(trt=1,  fact1=1,
                         smoking=v0.,  age=v1., covar3=v2., covar1=v3., vas=v4., time =v5.,
                         covar2=v6., binary2=v8., sex=v9. , bmi=v10.),
                    
                    list(trt=2,  fact1=0,
                         smoking=v0.,  age=v1., covar3=v2., covar1=v3., vas=v4., time =v5.,
                         covar2=v6., binary2=v8., sex=v9. , bmi=v10. ),
                    
                    list(trt=1,  fact1=0,
                         smoking=v0.,  age=v1., covar3=v2., covar1=v3., vas=v4., time =v5.,
                         covar2=v6., binary2=v8., sex=v9. , bmi=v10. ), 
                    
                    conf.int=.95)
 double
 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 # the interaction, double difference! just showing other covariate settings do not matter!
 
 da$fact1 <- relevel(da$fact1, ref="0")
 A<-lrm(y~   trt * (smoking  + age  + bmi + covar3 + covar1 + vas + time + covar2 + fact1 + binary2 +sex),da, y=TRUE ,x=TRUE)  # all interact with trt
 A
 
 double <- contrast(A, 
                    
                    list(trt=2,  fact1=1, age=100
                    ),
                    
                    list(trt=1,  fact1=1, age=100
                    ),
                    
                    list(trt=2,  fact1=0,age=100
                    ),
                    
                    list(trt=1,  fact1=0, age=100
                    ), 
                    
                    conf.int=.95)
 double
 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
 
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
 
 
 k1
 double
 
 
 
 k1 <- rms::contrast(A,   
                     list(fact1=c(1,0),  
                          smoking=v0.,  age=v1., covar3=v2., covar1=v3., vas=v4., time =v5.,
                          covar2=v6., binary2=v8., sex=v9. , bmi=v10., 
                          trt=c(2)),
                     
                     list(fact1=c(1,0),   #this will match trt 2 effect
                          smoking=v0.,  age=v1., covar3=v2., covar1=v3., vas=v4., time =v5.,
                          covar2=v6., binary2=v8., sex=v9. , bmi=v10., 
                          trt=c(1))
                     
 )
 
 double <- contrast(A, 
                    list(trt=2,  fact1=1, age=100
                    ),
                    
                    list(trt=1,  fact1=1, age=100
                    ),
                    
                    list(trt=2,  fact1=0,age=100
                    ),
                    
                    list(trt=1,  fact1=0, age=100
                    ), 
                    conf.int=.95)
 
 
 # lets get the interaction p-value
 x <- anova(A, india=FALSE )
 pv <- x[grep("* fact1", rownames(x)),"P"]
 x[grep("* fact1", rownames(x)),]
 
 
 
 
 #################################################################new plot function?
 
 
  # log scale
 zz <- k1
 Scorex=as.vector(zz$Contrast)
 lbx =  as.vector(zz$Lower)
 ubx =  as.vector(zz$Upper)
 
 f <- names(zz[1])
 top <-  zz[1][1][[1]][1]
 bot <- zz[1][1][[1]][2]
 
 
 trt.rff <- paste0("Treatment",t1," - Treatment",t2,)
 
 
 
 # create a data set
 df.plot <- data.frame(x=c(top,bot),
                       factor.=c(treatment,second.grp ),
                       Score=exp(Scorex),
                       lb = exp(lbx),
                       ub =exp(ubx)
 )
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 

 
 
 
 
 
 
 
 
 
 
 #############################

 int.plot <- function( 
   
   var.="x", 
                      factor.="factor of interest",
                      effect="Treatment 2 - Treatment 1", 
                      first.grp="Absent", 
                      second.grp="Present",
                      interaction.p=pv,
                      
                      N,M,N1, M1
 ) {
   
  
   
   
   
   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   #https://stackoverflow.com/questions/11094822/numbers-in-geometric-progression
   v <- 2^seq(-8, 8, by=1)
   #https://stackoverflow.com/questions/5046026/print-number-as-reduced-fraction-in-r
   v2 <- as.character(MASS::fractions(v)) # labels for axis, mix of fractions and numeric
   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   
   # log scale
   zz <- k1
   Scorex=as.vector(zz$Contrast)
   lbx =  as.vector(zz$Lower)
   ubx =  as.vector(zz$Upper)
   
   # create a data set
   df.plot <- data.frame(x=c(effect,effect),
                         factor.=c(first.grp,second.grp ),
                         Score=exp(Scorex),
                         lb = exp(lbx),
                         ub =exp(ubx)
   )
   
   df.plot$factor. = factor(df.plot$factor., 
                            levels = c(first.grp,second.grp ))
   # capture interaction effect to present on graph
   interaction. <- max(Scorex[2],Scorex[1]) -  min(Scorex[2],Scorex[1])  # difference on log odds scale, could use abs(diff(Scorex))
   
   gp <- ggplot(df.plot, aes(x=factor., y=log(Score), fill="black", group=x))
   gg <- gp + #geom_line(aes(linetype=x), size=.6) + 
     geom_point(aes(shape=x), size=4,  color="blue") + 
     geom_errorbar(aes(ymax=log(ub), ymin=log(lb)), width=0.1, size=1, color="blue") +
     theme(legend.position="none") + ylab("Odds Ratio (OR > 1 better outcomes) ") + xlab(factor.) +
     
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
     
     ggtitle( paste0("Adjusted odds ratio for treatment effect (",effect,") for factor of interest, interaction p-value ",p5(interaction.p) ) )
   
   gg <- gg + labs(caption = c(paste0("The p-value tests for the necessity of the interaction printed in orange, it is the result of a hypothesis test assessing the interaction with treatment alone."))) + 
     
     #theme(plot.caption = element_text(hjust=c(1, 0), size = 14, face = "bold")) 
     theme(plot.caption = element_text( size = 14, face = "bold")) 
   # p + theme(
   #     plot.title = element_text(color = "red", size = 12, face = "bold"),
   #     plot.subtitle = element_text(color = "blue"),
   #     plot.caption = element_text(color = "green", face = "italic")
   # )
   
   # Add arrows
   
   i <- gg + geom_segment(
     x = 1.5, y =  Scorex[1],
     xend = 1.5, yend =  Scorex[2],
     lineend = "round", # See available arrow types in example above
     linejoin = "round",
     size = .5, 
     arrow = arrow(length = unit(0.2, "cm")),
     colour = "#EC7014" # Also accepts "red", "blue' etc
   ) 
   
   # double headed arrow   
   
   j <-  i  + geom_segment(
     xend = 1.5, yend =  Scorex[1],
     x = 1.5, y =  Scorex[2],
     lineend = "round", # See available arrow types in example above
     linejoin = "round",
     size = .5, 
     arrow = arrow(length = unit(0.2, "cm")),
     colour = "#EC7014" # Also accepts "red", "blue' etc
   )   
   
   
   # now add text , we exponentiate the dif of the log odds ratios and show the interaction form both points of view
   
   k <- j + geom_text( aes(
     x = 1.5, y = (Scorex[1]+Scorex[2])/2,
     label = paste0("Adjusted odds of response ",p3(exp(   interaction.  )),"x (alternatively ",p3(exp(  - interaction.  )),"x)"), 
     group = NULL,
     vjust = -1, #.3
     hjust = .7 #1
   ), size=5.8 , color="#EC7014") 
   
 }
 
 
  
 
 
 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
 z1 <- print(k1, X=TRUE, fun=exp)  # exponentiate...not used here, but see * below
 # z1 <- print(k1, X=TRUE)             # no exponentiation
 
 # execute plot function
 p1x <- int.plot(k1, factor.="Factor of Interest",
                 effect=paste0("Treatment ",M," - Treatment ",N,""), 
                 first.grp=paste0("level " ,M1), 
                 second.grp=paste0("level " ,N1),
                 interaction.p=pv
 )
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 #######################
 #######################good to here
 #######################
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 





#pv <- A[grep("^trt=2$", rownames(A)),]

da$fact1 <- relevel(da$fact1, ref="1")
A<-lrm(y~   trt * (smoking  + age  + bmi + covar3 + covar1 + vas + time + covar2 + fact1 + binary2 +sex),da, y=TRUE ,x=TRUE)  # all interact with trt
A

























da$trt <- relevel(da$trt, ref="3")
A<-lrm(y~   trt * (smoking  + age  + bmi + covar3 + covar1 + vas + time + covar2 + fact1 + binary2 +sex),da, y=TRUE ,x=TRUE)  # all interact with trt
A

# now imagine the treatment effect for a patient identical expect for different level of a covariate
# the shift is the interaction!



da$trt <- relevel(da$trt, ref="1")
A<-lrm(y~   trt * (smoking  + age  + bmi + covar3 + covar1 + vas + time + covar2 + fact1 + binary2 +sex),da, y=TRUE ,x=TRUE)  # all interact with trt
A


k1

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
da$trt <- factor(da$trt, levels=gtools::mixedsort(levels(da$trt)))  # revert back
# da$trt <- relevel(da$trt, ref=1)
A<-lrm(y~   trt * (smoking  + age  + bmi + covar3 + covar1 + vas + time + covar2 + fact1 + binary2 +sex),da, y=TRUE ,x=TRUE)  # all interact with trt

A
 

# set all to zero to mimic the regression table

k1 <- rms::contrast(A,  # smoking 2v1 
                    
                    list(fact1=0,
                         smoking=c(2,1), age=0, covar3=0, covar1=0, vas=0, time=0,
                         covar2=0, binary2=0, sex=0, bmi=1,
                         trt=c(1)),
                    
                    list(fact1=0,   #this will match trt 2 effect
                         smoking=c(2,1), age=0, covar3=0, covar1=0, vas=0, time=0,
                         covar2=0, binary2=0, sex=0, bmi=1,
                         trt=c(2))    
                    
                    
)







































# get contrast 


if (v %in% "smoking") {
  
  
  k1 <- rms::contrast(A,  # smoking 2v1 
                      
                      list(fact1=v10.,
                           smoking=c(2,1), age=v1., covar3=v2., covar1=v3., vas=v4., time=v5.,
                           covar2=v6., binary2=v8., sex=v9., bmi=v10.,
                           trt=c(1)),
                      
                      list(fact1=v10.,
                           smoking=c(2,1), age=v1., covar3=v2., covar1=v3., vas=v4., time=v5.,
                           covar2=v6.,  binary2=v8., sex=v9., bmi=v10.,
                           trt=c(2))    ,
                       
                     
  )
  
  
  # set all to zero to mimic the regression table
  
  k1 <- rms::contrast(A,  # smoking 2v1 
                      
                      list(fact1=0,
                           smoking=c(2,1), age=0, covar3=0, covar1=0, vas=0, time=0,
                           covar2=0, binary2=0, sex=0, bmi=1,
                           trt=c(1)),
                      
                      list(fact1=0,   #this will match trt 2 effect
                           smoking=c(2,1), age=0, covar3=0, covar1=0, vas=0, time=0,
                           covar2=0, binary2=0, sex=0, bmi=1,
                           trt=c(2))    
                      
                      
  )
  
  
  da$trt <- relevel(da$trt, ref=2)
  A<-lrm(y~   trt * (smoking  + age  + bmi + covar3 + covar1 + vas + time + covar2 + fact1 + binary2 +sex),da, y=TRUE ,x=TRUE)  # all interact with trt
  
  A
  k1
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  da$trt <- factor(da$trt, levels=gtools::mixedsort(levels(da$trt)))  # revert back
 # da$trt <- relevel(da$trt, ref=1)
  A<-lrm(y~   trt * (smoking  + age  + bmi + covar3 + covar1 + vas + time + covar2 + fact1 + binary2 +sex),da, y=TRUE ,x=TRUE)  # all interact with trt
  
  A
  k1
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # k2 <- rms::contrast(A,  # smoking 2v1 in treatment 1
  #                     
  #                     list(fact1=v10.,
  #                          smoking=c(1), age=v1., covar3=v2., covar1=v3., vas=v4., time=v5.,
  #                          covar2=v6., binary2=v8., sex=v9., bmi=v10.,
  #                          trt=c(1,2)),
  #                     
  #                     
  #                     list(fact1=v10.,
  #                          smoking=c(2), age=v1., covar3=v2., covar1=v3., vas=v4., time=v5.,
  #                          covar2=v6.,  binary2=v8., sex=v9., bmi=v10.,
  #                          trt=c(1,2))    ,
  #                     
  #                     
  # )
  # 
  # 
  # 
  # k1 <- rms::contrast(A,  # smoking 2v1
  # 
  #                     list(fact1=v10.,
  #                          smoking=c(1), age=v1., covar3=v2., covar1=v3., vas=v4., time=v5.,
  #                          covar2=v6., binary2=v8., sex=v9., bmi=v10.,
  #                          trt=c(1,2)),
  # 
  # 
  # 
  # )

  # abs(diff(k1$Contrast))
  # 
  # 
  # 
  # 
  
  
  
  
  
  
  
  
  
  
} else if (v %in% "age") {
  
  
  k1 <- rms::contrast(A,
                      
                      list(fact1=v10.,
                           smoking=v0., age=c(M1,N1), covar3=v2., covar1=v3., vas=v4., time=v5.,
                           covar2=v6., binary2=v8., sex=v9., bmi=v10.,
                           trt=M),
                      
                      list(fact1=v10.,
                           smoking=v0., age=c(M1,N1), covar3=v2., covar1=v3., vas=v4., time=v5.,
                           covar2=v6.,  binary2=v8., sex=v9., bmi=v10.,
                           trt=N) 
  )
  
  
}   else if (v %in% "bmi") {
  
  k1 <- rms::contrast(A,
                      
                      list(fact1=v10.,
                           smoking=v0., age=v1., covar3=v2., covar1=v3., vas=v4., time=v5.,
                           covar2=v6., binary2=v8., sex=v9., bmi=c(M1,N1),
                           trt=M),
                      
                      list(fact1=v10.,
                           smoking=v0., age=v1., covar3=v2., covar1=v3., vas=v4., time=v5.,
                           covar2=v6.,  binary2=v8., sex=v9., bmi=c(M1,N1),
                           trt=N) 
  )
  
  
  
} else if (v %in% "covar3") {
  
  
  k1 <- rms::contrast(A,
                      
                      list(fact1=v10.,
                           smoking=v0., age=v1., covar3=c(M1,N1), covar1=v3., vas=v4., time=v5.,
                           covar2=v6., binary2=v8., sex=v9., bmi=v10.,
                           trt=M),
                      
                      list(fact1=v10.,
                           smoking=v0., age=v1., covar3=c(M1,N1), covar1=v3., vas=v4., time=v5.,
                           covar2=v6.,  binary2=v8., sex=v9., bmi=v10.,
                           trt=N) 
  )
  
  
  
  
} else if (v %in% "covar1") {
  
  
  k1 <- rms::contrast(A,
                      
                      list(fact1=v10.,
                           smoking=v0., age=v1., covar1=c(M1,N1), covar3=v2., vas=v4., time=v5.,
                           covar2=v6., binary2=v8., sex=v9., bmi=v10.,
                           trt=M),
                      
                      list(fact1=v10.,
                           smoking=v0., age=v1., covar1=c(M1,N1), covar3=v2., vas=v4., time=v5.,
                           covar2=v6.,  binary2=v8., sex=v9., bmi=v10.,
                      trt=N) 
  )


  
  
  
} else if (v %in% "vas") {
  
  
  k1 <- rms::contrast(A,
                      
                      list(fact1=v10.,
                           smoking=v0., age=v1., covar3=v2., covar1=v3., vas=c(M1,N1), time=v5.,
                           covar2=v6., binary2=v8., sex=v9., bmi=v10.,
                           trt=M),
                      
                      list(fact1=v10.,
                           smoking=v0., age=v1., covar3=v2., covar1=v3., vas=c(M1,N1), time=v5.,
                           covar2=v6.,  binary2=v8., sex=v9., bmi=v10.,
                           trt=N) 
  )
  
  
  
} else if (v %in% "covar2") {
  
  
  
  k1 <- rms::contrast(A,
                      
                      list(fact1=v10.,
                           smoking=v0., age=v1., covar3=v2., covar1=v3., vas=v4., time=v5.,
                           covar2=c(M1,N1), binary2=v8., sex=v9., bmi=v10.,
                           trt=M),
                      
                      list(fact1=v10.,
                           smoking=v0., age=v1., covar3=v2., covar1=v3., vas=v4., time=v5.,
                           covar2=c(M1,N1),  binary2=v8., sex=v9., bmi=v10.,
                           trt=N) 
  )
  
  
  
  
} else if (v %in% "time") {
  
  
  
  k1 <- rms::contrast(A,
                      
                      list(fact1=v10.,
                           smoking=v0., age=v1., covar3=v2., covar1=v3., vas=v4., time=c(M1,N1),
                           covar2=v6., binary2=v8., sex=v9., bmi=v10.,
                           trt=M),
                      
                      list(fact1=v10.,
                           smoking=v0., age=v1., covar3=v2., covar1=v3., vas=v4., time=c(M1,N1),
                           covar2=v6.,  binary2=v8., sex=v9., bmi=v10.,
                           trt=N) 
  )
  
  
  
}   else if (v %in% "sex") {
  
  
  k1 <- rms::contrast(A,
                      
                      list(fact1=v10.,
                           smoking=v0., age=v1., covar3=v2., covar1=v3., vas=v4., time=v5.,
                           covar2=v6., binary2=v8., sex=c(M1,N1), bmi=v10.,
                           trt=M),
                      
                      list(fact1=v10.,
                           smoking=v0., age=v1., covar3=v2., covar1=v3., vas=v4., time=v5.,
                           covar2=v6.,  binary2=v8., sex=c(M1,N1), bmi=v10.,
                           trt=N) 
  )
  
  
  
}   else if (v %in% "binary2") {
  
  
  
  k1 <- rms::contrast(A,
                      
                      list(fact1=v10.,
                           smoking=v0., age=v1., covar3=v2., covar1=v3., vas=v4., time=v5.,
                           covar2=v6., binary2=c(M1,N1), sex=v9., bmi=v10.,
                           trt=M),
                      
                      list(fact1=v10.,
                           smoking=v0., age=v1., covar3=v2., covar1=v3., vas=v4., time=v5.,
                           covar2=v6.,  binary2=c(M1,N1), sex=v9., bmi=v10.,
                           trt=N) 
  )
  
  
  
}   else if (v %in% "fact1") {
  
  
  
  
  k1 <- rms::contrast(A,
                      
                      list(fact1=c(M1,N1),
                           smoking=v0., age=v1., covar3=v2., covar1=v3., vas=v4., time=v5.,
                           covar2=v6., binary2=v8., sex=v9., bmi=v10.,
                           trt=M),
                      
                      list(fact1=c(M1,N1),v8.
                           smoking=v0., age=v1., covar3=v2., covar1=v3., vas=v4., time=v5.,
                           covar2=v6.,  binary2=v8., sex=v9., bmi=v10.,
                           trt=N) 
  )
  
}









































#############################


int.plot <- function(data=da, var.="x", factor.="factor of interest",
                     effect="Treatment 2 - Treatment 1", 
                     first.grp="Absent", 
                     second.grp="Present",
                     interaction.p=pv,
                     
                     N,M,N1, M1
                      ) {
  
   da$var <- da$smoking
  
  
   k1 <- rms::contrast(A,
                       
                       list(var=c(M1,N1),
                            smoking=v0., age=v1., covar3=v2., covar1=v3., vas=v4., time=v5.,
                            covar2=v6., binary2=v8., sex=v9., bmi=v10.,
                            trt=M),
                       
                       list(var=c(M1,N1),
                            smoking=v0., age=v1., covar3=v2., covar1=v3., vas=v4., time=v5.,
                            covar2=v6.,  binary2=v8., sex=v9., bmi=v10.,
                            trt=N) 
   )
   
   
   
   
   
   
   
   
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #https://stackoverflow.com/questions/11094822/numbers-in-geometric-progression
  v <- 2^seq(-8, 8, by=1)
  #https://stackoverflow.com/questions/5046026/print-number-as-reduced-fraction-in-r
  v2 <- as.character(MASS::fractions(v)) # labels for axis, mix of fractions and numeric
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # log scale
  zz <- k1
  Scorex=as.vector(zz$Contrast)
  lbx =  as.vector(zz$Lower)
  ubx =  as.vector(zz$Upper)
  
  # create a data set
  df.plot <- data.frame(x=c(effect,effect),
                        factor.=c(first.grp,second.grp ),
                        Score=exp(Scorex),
                        lb = exp(lbx),
                        ub =exp(ubx)
  )
  
  df.plot$factor. = factor(df.plot$factor., 
                           levels = c(first.grp,second.grp ))
  # capture interaction effect to present on graph
  interaction. <- max(Scorex[2],Scorex[1]) -  min(Scorex[2],Scorex[1])  # difference on log odds scale, could use abs(diff(Scorex))
  
  gp <- ggplot(df.plot, aes(x=factor., y=log(Score), fill="black", group=x))
  gg <- gp + #geom_line(aes(linetype=x), size=.6) + 
    geom_point(aes(shape=x), size=4,  color="blue") + 
    geom_errorbar(aes(ymax=log(ub), ymin=log(lb)), width=0.1, size=1, color="blue") +
    theme(legend.position="none") + ylab("Odds Ratio (OR > 1 better outcomes) ") + xlab(factor.) +
    
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
    
    ggtitle( paste0("Adjusted odds ratio for treatment effect (",effect,") for factor of interest, interaction p-value ",p5(interaction.p) ) )
  
  gg <- gg + labs(caption = c(paste0("The p-value tests for the necessity of the interaction printed in orange, it is the result of a hypothesis test assessing the interaction with treatment alone."))) + 
    
    #theme(plot.caption = element_text(hjust=c(1, 0), size = 14, face = "bold")) 
    theme(plot.caption = element_text( size = 14, face = "bold")) 
  # p + theme(
  #     plot.title = element_text(color = "red", size = 12, face = "bold"),
  #     plot.subtitle = element_text(color = "blue"),
  #     plot.caption = element_text(color = "green", face = "italic")
  # )
  
  # Add arrows
  
  i <- gg + geom_segment(
    x = 1.5, y =  Scorex[1],
    xend = 1.5, yend =  Scorex[2],
    lineend = "round", # See available arrow types in example above
    linejoin = "round",
    size = .5, 
    arrow = arrow(length = unit(0.2, "cm")),
    colour = "#EC7014" # Also accepts "red", "blue' etc
  ) 
  
  # double headed arrow   
  
  j <-  i  + geom_segment(
    xend = 1.5, yend =  Scorex[1],
    x = 1.5, y =  Scorex[2],
    lineend = "round", # See available arrow types in example above
    linejoin = "round",
    size = .5, 
    arrow = arrow(length = unit(0.2, "cm")),
    colour = "#EC7014" # Also accepts "red", "blue' etc
  )   
  
  
  # now add text , we exponentiate the dif of the log odds ratios and show the interaction form both points of view
  
  k <- j + geom_text( aes(
    x = 1.5, y = (Scorex[1]+Scorex[2])/2,
    label = paste0("Adjusted odds of response ",p3(exp(   interaction.  )),"x (alternatively ",p3(exp(  - interaction.  )),"x)"), 
    group = NULL,
    vjust = -1, #.3
    hjust = .7 #1
  ), size=5.8 , color="#EC7014") 
  
}

































#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

z1 <- print(k1, X=TRUE, fun=exp)  # exponentiate...not used here, but see * below
# z1 <- print(k1, X=TRUE)             # no exponentiation

# execute plot function
p1x <- int.plot(k1, factor.="Factor of Interest",
                effect=paste0("Treatment ",M," - Treatment ",N,""), 
                first.grp=paste0("level " ,M1), 
                second.grp=paste0("level " ,N1),
                interaction.p=pv
)