#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# load the required packages
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# https://thestatsgeek.com/2014/03/28/interpreting-changes-in-hazard-and-hazard-ratios/

set.seed(333) # reproducible
library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)
library(directlabels)
library(Hmisc)
library(ggplot2)
library(tidyverse)
library(plotly)
library(survminer)
library(rms)
# library(scales) # For the trans_format function
# library(shinyalert)
library(DT)
library(survival)
options(max.print=1000000)    


# march21 new function to plot treatment contrasts



int.plot <- function(k1, factor.="factor of interest",
                     effect="Treatment 2 - Treatment 1", 
                     first.grp="Absent", 
                     second.grp="Present") {
  
  v <- c(1/32,1/16,1/8, 1/4,1/2,  1, 2, 4 ,8, 16,32,64, 128) 
  
  zz <- k1
  # log scale
  Scorex=as.vector(zz$Contrast)
  lbx =  as.vector(zz$Lower)
  ubx =  as.vector(zz$Upper)
  
  # create a dataset
  df.plot <- data.frame(x=c(effect,effect),
                        factor.=c(first.grp,second.grp ),
                        Score=exp(Scorex),
                        lb = exp(lbx),
                        ub =exp(ubx))
  
  df.plot$factor. = factor(df.plot$factor., 
                           levels = c(first.grp,second.grp ))
  
  
  gp <- ggplot(df.plot, aes(x=factor., y=log(Score), fill="black", group=x))
  gg <- gp + #geom_line(aes(linetype=x), size=.6) + 
    geom_point(aes(shape=x), size=3) + 
    geom_errorbar(aes(ymax=log(ub), ymin=log(lb)), width=.1) +
    theme(legend.position="none") + ylab("Odds Ratio (OR > 1 better outcomes) ") + xlab(factor.) +
    
    geom_hline(yintercept=log(1), linetype="dashed", color = "blue") +
    
    scale_y_continuous(
      breaks= log(v)  ,  
      limits = c(log(min(v)),log(max(v))),  
      labels=     v
    ) +
    
    coord_flip() +
    
    geom_text(aes(   
      y=log(40),
      label = paste0(p3(Score),", 95%CI (" ,p3(lb),", ",p3(ub), ")"), 
      vjust=-1.0), size=2.8, color='blue') +
    
    ggtitle( paste0("Adjusted Odds ratio of response for ", effect) )
  
  
  gg <- gg + labs(caption = c("xxxxxxxxxxxxxx", 
                              "xxxxxxxxxxxxxx")) + 
    theme(plot.caption = element_text(hjust=c(1, 0)))
  
  # Add text and arrows
  i <- gg + geom_segment(
    x = 1.5, y =  Scorex[1],
    xend = 1.5, yend =  Scorex[2],
    lineend = "round", # See available arrow types in example above
    linejoin = "round",
    size = .5, 
    arrow = arrow(length = unit(0.2, "cm")),
    colour = "#EC7014" # Also accepts "red", "blue' etc
  ) 
  
  k <- i + geom_text( aes(
    x = 1.5, y = (Scorex[1]+Scorex[2])/2,
    label = paste0("Adjusted odds of response ",p0(exp(   max(Scorex[2],Scorex[1]) -  min(Scorex[2],Scorex[1])  ))," x"), 
    group = NULL,
    vjust = -1, #.3
    hjust = .7 #1
  ), size=2.8 , color="#EC7014") 
  
}













#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function to format 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# https://stackoverflow.com/questions/3245862/format-numbers-to-significant-figures-nicely-in-r
formatz <- function(x){
  
  if (!is.na(x)  ) {
    
    formatC(signif(x,digits=5), digits=5,format="fg", flag="#",big.mark=",")
    
  }
  
}

formatz0 <- function(x){
  sprintf(x, fmt = '%s')  
}
formatz1 <- function(x){
  sprintf(x, fmt = '%#.1f')  
}
formatz2 <- function(x){
  sprintf(x, fmt = '%#.2f')  
}
formatz00 <- function(x){
  round(x,0) 
}
formatz4 <- function(x){
  sprintf(x, fmt = '%#.4f')  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logit <- function(p) log(1/(1/p-1))
expit <- function(x) 1/(1/exp(x) + 1)
inv_logit <- function(logit) exp(logit) / (1 + exp(logit))
is.even <- function(x){ x %% 2 == 0 } # function to identify odd maybe useful

options(width=200)
options(scipen=999)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
# function to create data and analyse, ref rms help page
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

coxdata <- function(n, allocation, hr, baseline) { 
  
  #n=1000; allocation =.5; hr=2; baseline=.4
  
  trt <- sample(0:1, n,  rep=TRUE, prob=c(1-allocation, allocation))
  
  cens <- 15*runif(n)
  
  h <- baseline*exp(log(hr)*(trt==1))  # hazard function h(t)
  
  dt <- -log(runif(n))/h
  
  label(dt) <- 'Follow-up Time'
  
  e <- ifelse(dt <= cens,1,0)
  
  dt <- pmin(dt, cens)
  
  units(dt) <- "Year"
  
  d <<- data.frame(cbind(dt,e,trt=trt))  ##why the << required to circumvent error?
  
  dd <<- datadist(d)
  options(datadist='dd')
  
  foo <-d
  # S <- Surv(dt,e)
  f <- cph(Surv(dt,e) ~  trt, x=TRUE, y=TRUE, data=d )
  f0 <- f$coefficients[[1]] #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  LL1 <- f$loglik[2]
  LL0 <- f$loglik[1]
  
  sf <- summary(f)
  
  f1 <- survfit(Surv(dt,e) ~ trt, data = d)
  
  np <- npsurv(Surv(dt,e) ~ trt,d)
  
  S <- Surv(d$dt, d$e)
  
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  d <- plyr::arrange(d,dt)
  d$dt <- dd<- NULL
  d$dt <- sort(2*rexp(nrow(d)))# new times
  
  dx <<- datadist(d)
  options(datadist='dx')
  f0a <- cph(Surv(dt,e) ~  trt, x=TRUE, y=TRUE, data=d )
  f0a <- f0a$coefficients[[1]]
  f2 <- survfit(Surv(dt ,e)  ~ trt, data = d)
  
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  return(list(f=f, d=d, f1=f1, sf=sf, np=np, LL1=LL1, LL0=LL0, S=S,                  
              
              f0=f0,  f2=f2, f0a=f0a, foo=foo))
  
}

# dummy <- coxdata(n=1000, allocation =.5, hr=2, baseline=.4)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function that can use the information from coxdata function to learn about the 
# behind the scenes cal in Cox PH, we allow a guess at HR and see the log likelihood
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## enter hr guess, data, dead var, trt var and treat var

loglike2 <- function(x, dat, dead, indep , time) {
  
  dd <- dat          # make another data object
  dd$dead <- dead    # take the key variables to run Cox PH
  dd$indep <- indep
  dd$time <- time
  
  ## run the analysis to get hr and log like
  ddd <<- datadist(dd)
  options(datadist='ddd')
  
  S <- Surv(time,dead)  # run Cox PH
  f <- cph(S ~  indep, x=TRUE, y=TRUE,dd)
  
  #~~~~~~~~~~extract hr and loglikelihood at null and maximised log likelihood
  dd$hr <- exp(f$coefficients)
  dd$lognull <- f$loglik[[1]]
  dd$lognmax <- f$loglik[[2]]
  
  #~~~~~~~~~using our guess x calculate log likelihood by jand
  dd$expB <- exp(x*dd$indep)
  dd$part1 <- dd$dead  
  dd$part2 <- x*dd$indep 
  
  dd <- plyr::arrange(dd,time)
  
  dd$part3 <- log(rev(cumsum(rev(dd$expB))))
  
  dd$guess <- exp(x)
  dd$likelihoodi <- dd$part1*(dd$part2 - dd$part3)
  dd$L <- sum(dd$likelihoodi)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dd <- as.data.frame(dd)
  dd$dead <- dd$indep <- dd$part1 <- dd$part2 <- dd$part3 <- NULL
  
  return(dd)
  
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Start app
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ui <- dashboardPage(  title="xxxx",
                      # Dashboard header carrying the title of the dashboard,
                      
                      dashboardHeader(title = h4(HTML("name"))),
                      #Sidebar content of the dashboard
                      sidebar <- dashboardSidebar(width=300,
                                                  
                                                  sidebarMenu(
                                                    
                                                    id = "tabs",
                                                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                                    
                                                    br(),
                                                    tags$head(
                                                      tags$style(HTML('#resample{background-color:palegreen}'))
                                                    ),
                                                    actionButton("resample"," Hit to sample another data set", icon = icon("th"),  width =250  ),
                                                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                                    
                                                    menuItem("xxxxxxxxxxxxxx", tabName = "Wiki",      icon = icon("bar-chart-o"), selected = FALSE),
                                                    
                                                    
                                                    menuItem("xxxxxxxxxxxxxx", icon = icon("bar-chart-o"),
                                                             splitLayout(
                                                               
                                                               tags$div(
                                                                 textInput(inputId="n", label='xxxx', width = '90%' , value="800"),
                                                               ),
                                                               
                                                               tags$div(
                                                                 textInput(inputId='allocation', label='xxxx', width = '90%' , ".5"),
                                                               ) 
                                                             ) ,
                                                             
                                                             splitLayout(
                                                               
                                                               tags$div(
                                                                 textInput(inputId='baseline', label='xxxx', width = '90%' , ".4"),
                                                               ),
                                                               
                                                               tags$div(
                                                                 textInput(inputId='hr', label='xxxx', width = '90%' , ".75"),
                                                               ) 
                                                               
                                                             ) 
                                                    ),
                                                    
                                                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                                    menuItem("xxxx",  startExpanded = FALSE,  icon = icon("bar-chart-o"),
                                                             #~~~~~~~~~~~~~~~~~~~~~~~~
                                                             menuSubItem("xxxx",    tabName = "OVERVIEW",  icon = icon("bar-chart-o"), selected = TRUE),
                                                             menuSubItem("xxxx",                 tabName = "RESULTS2",  icon = icon("bar-chart-o")),
                                                             menuSubItem("xxxx",       tabName = "RESULTS3",  icon = icon("bar-chart-o")),
                                                             menuSubItem("xxxx",                    tabName = "HELP",      icon = icon("bar-chart-o"), selected = FALSE),
                                                             menuSubItem("xxxx",         tabName = "RESULTS4",  icon = icon("bar-chart-o")),
                                                             menuSubItem("xxxx",         tabName = "RESULTS1",  icon = icon("table")),
                                                             #~~~~~~~~~~~~~~~~~~~~~~~~
                                                             menuSubItem("xxxx", tabName = "OVERVIEW2",  icon = icon("bar-chart-o"), selected = FALSE),
                                                             #~~~~~~~~~~~~~~~~~~~~~~~~
                                                             menuSubItem("xxxx", tabName = "OVERVIEW3",  icon = icon("bar-chart-o"), selected = FALSE),
                                                             #~~~~~~~~~~~~~~~~~~~~~~~~
                                                             menuSubItem("xxxx",                   tabName = "KMTABLE",  icon = icon("list-alt")),
                                                             #~~~~~~~~~~~~~~~~~~~~~~~~
                                                             menuItem("xxxx",  startExpanded = FALSE,    icon = icon("table"),
                                                                      
                                                                      tags$div(
                                                                        textInput(inputId="guess", label='xxxx', width = '90%' , value="1"),
                                                                      ),
                                                                      
                                                                      menuSubItem("Hit to reveal xxxx",  tabName = "partial")
                                                             )
                                                             #~~~~~~~~~~~~~~~~~~~~~~~~
                                                             
                                                    ),
                                                    #
                                                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                                    menuItem("xxxxxxxxxxxxxx",  startExpanded = FALSE,    icon = icon("table")  ,
                                                             
                                                             tags$div(
                                                               textInput(inputId="base", label='Enter xxxx', width = '90%' , value="0.03"),
                                                             ),
                                                             tags$div(
                                                               textInput(inputId="cens", label='Enter xxxx', width = '90%' , value="0.02"),
                                                             ),
                                                             tags$div(
                                                               textInput(inputId="hr2", label='Enter xxxx', width = '90%' , value="1.2"),
                                                             ),
                                                             tags$div(
                                                               textInput(inputId="per", label='Enter xxxx',        width = '90%' , value="0.70"),
                                                               textInput(inputId="per2", label='Enter xxxx', width = '90%' , value="0.50"),
                                                             ),
                                                             
                                                             menuSubItem("Hit to reveal xxxx",  tabName = "Change"),
                                                             menuSubItem("Hit to reveal xxxx",  tabName = "Changeh")    ,
                                                             menuSubItem("Hit to reveal xxxx",  tabName = "Changeh2") 
                                                    ),
                                                    
                                                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                                    menuItem("xxxxxxxxxxxxxx",  startExpanded = FALSE,    icon = icon("table")  ,
                                                             
                                                             splitLayout(
                                                               tags$div(
                                                                 textInput(inputId="ss", label='Enter xxxx',   value="0.7, 0.5"),
                                                               ),
                                                               tags$div(
                                                                 textInput(inputId="ss2", label='Enter xxxx',  value="11.9, 23.1"),
                                                               )
                                                               
                                                             ),
                                                             
                                                             #~~~~~~~~~~~~~~~~~~~~~~~~~
                                                             splitLayout(
                                                               tags$div(
                                                                 textInput(inputId="tt", label='Enter xxxx',        value="500,500"),
                                                               ),
                                                               tags$div(
                                                                 textInput(inputId="hrx", label='Enter xxxx',       value="1.2"),
                                                               )
                                                               
                                                             ),
                                                             #~~~~~~~~~~~~~~~~~~~~~~~~~
                                                             splitLayout(
                                                               tags$div(
                                                                 textInput(inputId="af", label='Enter xxxx',   value="3"),
                                                               ),
                                                               tags$div(
                                                                 textInput(inputId="af2", label='Enter xxxx',    value="160"),
                                                               )
                                                               
                                                             ),
                                                             #~~~~~~~~~~~~~~~~~~~~~~~~~
                                                             # Here, let us accrue patients over x years, and follow them for an additional x years
                                                             
                                                             splitLayout(
                                                               tags$div(
                                                                 textInput(inputId="sim", label='xxxx',  width = '60%' ,  value="500"),
                                                               ),
                                                               tags$div(
                                                                 
                                                                 textInput(inputId="t2", label='xxxx', width = '55%' , value="0.1"),
                                                               )
                                                               
                                                             ),
                                                             
                                                             #~~~~~~~~~~~~~~~~~~~~~~~~~
                                                             
                                                             menuSubItem("Hit to reveal xxxx",  tabName = "power"),
                                                             menuSubItem("Hit to reveal xxxx",  tabName = "weibull")
                                                             
                                                    ),
                                                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                                    
                                                    menuItem("xxxx",  startExpanded = FALSE,    icon = icon("table"),
                                                             
                                                             tags$div(
                                                               textInput(inputId="shape", label='xxxx', width = '90%' , value="1"),
                                                             ),
                                                             
                                                             tags$div(
                                                               textInput(inputId="scale", label='xxxx', width = '90%' , value="0.03"),
                                                             ),
                                                             
                                                             menuSubItem("Hit to reveal xxxx",  tabName = "survhaz")
                                                    ),
                                                    
                                                    # menuItem("Explanation",                    tabName = "HELP",icon = icon("bar-chart-o"), selected = FALSE),
                                                    # menuItem("Wiki", tabName = "Wiki",                          icon = icon("bar-chart-o"), selected = FALSE),
                                                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                                    menuItem("xxxxxxxxxxxxxx", icon = icon("bar-chart-o"),
                                                             menuSubItem("Shiny",  
                                                                         icon = icon("send",lib='glyphicon'), 
                                                                         href = "https://raw.githubusercontent.com/eamonn2014/PARTIAL-LIKELIHOOD-METHOD/master/app.R"),
                                                             
                                                             menuSubItem("R",  
                                                                         icon = icon("send",lib='glyphicon'), 
                                                                         href = "https://raw.githubusercontent.com/eamonn2014/PARTIAL-LIKELIHOOD-METHOD/master/R%20code%20for%20app.R") 
                                                    ),
                                                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                                    menuItem("References", icon = icon("bar-chart-o"),
                                                             
                                                             menuSubItem(h5(HTML( "xxxx")),  
                                                                         icon = icon("send",lib='glyphicon'), 
                                                                         href = "http://www.stat.cmu.edu/~ryantibs/journalclub/cox_1972.pdf"),
                                                             
                                                             menuSubItem(h5(HTML( "xxxx")),
                                                                         icon = icon("send",lib='glyphicon'), 
                                                                         href = "https://jme.bmj.com/content/medethics/31/12/703.full.pdf") ,
                                                             #dashboardHeader(title = h4(HTML("This title<br/>is just way too long")))
                                                             
                                                             menuSubItem( h5(HTML("xxxx")),  
                                                                          icon = icon("send",lib='glyphicon'), 
                                                                          href = "https://understandinguncertainty.org/node/759"),
                                                             
                                                             menuSubItem( h5(HTML("xxxx")),  
                                                                          icon = icon("send",lib='glyphicon'), 
                                                                          href = "https://github.com/eamonn2014/PARTIAL-LIKELIHOOD-METHOD/blob/master/Analysis%20of%20time-to-event%20for%20observational%20studies.pdf"),
                                                             
                                                             menuSubItem( h5(HTML("xxxx")),  
                                                                          icon = icon("send",lib='glyphicon'), 
                                                                          href = "https://influentialpoints.com/Training/coxs_proportional_hazards_regression_model-principles-properties-assumptions.htm"),
                                                             
                                                             menuSubItem( h5(HTML("xxxx")),  
                                                                          icon = icon("send",lib='glyphicon'), 
                                                                          href = "https://rdrr.io/cran/rms/man/cph.html"),
                                                             
                                                             menuSubItem( h5(HTML("xxxx")),  
                                                                          icon = icon("send",lib='glyphicon'), 
                                                                          href = "https://thestatsgeek.com/2014/03/28/interpreting-changes-in-hazard-and-hazard-ratios")
                                                             
                                                             
                                                             #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                                             
                                                    )
                                                  )
                                                  
                      ),
                      
                      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                      dashboardBody(
                        # https://stackoverflow.com/questions/54876731/inline-latex-equations-in-shiny-app-with-mathjax
                        tags$head(
                          tags$link(rel="stylesheet", 
                                    href="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/katex.min.css", 
                                    integrity="sha384-dbVIfZGuN1Yq7/1Ocstc1lUEm+AT+/rCkibIcC/OmWo5f0EA48Vf8CytHzGrSwbQ",
                                    crossorigin="anonymous"),
                          HTML('<script defer src="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/katex.min.js" integrity="sha384-2BKqo+exmr9su6dir+qCw08N2ZKRucY4PrGQPPWU1A7FtlCGjmEGFqXCv5nyM5Ij" crossorigin="anonymous"></script>'),
                          HTML('<script defer src="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/contrib/auto-render.min.js" integrity="sha384-kWPLUVMOks5AQFrykwIup5lo0m3iMkkHrD0uJ4H5cjeGihAutqP0yW0J6dpFiVkI" crossorigin="anonymous"></script>'),
                          HTML('
    <script>
      document.addEventListener("DOMContentLoaded", function(){
        renderMathInElement(document.body, {
          delimiters: [{left: "$", right: "$", display: false}]
        });
      })
    </script>')
                        ),
                        
                        fluidRow(
                          valueBoxOutput("value1")
                          ,valueBoxOutput("value2")
                          ,valueBoxOutput("value3")
                        ),
                        
                        tabItems(
                          
                          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                          
                          tabItem("xxxxxxxxxxxxxx", 
                                  fluidRow(
                                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                    box(  width=6,
                                          title='Wiki'
                                          ,status = "primary"
                                          ,solidHeader = TRUE 
                                          ,collapsible = TRUE 
                                          
                                          ,p("xxxx")  
                                          
                                          
                                          
                                          
                                    )
                                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                    ,box(
                                      title='Wiki continued'
                                      ,status = "primary"
                                      ,solidHeader = TRUE 
                                      ,collapsible = TRUE 
                                      
                                      ,p("xxxx")
                                      
                                      ,p("xxxx")
                                      
                                      ,p("The last selection provides some links to resources. More useful resources are linked to below:"),
                                      
                                      
                                      tags$a(href = "https://rviews.rstudio.com/2020/11/02/simulating-biologically-plausible-survival-data/", tags$span(style="color:blue", "xxxx"),),
                                      div(p(" ")),
                                      
                                      tags$a(href = "https://thestatsgeek.com/list-of-posts/#surv", tags$span(style="color:blue", "xxxx"),),
                                      div(p(" ")),
                                      
                                      tags$a(href = "https://grokbase.com/t/r/r-help/04a5ydyst0/r-nelson-aalen-estimator-in-r", tags$span(style="color:blue", "xxxx"),),
                                      div(p(" ")),
                                      
                                      tags$a(href = "https://rstudio-pubs-static.s3.amazonaws.com/258589_cd197f86fb5548ac89d7bcffd4bc6afe.html", tags$span(style="color:blue", "xxxx"),),
                                      div(p(" ")),
                                      
                                      tags$a(href = "https://stattools.crab.org/index.html", tags$span(style="color:blue", "xxxx"),),
                                      div(p(" ")),
                                      
                                      tags$a(href = "https://www.itl.nist.gov/div898/handbook/eda/section3/eda3668.htm", tags$span(style="color:blue", "xxxx"),),
                                      div(p(" ")),
                                      
                                      tags$a(href = "https://sas-and-r.blogspot.com/2010/03/example-730-simulate-censored-survival.html?utm_source=feedburner&utm_medium=feed&utm_campaign=Feed%3A+SASandR+%28SAS+and+R%29", tags$span(style="color:blue", "xxxx"),),
                                      div(p(" ")),
                                      
                                      tags$a(href = "https://www.youtube.com/watch?v=EoIB_Obddrk&t=327s&ab_channel=RMSRegression", tags$span(style="color:blue", "xxxx"),),
                                      div(p(" "))
                                      
                                      ,p("xxxx")
                                      ,p("xxxx")
                                      ,p("xxxx")
                                      ,p("xxxx")
                                      
                                    ),  # box end
                                  )
                          ),
                          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                          tabItem("OVERVIEW",
                                  fluidRow(
                                    box(
                                      title = "xxxx"
                                      ,status = "primary"
                                      ,solidHeader = TRUE 
                                      ,collapsible = TRUE 
                                      ,plotlyOutput("plot1", height = "720px"),
                                      
                                      h5(textOutput("Staff_name")),
                                      h5(textOutput("Staff_name3")),
                                      h5(textOutput("Staff_name4")),
                                      h5(textOutput("Staff_name5")),
                                      
                                      tags$a(href = "https://www.youtube.com/watch?v=EoIB_Obddrk&t=327s&ab_channel=RMSRegression", tags$span(style="color:blue", "xxxx"),),
                                      div(p(" "))
                                      
                                      
                                    )
                                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                    ,box(
                                      title='xxxx'
                                      ,status = "primary"
                                      ,solidHeader = TRUE 
                                      ,collapsible = TRUE 
                                      ,plotOutput("plot2", height = "720px")
                                    ))),               
                          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                          tabItem("xxxxxxxxxxxxxx",
                                  fluidRow(
                                    box(
                                      title =   "xxxx"   # uiOutput('product'), 
                                      ,status = "primary"
                                      ,solidHeader = TRUE 
                                      ,collapsible = TRUE 
                                      ,plotlyOutput("plot5a", height = "720px")
                                      ,h5(textOutput("info2"))
                                    )
                                    
                                    ,box(
                                      title="xxxx"
                                      ,status = "primary"
                                      ,solidHeader = TRUE 
                                      ,collapsible = TRUE 
                                      ,plotlyOutput("plot5b", height = "720px"),
                                      h5(textOutput("info"))
                                    ))),   
                          
                          
                          tabItem("xxxxxxxxxxxxxx",
                                  fluidRow(
                                    box(
                                      title =   "xxxxxxxxxxxxxx"     
                                      ,status = "primary"
                                      ,solidHeader = TRUE 
                                      ,collapsible = TRUE 
                                      ,plotOutput("FH", height = "720px")
                                      #,h5(textOutput("info2"))
                                    )
                                    
                                    ,box(
                                      title=" "
                                      ,status = "primary"
                                      ,solidHeader = TRUE 
                                      ,collapsible = TRUE 
                                      ,p("xxxxxxxxxxxxxx")
                                      
                                    ))),   
                          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                          tabItem("xxxxxxxxxxxxxx",
                                  fluidRow(        
                                    
                                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                    box(width=8,
                                        title = "xxxxxxxxxxxxxx"
                                        ,status = "primary"
                                        ,solidHeader = TRUE 
                                        ,collapsible = TRUE 
                                        , DT::dataTableOutput("mytable2")
                                    ))),
                          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                          # new tab
                          tabItem("xxxxxxxxxxxxxx",
                                  fluidRow(
                                    box(
                                      width=7,
                                      # background = "green",
                                      title = "xxxxxxxxxxxxxx"
                                      ,status = "primary"
                                      ,solidHeader = TRUE 
                                      ,collapsible = TRUE 
                                      , DT::dataTableOutput("exercise")
                                    )
                                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                    ,box(
                                      width=5,
                                      title='xxxxxxxxxxxxxx'
                                      ,status = "primary"
                                      ,solidHeader = TRUE 
                                      ,collapsible = TRUE 
                                      , DT::dataTableOutput("exercise2")
                                      ,p("")
                                      ,p("xxxxxxxxxxxxxx")
                                    ))),        
                          
                          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                          tabItem("xxxxxxxxxxxxxx", 
                                  fluidRow(
                                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                    box(  
                                      title='xxxxxxxxxxxxxx'
                                      ,status = "primary"
                                      ,solidHeader = TRUE 
                                      ,collapsible = TRUE ,
                                      textOutput("help"),
                                      withMathJax(),  # need this to be stated
                                      
                                      p("xxxxxxxxxxxxxx"),
                                      
                                      p("xxxxxxxxxxxxxx"),
                                      
                                      p("xxxxxxxxxxxxxx"),   
                                      
                                      p("xxxxxxxxxxxxxx"),
                                      
                                    )
                                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                    ,box(
                                      title='Partial likelihood'
                                      ,status = "primary"
                                      ,solidHeader = TRUE 
                                      ,collapsible = TRUE ,
                                      p("xxxxxxxxxxxxxx")
                                      
                                    ),  # box end
                                    
                                  )
                                  
                          ),
                          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                          tabItem("xxxxxxxxxxxxxx",
                                  fluidRow(
                                    
                                    box(width=6,
                                        title = "xxxxxxxxxxxxxx"
                                        ,status = "primary"
                                        ,solidHeader = TRUE 
                                        ,collapsible = TRUE 
                                        ,plotOutput("plot2x", height = "720px")
                                        ,h5(textOutput("Staff_name2"))
                                    ),
                                    
                                    box(width=6,
                                        title = "xxxxxxxxxxxxxx"
                                        ,status = "primary"
                                        ,solidHeader = TRUE 
                                        ,collapsible = TRUE 
                                        ,plotOutput("plot3", height = "720px")
                                        ,p("xxxxxxxxxxxxxx ")
                                    )
                                    
                                  )),
                          
                          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
                          tabItem("xxxxxxxxxxxxxx",
                                  fluidRow(
                                    
                                    box(width=6,
                                        title='xxxxxxxxxxxxxx'
                                        ,status = "primary"
                                        ,solidHeader = TRUE 
                                        ,collapsible = TRUE 
                                        ,plotOutput("plot4", height = "720px")
                                        ,p("xxxxxxxxxxxxxx")
                                    )
                                    
                                    ,box(width=6,
                                         title='xxxxxxxxxxxxxx'
                                         ,status = "primary"
                                         ,solidHeader = TRUE 
                                         ,collapsible = TRUE 
                                         ,plotOutput("plot2y", height = "720px")
                                    ))),
                          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                          tabItem("survhaz",
                                  fluidRow(
                                    
                                    box(width=6,
                                        title='xxxxxxxxxxxxxx'
                                        ,status = "primary"
                                        ,solidHeader = TRUE 
                                        ,collapsible = TRUE 
                                        ,plotOutput("survhaz", height = "720px")
                                        ,p("xxxxxxxxxxxxxx")
                                    )
                                    
                                    ,box(width=6,
                                         title='Hazard function'
                                         ,status = "primary"
                                         ,solidHeader = TRUE 
                                         ,collapsible = TRUE 
                                         ,plotOutput("survhaz2", height = "720px")
                                         ,p("xxxxxxxxxxxxxx")
                                    ))),
                          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
                          tabItem("Change",
                                  fluidRow(
                                    
                                    box(width=6,
                                        title='xxxxxxxxxxxxxx'
                                        ,status = "primary"
                                        ,solidHeader = TRUE 
                                        ,collapsible = TRUE 
                                        ,plotOutput("plot1c", height = "720px")
                                        ,p("xxxxxxxxxxxxxx")
                                    )
                                    
                                    ,box(width=6,
                                         title='xxxxxxxxxxxxxx'
                                         ,status = "primary"
                                         ,solidHeader = TRUE 
                                         ,collapsible = TRUE 
                                         ,plotOutput("plot1d", height = "720px")
                                         ,h5(textOutput("info4"))
                                         ,h5(textOutput("info5"))
                                         
                                    ))),
                          
                          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
                          
                          tabItem("xxxxxxxxxxxxxx",
                                  fluidRow(
                                    
                                    box(width=6,
                                        title='xxxxxxxxxxxxxx'
                                        ,status = "primary"
                                        ,solidHeader = TRUE 
                                        ,collapsible = TRUE 
                                        ,plotOutput("ploth", height = "720px")
                                        ,p("xxxxxxxxxxxxxx")
                                    )
                                    
                                    ,box(width=6,
                                         title='xxxxxxxxxxxxxx'
                                         ,status = "primary"
                                         ,solidHeader = TRUE 
                                         ,collapsible = TRUE 
                                         ,plotOutput("ploth1", height = "720px")
                                         ,p("xxxxxxxxxxxxxx")
                                         
                                         
                                    ))),
                          
                          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                          
                          tabItem("Changeh2",
                                  fluidRow(
                                    
                                    box(width=12,
                                        title='xxxxxxxxxxxxxx'
                                        ,status = "primary"
                                        ,solidHeader = TRUE 
                                        ,collapsible = TRUE 
                                        ,plotOutput("ploth2", height = "720px")
                                        , p("xxxxxxxxxxxxxx")
                                        
                                    )
                                    
                                    # room for more here?
                                    # ,box(width=6,
                                    #      title='xxxxxxxxxxxxxxxxxxxxxxxxxx'
                                    #      ,status = "primary"
                                    #      ,solidHeader = TRUE 
                                    #      ,collapsible = TRUE 
                                    #    
                                    # )
                                    
                                  )),
                          
                          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                          
                          
                          tabItem("power",
                                  fluidRow(
                                    
                                    box(width=6,
                                        title='xxxxxxxxxxxxxx'
                                        ,status = "primary"
                                        ,solidHeader = TRUE 
                                        ,collapsible = TRUE 
                                        ,plotOutput("powerp1", height = "720px")
                                        ,p("xxxxxxxxxxxxxx")
                                        ,h5(verbatimTextOutput("powerp2"))
                                    )
                                    
                                    ,box(width=6,
                                         title='xxxxxxxxxxxxxx'
                                         ,status = "primary"
                                         ,solidHeader = TRUE 
                                         ,collapsible = TRUE 
                                         
                                         ,plotlyOutput("powerp3", height = "720px")
                                         ,p("xxxxxxxxxxxxxx")
                                         ,h5(verbatimTextOutput("powerp5"))
                                         ,h5(verbatimTextOutput("powerp4"))
                                    ))),
                          
                          
                          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                          
                          tabItem("weibull",
                                  fluidRow(
                                    
                                    box(width=6,
                                        title='xxxxxxxxxxxxxx'
                                        ,status = "primary"
                                        ,solidHeader = TRUE 
                                        ,collapsible = TRUE 
                                        ,plotOutput("powerw", height = "720px")
                                        
                                    )
                                    
                                    ,box(width=6,
                                         title='xxxxxxxxxxxxxx'
                                         ,status = "primary"
                                         ,solidHeader = TRUE 
                                         ,collapsible = TRUE 
                                         ,plotlyOutput("powerp3w", height = "720px")
                                         
                                    ))),
                          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                          
                          tabItem("KMTABLE",
                                  fluidRow(
                                    box(
                                      width=6,
                                      # background = "green",
                                      title = "xxxxxxxxxxxxxx"
                                      ,status = "primary"
                                      ,solidHeader = TRUE 
                                      ,collapsible = TRUE 
                                      , DT::dataTableOutput("KM")
                                    )
                                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                    ,box(
                                      width=6,
                                      title='xxxxxxxxxxxxxx'
                                      ,status = "primary"
                                      ,solidHeader = TRUE 
                                      ,collapsible = TRUE 
                                      , DT::dataTableOutput("CHAZ")
                                      ,p("")
                                      ,p("xxxxxxxxxxxxxx")
                                    ))),        
                          
                          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                          tabItem("RESULTS2",
                                  fluidRow(
                                    box(width=4,
                                        title = "xxxxxxxxxxxxxx" 
                                        ,status = "primary"
                                        ,solidHeader = TRUE 
                                        ,collapsible = TRUE 
                                        ,plotlyOutput("plot99a", height = "720px")
                                        ,p("xxxxxxxxxxxxxx")
                                    )
                                    
                                    ,box(width=4,
                                         title="Cumulative Hazard" 
                                         ,status = "primary"
                                         ,solidHeader = TRUE
                                         ,collapsible = TRUE
                                         ,plotlyOutput("plot99b", height = "720px")
                                         
                                         ,h5(textOutput("info3"))
                                         ,p("xxxxxxxxxxxxxx")
                                    ) 
                                    
                                    ,box(width=4,
                                         title="Complementary log−log" 
                                         ,status = "primary"
                                         ,solidHeader = TRUE
                                         ,collapsible = TRUE
                                         ,plotlyOutput("plot99c", height = "720px")  
                                         ,p("xxxxxxxxxxxxxx")
                                    ) 
                                    
                                    
                                  )  #fluidrow
                          ) #tabitem
                          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                          
                        )
                      ))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# create the server functions for the dashboard  
server <- function(input, output) { 
  output$res <- renderText({
    paste("You've selected:", input$tabs)
  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # https://stackoverflow.com/questions/55043092/r-shinydashboard-display-sum-of-selected-input-in-a-valuebox
  output$value1 <- renderValueBox({
    
    valueBox(
      value =  tags$p(paste0(formatz0(setUpByName())," / ",formatz0(setUpByNamea()) ," / ",formatz00(setUpByNameb()) ," / ",formatz1(setUpByNamec()) ," / ",formatz2(setUpByNamea()/setUpByNameb()  )    )
                      ,style = "font-size: 100%;")
      ,subtitle = tags$p('N; Events (a); Exposure (b); Median surv.; Hazard (a/b)', style = "font-size: 150%;")
      ,icon = icon("stats",lib='glyphicon')
      ,color = "red" )
    
  })
  
  output$value2 <- renderValueBox({
    
    valueBox(
      value =  tags$p(paste0(formatz0(setUpByName2())," / ",formatz0(setUpByName2a()) ," / ",formatz00(setUpByName2b()) ," / ",formatz1(setUpByName2c()) ," / ",formatz2(setUpByName2a()/setUpByName2b()  )    )
                      ,style = "font-size: 100%;")
      ,subtitle = tags$p('N; Events (a); Exposure (b); Median surv.; Hazard (a/b)', style = "font-size: 150%;")
      ,icon = icon("stats",lib='glyphicon')
      ,color = "teal")
    
  })
  
  output$value3 <- renderValueBox({
    
    valueBox(
      value =  tags$p(paste0(formatz2(setUpByName4())," ( ",formatz2(setUpByName5()),", ",formatz2(setUpByName6())," ) " ," ; ",formatz1(setUpByNameLL()))
                      ,style = "font-size: 100%;")
      ,subtitle = tags$p(paste0("Hazard ratio trt 1 v 0 with 95% conf. ; log likelihood"), style = "font-size: 150%;")
      ,icon = icon("education",lib='glyphicon')
      ,color = "green")
    
  }) 
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # This is where a new sample is instigated and inputs converted to numeric
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  random.sample <- reactive({
    
    foo <- input$resample
    
    n <- as.numeric(input$n)
    
    allocation <-as.numeric(input$allocation)
    
    hr <- as.numeric(input$hr)
    
    baseline <- as.numeric(input$baseline)
    
    ###############################################
    
    base <- as.numeric(input$base)
    cens <- as.numeric(input$cens)
    hr2 <-  as.numeric(input$hr2)
    per <-  as.numeric(input$per)
    per2 <-  as.numeric(input$per2)
    ###############################################
    
    return(list(  
      
      n=n,
      allocation =allocation,
      hr=hr,
      baseline=baseline,
      
      base =  base,
      cens =  cens,
      hr2  =  hr2,
      per=per,
      per2=per2
      
    ))
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # start of power section
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  power <- reactive({
    
    foo <- input$resample
    
    ss <- as.numeric(unlist(strsplit(input$ss,",")))
    
    ss2x <- as.numeric(unlist(strsplit(input$ss2,",")))
    
    tt <- as.numeric(unlist(strsplit(input$tt,",")))
    
    af <- as.numeric(unlist(strsplit(input$af,",")))  
    
    af2 <- as.numeric(unlist(strsplit(input$af2,","))) 
    
    hrx <- as.numeric(unlist(strsplit(input$hrx,",")))
    
    nonc <- as.numeric(unlist(strsplit(input$t2,",")))
    
    sim <- as.numeric(unlist(strsplit(input$sim,",")))
    
    return(list(  
      
      ss1=ss[1],
      ss2=ss[2],
      prob1=ss2x[1],
      prob2=ss2x[2],
      
      nc=tt[1],
      ni=tt[2],
      
      AA=af[1],
      FF=af2[1],
      
      hrx=hrx[1],
      
      nonc=nonc[1],
      
      sim=sim[1]
      
    ))
    
  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  power1 <- reactive({
    
    sample <- power()
    
    t1=sample$ss1
    t2=sample$ss2
    
    s1=sample$prob1
    s2=sample$prob2
    
    N1=sample$nc
    N2=sample$ni
    
    start=sample$AA
    fini=sample$FF
    
    hrx=sample$hrx
    drop=sample$nonc
    sim=sample$sim
    
    # Frank Harrell functions
    library(Hmisc) 
    
    Weib.p <- Weibull2(c(s1,s2),c(t1,t2))
    
    rcens <- function(n) runif(n, start, fini)#
    
    ff.dropout <- Quantile2(Weib.p,hratio=function(x) hrx,
                            dropout=function(x) drop)
    
    #plot(ff.dropout)
    
    rcontrol <-      function(n) ff.dropout(n, what='control')
    rintervention <- function(n) ff.dropout(n, what='intervention')
    
    x<-spower(rcontrol, rintervention, rcens, pr=FALSE,
              nc=N1, 
              ni=N2,
              test=logrank, nsim=sim, alpha=0.025, cox=TRUE)
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # get one simulation realization and plot it for information
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    ##lifted from spower function
    
    yc <- rcontrol(N1)
    yi <- rintervention(N2)
    cens <- rcens(N1+ N2)
    group <- c(rep(0, N1), rep(1, N2))
    y <- c(yc, yi)
    maxfail <- 0
    maxcens <- 0
    maxfail <- max(maxfail, max(y))
    maxcens <- max(maxcens, max(cens))
    S <- cbind(pmin(y, cens), 1 * (y <= cens))
    nexceed <- 0 + (logrank(S, group) > .025)
    fit <- coxph.fit(as.matrix(group), S, strata = NULL, 
                     offset = NULL, init = NULL, control = coxph.control(iter.max = 10, 
                                                                         eps = 0.0001), method = "efron", 
                     rownames = NULL)
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    d <- cbind(S, group)
    d <- data.frame(d)
    names(d) <- c("dt","e", "trt")
    dd <<- datadist(d)
    options(datadist='dd')
    
    fit <- cph(Surv(dt, e) ~ trt, data = d, x=TRUE, y=TRUE, surv=TRUE)
    
    
    # survplot(fit, type="kaplan-meier", conf.int = TRUE, 
    #          col.fill=c("firebrick1","cyan2"), grid=TRUE, what='survival')
    
    
    f1 <- survfit(Surv(dt,e) ~ trt, data = d)
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    return(list(x=x, f=ff.dropout , f1=f1, fit=fit)) 
    
    
  })
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # estimating hazard plot
  output$powerp1 <-renderPlot({     
    
    H=power1()$f
    plot(H)
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$powerp3w <- output$powerp3 <-renderPlotly({     
    
    f1= power1()$f1
    fit=power1()$fit
    
    p1 <- ggsurvplot(f1, main = "Kaplan-Meier Curve",
                     
                     
                     legend.title = "Trt."
                     
                     #,xlab=paste0("Time : HR=",round(exp(fit$coefficients),4))
                     ,xlab= "Time"
                     
    )
    ggplotly(p1[[1]] )
  })
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$powerp2 <- renderPrint({  # renderText not so useful
    
    sample <- random.sample()
    
    x <- power1()$x
    
    return(print(x, digits=4))
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$powerp4 <- renderPrint({  # renderText not so useful
    
    sample <- random.sample()
    
    x <- power1()$fit
    
    return(print(summary(x), digits=4))
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$powerp5 <- renderPrint({  # renderText not so useful
    
    sample <- random.sample()
    
    x <- power1()$fit
    
    return(print((x), digits=4))
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Here we plot weibull distributions
  output$powerw <-renderPlot({  
    
    library(Hmisc)
    
    sample <- power()
    
    s1=sample$ss1
    s2=sample$ss2
    
    t1=sample$prob1
    t2=sample$prob2
    
    hr=sample$hrx
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    Weib.p <- Weibull2(c(t1,t2),c(s1,s2))
    #Weib.p
    ff <- Quantile2(Weib.p,hratio=function(x) hr ) # we get weibull parameters 
    
    ##pull out intervention survival probs
    fff <-      attributes(ff)  #lets get the data
    time <-     fff$plot.info$`I Survival`$Time
    survival <- fff$plot.info$`I Survival`$Survival
    #plot(survival~time, lty=1)   # interventions
    
    #pull out control survival data
    timec <-     fff$plot.info$`C Survival`$Time
    survivalc <- fff$plot.info$`C Survival`$Survival
    
    ###~~~~~~~~~~~~~~~~~~~~~~~~# what time is survival at t in intevention
    
    s50i <- which.min(abs(survival-s2)) # what index is time ~ .5
    s70i <- which.min(abs(survival-s1))
    
    Weib.i <- Weibull2(c(  time[s70i], time[s50i]),c(s1, s2))
    #Weib.i
    ffi <-   Quantile2(Weib.i,hratio=function(x) 1)  # use hr of 1 here so no intervention effect
    fff <-   attributes(ffi)  #lets get the data
    timei <- fff$plot.info$`I Survival`$Time
    survivali <- fff$plot.info$`I Survival`$Survival
    
    #par(mfrow=c(2,2))
    #plot(survivalc~timec,    type = "l", lty = 1 , main ="Control arm") # ok
    
    time <-       fff$plot.info$`C Survival`$Time
    survival   <- fff$plot.info$`C Survival`$Survival
    #plot(survival~time,    type = "l", lty = 2,  main ="intervention arm") 
    #plot(ff)
    #plot(ffi)
    #par(mfrow=c(1,1))
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # control, pull out weibull parameters
    p <- lapply(Weib.p, unlist)
    t1x <- (p$alpha)
    t2x <- (p$gamma)
    
    # intervention, pull out weibull parameters
    i <- lapply(Weib.i, unlist)
    t3x <- (i$alpha)
    t4x <- (i$gamma)
    
    A <- expression( paste("control ",      alpha) )
    B <- expression( paste("control ",      gamma) )
    C <- expression( paste("intervention ", alpha) )
    D <- expression( paste("intervention ", gamma) )
    
    #dweibull(x, shape=gamma, scale = 1/alpha), from=0, to=40)
    FF <- expression( paste("Note the Weibull parameterisation shape= ",      gamma," scale=1/ ",      alpha) ) 
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~,
    plot(survivalc~timec,    type = "l", lty = 2,  ylab="Probability of survival",
         
         main =paste("Weibull distibutions, intervention HR =",hr) , col='red', xlab= "Time",
         sub=FF)
    
    lines(survivali~timei, type = "l", lty = 1, col='blue')  
    
    # help placing texxt
    jump <- .85
    jump0 <-.9
    jump1 <-.76
    jump2 <-.72
    
    if (hr < 1) {
      
      jump <- .5
      jump0 <-.55
      jump1 <-.46
      jump2 <-.46  #76
      
    }
    
    text(quantile(prob=jump0,c(timec,timei)),  0.90, c(round(t1x ,5)), cex = 1)
    text(quantile(prob=jump1,c(timec,timei)), 0.90, A,                cex = 1)
    
    text(quantile(prob=jump0,c(timec,timei)),  0.96, c(round(t2x ,5)), cex = 1) #90
    text(quantile(prob=jump1,c(timec,timei)), 0.96, B,                cex = 1)
    
    text(quantile(prob=jump0,c(timec,timei)),  0.78, c(round(t3x ,5)), cex = 1)
    text(quantile(prob=jump1,c(timec,timei)), 0.78, C,                cex = 1)
    
    text(quantile(prob=jump0,c(timec,timei)),  0.84, c(round(t4x ,5)), cex = 1) #78
    text(quantile(prob=jump1,c(timec,timei)), 0.84, D,                 cex = 1)
    
    s1i=survival[which.min(abs(time-t1))]
    s2i=survivali[which.min(abs(timei-t2))]
    
    text(quantile(prob=jump2,c(timec,timei)), 0.72, paste0("At time ",t1,":"),   cex = 1)
    text(quantile(prob=jump,c(timec,timei)), 0.66, paste0("Control survival prob ",s1," "),   cex = 1)
    text(quantile(prob=jump,c(timec,timei)), 0.60, paste0("Interv. survival prob ",round(s1i,2),""),   cex = 1)
    
    text(quantile(prob=jump2,c(timec,timei)), 0.54, paste0("At time ",t2,":"),   cex = 1)
    text(quantile(prob=jump,c(timec,timei)), 0.48, paste0("Control survival prob ",s2," "),   cex = 1)
    text(quantile(prob=jump,c(timec,timei)), 0.42, paste0("Interv. survival prob ",round(s2i,2)," "),   cex = 1)
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # add arrows to explain  
    arrows(                                   
      t1,                                  # x start  
      s1 ,                                 # surv prob at t1 in control
      t1 ,                                 # x finish
      s1i ,                                # surv prob at t1 in intervention     
      col="black", lty=1 )       
    
    arrows(                                   
      t2,                                    # x start  
      s2 ,                                   # surv prob at t2 in control
      t2 ,                                   # x finish
      s2i,                                   # surv prob at t2 in intervention     
      col="black", lty=1 )          
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # end of power section
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # The change in hazard tab, data generation
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  datc <- reactive({
    
    sample <- random.sample()
    
    n =        sample$n
    lambdaT =  sample$base  # event hazard
    lambdaC =  sample$cens  # censoring haz
    beta1 =    sample$hr2   # hr
    per =      sample$per   # survival probability
    per2 =     sample$per2  # survival probability
    
    #  n=800; lambdaT=14.4; lambdaC=12; beta1=1.2; per=0.7 ;  # use this for on the fly
    
    beta1 <- log(as.numeric(beta1))  # log hr
    
    x1 = sample(0:1, n,replace=TRUE)  # assign trt randomly
    
    # create distributions
    T = rweibull(n, shape=1, scale=1/lambdaT)*exp(-beta1*x1)   # say if lambda is entered as 14 big hazard need to use 1/14 here?
    C = rweibull(n, shape=1, scale=1/lambdaC)                  #censoring time
    time = pmin(T,C)  # observed time is min of censored and true, pmin useful here
    event = time==T   # set to 1 if event is observed
    
    # run cox regression
    require(survival)
    f <- coxph(Surv(time, event)~ x1 , method="breslow")
    survfit <- survfit(Surv(time,event) ~ x1)
    # f
    #plot(survfit, ylab="Survival probability", xlab="Time", col=c('blue','red'))
    #run weibull regression
    w <- survreg(formula = Surv(time, event) ~ x1, dist = "w", control = list(maxiter = 90) )
    
    # grab the hr estimates from weibull
    hr <-  (c(w$coefficient[2],  confint(w)[2,]))
    hr <- exp(hr)      #exp(-coef(f))^exp(coef(f)["shape"])
    # grab the hr estimates from Cox
    hrc <- exp(c(f$coefficient,  confint(f) ) )
    
    # capture for later
    ss <- Surv(time, event)
    
    #  for practice run parametric regression using harrell package
    dd <<- datadist( x1=x1)
    options(datadist='dd')
    f.exp  <- psm(ss  ~ x1, dist ='exponential')
    fw    <-  psm(ss  ~ x1,  dist ='weibull')
    
    d <- table(x1,event)
    ev <- d[,"TRUE"]  # number of events
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # new, plotting the hazard, Dirk F moore page 38
    
    library(muhaz)
    
    # plotH
    result.simple <- muhaz(time, event, max.time=max(time),
                           bw.grid=2.25, bw.method="global", b.cor="none")
    
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # PlotH1
    result.pe5 <- pehaz(time, event, width=5, max.time=max(time))  # in the text these are months, so will need to adjust? 
    result.pe1 <- pehaz(time, event, width=1, max.time=max(time))
    
    result.smooth <- muhaz(time, event, bw.smooth=max(time),
                           b.cor="left", max.time=max(time))
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #One use of smoothing the hazard function is to obtain a smooth estimate of the
    # survival function
    
    haz <- result.smooth$haz.est
    times <- result.smooth$est.grid
    survx <- exp(-cumsum(haz[1:(length(haz)-1)]*diff(times)))
    
    # We may compare our
    # smoothed survival estimate to the Kaplan-Meier estimate as follows:
    result.km <- survfit(Surv(time, event) ~ 1,
                         conf.type="none")
    
    return(list(s=survfit, f=f ,w=w , hr=hr, hrc=hrc, ev=ev, d=d, result.simple=result.simple,
                result.pe5= result.pe5,result.pe1=result.pe1,result.smooth=result.smooth,  result.km= result.km,
                time=time, event=event , survx=survx, times=times)) 
    
  })
  
  # right exponential plot....................., 
  output$plot1d<-renderPlot({     
    
    sample <- random.sample()
    
    n =        sample$n
    lambdaT =  sample$base  # event hazard
    lambdaC =  sample$cens  # censoring haz
    beta1 =    sample$hr2   # hr
    per =      sample$per   # survival probability
    per2 =     sample$per2  # survival probability
    
    s <- datc()$s  # survfit object
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    yo <- abs(100*((beta1/1)-1))
    
    wordd <- ifelse(beta1 < 1,"will reduce", 
                    ifelse(beta1 > 1, "will increase","will not change")) 
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # plot weibull density
    x <- curve(dweibull(x, shape=1, scale = 1/lambdaT), from=0, to=max(s$time))  
    x$y <- x$y/max(x$y)    #scale the weibull to max of 1 
    plot(x, type = "l", lty = 1, col='blue' , ylab="Survival probability", xlab="Time" , ylim=c(0,1))
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    text(x = max(s$time)*.58, y = .99,                # Text with different color & size
         paste0(" For the blue reference curve:"),
         col = "#1b98e0",
         cex = 1)
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    text(x = max(s$time)*.655, y = .97,                # Text with different color & size
         paste0(" At ",  formatz1(-log(per)* 1/lambdaT), " months the survival probability is ",per* 100,"%"),
         col = "#1b98e0",
         cex = 1)
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    text(x = max(s$time)*.66, y = .95,                # Text with different color & size
         paste0(" At ",  formatz1(-log(per2)* 1/lambdaT), " months the survival probability is ",per2* 100,"%"),
         col = "#1b98e0",
         cex = 1)
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    text(max(s$time)*.57, 0.92, expression( paste(
      "Using S(1)t = S(0)t"^{exp(beta.x)}
    )), cex = 1)
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    text(x = max(s$time)*.7, y = .88,                # Text with different color & size
         paste0(" Postulating treatment " ,wordd," the hazard by ",yo,"%"),
         col = "red",
         cex = 1)
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    text(x = max(s$time)*.69, y = .86,                # Text with different color & size
         paste0(" At ",  formatz1(-log(per)*  1/lambdaT), " months the survival probability becomes ",formatz00(100*(per)^(beta1)) ,"%"),
         col = "red",
         cex = 1)
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    text(x = max(s$time)*.695, y = .84,                # Text with different color & size
         paste0(" At ",  formatz1(-log(per2)*  1/lambdaT), " months the survival probability becomes ",formatz00(100*(per2)^(beta1)) ,"%"),
         col = "red",
         cex = 1)
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # with the effect of trt and plot
    y <- x$y^(beta1)   # add another line based on S1(t) = S(0) ^exp(B)
    lines(y~x$x, col='red')     
    
    # add arrows to explain  
    arrows(                                  # x start
      -log(per2) *1/lambdaT,                   # time  
      per2 ,#                                # surv prob  
      -log(per2) *1/lambdaT ,  
      per2^beta1 ,
      col="black", lty=1 )       
    
    arrows(                                  # x start
      -log(per) *1/lambdaT,    
      per, # 
      -log(per) *1/lambdaT ,  
      per^beta1 ,
      col="black", lty=1 )    
    
    
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # left plot
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$plot1c<-renderPlot({     
    
    sample <- random.sample()
    
    n =        sample$n
    lambdaT =  sample$base  # event hazard
    lambdaC =  sample$cens  # censoring haz
    beta1 =    sample$hr2   # hr
    per =      sample$per   # survival probability
    # per2 =     sample$per2  # survival probability
    
    hr <- datc()$hr     # Weibull hr
    hrc <- datc()$hrc   # cox hr
    
    ev <- datc()$ev    # events
    
    N <- datc()$d      # sample size
    
    f <- datc()$f      # Get the data
    s <- datc()$s      # survfit object
    
    plot(s, ylab="Survival probability", xlab="Time", col=c('blue','red'))  # plot the survfit object
    
    text(x = max(s$time)*.58, y = .99,                # Text with different color & size
         paste0("HR from Weibull=",formatz2(1/hr[1])," 95%CI ( ",formatz2(1/hr[3]),", ",formatz2(1/hr[2])," )"),
         col = "#1b98e0",
         cex = 1)
    text(x = max(s$time)*.58, y = .97,                # Text with different color & size
         paste0("HR from Cox PH=",formatz2(hrc[1])," 95%CI ( ",formatz2(hrc[2]),", ",formatz2(hrc[3])," )"),
         col = "#1b98e0",
         cex = 1)
    text(x = max(s$time)*.54, y = .95,                # Text with different color & size
         paste0( "Actual N= ",sum(N), ", Actual events= ",sum(ev)),
         col = "#1b98e0",
         cex = 1)
    
    
    ## power 
    A <- 0.05 # alpha
    B <- 0.1  # beta
    
    s1=per
    s2=per^(beta1)
    
    # (-log(per)* lambdaT)  # time of s1 s2
    
    f <- ((qnorm(1-A/2))+qnorm(1-B))^2
    d1 <- (4*f)/ (log(beta1)^2)
    
    N2 <- d1/(1-(s1+s2)/2)
    
    # drop this as does not seem correct..come back and check
    # text(x = max(s$time)*.667, y = .93,                # Text with different color & size
    #      paste0( "Alpha 2 sided ",A," Power ",1-B," events required= ",ceiling(d1), ", N= ",ceiling(N2)),
    #      col = "#1b98e0",
    #      cex = 1)
    
    
    
    
    
    # abline(h=.5,                lty=2, col='blue')
    # abline(v=-log(.5)*lambdaT,  lty=2, col='blue')
    # abline(h=per,               lty=2, col='red')
    # abline(v=-log(per)*lambdaT, lty=2, col='red')
    
    
    #lines(x=.5, y=-log(.5)*lambdaT , col="pink")
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    
    #   segments(0,                0.5, -log(.5)*lambdaT , 0.5 , col="blue", lty=2 ) #h
    # segments( -log(.5)*lambdaT,  0, -log(.5)*lambdaT , 0.5 , col="blue", lty=2 ) #v
    # 
    # segments(0,                per , -log(per)*lambdaT , per , col="blue", lty=2 )  #h
    # segments( -log(per)*lambdaT,  0, -log(per)*lambdaT , per , col="blue", lty=2 )  #v
    # 
    # #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    # 
    # segments(0,                              # x start
    #          per^beta1 ,                     # x finish
    #          -log(per^beta1)*lambdaT ,       # y start 
    #          per^beta1  , col="red", lty=2 ) # y finsh
    # 
    # 
    # segments(0,                                   # x start
    #          0.5^beta1,                           # x finish
    #          -log(.5^beta1) *lambdaT ,            # y start
    #          0.5^beta1 ,  col="red", lty=2 )      # y finish
    
    
    # segments(                                  # x start
    #          -log(.5) *lambdaT,    
    #          0.5^beta1, # 
    #           -log(.5) *lambdaT ,  
    #          0.5 ,
    #           col="purple", lty=1 )       
    # 
    # segments(                                  # x start
    #   -log(per) *lambdaT,    
    #   per^beta1, # 
    #   -log(per) *lambdaT ,  
    #   per ,
    #   col="darkgreen", lty=1 )    
    # 
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  
  # estimating hazard plot
  output$ploth <-renderPlot({     
    
    H=datc()$result.simple
    plot(H)
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  
  output$ploth1 <-renderPlot({     
    
    H=datc()$result.pe5
    H1=datc()$result.pe1
    HS=datc()$result.smooth
    
    
    plot(H,  col="green")
    lines(H1, col='blue')
    lines(HS, col='red')
    
    
  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  
  output$ploth2 <-renderPlot({     
    
    H2=datc()$result.km
    
    times=datc()$times
    result.smooth=datc()$result.smooth
    
    haz <- result.smooth$haz.est
    times <- result.smooth$est.grid
    surv <- exp(-cumsum(haz[1:(length(haz)-1)]*diff(times)))
    
    plot(H2, conf.int=F, mark="|", xlab="Time",  # confint true throws an error
         #xlim=c(0,30), 
         ylab="Survival probability")
    lines(surv ~ times[1:(length(times) - 1)], col='blue')
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  
  
  output$survhaz <-renderPlot({     
    
    alpha<- as.numeric(input$shape)
    
    lambda <- as.numeric(input$scale)
    
    weibSurv <- function(t, shape, scale) pweibull(t, shape=shape,
                                                   scale=scale, lower.tail=F)
    curve(weibSurv(x, shape=alpha, scale=1/lambda), from=0, to=2/lambda,
          ylim=c(0,1), ylab='Survival probability', xlab='Time')
    
    text(x = 2/lambda*.85, y = .95,                # Text with different color & size
         paste0(" Shape ", alpha,  ""),
         col = "blue",
         cex = 1.2)
    
    text(x = 2/lambda*.85, y = .9,                # Text with different color & size
         paste0("  Scale ", lambda ,""),
         col = "blue",
         cex = 1.2)
    
    
    text(x = 2/lambda*.85, y = .85,                # Text with different color & size
         paste0(" Mean ", formatz2(gamma(1 + 1/alpha)/lambda),  ""),
         col = "blue",
         cex = 1.2)
    
    
    
    text(x = 2/lambda*.85, y = .8,                # Text with different color & size
         paste0( " Median ",  formatz2((log(2)^(1/alpha))/lambda ),""),
         col = "blue",
         cex = 1.2)
    
  })
  
  output$survhaz2 <-renderPlot({     
    
    alpha<- as.numeric(input$shape)
    
    lambda <- as.numeric(input$scale)
    
    weibHaz <- function(x, shape, scale) 
      dweibull(x, shape=shape, scale=scale)/
      pweibull(x, shape=shape, scale=scale,lower.tail=F)
    
    curve(weibHaz(x, shape=alpha, scale=1/lambda), from=0, to=2/lambda,
          ylab='Hazard', xlab='Time', col="red")
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # text below right plot in changing hazard 
  output$info4 <- renderText({  
    
    sample <- random.sample()
    
    n = sample$n
    
    lambdaT =  sample$base
    lambdaC =  sample$cens
    beta1 =    sample$hr2
    per=       sample$per
    per2=       sample$per2
    
    #s <-  datC()$w
    #hr <- datC()$hr
    
    yo <- abs(100*((beta1/1)-1))
    
    wordd <- ifelse(beta1 < 1,"will reduce", 
                    ifelse(beta1 > 1, "will increase ","will not change")) 
    
    c(paste0(" 
    With a shape parameter of 1 and a scale parameter of ",lambdaT," a survival curve is created based on a Weibull distribution.
    When the shape parameter equals 1 the Weibull reduces to an exponential distribution. Let's assume time is measured in months.
    The time to reach median survival is equal to -log(0.5) / ",formatz2(lambdaT),". 
             This equates to a median survival of ", formatz2(-log(.5)*1/lambdaT)," months. 
             
             Replacing 0.5 with a desired survival percentile will return the associated time. 
             Enter survival probabilities in the two boxes on the left. 
             
             The time at which the survival probability is ",per* 100,"% is ",  formatz2(-log(per)* 1/lambdaT), " months.
             The time at which the survival probability is ",per2*100,"% is ",  formatz2(-log(per2)*1/lambdaT), " months"
    ))
    
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  # another piece of text below right plot in changing hazard 
  
  output$info5 <- renderText({  
    
    sample <- random.sample()
    
    n=          sample$n
    
    lambdaT =  sample$base
    lambdaC =  sample$cens
    beta1 =   sample$hr2
    per = sample$per
    per2 = sample$per2
    
    yo <- abs(100*((beta1/1)-1))
    
    wordd <- ifelse(beta1 < 1,"will reduce", 
                    ifelse(beta1 > 1, "will increase ","will not change")) 
    
    
    c(paste0("Now if we are postulating that a new treatment " ,wordd," the hazard by ",yo,"% we can use 
               the fact S1(t) = S0(t)^exp Bx, with exp Bx=", (beta1)," the 
              probability of survival at ",  formatz2(-log(per)*  1/lambdaT), " months now becomes ",formatz2((per)^(beta1)) ,".
          The probability of survival at ",  formatz2(-log(per2)* 1/lambdaT), " months now becomes ",formatz2((per2)^(beta1)) ,"
          . See the arrows in the plot showing the changes in survival. 
             " ) )
    
  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  
  
  # theoretical median, self learning txt p301
  #   -log(.5)*lambdaT   # qweibull(.5,1,15.6)           ## median
  #  -log(.7)*lambdaT                                   ## survival prob 70%
  
  ## surv rates 5 year  survival 70%  
  ## surv rates 10 year survival 50%
  
  ## detect 20% increase in haz rate, what does this inply for 5 n 10 year surv rates
  ## S1(t) = So(t)expBx =0.7^1.2=65.2 
  ###                    0.5^1.2=43.5%
  ## so 5 yr drops to 65.2 and 10 yr drops to 43.5
  
  
  #mfit$surv
  #  survfit <- survfit(Surv(time,event) ~ x1)
  #  plot(survfit, ylab="Survival probability", xlab="Time")
  #survfit
  #summary(survfit)
  # quantile(survfit, probs = c(.3,.348,0.5, .565,0.7), conf.int = FALSE)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # summary(survfit, times=5)
  #  summary(survfit, times=10)
  
  #return(list(  d=res$d, f=res$f, f1=res$f1, sf=res$sf, np=res$np , LL1=res$LL1, LL0=res$LL0, S=res$S, res=res
  
  
  #  ))
  
  #})
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # GENERATE THE DATA Execute analysis for the landing page
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dat <- reactive({
    
    sample <- random.sample()
    
    n=         sample$n
    allocation=sample$allocation
    hr=        sample$hr
    baseline=  sample$baseline
    
    res <- coxdata(n, allocation, hr, baseline)
    
    return(list(  d=res$d, f=res$f, f1=res$f1, sf=res$sf, np=res$np , LL1=res$LL1, LL0=res$LL0, S=res$S, res=res
                  
                  ,f0a=res$f0a, f0=res$f0, f2=res$f2 #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@      
                  
    ))
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  setUpByName <- reactive ({
    f <-dat()$np  # Get the  data
    f <-  f$n[1]
    y <- as.numeric(as.character(f))
    return(y)
  })
  
  setUpByNamea <- reactive ({
    f <-dat()$np  # Get the  data
    f <-  f$numevents[1]
    y <- as.numeric(as.character(f))
    return(y)
  })
  
  setUpByNameb <- reactive ({
    f <-dat()$np  # Get the  data
    f <-  f$exposure[1]
    y <- as.numeric(as.character(f))
    return(y)
  })
  
  setUpByNamec <- reactive ({
    f <-dat()$np  # Get the  data
    f <-  summary(f)$table[,'median'][1]
    y <- as.numeric(as.character(f))
    return(y)
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  setUpByName2 <- reactive ({
    f <-dat()$np  # Get the  data
    f <-  f$n[2]
    y <- as.numeric(as.character(f))
    return(y)
  })
  
  setUpByName2a <- reactive ({
    f <-dat()$np  # Get the  data
    f <-  f$numevents[2]
    y <- as.numeric(as.character(f))
    return(y)
  })
  
  
  setUpByName2b <- reactive ({
    f <-dat()$np  # Get the  data
    f <-  f$exposure[2]
    y <- as.numeric(as.character(f))
    return(y)
  })
  
  setUpByName2c <- reactive ({
    f <-dat()$np  # Get the  data
    f <-  summary(f)$table[,'median'][2]
    y <- as.numeric(as.character(f))
    return(y)
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  setUpByNameLL <- reactive ({
    f <-dat()$LL1  
    y <- as.numeric(as.character(f))
    return(y)
  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  setUpByName3 <- reactive ({
    f <- dat()$f  # Get the  data
    y <- as.numeric(as.character(f$coefficients))
    return(y)
  })
  
  
  setUpByName4 <- reactive ({
    f <-dat()$sf  # Get the  data
    y <- as.numeric(as.character(f[2,c("Effect")]))
    return(y)
  })
  
  
  setUpByName5 <- reactive ({
    f <-dat()$sf  # Get the  data
    y <- as.numeric(as.character(f[2,c("Lower 0.95")]))
    return(y)
  })
  
  setUpByName6 <- reactive ({
    f <-dat()$sf  # Get the  data
    y <- as.numeric(as.character(f[2,c("Upper 0.95")]))
    return(y)
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # MAIN PLOT! updated with log transformation  option
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$plot5a <- output$plot1 <- renderPlotly({
    
    f <- dat()$f1  # Get the  obj
    
    sf  <- dat()$sf
    X <- as.numeric(as.character(sf[2,c("Effect")]))
    Y <- as.numeric(as.character(sf[2,c("Lower 0.95")]))
    Z <- as.numeric(as.character(sf[2,c("Upper 0.95")]))
    f0 <- dat()$f0
    
    
    f <- dat()$np
    p1 <- survplotp(f, time.inc=1, times=c(5,10))
    
    
    # p2 <- ggsurvplot(f, main = "Kaplan-Meier Curve",
    
    ##   legend.title = "Trt."
    
    ##       ,xlab=paste0("Time : HR=",formatz4(exp(f0)) )
    
    ##)
    ##ggplotly(p1[[1]] )
    
    
    p1
  })
  
  # p <- ggplot(mtcars, 
  #             aes(x = wt, y = qsec, 
  #                 text = paste0("wt: ", round(wt), "</br></br>qsec: ", round(qsec)))) +
  #   geom_point()
  # 
  # ggplotly(p, tooltip = "text")
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$plot99a <- renderPlotly({
    
    f <- dat()$f1  # Get the survfit obj
    
    p1 <-  ggsurvplot(f, fun = "event",   main = "Cumulative proportion",  
                      legend.title = "Trt.")#
    #  palette = c("orange", "purple"))
    ggplotly(p1[[1]])
    
  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$plot99b <- renderPlotly({
    
    f <- dat()$f1  # Get the survfit obj
    
    sample <- random.sample()
    
    hr=        sample$hr
    baseline=  sample$baseline
    
    p1 <- ggsurvplot(f, fun = "cumhaz",  main = "Cumulative Hazard"  ,
                     legend.title = "Trt.")  
    
    p1$plot <- p1$plot + ggplot2::geom_abline(intercept = 0, slope = baseline,    linetype="dotted", col='red') 
    p1$plot <- p1$plot + ggplot2::geom_abline(intercept = 0, slope = baseline*hr, linetype="dotted", col='blue')
    
    # p1$plot <- p1$plot + scale_y_continuous(trans = 'log')
    
    
    
    ggplotly(p1[[1]])
    
  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$plot99c <- renderPlotly({
    
    f <- dat()$f1  # Get the survfit obj
    
    p1 <- ggsurvplot(f, fun = "cloglog",  
                     main = "Complementary log−log" ,
                     legend.title = "Trt.")
    # legend.labs = c("0", "1")) 
    #palette = c("jco"))
    ggplotly(p1[[1]])
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$plot2<-renderPlot({     
    
    f <- dat()$np  # Get the  data
    survdiffplot(f, col='darkgreen' , xlab= "Time")
    
    # survplot(f, conf='diffbands',col='purple',cex.aehaz=5,
    #         col.fill='blue'
    #                      , aehaz=TRUE, #times= c(5), 
    #      label.curves=list(method="arrow", cex=2), 
    #  label.curves=list(keys=1:2, cex=2),
    #      abbrev.label=TRUE, levels.only = FALSE)
    
    
  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$plot2x <-renderPlot({     # Cox
    
    d <- dat()$d
    
    dd <<- datadist(d)
    options(datadist='dd')
    
    fit <- cph(Surv(dt, e) ~ trt, data = d, x=TRUE, y=TRUE, surv=TRUE)
    
    survplot(fit, type="kaplan-meier", conf.int = TRUE, 
             col.fill=c("firebrick1","cyan2"), grid=TRUE, what='survival')
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$plot4<-renderPlot({     
    
    d <- dat()$d  # Get the  obj
    #S <- dat()$S
    
    S <- Surv(d$dt, d$e)
    
    hazard.ratio.plot(d$trt, S, e=20, legendloc='ll', xlab='Time', antilog=FALSE, col='blue', smooth=TRUE,
                      ylim=c(-4,4), ylab=c("Log hazard ratio"))
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$plot2y <-renderPlot({     # Cox
    
    d <- dat()$d
    
    fit <- cph(Surv(dt, e) ~ trt, data = d, x=TRUE, y=TRUE, surv=TRUE)
    
    plot(cox.zph(fit, transform="identity" ), ylim=c(-4,4), ylab=c("Log hazard ratio"))
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$plot3 <- renderPlot({
    
    d <- dat()$d  # Get the  obj
    S <- dat()$S
    f <- dat()$f
    
    #plot(survfit(S~ d$trt), col=c("purple", "orange"), fun="cloglog", xlab="Time", ylab="log(-log(Survival)" , lwd=3)
    survplot(f,logt=TRUE, loglog=TRUE, 
             col=c("red", "lightblue")
             
             # col=c("orange", "purple")
    )   #Check for Weibull-ness (linearity)
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$plot5b <- renderPlotly({
    
    fx <-  dat()$f2 # Get the  obj #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    f0a <- dat()$f0a              #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    p1 <- ggsurvplot(fx, main = "Kaplan-Meier Curve", legend.title = "Trt.",
                     # palette = c("orange", "purple")  ,
                     xlab=paste0("Time : HR=", formatz4(exp(f0a)))
                     # ggtheme = theme_bw() # Change ggplot2 theme
    )
    ggplotly(p1[[1]])
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$mytable <- DT::renderDataTable({
    
    d=dat()$d
    
    d <- plyr::arrange(d, dt)
    
    DT::datatable(d, rownames=FALSE,
                  plugins = 'natural',
                  colnames=c('Time' = 'dt', 'Event or censored' = 'e', 
                             'Treatment'='trt'),
                  
                  options = list(
                    #  dom = 't',
                    columnDefs = list(list(type = 'natural', targets = c(1,2)))
                  )
    ) %>%
      
      formatRound(
        columns= c("Time" ), 
        digits=4 )
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~KM table
  output$KM <- DT::renderDataTable({
    
    library(survminer)
    
    require(ggfortify)
    d=dat()$d
    
    KM_fit <- survfit(Surv(dt, e) ~ trt ,data = d)
    
    KM <- fortify(KM_fit) # fortify(KM_fit, fun = "cumhaz")'
    
    DT::datatable(KM, rownames=FALSE,
                  plugins = 'natural',
                  #   colnames=c('Time' = 'dt', 'Event or censored' = 'e', 
                  #              'Treatment'='trt'),
                  
                  options = list(
                    #  dom = 't',
                    columnDefs = list(list(type = 'natural', targets = c(1,2)))
                  )
    ) %>%
      
      formatRound(
        columns= c("time" ,"surv","std.err","upper","lower"), 
        digits=4 )
  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$CHAZ <- DT::renderDataTable({
    
    library(survminer)
    
    require(ggfortify)
    d=dat()$d
    
    KM_fit <- survfit(Surv(dt, e) ~ trt ,data = d)
    
    KM <-  fortify(KM_fit, fun = "cumhaz")  # convert survival::survfit to data.frame
    
    DT::datatable(KM, rownames=FALSE,
                  plugins = 'natural',
                  #   colnames=c('Time' = 'dt', 'Event or censored' = 'e', 
                  #              'Treatment'='trt'),
                  
                  options = list(
                    #  dom = 't',
                    columnDefs = list(list(type = 'natural', targets = c(1,2)))
                  )
    ) %>%
      
      formatRound(
        columns= c("time" ,"surv","std.err","upper","lower"), 
        digits=4 )
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~maximum likelihood
  
  output$mytable2 <- DT::renderDataTable({
    
    d=dat()$d
    
    f <- dat()$f  # Get the  data
    y <- as.numeric(as.character(f$coefficients))
    guess=y  # we use the actual model hr estimate
    # sample <- random.sample()
    # hr=   sample$hr
    
    d <- plyr::arrange(d, dt)
    
    
    d$expB <- exp(guess*d$trt)
    d$part2 <- guess*d$trt
    d$part3 <- log(rev(cumsum(rev(d$expB))))
    
    d$likelihoodi <- d$e*(d$part2 - d$part3)
    d$LL <- sum(d$likelihoodi)
    
    datatable(d, rownames=FALSE,
              plugins = 'natural',
              colnames=c('Time' = 'dt', 
                         'Event or censored' = 'e', 
                         'Treatment'='trt',
                         'HR'='expB',
                         'Individual likelihoods' ='likelihoodi',
                         'logHR x trt'='part2',
                         'Log(exp HR) of each risk set'='part3',
                         'Sum the Individual likelihoods to give log likelihood' ='LL'
              ),
              
              options = list(
                #  dom = 't',
                columnDefs = list(list(type = 'natural', targets = c(1,2)))
              )
    ) %>%
      
      formatRound(
        columns= c("Time","HR", 
                   #"A",
                   "logHR x trt","Log(exp HR) of each risk set",'Individual likelihoods'), 
        digits=4 ) %>%
      formatRound(
        columns= c("Sum the Individual likelihoods to give log likelihood"), 
        digits=1 )
  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$exercise <- DT::renderDataTable({
    
    dummy = dat()$res
    
    g <- log(as.numeric(input$guess))
    
    foo <- loglike2(g, dat=dummy$d, dead=dummy$d$e, indep=dummy$d$trt, time=dummy$d$dt)  # try
    foo$time =NULL
    foo$expB = NULL
    
    datatable(foo, rownames=FALSE,
              plugins = 'natural',
              colnames=c('Time' = 'dt', 
                         'Event or censored' = 'e', 
                         'Treat.'='trt',
                         'Model Hazard Ratio'='hr',
                         'Null Log Likelihood'='lognull',
                         'Maximised Log Likelihood'='lognmax',
                         'HR guess' ='guess',
                         'Individual likelihoods' ='likelihoodi',
                         'Sum the Individual likelihoods to give log likelihood based on guess' ='L'
              ),
              
              options = list(
                #  dom = 't',
                columnDefs = list(list(type = 'natural', targets = c(1,2)))
              )
    ) %>%
      
      formatRound(
        columns= c("Time","Model Hazard Ratio",  
                   'Individual likelihoods'
        ),
        digits=4 ) %>%
      formatRound(
        columns= c( 'Null Log Likelihood',
                    'Maximised Log Likelihood',
                    'Sum the Individual likelihoods to give log likelihood based on guess'), 
        digits=0 )
    
  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$exercise2 <- DT::renderDataTable({
    
    sample <- random.sample()
    
    allocation=sample$allocation
    hr=        sample$hr
    baseline=  sample$baseline
    
    res <- coxdata(n=10, allocation=allocation, hr=hr, baseline =baseline)
    
    d <- res$d
    d <- plyr::arrange(d, dt)  # sort by time
    
    # Calculate Li for everyone
    d$Numerator   <- exp(res$f$coefficients[[1]] * d$trt)
    d$Denominator <- (rev(cumsum(rev(d$Numerator))))
    d$Li          <- d$Numerator/d$Denominator
    
    # all censored contribute 1 (on multiplicative scale)
    d$Li<- ifelse(d$e %in% 1,d$Li,1)
    
    # get the product of all and log answer
    d$LL <- log(prod(d$Li))  
    d
    # model LL, prove we have ecalc correctly
    res$f$loglik
    
    
    datatable(d, rownames=FALSE,
              plugins = 'natural',
              colnames=c('Time' = 'dt', 
                         'Event or censored' = 'e', 
                         'Treat.'='trt',
                         'Num.'='Numerator',
                         'Den.'='Denominator',
                         'Individual likelihoods'='Li',
                         'log of product of Individual likelihoods to give maximizes log likelihood' ='LL'
                         
              ),
              
              options = list(
                #  dom = 't',
                columnDefs = list(list(type = 'natural', targets = c(1,2)))
              )
    ) %>%
      
      formatRound(
        columns= c("Time","Num.","Den.", 
                   'Individual likelihoods', 'log of product of Individual likelihoods to give maximizes log likelihood'
        ),
        digits=4 ) %>%
      formatRound(
        columns= c( 'Event or censored',
                    'Treat.'), 
        digits=0 )
  })
  
  output$help <- renderText({
    HTML(" ")
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$Staff_name2 <- output$Staff_name <- renderText({  
    
    sf  <- dat()$sf
    X <- as.numeric(as.character(sf[2,c("Effect")]))
    Y <- as.numeric(as.character(sf[2,c("Lower 0.95")]))
    Z <- as.numeric(as.character(sf[2,c("Upper 0.95")]))
    
    Xp  <- X/(X+1)
    Yp  <- Y/(Y+1)
    Zp  <- Z/(Z+1)
    
    wordup <- ifelse(X>1,"higher", "")
    
    wordup2 <- ifelse(X>1,"increase", "reduction")
    
    paste0( "xxxxxxxxxxxxxx")   
    
  })
  
  output$Staff_name3<- renderText({  
    paste0( "xxxxxxxxxxxxxx")
  })
  
  output$Staff_name4<- renderText({  
    paste0( "xxxxxxxxxxxxxx")
  })
  
  output$Staff_name5<- renderText({  
    paste0( "xxxxxxxxxxxxxx") 
    
  })
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # frank Harrell rms page 479
  output$FH <- renderPlot({
    
    sample <- random.sample()
    
    d <- dat()$d
    
    trt <-  d$trt
    e   <-  d$e
    dt  <-  d$dt
    d   <-  d$d
    
    limx <- quantile(dt, prob=.99)
    limx <- max(dt)*.8
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    S <- Surv(dt,e)
    f <- npsurv(S ~ trt)
    
    for (meth in c('exact','breslow','efron')) {
      
      g <- cph(S  ~ trt, method=meth, surv=TRUE, x=TRUE, y=TRUE)
      
    }
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    f.exp  <- psm(S  ~ trt, dist ='exponential')
    fw    <-  psm(S  ~ trt,  dist ='weibull')
    phform <- pphsm(fw)
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    co <- gray(c(0,.8))
    co <- c("red", "lightblue")
    survplot(f, lty=c(1,1)   , lwd=c(1,3), col=co,           label.curves=FALSE, conf='none')
    survplot(g, lty=c(3,3)   , lwd=c(1,3), col=co, add=TRUE, label.curves=FALSE, conf.type='none')
    
    legend(c(limx,160),c(.38,.99),
           c('Nonparametric estimates', 'Cox-Breslow estimates'),
           lty=c(1,3), bty='n',    cex=1.0) # col=co 
    
    legend(c(limx,160),c(.18,.89), 
           c('Trt 0','Trt 1'), lwd=c(1,3), col=co, bty='n',  cex=1.0)
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$info <- renderText({  
    
    c("xxxxxxxxxxxxxx.")
    
  })
  
  output$info2 <- renderText({  
    
    c("xxxxxxxxxxxxxx")
    
  })
  
  output$info3 <- renderText({  
    
    sample <- random.sample()
    
    hr=        sample$hr
    baseline=  sample$baseline
    
    c(paste0("xxxxxxxxxxxxxx"))
    
  })
  
  
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

shinyApp(ui, server)