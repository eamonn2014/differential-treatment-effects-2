#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# blank-template2.app
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


fig.width <- 400
fig.height <- 300
fig.width1 <- 1380
fig.height1 <- 700
fig.width2 <- 1400
fig.height2 <- 300
fig.width3 <- 1400  
fig.height3 <- 600
fig.width4 <- 1380
fig.height4 <- 450
fig.width5 <- 1380
fig.height5 <- 225
fig.width6 <- 400
fig.height6 <- 550
fig.width7 <- 600
fig.widthx <- 593
fig.heightx <- 268
fig.height7 <- 600
fig.width9 <- 1380
fig.height9 <- 679

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



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Start app
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ui <- dashboardPage(  title="xxxxxxxxxxxxx",
                      # Dashboard header carrying the title of the dashboard,
                      
                      dashboardHeader(title = h4(HTML("xxxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxxxx"))),
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
                                                      
                                                      menuItem("Wiki", tabName = "Wiki",      icon = icon("bar-chart-o"), selected = FALSE),
                                                      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                                      menuItem("Models and sample size", tabName = "models",      icon = icon("bar-chart-o"), 
                                                               
                                                               textInput('n',
                                                                         div(h5(tags$span(style="color:yellow", "Sample size"))), value= "1000"),
                                                               
                                                               selectInput("Design",
                                                                           div(h5(tags$span(style="color:yellow", "Select design preference:"))),
                                                                           
                                                                           choices=c(  "No-interaction logit-additive model", 
                                                                                       "Treatment interacts with smoking only" ,
                                                                                       "Treatment interacts with all variables" 
                                                                           ), width='98%'),
                                                               
                                                               selectInput("Model",
                                                                           div(h5(tags$span(style="color:yellow", "Select modelling preference (impacts Table x & tab x & x):"))),
                                                                           choices=c(  "No-interaction logit-additive model",
                                                                                       "Treatment interacts with smoking only" ,
                                                                                       "Treatment interacts with all variables"
                                                                           ), width='98%'),
                                                               
                                                               selected = FALSE),
                                                      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                                      menuItem("Define parameters ", icon = icon("bar-chart-o"),
                                                               
                                                               
                                                               #~~~~~~
                                                               splitLayout(
                                                                 
                                                               tags$div(
                                                                 textInput("v1", div(h5(tags$span(style="color:white", "Trt 3 arms"))), width = '90%' ,value= "1"),
                                                               ),
                                                               
                                                               tags$div(
                                                                 textInput("v2", div(h5(tags$span(style="color:white", "Age (continuous)"))), width = '90%' , value= "1/(65-18)"),
                                                               ),
                                                                 tags$div(
                                                                   textInput("v3", div(h5(tags$span(style="color:white", "Smoking (factor)"))),  width = '90%' ,value= "0.4"),
                                                                 
                                                               ) 
                                                      ) ,
                                                      
                                                  
                                                      splitLayout(
                                                        
                                                        tags$div(
                                                          textInput("v4", div(h5(tags$span(style="color:white", "BMI (factor)"))),  width = '90%' ,value= "0"),
                                                      
                                                      ) ,
                                                      

                                                        tags$div(
                                                          textInput("v5", div(h5(tags$span(style="color:white", "covar3 (biomarker)"))),  width = '90%' ,value= "1/3"),
                                                        ),
                                                        
                                                        tags$div(
                                                          textInput("v6", div(h5(tags$span(style="color:white", "covar1 (Blood)"))),  width = '90%' ,value= "-.5/10")
                                                          
                                                        ) 
                                                        
                                                      ) ,
                                                      
                                                      splitLayout(
                                                        
                                                        tags$div(
                                                          textInput("v7", div(h5(tags$span(style="color:white", "Vas (cont.)"))),  width = '90%' ,value= "0.25/30"),
                                                        ),
                                                        
                                                        tags$div(
                                                          textInput("v8", div(h5(tags$span(style="color:white", "Time (cont.)"))),  width = '90%' ,value= "-.1/10"),
                                                        ) ,
                                                        
                                                        
                                                        tags$div(
                                                          textInput("v9", div(h5(tags$span(style="color:white", "covar2 (Fitness)"))),  width = '90%' ,value= "-1/50"),
                                                        )
                                                        
                                                      ) ,
                                                    
                                                      
                                                      splitLayout(
                                                        
                                                      tags$div(
                                                          textInput("v10", div(h5(tags$span(style="color:white", "fact1 (History)"))),  width = '90%' ,value= "log(2)"),
                                                        ) ,

                                                        tags$div(
                                                          textInput("v11", div(h5(tags$span(style="color:white", "binary2 (Employ)"))),  width = '90%' ,value= "-log(1)"),
                                                        ),
                                                        
                                                        tags$div(
                                                          textInput("v12", div(h5(tags$span(style="color:white", "Sex (binary)"))),  width = '90%' ,value= "log(0.5)"),
                                                        ) 
                                                        
                                                      ) 
                                                               
                                                       ),
                                                      
                                                      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                                      menuItem("Analyses",  startExpanded = FALSE,  icon = icon("bar-chart-o"),
                                                               #~~~~~~~~~~~~~~~~~~~~~~~~
                                                               menuSubItem("main(landing page)",    tabName = "OVERVIEW",  icon = icon("bar-chart-o"), selected = TRUE),
                                                               menuSubItem("trt x smoking",         tabName = "RESULTS2",  icon = icon("bar-chart-o")),
                                                               menuSubItem("trt x all",         tabName = "RESULTS3",  icon = icon("bar-chart-o")),
                                                               menuSubItem("xxxxxxxxxxxx",         tabName = "HELP",      icon = icon("bar-chart-o"), selected = FALSE),
                                                               menuSubItem("xxxxxxxxxxxx",         tabName = "RESULTS4",  icon = icon("bar-chart-o")),
                                                               menuSubItem("xxxxxxxxxxxx",         tabName = "RESULTS1",  icon = icon("table")),
                                                               #~~~~~~~~~~~~~~~~~~~~~~~~
                                                               menuSubItem("Only ranks of event times needed!", tabName = "OVERVIEW2",  icon = icon("bar-chart-o"), selected = FALSE),
                                                               #~~~~~~~~~~~~~~~~~~~~~~~~
                                                               menuSubItem("Model assumptions", tabName = "OVERVIEW3",  icon = icon("bar-chart-o"), selected = FALSE),
                                                               #~~~~~~~~~~~~~~~~~~~~~~~~
                                                               menuSubItem("KM lifetable",                   tabName = "KMTABLE",  icon = icon("list-alt")),
                                                               #~~~~~~~~~~~~~~~~~~~~~~~~
                                                               menuItem("Partial likelihood exercise",  startExpanded = FALSE,    icon = icon("table"),
                                                                        
                                                                        tags$div(
                                                                            textInput(inputId="guess", label='xxxxxxxxxxxxx', width = '90%' , value="1"),
                                                                        ),
                                                                        
                                                                        menuSubItem("Hit to reveal Partial log likelihood",  tabName = "partial")
                                                               )
                                                               #~~~~~~~~~~~~~~~~~~~~~~~~
                                                               
                                                      ),
                                                      #
                                                      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                                      menuItem("Change in hazard",  startExpanded = FALSE,    icon = icon("table")  ,
                                                               
                                                               
                                                               
                                                               # splitLayout(
                                                               #   textInput("age.range", div(h5(tags$span(style="color:yellow", "Age (continuous)"))), value= "30, 54"),
                                                               #   textInput("biomarker.range", div(h5(tags$span(style="color:yellow", "covar3 (biomarker)"))), value= "0.7675, 2.2300"),  #18
                                                               #   textInput("blood.range", div(h5(tags$span(style="color:yellow", "covar1 (Blood score)"))), value= "2.5700, 7.7525")
                                                               #   
                                                               # 
                                                               # ),
                                                               # splitLayout(
                                                               #    
                                                               #   textInput("vas.range", div(h5(tags$span(style="color:yellow", "Vas (continuous)"))), value= "18, 23"),  #1
                                                               #   textInput("time.range", div(h5(tags$span(style="color:yellow", "Time (continuous)"))), value= "2.355, 7.420"),
                                                               #   textInput("fitness.range", div(h5(tags$span(style="color:yellow", "covar2 (Fitness score)"))), value= "13, 38")
                                                               #   
                                                               # ),
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               tags$div(
                                                                   textInput(inputId="base", label='Enter xxxxxxxxxxxxx', width = '90%' , value="0.03"),
                                                               ),
                                                               tags$div(
                                                                   textInput(inputId="cens", label='Enter xxxxxxxxxxxxx', width = '90%' , value="0.02"),
                                                               ),
                                                               tags$div(
                                                                   textInput(inputId="hr2", label='Enter xxxxxxxxxxxxx', width = '90%' , value="1.2"),
                                                               ),
                                                               tags$div(
                                                                   textInput(inputId="per", label='Enter xxxxxxxxxxxxx',        width = '90%' , value="0.70"),
                                                                   textInput(inputId="per2", label='Enter xxxxxxxxxxxxx', width = '90%' , value="0.50"),
                                                               ),
                                                               
                                                               menuSubItem("Hit to reveal xxxxxxxxxxxxx",  tabName = "Change"),
                                                               menuSubItem("Hit to reveal xxxxxxxxxxxxx",  tabName = "Changeh")    ,
                                                               menuSubItem("Hit to reveal xxxxxxxxxxxxx",  tabName = "Changeh2") 
                                                      ),
                                                      
                                                      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                                      menuItem("Power",  startExpanded = FALSE,    icon = icon("table")  ,
                                                               
                                                               splitLayout(
                                                                   tags$div(
                                                                       textInput(inputId="ss", label='Enter xxxxxxxxxxxxx',   value="0.7, 0.5"),
                                                                   ),
                                                                   tags$div(
                                                                       textInput(inputId="ss2", label='Enter xxxxxxxxxxxxx',  value="11.9, 23.1"),
                                                                   )
                                                                   
                                                               ),
                                                               
                                                               #~~~~~~~~~~~~~~~~~~~~~~~~~
                                                               splitLayout(
                                                                   tags$div(
                                                                       textInput(inputId="tt", label='Enter xxxxxxxxxxxxx',        value="500,500"),
                                                                   ),
                                                                   tags$div(
                                                                       textInput(inputId="hrx", label='Enter xxxxxxxxxxxxx',       value="1.2"),
                                                                   )
                                                                   
                                                               ),
                                                               #~~~~~~~~~~~~~~~~~~~~~~~~~
                                                               splitLayout(
                                                                   tags$div(
                                                                       textInput(inputId="af", label='Enter xxxxxxxxxxxxx',   value="3"),
                                                                   ),
                                                                   tags$div(
                                                                       textInput(inputId="af2", label='Enter xxxxxxxxxxxxx',    value="160"),
                                                                   )
                                                                   
                                                               ),
                                                               #~~~~~~~~~~~~~~~~~~~~~~~~~
                                                               # Here, let us accrue patients over x years, and follow them for an additional x years
                                                               
                                                               splitLayout(
                                                                   tags$div(
                                                                       textInput(inputId="sim", label='Number of xxxxxxxxxxxxx',  width = '60%' ,  value="500"),
                                                                   ),
                                                                   tags$div(
                                                                       
                                                                       textInput(inputId="t2", label='xxxxxxxxxxxxx', width = '55%' , value="0.1"),
                                                                   )
                                                                   
                                                               ),
                                                               
                                                               #~~~~~~~~~~~~~~~~~~~~~~~~~
                                                               
                                                               menuSubItem("Hit to xxxxxxxxxxxxx",  tabName = "power"),
                                                               menuSubItem("Hit to xxxxxxxxxxxxx",  tabName = "weibull")
                                                               
                                                      ),
                                                      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                                      
                                                      menuItem("The Weibull Distributions",  startExpanded = FALSE,    icon = icon("table"),
                                                               
                                                               tags$div(
                                                                   textInput(inputId="shape", label='Weibull shape', width = '90%' , value="1"),
                                                               ),
                                                               
                                                               tags$div(
                                                                   textInput(inputId="scale", label='Weibull scale', width = '90%' , value="0.03"),
                                                               ),
                                                               
                                                               menuSubItem("Hit to reveal Survival hazard relationship",  tabName = "survhaz")
                                                      ),
                                                      
                                                      # menuItem("Explanation",                    tabName = "HELP",icon = icon("bar-chart-o"), selected = FALSE),
                                                      # menuItem("Wiki", tabName = "Wiki",                          icon = icon("bar-chart-o"), selected = FALSE),
                                                      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                                      menuItem("Code", icon = icon("bar-chart-o"),
                                                               menuSubItem("Shiny",  
                                                                           icon = icon("send",lib='glyphicon'), 
                                                                           href = "https://raw.githubusercontent.com/eamonn2014/PARTIAL-LIKELIHOOD-METHOD/master/app.R"),
                                                               
                                                               menuSubItem("R",  
                                                                           icon = icon("send",lib='glyphicon'), 
                                                                           href = "https://raw.githubusercontent.com/eamonn2014/PARTIAL-LIKELIHOOD-METHOD/master/R%20code%20for%20app.R") 
                                                      ),
                                                      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                                      menuItem("References", icon = icon("bar-chart-o"),
                                                               
                                                               menuSubItem(h5(HTML( "[1] Andrew Gelman")),  
                                                                           icon = icon("send",lib='glyphicon'), 
                                                                           href = "https://statmodeling.stat.columbia.edu/2018/03/15/need-16-times-sample-size-estimate-interaction-estimate-main-effect/"),
                                                               
                                                               menuSubItem(h5(HTML( "[2] Frank Harrell...much more here")),
                                                                           icon = icon("send",lib='glyphicon'), 
                                                                           href = "https://www.fharrell.com/post/varyor/") ,
                                                               #dashboardHeader(title = h4(HTML("This title<br/>is just way too long")))
                                                               
                                                               menuSubItem( h5(HTML("[3] Responder non responder fallacy")),  
                                                                            icon = icon("send",lib='glyphicon'), 
                                                                            href = "https://eamonn.shinyapps.io/responder-non-responder-fallacy-in-RCTs/"),
                                                               
                                                               menuSubItem( h5(HTML("[4] Andrew Gelman")),  
                                                                            icon = icon("send",lib='glyphicon'), 
                                                                            href = "https://projecteuclid.org/download/pdfview_1/euclid.aoas/1231424214") 
                                                               
                                                               
                                                                
                                                               
                                                               
                                                               
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
                              
                              tabItem("Wiki", 
                                      fluidRow(
                                          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                          box(  width=6,
                                                title='Wiki'
                                                ,status = "primary"
                                                ,solidHeader = TRUE 
                                                ,collapsible = TRUE 
                                                
                                               , h4(paste("An explanation of the inputs and tabs")), 
                                                h4(paste("The first input left is the sample size for patients randomly 
                                              assigned to treatment arms in a 1:1:1 fashion.
                                                       The second selection allows the choice of 3 designs i) a main effects model, that is a  
                                                       no-interaction logit-additive model that assumes constancy of treatment ORs ii) 
                                                       a model with a treatment X smoking interaction and iii) a model in which all baseline covariates interact 
                                                       with treatment. ")),
                                                #br(),
                                                h4(paste("The next selection is a choice of analysis performed and presented in Table 1. 
                                                       There are three choices once again i) a main effects model, that is a 
                                                       no-interaction logit-additive model that assumes constancy of treatment ORs ii) 
                                                       a model with a treatment X smoking interaction and iii) a model in which all baseline covariates interact 
                                                       with treatment. This only impacts what is presented in Table 1 and tab 10/11.")), 
                                                #br(),
                                                h4(paste("Twelve input boxes follow and allow the user to specify the coefficients for treatment and 11
                                              baseline covariates on the log odds scale.
                                                       Note a typical change in an input variable would be unlikely to correspond to a change as large 
                                                       as 5 on the logistic scale (which would move the probability from 0.01 to 0.50 or from 0.50 to 0.99) [4].
                                                       Age in years is uniformly distributed between 18 and 65. covar3 is uniformly distributed
                                                       between 0 to 3, covar1 uniformly distributed between 0 to 10, vas between 1 to 30 and time in years uniformly
                                                       distributed between 0 to 10. Smoking and BMI are 3 level factors and fact1, binary2 and sex are binary factors.
                                                       For the factors the coefficient entered describes the true relationship between all adjacent levels. We also 
                                                       add labels to the variables and they appear on some of the outputs."  )), 
                                                h4(paste("Click the simulate button to generate another data set from the same population.")),
                                                h4(paste("Tab 1 presents the regression table of the 3 models, the particular model can be selected. ")),
                                                h4(paste("Tab 2 presents the regression table of the 
                                                        no-interaction logit-additive model that assumes constancy of treatment ORs and provides an explanation.
                                                        ")),
                                                h4(paste("Tab 3 presents the a forest plot of the no-interaction logit-additive model that assumes constancy of treatment ORs plus 
                                                       a table of the ORs and log odds ratios.")),
                                                h4(paste("Tab 4 presents the likelihood ratio test assessing each model with each other.")),
                                                
                                                h4(paste("Tab 5 presents the forest plots by treatment for the treatment interacting with all baseline covariates model plus 
                                                       tables of the ORs and log odds ratios and the regression tables. We allow the user to adjust reference levels 
                                                       and the range for which continuous predictor effects are estimated and presented. 
                                                       For good measure we also describe a couple of the estimated regression coefficients.")),
                                                h4(paste("Tab 6 presents the forest plots by treatment for the treatment x smoking interaction model and 
                                                       tables of the ORs and log odds ratios.")),
                                                h4(paste("Tab 7 presents relative measures of explained variation and the AIC of each model is reported.")),
                                                h4(paste("Tab 8 tab presents anova table and dot plot.")),
                                                h4(paste("Tab 9 tab presents double differences on the log odds scale (ie. the interactions) .")),
                                                
                                                h4(paste("Tab 10 No new information is presented on this tab, but only the 3 model outputs presented together.")),
                                                
                                                h4(paste("The 11th  tab presents another way to estimate treatment effects with interactions using contrast statements.")),
                                                
                                                h4(paste("The final tab presents a listing of the simulated data and diagnostics."))
                                                
                                                
                                                
                                          )
                                          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                          ,box(
                                              title='Wiki continued'
                                              ,status = "primary"
                                              ,solidHeader = TRUE 
                                              ,collapsible = TRUE 
                                              
                                              ,p("xxxxxxxxxxxxx")
                                              
                                          ),  # box end
                                      )
                              ),
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                              tabItem("OVERVIEW",
                                      fluidRow(
                                        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                          box(
                                              title = "No-interaction logit-additive model that assumes constancy of treatment ORs"
                                              ,status = "primary"
                                              ,solidHeader = TRUE 
                                              ,collapsible = TRUE ,
                                               collapsed=TRUE,
                                             # background = "white",
                                              width = 6,
                                              
                                             div( verbatimTextOutput("user") ),
                                              tags$a(href = "https://www.youtube.com/watch?v=EoIB_Obddrk&t=327s&ab_channel=RMSRegression",
                                                     tags$span(style="color:blue", "xxxxxxxxxxxxx"),),
                                              div(p(" "))

                                          )
                                          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                           , box(  width=6, 
                                                title='Wiki'
                                                ,status = "primary"
                                                ,solidHeader = TRUE 
                                                ,collapsible = TRUE , 
                                                 collapsed=TRUE,
                                                
                                                h4(paste("An explanation of the inputs and tabs")), 
                                                h4(paste("The first input left is the sample size for patients randomly 
                                              assigned to treatment arms in a 1:1:1 fashion.
                                                       The second selection allows the choice of 3 designs i) a main effects model, that is a  
                                                       no-interaction logit-additive model that assumes constancy of treatment ORs ii) 
                                                       a model with a treatment X smoking interaction and iii) a model in which all baseline covariates interact 
                                                       with treatment. ")),
                                                #br(),
                                                h4(paste("The next selection is a choice of analysis performed and presented in Table 1. 
                                                       There are three choices once again i) a main effects model, that is a 
                                                       no-interaction logit-additive model that assumes constancy of treatment ORs ii) 
                                                       a model with a treatment X smoking interaction and iii) a model in which all baselin
                                                       e covariates interact 
                                                       with treatment. This only impacts what is presented in Table 1 and tab 10/11.")), 
                                             
                                                
                                                h4(paste("xxxxxxxxxxxxxxxxxxxx")), 
                                                
                                                h4(paste("
                                                         
                                                         
                                                         
                                                         
                                                         
                                                         
                                                         
                                                         
                                                         
                                                         
                                                         
                                                         ")), 
                                                
                                                
                                          )
                                          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                          ,box(
                                            title = "Forest plot, No-interaction logit-additive model"
                                            ,status = "primary"
                                            ,solidHeader = TRUE 
                                            ,collapsible = TRUE ,
                                          
                                            div(plotOutput("f.plot3", width=fig.height1, height=615)),   
                                            
                                            div(p(" "))
                                            
                                          )
                                          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                                          
                                          ,box(
                                            title = "No-interaction logit-additive model summary"
                                            ,status = "primary"
                                            ,solidHeader = TRUE 
                                            ,collapsible = TRUE ,
                                           
                                            div( verbatimTextOutput("int.trt1C" ) ),
                                            
                                            div(p(" "))
                                            
                                          )
                                           
                                           #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                       #    ,box(
                                       #        title='xxxxxxxxxxxxx'
                                       #        ,status = "primary"
                                       #        ,solidHeader = TRUE
                                       #        ,collapsible = TRUE ,
                                       #        #,plotOutput("plot2", height = "720px")
                                       # 
                                       # 
                                       #        splitLayout(
                                       #          textInput("age.range", div(h5(tags$span(style="color:black", "Age (continuous)"))), value= "30, 54"),
                                       #          textInput("biomarker.range", div(h5(tags$span(style="color:black", "covar3 (biomarker)"))), value= "0.7675, 2.2300"),  #18
                                       #          textInput("blood.range", div(h5(tags$span(style="color:black", "covar1 (Blood score)"))), value= "2.5700, 7.7525")
                                       # 
                                       # 
                                       #        ),
                                       #        splitLayout(
                                       # 
                                       #          textInput("vas.range", div(h5(tags$span(style="color:black", "Vas (continuous)"))), value= "18, 23"),  #1
                                       #          textInput("time.range", div(h5(tags$span(style="color:black", "Time (continuous)"))), value= "2.355, 7.420"),
                                       #          textInput("fitness.range", div(h5(tags$span(style="color:black", "covar2 (Fitness score)"))), value= "13, 38")
                                       # 
                                       #        ),
                                       # 
                                       #        h4(paste("The boxes below can be used to adjust the factor reference levels (affecting forest plot and presentation of treatment effects at very bottom). The continuous variables are held at sensible values (we did not center the continuous variables in the regression).
                                       # Set the continuous to zero and observe the treatment comparison confidence intervals. Only the treatment bars will change as treatment interacts with all variables. ")),
                                       # 
                                       #        splitLayout(
                                       #          textInput("adj.smoking", div(h5(tags$span(style="color:blue", "Smoking ref (factor)"))), value= "1"),
                                       #          textInput("adj.age", div(h5(tags$span(style="color:blue", "Age (continuous)"))), value= "40"),  #18
                                       #          textInput("adj.biomarker", div(h5(tags$span(style="color:blue", "covar3 (biomarker)"))), value=  "1.3"),
                                       #          textInput("adj.blood", div(h5(tags$span(style="color:blue", "covar1 (Blood score)"))), value= "5"),
                                       #          textInput("adj.vas", div(h5(tags$span(style="color:blue", "Vas (continuous)"))), value= "17"),  #1
                                       #          textInput("adj.time", div(h5(tags$span(style="color:blue", "Time (continuous)"))), value= "4")
                                       # 
                                       #        ),
                                       # 
                                       #        splitLayout(
                                       # 
                                       # 
                                       #          textInput("adj.fitness", div(h5(tags$span(style="color:blue", "covar2 (Fitness score)"))), value= "20"),  #1
                                       #          textInput("adj.history", div(h5(tags$span(style="color:blue", "fact1 ref (History binary)"))), value= "0"),
                                       #          textInput("adj.employed", div(h5(tags$span(style="color:blue", "binary2 ref (Employed)"))), value= "0"),
                                       #          textInput("adj.sex", div(h5(tags$span(style="color:blue", "Sex red (binary)"))), value= "0"),
                                       #          textInput("adj.BMI", div(h5(tags$span(style="color:blue", "BMI ref (factor)"))), value= "1")
                                       # 
                                       # 
                                       #        ),
                                       #           )
                                          
                                          
                                          )),               
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                              tabItem("OVERVIEW2",
                                      fluidRow(
                                          box(
                                              title =   "xxxxxxxxxxxxxxxx"   # uiOutput('product'), 
                                              ,status = "primary"
                                              ,solidHeader = TRUE 
                                              ,collapsible = TRUE 
                                              #,plotlyOutput("plot5a", height = "720px")
                                              #,h5(textOutput("info2"))
                                          )
                                          
                                      
                                          
                                          
                                          )),   
                              
                              
                              tabItem("OVERVIEW3",
                                      fluidRow(
                                          box(
                                              title =   "xxxxxxxxxxxxx"     
                                              ,status = "primary"
                                              ,solidHeader = TRUE 
                                              ,collapsible = TRUE 
                                              #,plotOutput("FH", height = "720px")
                                              #,h5(textOutput("info2"))
                                          )
                                          
                                       # ,box(
                                       #      title='xxxxxxxxxxxxx'
                                       #      ,status = "primary"
                                       #      ,solidHeader = TRUE 
                                       #      ,collapsible = TRUE ,
                                       #      #,plotOutput("plot2", height = "720px")
                                       #      
                                       #      
                                       #      splitLayout(
                                       #        textInput("age.range", div(h5(tags$span(style="color:black", "Age (continuous)"))), value= "30, 54"),
                                       #        textInput("biomarker.range", div(h5(tags$span(style="color:black", "covar3 (biomarker)"))), value= "0.7675, 2.2300"),  #18
                                       #        textInput("blood.range", div(h5(tags$span(style="color:black", "covar1 (Blood score)"))), value= "2.5700, 7.7525")
                                       #        
                                       #        
                                       #      ),
                                       #      splitLayout(
                                       #        
                                       #        textInput("vas.range", div(h5(tags$span(style="color:black", "Vas (continuous)"))), value= "18, 23"),  #1
                                       #        textInput("time.range", div(h5(tags$span(style="color:black", "Time (continuous)"))), value= "2.355, 7.420"),
                                       #        textInput("fitness.range", div(h5(tags$span(style="color:black", "covar2 (Fitness score)"))), value= "13, 38")
                                       #        
                                       #      ),
                                       #      
                                       #      h4(paste("The boxes below can be used to adjust the factor reference levels (affecting forest plot and presentation of treatment effects at very bottom). The continuous variables are held at sensible values (we did not center the continuous variables in the regression). 
                                       # Set the continuous to zero and observe the treatment comparison confidence intervals. Only the treatment bars will change as treatment interacts with all variables. ")),
                                       #      
                                       #      splitLayout(
                                       #        textInput("adj.smoking", div(h5(tags$span(style="color:blue", "Smoking ref (factor)"))), value= "1"),
                                       #        textInput("adj.age", div(h5(tags$span(style="color:blue", "Age (continuous)"))), value= "40"),  #18
                                       #        textInput("adj.biomarker", div(h5(tags$span(style="color:blue", "covar3 (biomarker)"))), value=  "1.3"),
                                       #        textInput("adj.blood", div(h5(tags$span(style="color:blue", "covar1 (Blood score)"))), value= "5"),
                                       #        textInput("adj.vas", div(h5(tags$span(style="color:blue", "Vas (continuous)"))), value= "17"),  #1
                                       #        textInput("adj.time", div(h5(tags$span(style="color:blue", "Time (continuous)"))), value= "4")
                                       #        
                                       #      ),
                                       #      
                                       #      splitLayout(
                                       #        
                                       #        
                                       #        textInput("adj.fitness", div(h5(tags$span(style="color:blue", "covar2 (Fitness score)"))), value= "20"),  #1
                                       #        textInput("adj.history", div(h5(tags$span(style="color:blue", "fact1 ref (History binary)"))), value= "0"),
                                       #        textInput("adj.employed", div(h5(tags$span(style="color:blue", "binary2 ref (Employed)"))), value= "0"),
                                       #        textInput("adj.sex", div(h5(tags$span(style="color:blue", "Sex red (binary)"))), value= "0"),
                                       #        textInput("adj.BMI", div(h5(tags$span(style="color:blue", "BMI ref (factor)"))), value= "1")
                                       #        
                                       #        
                                       #      ),
                                       #      
                                       #      
                                       #      
                                       #      
                                       #      
                                       #      
                                       #      
                                       #      
                                       #      
                                       #      
                                       #      
                                       #      
                                       #      
                                       #      
                                       #      
                                       #      
                                       #      
                                       #    )
                                          
                                          )),   
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                              tabItem("RESULTS1",
                                      fluidRow(        
                                          
                                          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                          box(width=8,
                                              title = "xxxxxxxxxxxxx"
                                              ,status = "primary"
                                              ,solidHeader = TRUE 
                                              ,collapsible = TRUE 
                                              # , DT::dataTableOutput("mytable2")
                                          ))),
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                              # new tab
                              tabItem("partial",
                                      fluidRow(
                                          box(
                                              width=7,
                                              # background = "green",
                                              title = "xxxxxxxxxxxxx"
                                              ,status = "primary"
                                              ,solidHeader = TRUE 
                                              ,collapsible = TRUE 
                                              #, DT::dataTableOutput("exercise")
                                          )
                                          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                          ,box(
                                              width=5,
                                              title='xxxxxxxxxxxxx'
                                              ,status = "primary"
                                              ,solidHeader = TRUE 
                                              ,collapsible = TRUE 
                                              #, DT::dataTableOutput("exercise2")
                                              ,p("")
                                              ,p("xxxxxxxxxxxxx")
                                          ))),        
                              
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                              tabItem("HELP", 
                                      fluidRow(
                                          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                          box(  
                                              title='xxxxxxxxxxxxx xxxxxxxxxxxxx'
                                              ,status = "primary"
                                              ,solidHeader = TRUE 
                                              ,collapsible = TRUE ,
                                              textOutput("help"),
                                              withMathJax(),  # need this to be stated
                                              
                                              p("xxxxxxxxxxxxx"),
                                              
                                          )
                                          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                          ,box(
                                              title='xxxxxxxxxxxxx xxxxxxxxxxxxx'
                                              ,status = "primary"
                                              ,solidHeader = TRUE 
                                              ,collapsible = TRUE ,
                                              p("xxxxxxxxxxxxx"),
                                              
                                              p("xxxxxxxxxxxxx")
                                              
                                          ),  # box end
                                          
                                      )
                                      
                              ),
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                              tabItem("RESULTS3",
                                      fluidRow(
                                          
                                          box(width=6,
                                              title = "xxxxxxxxxxxxx"
                                              ,status = "primary"
                                              ,solidHeader = TRUE 
                                              ,collapsible = TRUE 
                                              # ,plotOutput("plot2x", height = "720px")
                                              ,h5(textOutput("Staff_name2"))
                                          ),
                                          
                                          box(width=6,
                                              title = "xxxxxxxxxxxxx"
                                              ,status = "primary"
                                              ,solidHeader = TRUE 
                                              ,collapsible = TRUE 
                                              #,plotOutput("plot3", height = "720px")
                                              ,p("xxxxxxxxxxxxx")
                                          )
                                          
                                      )),########
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
                              tabItem("RESULTS4",
                                      fluidRow(
                                          
                                          box(width=6,
                                              title='xxxxxxxxxxxxx'
                                              ,status = "primary"
                                              ,solidHeader = TRUE 
                                              ,collapsible = TRUE 
                                              #,plotOutput("plot4", height = "720px")
                                              ,p("xxxxxxxxxxxxx")
                                          )
                                          
                                          ,box(width=6,
                                               title='xxxxxxxxxxxxx'
                                               ,status = "primary"
                                               ,solidHeader = TRUE 
                                               ,collapsible = TRUE 
                                               #,plotOutput("plot2y", height = "720px")
                                          ))),
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                              tabItem("survhaz",
                                      fluidRow(
                                          
                                          box(width=6,
                                              title='xxxxxxxxxxxxx'
                                              ,status = "primary"
                                              ,solidHeader = TRUE 
                                              ,collapsible = TRUE 
                                              #,plotOutput("survhaz", height = "720px")
                                              ,p("xxxxxxxxxxxxx")
                                          )
                                          
                                          ,box(width=6,
                                               title='xxxxxxxxxxxxx'
                                               ,status = "primary"
                                               ,solidHeader = TRUE 
                                               ,collapsible = TRUE 
                                               #,plotOutput("survhaz2", height = "720px")
                                               ,p("xxxxxxxxxxxxx")
                                          ))),
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
                              tabItem("Change",
                                      fluidRow(
                                          
                                          box(width=6,
                                              title='xxxxxxxxxxxxx'
                                              ,status = "primary"
                                              ,solidHeader = TRUE 
                                              ,collapsible = TRUE 
                                              #,plotOutput("plot1c", height = "720px")
                                              ,p("xxxxxxxxxxxxx")
                                          )
                                          
                                          ,box(width=6,
                                               title='xxxxxxxxxxxxx'
                                               ,status = "primary"
                                               ,solidHeader = TRUE 
                                               ,collapsible = TRUE 
                                               #,plotOutput("plot1d", height = "720px")
                                               #,h5(textOutput("info4"))
                                               #,h5(textOutput("info5"))
                                               
                                          ))),
                              
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
                              
                              tabItem("Changeh",
                                      fluidRow(
                                          
                                          box(width=6,
                                              title='xxxxxxxxxxxxx'
                                              ,status = "primary"
                                              ,solidHeader = TRUE 
                                              ,collapsible = TRUE 
                                              #  ,plotOutput("ploth", height = "720px")
                                              ,p("xxxxxxxxxxxxx")
                                          )
                                          
                                          ,box(width=6,
                                               title='xxxxxxxxxxxxx'
                                               ,status = "primary"
                                               ,solidHeader = TRUE 
                                               ,collapsible = TRUE 
                                               #  ,plotOutput("ploth1", height = "720px")
                                               ,p("xxxxxxxxxxxxx")
                                               
                                               
                                          ))),
                              
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                              
                              tabItem("Changeh2",
                                      fluidRow(
                                          
                                          box(width=12,
                                              title='xxxxxxxxxxxxx xxxxxxxxxxxxx'
                                              ,status = "primary"
                                              ,solidHeader = TRUE 
                                              ,collapsible = TRUE 
                                              #,plotOutput("ploth2", height = "720px")
                                              , p("xxxxxxxxxxxxx")
                                              
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
                                              title='xxxxxxxxxxxxx'
                                              ,status = "primary"
                                              ,solidHeader = TRUE 
                                              ,collapsible = TRUE 
                                              # ,plotOutput("powerp1", height = "720px")
                                              ,p("xxxxxxxxxxxxx")
                                              #  ,h5(verbatimTextOutput("powerp2"))
                                          )
                                          
                                          ,box(width=6,
                                               title='xxxxxxxxxxxxx'
                                               ,status = "primary"
                                               ,solidHeader = TRUE 
                                               ,collapsible = TRUE 
                                               
                                               #  ,plotlyOutput("powerp3", height = "720px")
                                               ,p("xxxxxxxxxxxxx")
                                               # ,h5(verbatimTextOutput("powerp5"))
                                               #,h5(verbatimTextOutput("powerp4"))
                                          ))),
                              
                              
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                              
                              tabItem("weibull",
                                      fluidRow(
                                          
                                          box(width=6,
                                              title='xxxxxxxxxxxxx'
                                              ,status = "primary"
                                              ,solidHeader = TRUE 
                                              ,collapsible = TRUE 
                                              #,plotOutput("powerw", height = "720px")
                                              
                                          )
                                          
                                          ,box(width=6,
                                               title='xxxxxxxxxxxxx'
                                               ,status = "primary"
                                               ,solidHeader = TRUE 
                                               ,collapsible = TRUE 
                                               #,plotlyOutput("powerp3w", height = "720px")
                                               
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
                                              #  , DT::dataTableOutput("KM")
                                          )
                                          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                          ,box(
                                              width=6,
                                              title='xxxxxxxxxxxxx xxxxxxxxxxxxx'
                                              ,status = "primary"
                                              ,solidHeader = TRUE 
                                              ,collapsible = TRUE 
                                              # , DT::dataTableOutput("CHAZ")
                                              ,p("")
                                              ,p("xxxxxxxxxxxxx")
                                          ))),        
                              
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                              tabItem("RESULTS2",   # trt x all 
                                      
                                      fluidRow(
                                        box(
                                          width=12,
                                          title='xxxxxxxxxxxxx'
                                          ,status = "primary"
                                          ,solidHeader = TRUE 
                                          ,collapsible = TRUE ,
                                          #,plotOutput("plot2", height = "720px")
                                          
                                          
                                          splitLayout(
                                            textInput("age.range", div(h5(tags$span(style="color:black", "Age (continuous)"))), value= "30, 54", width='85%'),
                                            textInput("biomarker.range", div(h5(tags$span(style="color:black", "covar3 (biomarker)"))), value= "0.7675, 2.2300", width='85%'),
                                            textInput("blood.range", div(h5(tags$span(style="color:black", "covar1 (Blood score)"))), value= "2.5700, 7.7525", width='85%'),
                                            
                                            
                                         # ),
                                        #  splitLayout(
                                            
                                            textInput("vas.range", div(h5(tags$span(style="color:black", "Vas (continuous)"))), value= "18, 23", width='85%'),
                                            textInput("time.range", div(h5(tags$span(style="color:black", "Time (continuous)"))), value= "2.355, 7.420", width='85%'),
                                            textInput("fitness.range", div(h5(tags$span(style="color:black", "covar2 (Fitness score)"))), value= "13, 38", width='85%')
                                            
                                          ),
                                          
                                          h4(paste("The boxes below can be used to adjust the factor reference levels. The continuous variables are held at sensible values (we did not center the continuous variables in the regression). 
                                       Set the continuous to zero and observe the treatment comparison confidence intervals. Only the treatment bars will change as treatment interacts with all variables. ")),
                                          
                                          splitLayout(
                                            textInput("adj.smoking", div(h5(tags$span(style="color:blue", "Smoking ref (factor)"))), value= "1", width='80%'),
                                            textInput("adj.age", div(h5(tags$span(style="color:blue", "Age (continuous)"))), value= "40", width='80%'),
                                            textInput("adj.biomarker", div(h5(tags$span(style="color:blue", "covar3 (biomarker)"))), value=  "1.3", width='80%'),
                                            textInput("adj.blood", div(h5(tags$span(style="color:blue", "covar1 (Blood score)"))), value= "5", width='80%'),
                                            textInput("adj.vas", div(h5(tags$span(style="color:blue", "Vas (continuous)"))), value= "17", width='80%'),
                                            textInput("adj.time", div(h5(tags$span(style="color:blue", "Time (continuous)"))), value= "4", width='80%'),
                                            
                                         # ),
                                          
                                        #  splitLayout(
                                            
                                            #textInput(inputId="hr2", label='Enter xxxxxxxxxxxxx', width = '90%' , value="1.2"),
                                            textInput("adj.fitness", div(h5(tags$span(style="color:blue", "covar2 (Fitness score)"))), value= "20", width='80%'),
                                            textInput("adj.history", div(h5(tags$span(style="color:blue", "fact1 ref (History bin.)"))), value= "0", width='80%'),
                                            textInput("adj.employed", div(h5(tags$span(style="color:blue", "binary2 ref (Empl.)"))), value= "0", width='80%'),
                                            textInput("adj.sex", div(h5(tags$span(style="color:blue", "Sex red (binary)"))), value= "0", width='80%'),
                                            textInput("adj.BMI", div(h5(tags$span(style="color:blue", "BMI ref (factor)"))), value= "1", width='80%')
                                            
                                            
                                          ),   
                                        )
                                        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~put forest plots here
                                        # ,box(
                                        #   width=6,
                                        #   title='xxxxxxxxxxxxx xxxxxxxxxxxxx'
                                        #   ,status = "primary"
                                        #   ,solidHeader = TRUE 
                                        #   ,collapsible = TRUE 
                                        #   # , DT::dataTableOutput("CHAZ")
                                        #   ,p("")
                                        #   #div(plotOutput("f.plot1", width=fig.width4, height=fig.height7))
                                        #   ,p("xxxxxxxxxxxxx")
                                        # )
                                        
                                        
                                        ),   
                                      
                                      
                                      
                                      # fluidRow(
                                      #   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                      #   
                                      #   # 3 boxes code out
                                      #     box(width=4,
                                      #         title = "xxxxxxxxxxxxx xxxxxxxxxxxxx" 
                                      #         ,status = "primary"
                                      #         ,solidHeader = TRUE 
                                      #         ,collapsible = TRUE 
                                      #        , div(plotOutput("f.plot1", width=fig.width, height=fig.height7))
                                      #         ,p("xxxxxxxxxxxxx")
                                      #     )
                                      #     
                                      #     #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                      #     , box(width=4,
                                      #       title='xxxxxxxxxxxxx'
                                      #       ,status = "primary"
                                      #       ,solidHeader = TRUE 
                                      #       ,collapsible = TRUE 
                                      #      , div(plotOutput("f.plot2", width=fig.width, height=fig.height7))
                                      #       ,p("xxxxxxxxxxxxx")
                                      #     )
                                      #     
                                      #     #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                      #     ,box(width=4,
                                      #          title="xxxxxxxxxxxxx xxxxxxxxxxxxx" 
                                      #          ,status = "primary"
                                      #          ,solidHeader = TRUE
                                      #          ,collapsible = TRUE
                                      #          , div(plotOutput("f.plot3x", width=fig.width, height=fig.height7)) 
                                      #          ,p("xxxxxxxxxxxxx")
                                      #     ) 
                                      #     
                                      #     
                                      # ),  #fluidrow
                                      
                                      
                                      
                                      #~~~~~~~~~~~~~~~~~~~~~~~~~~
                                      
                                      fluidRow(
                                        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                        box(width=12,
                                            title = "xxxxxxxxxxxxx xxxxxxxxxxxxx" 
                                            ,status = "primary"
                                            ,solidHeader = TRUE 
                                            ,collapsible = TRUE 
                                            , div(plotOutput("f.plot99", width=1500, height=fig.height7))
                                            ,p("xxxxxxxxxxxxx")
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
            ,subtitle = tags$p('xxxxxxxxxxxxx xxxxxxxxxxxxx', style = "font-size: 150%;")
            ,icon = icon("stats",lib='glyphicon')
            ,color = "red" )
        
    })
    
    output$value2 <- renderValueBox({
        
        valueBox(
            value =  tags$p(paste0(formatz0(setUpByName2())," / ",formatz0(setUpByName2a()) ," / ",formatz00(setUpByName2b()) ," / ",formatz1(setUpByName2c()) ," / ",formatz2(setUpByName2a()/setUpByName2b()  )    )
                            ,style = "font-size: 100%;")
            ,subtitle = tags$p('xxxxxxxxxxxxx xxxxxxxxxxxxx', style = "font-size: 150%;")
            ,icon = icon("stats",lib='glyphicon')
            ,color = "teal")
        
    })
    
    output$value3 <- renderValueBox({
        
        valueBox(
            value =  tags$p(paste0(formatz2(setUpByName4())," ( ",formatz2(setUpByName5()),", ",formatz2(setUpByName6())," ) " ," ; ",formatz1(setUpByNameLL()))
                            ,style = "font-size: 100%;")
            ,subtitle = tags$p(paste0("xxxxxxxxxxxxx xxxxxxxxxxxxx"), style = "font-size: 150%;")
            ,icon = icon("education",lib='glyphicon')
            ,color = "green")
        
    }) 
    
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # This is where a new sample is instigated and inputs converted to numeric
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # This is where a new sample is instigated 
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    random.sample <- reactive({
      
      foo <- input$resample
      
      n <- as.numeric(input$n )
      
      # writing like this I can write log and fraction into input boxes!
      v1 <- as.numeric(    eval(parse(text= (input$v1)) ) )
      v2 <- as.numeric(    eval(parse(text= (input$v2)) ) )
      v3 <- as.numeric(    eval(parse(text= (input$v3)) ) )    
      v4 <- as.numeric(    eval(parse(text= (input$v4)) ) )   
      v5 <- as.numeric(    eval(parse(text= (input$v5)) ) )  
      v6 <- as.numeric(    eval(parse(text= (input$v6)) ) ) 
      v7 <- as.numeric(    eval(parse(text= (input$v7)) ) )
      v8 <- as.numeric(    eval(parse(text= (input$v8)) ) )
      v9 <- as.numeric(    eval(parse(text= (input$v9)) ) )
      v10 <- as.numeric(    eval(parse(text= (input$v10)) ) )
      v11 <- as.numeric(    eval(parse(text= (input$v11)) ) )
      v12 <- as.numeric(    eval(parse(text= (input$v12)) ) )
      
      check =c(v1 , v2 , v3 , v4 , v5,  v6, v7,  v8 , v9 , v10 , v11 , v12  )
      
      return(list(
        v1=v1, v2=v2, v3=v3, v4=v4, v5=v5, v6=v6, v7=v7, v8=v8, v9=v9, v10=v10, v11=v11, v12=v12 ,
        check=check, n=n
        
      ))
    })
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    randomness <- reactive({
      
      n <- as.numeric(input$n )
      randomi <- runif(n)
      
      return(list(
        randomi=randomi
      ))
      
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    design <- reactive({
      
      randomi <- randomness()$randomi
      
      sample <- random.sample()
      
      n <- as.numeric(input$n )
      v1 <- as.numeric(    eval(parse(text= (input$v1)) ) )
      v2 <- as.numeric(    eval(parse(text= (input$v2)) ) )
      v3 <- as.numeric(    eval(parse(text= (input$v3)) ) )    
      v4 <- as.numeric(    eval(parse(text= (input$v4)) ) )   
      v5 <- as.numeric(    eval(parse(text= (input$v5)) ) )  
      v6 <- as.numeric(    eval(parse(text= (input$v6)) ) ) 
      v7 <- as.numeric(    eval(parse(text= (input$v7)) ) )
      v8 <- as.numeric(    eval(parse(text= (input$v8)) ) )
      v9 <- as.numeric(    eval(parse(text= (input$v9)) ) )
      v10 <- as.numeric(    eval(parse(text= (input$v10)) ) )
      v11 <- as.numeric(    eval(parse(text= (input$v11)) ) )
      v12 <- as.numeric(    eval(parse(text= (input$v12)) ) )
      
      trt.coef       <-  v1     # log odds ratio so 1 -> 2.718, so 1 is LARGE
      age.coef       <-  v2     # log odds of 1 over the age range
      smoke.coef     <-  v3     # this is odds of 1.5
      bmi.coef       <-  v4     # this is an odds of 1..50:50
      covar3.coef    <-  v5     # log odds 1 over range of 3
      covar1.coef    <-  v6     # log odds -.05 per unit change
      vas.coef       <-  v7     # log odds .008 per unit change. log odds .25 over 30 units odds 1.27
      time.coef      <-  v8     # log odds -.01 per year, log odds -.1 over 10 years or odds .90
      covar2.coef    <-  v9     # log odds 0.02 per joint, log odds 1 over 50 units or odds 2.7
      fact1.coef     <-  v10    # log odds 0.693 per change in binary, or odds of 2   
      binary2.coef   <-  v11    # log odds 0 per change in binary, or odds of 1  
      sex.coef       <-  v12    # log odds -0.693 per change in binary, or odds of .5  
      
      intercept <- -5
      
      trt      <- sample(1:3,   n, replace=TRUE)      # trt 3 levels
      age      <- sample(18:65, n, replace=TRUE)      # continuous
      bmi      <- sample(1:3,   n, replace=TRUE)      # assume 3 equal groups?
      smoking  <- sample(1:3,   n, replace=TRUE)      # categorical assume 3 equal groups?
      covar3   <- round(runif(n,0,3),2)
      covar1   <- round(runif(n,0,10),2)
      vas      <- sample(1:30, n, replace=TRUE)
      time     <- round(runif(n,0,10),2)              # years
      covar2   <- sample(1:50, n, replace=TRUE)
      fact1    <- sample(0:1,  n, replace=TRUE)
      binary2  <- sample(0:1,  n, replace=TRUE)
      sex      <- sample(0:1,  n, replace=TRUE)
      
      return(list(    
        
        trt.coef       =trt.coef ,
        age.coef       =age.coef,
        smoke.coef     =smoke.coef,
        bmi.coef       =bmi.coef,
        covar3.coef    =covar3.coef,
        covar1.coef    =covar1.coef,
        vas.coef       =vas.coef,
        time.coef      =time.coef,
        covar2.coef    =covar2.coef,
        fact1.coef     =fact1.coef,
        binary2.coef   =binary2.coef,
        sex.coef       =sex.coef,
        
        trt= trt, 
        age=age, 
        bmi=bmi, 
        smoking=smoking,
        covar3=covar3,
        covar1=covar1, 
        vas=vas, 
        time=time, 
        covar2=covar2, 
        fact1=fact1 , 
        binary2=binary2, 
        sex=sex,
        
        randomi=randomi))
      
      
    })    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    lp1 <- reactive({
      
      d <- design()
      
      trt      <-d$trt      
      age      <-d$age       
      bmi      <-d$bmi       
      smoking  <-d$smoking  
      covar3      <-d$covar3      
      covar1   <-d$covar1   
      vas      <-d$vas       
      time     <-d$time      
      covar2   <-d$covar2    
      fact1    <-d$fact1     
      binary2 <-d$binary2  
      sex      <-d$sex  
      
      trt.coef      =d$trt.coef 
      age.coef      =d$age.coef
      smoke.coef    =d$smoke.coef
      bmi.coef      =d$bmi.coef
      covar3.coef      =d$covar3.coef
      covar1.coef   =d$covar1.coef
      vas.coef      =d$vas.coef
      time.coef     =d$time.coef
      covar2.coef   =d$covar2.coef
      fact1.coef    =d$fact1.coef
      binary2.coef =d$binary2.coef
      sex.coef      =d$sex.coef
      
      randomi <- d$randomi
      intercept <- -3
      
      
      if ( (input$Design) == "Treatment interacts with all variables" )  {
        
        lp = intercept + trt*trt.coef*(smoking*smoke.coef   +   age*age.coef  + bmi*bmi.coef + covar3*covar3.coef +
                                         covar1*covar1.coef + vas*vas.coef + time*time.coef + covar2*covar2.coef +
                                         fact1*fact1.coef +
                                         binary2*binary2.coef + sex*sex.coef) 
        
      }   else if ( (input$Design ) == "Treatment interacts with smoking only" ) {    
        
        # truth  only smoking interacts  with trt
        lp = intercept + (trt*trt.coef*smoking*smoke.coef)   +   age*age.coef   + bmi*bmi.coef + covar3*covar3.coef +
          covar1*covar1.coef + vas*vas.coef + time*time.coef + covar2*covar2.coef + fact1*fact1.coef +
          binary2*binary2.coef + sex*sex.coef
        
      }   else if ( (input$Design) == "No-interaction logit-additive model" ) {  
        
        # truth no interactions
        lp = intercept + trt*trt.coef + smoking*smoke.coef + age*age.coef  + bmi*bmi.coef + covar3*covar3.coef +
          covar1*covar1.coef + vas*vas.coef + time*time.coef + covar2*covar2.coef + fact1*fact1.coef +
          binary2*binary2.coef + sex*sex.coef
      }
      
      
      y <- ifelse(randomi < plogis(lp), 1, 0)   # one liner RANDOM!!!
      
      dat <- data.frame(cbind(y,  trt ,  smoking, age, covar3, covar1, vas, time, covar2, fact1, binary2, sex, bmi))
      
      return(list(datx=dat))
      
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    analysis <- reactive({
      
      da <- lp1()$datx 
      
      da$trt <-     factor(da$trt)
      da$smoking <- factor(da$smoking)
      da$fact1 <-   factor(da$fact1)
      da$binary2 <-factor(da$binary2)
      da$sex <-     factor(da$sex)
      da$bmi <-     factor(da$bmi)
      
      label(da$age)                <- 'Age'                       # label is in Hmisc
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
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # user can change the range at which effects are estimated
      
      Ages <-   (as.numeric(unlist(strsplit(input$age.range,","))))    
      
      dd$limits$age[1] <<- Ages[1]
      dd$limits$age[3] <<- Ages[2]
      
      Ages <-   (as.numeric(unlist(strsplit(input$biomarker.range,","))))    
      
      dd$limits$covar3[1] <<- Ages[1]
      dd$limits$covar3[3] <<- Ages[2]
      
      Ages <-   (as.numeric(unlist(strsplit(input$blood.range,","))))    
      
      dd$limits$covar1[1] <<- Ages[1]
      dd$limits$covar1[3] <<- Ages[2]
      
      
      Ages <-   (as.numeric(unlist(strsplit(input$vas.range,","))))    
      
      dd$limits$vas[1] <<- Ages[1]
      dd$limits$vas[3] <<- Ages[2]
      
      Ages <-   (as.numeric(unlist(strsplit(input$time.range,","))))    
      
      dd$limits$time[1] <<- Ages[1]
      dd$limits$time[3] <<- Ages[2]
      
      
      Ages <-   (as.numeric(unlist(strsplit(input$fitness.range,","))))    
      
      dd$limits$covar2[1] <<- Ages[1]
      dd$limits$covar2[3] <<- Ages[2]
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      A<-lrm(y~   trt * (smoking  + age  + bmi + covar3 + covar1 + vas + time + covar2 + fact1 + binary2 +sex),da, y=TRUE, x=TRUE)   # all interact with trt
      B<-lrm(y~  (trt *  smoking) + age  + bmi + covar3 + covar1 + vas + time + covar2 + fact1 + binary2 +sex, da, y=TRUE, x=TRUE)   # smoking * trt only
      C<-lrm(y~   trt +  smoking  + age +  bmi + covar3 + covar1 + vas + time + covar2 + fact1 + binary2 +sex, da, y=TRUE, x=TRUE)   # main effect
      
      outputx <- input$Model 
      
      if (  (outputx) == "Treatment interacts with all variables" )  {
        f <- A
        
      }   else if (  (outputx) == "Treatment interacts with smoking only" ) {
        
        f <- B
        
      }   else if (  (outputx) == "No-interaction logit-additive model" ) {
        
        f <- C
      }
      
      da$trt <- relevel(da$trt, "2")
      Aref2 <- lrm(y~   trt * (smoking  + age  + bmi + covar3 + covar1 + vas + time + covar2 + fact1 + binary2 +sex),da, y=TRUE, x=TRUE)   # all interact with trt
      da$trt <- relevel(da$trt, "3")
      Aref3 <- lrm(y~   trt * (smoking  + age  + bmi + covar3 + covar1 + vas + time + covar2 + fact1 + binary2 +sex),da, y=TRUE, x=TRUE)   # all interact with trt
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      return(list(  A=A, B=B, C=C, f=f, outputx=outputx, Aref2 = Aref2, Aref3 = Aref3)) 
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
    })
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$textWithNumber2 <- renderText({ 
      
      txt <- analysis()
      HTML(paste0("Table 1. ", tags$span(style="color:black", txt$outputx  )
      ))    
      
    })  
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$user <- renderPrint({
      return(print(analysis()$f, digits=3))
    }) 
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    
    output$f.plot3 <- renderPlot({
      
      X <- analysis()
      
      A <- X$C
      
     # par(oma=c(3,4,1,1))
      par(oma=c(0,1,1,1))
      options(digits=1)
      
      
      plot(summary(A, smoking=1, age, covar3, covar1, vas, time, covar2, fact1, binary2, sex, bmi=1, trt=1, 
                   est.all=FALSE, vnames=c( "labels"), antilog=TRUE),
           log=TRUE, xlim=c(log(.2),log(10)),
           q=c( 0.95 ), 
           #at=c( .1,.2,.3,.5,.75,1, 1.2,1.5, 2,3,4,6,8,10), 
           at=  (MASS::fractions(2^seq(-8 , 8, by=1))),
           lwd=3, pch=17,
           col=   rgb(red=.4,green=.1,blue=.5, alpha=c(.5,.3,.2)),
           col.points='black', cex=1,  main=" <- worse outcomes      Odds Ratio       better outcomes ->                ", cex.main=1.8 
      )
      
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # MAIN EFFECTS MODELL
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    zummaryC<- reactive({
      
      X <- analysis() 
      
      A1 <- summary(X$C, smoking=1, age, covar3, covar1, vas, time, covar2, fact1, binary2, sex, bmi=1, trt=1, est.all=FALSE, vnames=c( "labels"))
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      return(list(  A1=A1 )) 
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
    })
    
    output$int.trt1C <- renderPrint({
      return(print(zummaryC()$A1))
    }) 
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
    # trtt x all
    # output$f.plot1 <- renderPlot({   
    #   
    #   X <- analysis() 
    #   
    #   A <- X$A
    #   
    #   v0. <- as.numeric(    eval(parse(text= (input$adj.smoking)) ) )
    #   v1. <- as.numeric(    eval(parse(text= (input$adj.age)) ) )
    #   v2. <- as.numeric(    eval(parse(text= (input$adj.biomarker)) ) )
    #   v3. <- as.numeric(    eval(parse(text= (input$adj.blood)) ) )
    #   v4. <- as.numeric(    eval(parse(text= (input$adj.vas)) ) )   
    #   v5. <- as.numeric(    eval(parse(text= (input$adj.time)) ) ) 
    #   v6. <- as.numeric(    eval(parse(text= (input$adj.fitness)) ) ) 
    #   v7. <- as.numeric(    eval(parse(text= (input$adj.history)) ) )
    #   v8. <- as.numeric(    eval(parse(text= (input$adj.employed)) ) )
    #   v9. <- as.numeric(    eval(parse(text= (input$adj.sex)) ) )
    #   v10. <-as.numeric(    eval(parse(text= (input$adj.BMI)) ) )
    #   
    #   
    #   par(mfrow=c(1,1)) 
    #   
    #   par(oma=c(3,6,1,1)) 
    #   
    #   options(digits=1)
    #   
    #   
    #   plot(summary(A, smoking=v0., age=v1., covar3=v2., covar1=v3., vas=v4., time=v5., 
    #                covar2=v6., fact1=v7., binary2=v8., sex=v9., bmi=v10.,  
    #                trt=1, est.all=FALSE, vnames=c( "labels")), 
    #        log=TRUE, xlim=c(log(.01),log(40)),
    #        q=c(  0.95 ), at=c(.02,0.05,.1,.2,.5,1,2,4,8,20), lwd=3, pch=17,
    #        col=   rgb(red=.4,green=.1,blue=.5,alpha=c(.5,.3,.2)),
    #        col.points='black', cex=1, main= "Odds Ratio (Treatment 1)", cex.main=1.8
    #   )
    #   
    #   # plot(summary(A, smoking=v0., age=v1., covar3=v2., covar1=v3., vas=v4., time=v5., 
    #   #              covar2=v6., fact1=v7., binary2=v8., sex=v9., bmi=v10.,
    #   #              trt=2, est.all=FALSE, vnames=c( "labels")), 
    #   #      log=TRUE, xlim=c(log(.01),log(40)),
    #   #      q=c(  0.95 ), at=c(.02,0.05,.1,.2,.5,1,2,4,8,20), lwd=3, pch=17,
    #   #      col=   rgb(red=.4,green=.1,blue=.5,alpha=c(.5,.3,.2)),
    #   #      col.points='black', cex=1, main= "Odds Ratio (Treatment 2)", cex.main=1.8
    #   # )
    #   # 
    #   # plot(summary(A, smoking=v0., age=v1., covar3=v2., covar1=v3., vas=v4., time=v5., 
    #   #              covar2=v6., fact1=v7., binary2=v8., sex=v9., bmi=v10.,
    #   #              trt=3, est.all=FALSE, vnames=c( "labels")), 
    #   #      log=TRUE, xlim=c(log(.01),log(40)),
    #   #      q=c(  0.95 ), at=c(.02,0.05,.1,.2,.5,1,2,4,8,20), lwd=3, pch=17,
    #   #      col=   rgb(red=.4,green=.1,blue=.5,alpha=c(.5,.3,.2)),
    #   #      col.points='black', cex=1, main= "Odds Ratio (Treatment 3)", cex.main=1.8
    #   # )
    #   
    #   par(mfrow=c(1,1))
    #   
    #   
    #   
    # }) 
    
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
    # trtt x all
    # output$f.plot2 <- renderPlot({   
    #   
    #   X <- analysis() 
    #   
    #   A <- X$A
    #   
    #   v0. <- as.numeric(    eval(parse(text= (input$adj.smoking)) ) )
    #   v1. <- as.numeric(    eval(parse(text= (input$adj.age)) ) )
    #   v2. <- as.numeric(    eval(parse(text= (input$adj.biomarker)) ) )
    #   v3. <- as.numeric(    eval(parse(text= (input$adj.blood)) ) )
    #   v4. <- as.numeric(    eval(parse(text= (input$adj.vas)) ) )   
    #   v5. <- as.numeric(    eval(parse(text= (input$adj.time)) ) ) 
    #   v6. <- as.numeric(    eval(parse(text= (input$adj.fitness)) ) ) 
    #   v7. <- as.numeric(    eval(parse(text= (input$adj.history)) ) )
    #   v8. <- as.numeric(    eval(parse(text= (input$adj.employed)) ) )
    #   v9. <- as.numeric(    eval(parse(text= (input$adj.sex)) ) )
    #   v10. <-as.numeric(    eval(parse(text= (input$adj.BMI)) ) )
    #   
    #   
    #   par(mfrow=c(1,1)) 
    #   
    #   par(oma=c(3,6,1,1)) 
    #   
    #   options(digits=1)
    #   
    #   
    #   # plot(summary(A, smoking=v0., age=v1., covar3=v2., covar1=v3., vas=v4., time=v5., 
    #   #              covar2=v6., fact1=v7., binary2=v8., sex=v9., bmi=v10.,  
    #   #              trt=1, est.all=FALSE, vnames=c( "labels")), 
    #   #      log=TRUE, xlim=c(log(.01),log(40)),
    #   #      q=c(  0.95 ), at=c(.02,0.05,.1,.2,.5,1,2,4,8,20), lwd=3, pch=17,
    #   #      col=   rgb(red=.4,green=.1,blue=.5,alpha=c(.5,.3,.2)),
    #   #      col.points='black', cex=1, main= "Odds Ratio (Treatment 1)", cex.main=1.8
    #   # )
    #   
    #   plot(summary(A, smoking=v0., age=v1., covar3=v2., covar1=v3., vas=v4., time=v5., 
    #                covar2=v6., fact1=v7., binary2=v8., sex=v9., bmi=v10.,
    #                trt=2, est.all=FALSE, vnames=c( "labels")), 
    #        log=TRUE, xlim=c(log(.01),log(40)),
    #        q=c(  0.95 ), at=c(.02,0.05,.1,.2,.5,1,2,4,8,20), lwd=3, pch=17,
    #        col=   rgb(red=.4,green=.1,blue=.5,alpha=c(.5,.3,.2)),
    #        col.points='black', cex=1, main= "Odds Ratio (Treatment 2)", cex.main=1.8
    #   )
    #   
    #   # plot(summary(A, smoking=v0., age=v1., covar3=v2., covar1=v3., vas=v4., time=v5., 
    #   #              covar2=v6., fact1=v7., binary2=v8., sex=v9., bmi=v10.,
    #   #              trt=3, est.all=FALSE, vnames=c( "labels")), 
    #   #      log=TRUE, xlim=c(log(.01),log(40)),
    #   #      q=c(  0.95 ), at=c(.02,0.05,.1,.2,.5,1,2,4,8,20), lwd=3, pch=17,
    #   #      col=   rgb(red=.4,green=.1,blue=.5,alpha=c(.5,.3,.2)),
    #   #      col.points='black', cex=1, main= "Odds Ratio (Treatment 3)", cex.main=1.8
    #   # )
    #   
    #   par(mfrow=c(1,1))
    #   
    #   
    #   
    # }) 
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
    # trtt x all
    # output$f.plot3x <- renderPlot({   
    #   
    #   X <- analysis() 
    #   
    #   A <- X$A
    #   
    #   v0. <- as.numeric(    eval(parse(text= (input$adj.smoking)) ) )
    #   v1. <- as.numeric(    eval(parse(text= (input$adj.age)) ) )
    #   v2. <- as.numeric(    eval(parse(text= (input$adj.biomarker)) ) )
    #   v3. <- as.numeric(    eval(parse(text= (input$adj.blood)) ) )
    #   v4. <- as.numeric(    eval(parse(text= (input$adj.vas)) ) )   
    #   v5. <- as.numeric(    eval(parse(text= (input$adj.time)) ) ) 
    #   v6. <- as.numeric(    eval(parse(text= (input$adj.fitness)) ) ) 
    #   v7. <- as.numeric(    eval(parse(text= (input$adj.history)) ) )
    #   v8. <- as.numeric(    eval(parse(text= (input$adj.employed)) ) )
    #   v9. <- as.numeric(    eval(parse(text= (input$adj.sex)) ) )
    #   v10. <-as.numeric(    eval(parse(text= (input$adj.BMI)) ) )
    #   
    #   
    #   par(mfrow=c(1,1)) 
    #   
    #   par(oma=c(3,6,1,1)) 
    #   
    #   options(digits=1)
    #   
    #   
    #   # plot(summary(A, smoking=v0., age=v1., covar3=v2., covar1=v3., vas=v4., time=v5., 
    #   #              covar2=v6., fact1=v7., binary2=v8., sex=v9., bmi=v10.,  
    #   #              trt=1, est.all=FALSE, vnames=c( "labels")), 
    #   #      log=TRUE, xlim=c(log(.01),log(40)),
    #   #      q=c(  0.95 ), at=c(.02,0.05,.1,.2,.5,1,2,4,8,20), lwd=3, pch=17,
    #   #      col=   rgb(red=.4,green=.1,blue=.5,alpha=c(.5,.3,.2)),
    #   #      col.points='black', cex=1, main= "Odds Ratio (Treatment 1)", cex.main=1.8
    #   # )
    #   
    #   # plot(summary(A, smoking=v0., age=v1., covar3=v2., covar1=v3., vas=v4., time=v5., 
    #   #              covar2=v6., fact1=v7., binary2=v8., sex=v9., bmi=v10.,
    #   #              trt=2, est.all=FALSE, vnames=c( "labels")), 
    #   #      log=TRUE, xlim=c(log(.01),log(40)),
    #   #      q=c(  0.95 ), at=c(.02,0.05,.1,.2,.5,1,2,4,8,20), lwd=3, pch=17,
    #   #      col=   rgb(red=.4,green=.1,blue=.5,alpha=c(.5,.3,.2)),
    #   #      col.points='black', cex=1, main= "Odds Ratio (Treatment 2)", cex.main=1.8
    #   # )
    #   
    #   plot(summary(A, smoking=v0., age=v1., covar3=v2., covar1=v3., vas=v4., time=v5.,
    #                covar2=v6., fact1=v7., binary2=v8., sex=v9., bmi=v10.,
    #                trt=3, est.all=FALSE, vnames=c( "labels")),
    #        log=TRUE, xlim=c(log(.01),log(40)),
    #        q=c(  0.95 ), at=c(.02,0.05,.1,.2,.5,1,2,4,8,20), lwd=3, pch=17,
    #        col=   rgb(red=.4,green=.1,blue=.5,alpha=c(.5,.3,.2)),
    #        col.points='black', cex=1, main= "Odds Ratio (Treatment 3)", cex.main=1.8
    #   )
    #   
    #   par(mfrow=c(1,1))
    #   
    #   
    #   
    # }) 
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
    # trtt x all
    output$f.plot99 <- renderPlot({   
      
      X <- analysis() 
      
      A <- X$A
      
      v0. <- as.numeric(    eval(parse(text= (input$adj.smoking)) ) )
      v1. <- as.numeric(    eval(parse(text= (input$adj.age)) ) )
      v2. <- as.numeric(    eval(parse(text= (input$adj.biomarker)) ) )
      v3. <- as.numeric(    eval(parse(text= (input$adj.blood)) ) )
      v4. <- as.numeric(    eval(parse(text= (input$adj.vas)) ) )   
      v5. <- as.numeric(    eval(parse(text= (input$adj.time)) ) ) 
      v6. <- as.numeric(    eval(parse(text= (input$adj.fitness)) ) ) 
      v7. <- as.numeric(    eval(parse(text= (input$adj.history)) ) )
      v8. <- as.numeric(    eval(parse(text= (input$adj.employed)) ) )
      v9. <- as.numeric(    eval(parse(text= (input$adj.sex)) ) )
      v10. <-as.numeric(    eval(parse(text= (input$adj.BMI)) ) )
      
      
      par(mfrow=c(1,3)) ## change this to show nice plot 1,1
      
      par(oma=c(1,10,1,1))  # b l t  r
      #par(mar=c(3,0,3,3))
      options(digits=1)
      
      
       plot(summary(A, smoking=v0., age=v1., covar3=v2., covar1=v3., vas=v4., time=v5.,
                    covar2=v6., fact1=v7., binary2=v8., sex=v9., bmi=v10.,
                    trt=1, est.all=FALSE, vnames=c( "labels")),
            log=TRUE, xlim=c(log(.01),log(40)),
            q=c(  0.95 ), at=c(.02,0.05,.1,.2,.5,1,2,4,8,20), lwd=3, pch=17,
            col=   rgb(red=.4,green=.1,blue=.5,alpha=c(.5,.3,.2)),
            col.points='black', cex=1, main= "Odds Ratio (Treatment 1)", cex.main=1.8
       )

       plot(summary(A, smoking=v0., age=v1., covar3=v2., covar1=v3., vas=v4., time=v5.,
                    covar2=v6., fact1=v7., binary2=v8., sex=v9., bmi=v10.,
                    trt=2, est.all=FALSE, vnames=c( "labels")),
            log=TRUE, xlim=c(log(.01),log(40)),
            q=c(  0.95 ), at=c(.02,0.05,.1,.2,.5,1,2,4,8,20), lwd=3, pch=17,
            col=   rgb(red=.4,green=.1,blue=.5,alpha=c(.5,.3,.2)),
            col.points='black', cex=1, main= "Odds Ratio (Treatment 2)", cex.main=1.8
       )
      
      plot(summary(A, smoking=v0., age=v1., covar3=v2., covar1=v3., vas=v4., time=v5.,
                   covar2=v6., fact1=v7., binary2=v8., sex=v9., bmi=v10.,
                   trt=3, est.all=FALSE, vnames=c( "labels")),
           log=TRUE, xlim=c(log(.01),log(40)),
           q=c(  0.95 ), at=c(.02,0.05,.1,.2,.5,1,2,4,8,20), lwd=3, pch=17,
           col=   rgb(red=.4,green=.1,blue=.5,alpha=c(.5,.3,.2)),
           col.points='black', cex=1, main= "Odds Ratio (Treatment 3)", cex.main=1.8
      )
      
      par(mfrow=c(1,1))
      
      
      
    }) 
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    # 
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$powerp5 <- renderPrint({  # renderText not so useful
        
        
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Here we plot weibull distributions
    output$powerw <-renderPlot({  
        
        
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
        
        
        return(list(s=survfit, f=f ,w=w , hr=hr, hrc=hrc, ev=ev, d=d, result.simple=result.simple,
                    result.pe5= result.pe5,result.pe1=result.pe1,result.smooth=result.smooth,  result.km= result.km,
                    time=time, event=event , survx=survx, times=times)) 
        
    })
    
    # right exponential plot....................., 
    output$plot1d<-renderPlot({     
        
        
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # left plot
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$plot1c<-renderPlot({     
        
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    
    # estimating hazard plot
    output$ploth <-renderPlot({     
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    
    output$ploth1 <-renderPlot({     
        
        
        
    })
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    
    output$ploth2 <-renderPlot({     
        
        
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    
    
    output$survhaz <-renderPlot({     
        
        
        
    })
    
    output$survhaz2 <-renderPlot({     
        
        
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # text below right plot in changing hazard 
    output$info4 <- renderText({  
        
        
        
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    # another piece of text below right plot in changing hazard 
    
    output$info5 <- renderText({  
        
        
    })
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # GENERATE THE DATA Execute analysis for the landing page
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    dat <- reactive({
        
        
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    setUpByName <- reactive ({
        # f <-dat()$np  # Get the  data
        # f <-  f$n[1]
        # y <- as.numeric(as.character(f))
        y=1
        return(y)
    })
    
    setUpByNamea <- reactive ({
        # f <-dat()$np  # Get the  data
        # f <-  f$numevents[1]
        # y <- as.numeric(as.character(f))
        y=1
        return(y)
    })
    
    setUpByNameb <- reactive ({
        # f <-dat()$np  # Get the  data
        # f <-  f$exposure[1]
        # y <- as.numeric(as.character(f))
        y=1
        return(y)
    })
    
    setUpByNamec <- reactive ({
        # f <-dat()$np  # Get the  data
        # f <-  summary(f)$table[,'median'][1]
        # y <- as.numeric(as.character(f))
        y=1 
        return(y)
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    setUpByName2 <- reactive ({
        # f <-dat()$np  # Get the  data
        # f <-  f$n[2]
        # y <- as.numeric(as.character(f))
        # return(y)
        y=1 
        return(y)
    })
    
    setUpByName2a <- reactive ({
        # f <-dat()$np  # Get the  data
        # f <-  f$numevents[2]
        # y <- as.numeric(as.character(f))
        # return(y)
        
        y=1 
        return(y)
    })
    
    
    setUpByName2b <- reactive ({
        # f <-dat()$np  # Get the  data
        # f <-  f$exposure[2]
        # y <- as.numeric(as.character(f))
        # return(y)
        y=1 
        return(y)
    })
    
    setUpByName2c <- reactive ({
        # f <-dat()$np  # Get the  data
        # f <-  summary(f)$table[,'median'][2]
        # y <- as.numeric(as.character(f))
        # return(y)
        y=1 
        return(y)
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    setUpByNameLL <- reactive ({
        # f <-dat()$LL1  
        # y <- as.numeric(as.character(f))
        # return(y)
        y=1 
        return(y)
    })
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    setUpByName3 <- reactive ({
        # f <- dat()$f  # Get the  data
        # y <- as.numeric(as.character(f$coefficients))
        # return(y)
        y=1 
        return(y)
    })
    
    
    setUpByName4 <- reactive ({
        # f <-dat()$sf  # Get the  data
        # y <- as.numeric(as.character(f[2,c("Effect")]))
        # return(y)
        y=1 
        return(y)
    })
    
    
    setUpByName5 <- reactive ({
        # f <-dat()$sf  # Get the  data
        # y <- as.numeric(as.character(f[2,c("Lower 0.95")]))
        # return(y)
        y=1 
        return(y)
    })
    
    setUpByName6 <- reactive ({
        # f <-dat()$sf  # Get the  data
        # y <- as.numeric(as.character(f[2,c("Upper 0.95")]))
        # return(y)
        y=1 
        return(y)
    })
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # MAIN PLOT! updated with log transformation  option
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$plot5a <- output$plot1 <- renderPlotly({
        
        
    })
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$plot99a <- renderPlotly({
        
        
        
    })
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$plot99b <- renderPlotly({
        
        
        
    })
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$plot99c <- renderPlotly({
        
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$plot2<-renderPlot({     
        
        
    })
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$plot2x <-renderPlot({     # Cox
        
        
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$plot4<-renderPlot({     
        
        
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$plot2y <-renderPlot({     # Cox
        
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$plot3 <- renderPlot({
        
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$plot5b <- renderPlotly({
        
        
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$mytable <- DT::renderDataTable({
        
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~KM table
    output$KM <- DT::renderDataTable({
        
    })
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$CHAZ <- DT::renderDataTable({
        
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~maximum likelihood
    
    output$mytable2 <- DT::renderDataTable({
        
    })
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$exercise <- DT::renderDataTable({  
        
    })
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$exercise2 <- DT::renderDataTable({
        
        
    })
    
    output$help <- renderText({
        HTML(" ")
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$Staff_name2 <- output$Staff_name <- renderText({  
        
        
        
    })
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # frank Harrell rms page 479
    output$FH <- renderPlot({
        
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$info <- renderText({  
        
        c("xxxxxxxxxxx")
        
    })
    
    output$info2 <- renderText({  
        
        c("xxxxxxxxxxx")
        
    })
    
    output$info3 <- renderText({  
        
        
        c(paste0("xxxxxxxxxxx"))
        
    })
    
    
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

shinyApp(ui, server)