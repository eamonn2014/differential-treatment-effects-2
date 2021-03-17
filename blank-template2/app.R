#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# dashboard template
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
                                                      
                                                      
                                                      menuItem("Define parameters ", icon = icon("bar-chart-o"),
                                                               splitLayout(
                                                                   
                                                                   tags$div(
                                                                       textInput(inputId="n", label='xxxxxxxxxxxxx', width = '90%' , value="800"),
                                                                   ),
                                                                   
                                                                   tags$div(
                                                                       textInput(inputId='allocation', label='xxxxxxxxxxxxx', width = '90%' , ".5"),
                                                                   ) 
                                                               ) ,
                                                               
                                                               splitLayout(
                                                                   
                                                                   tags$div(
                                                                       textInput(inputId='baseline', label='xxxxxxxxxxxxx', width = '90%' , ".4"),
                                                                   ),
                                                                   
                                                                   tags$div(
                                                                       textInput(inputId='hr', label='xxxxxxxxxxxxx', width = '90%' , ".75"),
                                                                   ) 
                                                                   
                                                               ) 
                                                      ),
                                                      
                                                      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                                      menuItem("Analyses",  startExpanded = FALSE,  icon = icon("bar-chart-o"),
                                                               #~~~~~~~~~~~~~~~~~~~~~~~~
                                                               menuSubItem("Kaplan Meier (landing page)",    tabName = "OVERVIEW",  icon = icon("bar-chart-o"), selected = TRUE),
                                                               menuSubItem("KM diagnostics",                 tabName = "RESULTS2",  icon = icon("bar-chart-o")),
                                                               menuSubItem("Cox proportional hazards",       tabName = "RESULTS3",  icon = icon("bar-chart-o")),
                                                               menuSubItem("Cox PH Explanation",             tabName = "HELP",      icon = icon("bar-chart-o"), selected = FALSE),
                                                               menuSubItem("Hazard ratio over time",         tabName = "RESULTS4",  icon = icon("bar-chart-o")),
                                                               menuSubItem("Partial log likelihood",         tabName = "RESULTS1",  icon = icon("table")),
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
                                                               
                                                               menuSubItem(h5(HTML( "xxxxxxxxxxxxx")),  
                                                                           icon = icon("send",lib='glyphicon'), 
                                                                           href = "http://www.stat.cmu.edu/~ryantibs/journalclub/cox_1972.pdf"),
                                                               
                                                               menuSubItem(h5(HTML( "xxxxxxxxxxxxx")),
                                                                           icon = icon("send",lib='glyphicon'), 
                                                                           href = "https://jme.bmj.com/content/medethics/31/12/703.full.pdf") ,
                                                               #dashboardHeader(title = h4(HTML("This title<br/>is just way too long")))
                                                               
                                                               menuSubItem( h5(HTML("xxxxxxxxxxxxx")),  
                                                                            icon = icon("send",lib='glyphicon'), 
                                                                            href = "https://understandinguncertainty.org/node/759"),
                                                               
                                                               menuSubItem( h5(HTML("xxxxxxxxxxxxx")),  
                                                                            icon = icon("send",lib='glyphicon'), 
                                                                            href = "https://github.com/eamonn2014/PARTIAL-LIKELIHOOD-METHOD/blob/master/Analysis%20of%20time-to-event%20for%20observational%20studies.pdf"),
                                                               
                                                               menuSubItem( h5(HTML("xxxxxxxxxxxxx")),  
                                                                            icon = icon("send",lib='glyphicon'), 
                                                                            href = "https://influentialpoints.com/Training/coxs_proportional_hazards_regression_model-principles-properties-assumptions.htm"),
                                                               
                                                               menuSubItem( h5(HTML("xxxxxxxxxxxxx")),  
                                                                            icon = icon("send",lib='glyphicon'), 
                                                                            href = "https://rdrr.io/cran/rms/man/cph.html"),
                                                               
                                                               menuSubItem( h5(HTML("xxxxxxxxxxxxx")),  
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
                              
                              tabItem("Wiki", 
                                      fluidRow(
                                          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                          box(  width=6,
                                                title='Wiki'
                                                ,status = "primary"
                                                ,solidHeader = TRUE 
                                                ,collapsible = TRUE 
                                                
                                                ,p("xxxxxxxxxxxxx")  
                                                
                                                
                                                
                                                
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
                                          box(
                                              title = "xxxxxxxxxxxxx"
                                              ,status = "primary"
                                              ,solidHeader = TRUE 
                                              ,collapsible = TRUE ,
                                              # ,plotlyOutput("plot1", height = "720px"),
                                              
                                              #  h5(textOutput("Staff_name")),
                                              # h5(textOutput("Staff_name3")),
                                              #h5(textOutput("Staff_name4")),
                                              #h5(textOutput("Staff_name5")),
                                              
                                              tags$a(href = "https://www.youtube.com/watch?v=EoIB_Obddrk&t=327s&ab_channel=RMSRegression", tags$span(style="color:blue", "xxxxxxxxxxxxx"),),
                                              div(p(" "))
                                              
                                              
                                          )
                                          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                          ,box(
                                              title='xxxxxxxxxxxxx'
                                              ,status = "primary"
                                              ,solidHeader = TRUE 
                                              ,collapsible = TRUE 
                                              #,plotOutput("plot2", height = "720px")
                                          ))),               
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
                                          
                                          ,box(
                                              title="xxxxxxxxxxxxx"
                                              ,status = "primary"
                                              ,solidHeader = TRUE 
                                              ,collapsible = TRUE 
                                              #,plotlyOutput("plot5b", height = "720px"),
                                              #h5(textOutput("info"))
                                          ))),   
                              
                              
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
                                          
                                          ,box(
                                              title=" "
                                              ,status = "primary"
                                              ,solidHeader = TRUE 
                                              ,collapsible = TRUE 
                                              ,p("xxxxxxxxxxxxx")
                                              
                                          ))),   
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
                                          
                                      )),
                              
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
                              tabItem("RESULTS2",
                                      fluidRow(
                                          box(width=4,
                                              title = "xxxxxxxxxxxxx xxxxxxxxxxxxx" 
                                              ,status = "primary"
                                              ,solidHeader = TRUE 
                                              ,collapsible = TRUE 
                                              #  ,plotlyOutput("plot99a", height = "720px")
                                              ,p("xxxxxxxxxxxxx")
                                          )
                                          
                                          ,box(width=4,
                                               title="xxxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxxxx" 
                                               ,status = "primary"
                                               ,solidHeader = TRUE
                                               ,collapsible = TRUE
                                               #  ,plotlyOutput("plot99b", height = "720px")
                                               
                                               # ,h5(textOutput("info3"))
                                               ,p("xxxxxxxxxxxxx")
                                          ) 
                                          
                                          ,box(width=4,
                                               title="xxxxxxxxxxxxx xxxxxxxxxxxxx" 
                                               ,status = "primary"
                                               ,solidHeader = TRUE
                                               ,collapsible = TRUE
                                               #,plotlyOutput("plot99c", height = "720px")  
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
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        return(list(x=x, f=ff.dropout , f1=f1, fit=fit)) 
        
        
    })
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # estimating hazard plot
    output$powerp1 <-renderPlot({     
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$powerp3w <- output$powerp3 <-renderPlotly({     
        
    })
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$powerp2 <- renderPrint({  # renderText not so useful
        
        
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$powerp4 <- renderPrint({  # renderText not so useful
        
        
        
    })
    
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