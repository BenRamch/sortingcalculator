####installl Packages######


#if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install("flowCore")
#BiocManager::install("flowWorkspace")
#BiocManager::install("openCyto")
#BiocManager::install("flowStats")
#BiocManager::install("flowClust")
#BiocManager::install("ggridges")
#BiocManager::install("flowMerge")
#BiocManager::install("ggcyto")
#BiocManager::install("flowDensity")
#BiocManager::install("flowPeaks")
#install.packages("chron")

####attach Packages######


library(BiocManager)
library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(reshape2)
library(scales)
library(rsconnect)
library(shinyWidgets)
library(magick)
library(tidyverse)
library(ggforce)
library(shinythemes)
library(chron)
library(hms)
library(htmlwidgets)

#rsconnect::setAccountInfo(name='bramcharitarmskcc', token='6F68B559EC7D56A5CF531E07DC066D14', secret='vYL8TqCfzH2piZ1RRZJrCXK84SZdTdblbnNQotnB')

#setwd("C:/Users/RamcharB/OneDrive - Memorial Sloan Kettering Cancer Center/Desktop")

#setwd("//skimcs.mskcc.org/fccf/Personal_Folders/Benjamin Ramcharitar/sortcalculator/sortcalculator/calculatortest-v2")
#############################APP#################
ui <- fluidPage(
  titlePanel(h1("Sort Time Calculator",align="center",
                style='background-color:#1c75cd;color:white;font-size:50px;
                     padding-left: 15px;padding-bottom:20px;padding-top:20px;')),
  tags$style(HTML("
    .tabbable > .nav > li > a                  {background-color: white;  color:blue3}
    .tabbable > .nav > li > a[data-value='Calculator'] {background-color: white;   color:blue3}
    .tabbable > .nav > li > a[data-value='t2'] {background-color: blue;  color:white}
    .tabbable > .nav > li > a[data-value='t3'] {background-color: green; color:white}
    .tabbable > .nav > li[class=active]    > a {background-color: white; color:blue3}
  ")),
  
tabsetPanel(
    tabPanel(HTML(paste(h3("Sort Time Calculator"))), fluid = TRUE,
       tabsetPanel(
            tabPanel("Calculator", fluid = TRUE,
             sidebarLayout(
               # Sidebar to demonstrate various slider options ----
               sidebarPanel(id="sidebar",
                            radioGroupButtons("instrument", "Instrument",size='normal',width='auto',justified = TRUE,
                                              choices = c("Aria","S6"),status = "primary",individual = TRUE,
                                              checkIcon = list(
                                                yes = icon("ok", 
                                                           lib = "glyphicon"),
                                                no = icon("remove",
                                                    lib = "glyphicon"))),
                            br(),
                            radioGroupButtons("nozzle", "Nozzle", choiceNames=c("70um",
                                                                                "100um",
                                                                                "130um","Plates"),
                                              choiceValues = c("70um","100um","130um","plates"),
                                              
                                              checkIcon = list(
                                                yes = tags$i(class = "fa fa-circle", 
                                                             style = "color: steelblue"),
                                                no = tags$i(class = "fa fa-circle-o", 
                                                            style = "color: steelblue"))
                            ),
                            
                            
                            
                            numericInput("samples", "Number of Samples", 1, min = 0, step = 1),
                            hr(),
                            conditionalPanel("input.nozzle != 'plates'",
                          numericInput("cells", "Total Number of Cells Across Samples (in millions, ie. 1,000,000 = 1)", 1, min = 1, step = 1)
                            ),
                            conditionalPanel(
                              "input.nozzle == 'plates'",
                              numericInput("numberplates", "Number of Plates", 1, min = 1, step = 1)
                            )
                            
               ),
               mainPanel(
                 uiOutput(outputId = "my_ui"),
                 textOutput('text'),
                 hr(style = "border-top: 1px;"),
                 textOutput("text2"),
                 tags$head(tags$style("#text{color: #e8a35c;
                                 font-size: 35px;
                                 font-style: bold;
                                 margin-top:30px;
                                 }")
                 )
               
                 
               )
             )
    ),
    ##closes calc subtab
    
    
    tabPanel("Calculator Instructions", fluid = TRUE,
             tags$div(class = "header", checked = NA,
                      tags$h2("Calculator Instructions:",style = "margin-bottom: 15px; color:steelblue;"),
                      tags$h3("1) Identify which nozzle size is a best fit for your experiment",style = "margin-bottom: 5px;"),
                      tags$h3("2) Insert the number of samples",style = "margin-bottom: 5px;"),
                      tags$h3("3) Insert the total number of cells across all samples",style = "margin-bottom: 5px;"),
                      ),
             
             tags$div(class = "header", checked = NA,
                      fluidRow(
                        column(6,
                      h3("Nozzle Sizes",style = "margin-top:20px;margin-bottom: 5px; color:steelblue;"),
                      h4(strong("70um: "), "Primary cells, small suspension cell lines,", strong("cell size <15um"), style = "margin-bottom: 10px;"),
                      h4(strong("100um: "), "Culture or large or adherent cell lines,", strong("cell size <20um"), style = "margin-bottom: 10px;"),
                      h4(strong("130um: "), "Exceptionally large or fragile cells,", strong("cell size <30um"), style = "margin-bottom: 10px;")
                      ),
                      column(6,
                      h3("Plate Sorting",style = "margin-top: 20px;margin-bottom: 5px; color:steelblue;"),
                      h4("NOTE: The calculated time assumes that the sort is not limited by the frequency of the population being sorted. 
                         For instance, for a rare population, time to sort may exceed 10min per plate.",style = "margin-bottom: 50px;")
                      )
             )
             
    )),
    ###close calculator instruction SUB TAB
    
    
    )),
    
    
    
    
 #####OPEN COLLECTION TUBES MAINTAB###### 
tabPanel(HTML(paste(h3("Collection Tubes Calculator"))),
  tabsetPanel(
    tabPanel("Calculator",
          sidebarLayout(
               
               # Sidebar to demonstrate various slider options ----
              sidebarPanel(
                radioGroupButtons("instrumentcollection", "Instrument",size='normal',width='auto',justified = TRUE,
                                  choices = c("Aria & S6","Sony SH800"),status = "primary",individual = TRUE,
                                  checkIcon = list(
                                    yes = icon("ok",lib = "glyphicon"),
                                    no = icon("remove",lib = "glyphicon"))),
             
                conditionalPanel("input.instrumentcollection == 'Sony SH800'",
                                 awesomeRadio(inputId = "SonyMode",
                                   label = "Sort Mode", 
                                   choices = c("Purity", "Ultra-Purity"),
                                   selected = "Purity",inline = TRUE, checkbox = TRUE)
                ),
                conditionalPanel("input.instrumentcollection == 'Aria & S6'",
                                 awesomeRadio(inputId = "AriaMode",
                                    label = "Sort Mode", 
                                     choices = c("4-Way Purity"),
                                    selected = "4-Way Purity",inline = TRUE, checkbox = TRUE)
                ),
                
                br(),
               radioGroupButtons("tube", "Collection Tube", choiceNames=c("15 ml","5 ml", "1.5ml","96 well"),
                                   choiceValues = c("15ml","5ml","1.5ml","96 well")),
               br(),
               radioGroupButtons("nozzle2", "Nozzle", choiceNames=c("70um", "100um", "130um"),
                               choiceValues = c("70um","100um","130um"),selected=("100um")),
             
               br(),
             conditionalPanel("input.tube == '15ml'",
               sliderInput("obs", "Volume of Collection Media (ml):",
                           min = 0.5, max = 14,value=3,step=0.25)
             ),
             conditionalPanel("input.tube == '5ml'",
                sliderInput("obs2", "Volume of Collection Media (ml):",
                min = 0.1, max = 4.5,value=1,round=2,step=0.1)
             ),
             conditionalPanel("input.tube == '1.5ml'",
                              sliderInput("obs3", "Volume of Collection Media (ml):",
                                          min = 0.1, max = 1.3,value=0.2,round=1,step=0.05)
             ),
             conditionalPanel("input.tube == '96 well'",
                              sliderInput("obs4", "Volume of Collection Media (uL):",
                                          min = 1, max = 250,value=25)
             )
             
             
             
             
             ),
             mainPanel(
            htmlOutput("textcollection"),
             tags$head(tags$style("#textcollection{color: ##42b5eb;
                                 font-size: 30px;
                                 font-style: bold;
                                 margin-bottom:30px;
                                 }")),
             
             plotOutput("example_image"))),
  ),
  
  tabPanel("Background/Instructions", 
      tags$div(class = "header", checked = NA,
          tags$h2("Collection Tube Calculator Background",style = "margin-bottom: 20px; color:steelblue;"),
          tags$h3("Calculations are based on:",style = "margin-bottom: 5px;"),
          tags$h3("(a) The amount of sorting buffer added to collection tube",style = "margin-bottom: 5px;"),
          tags$h3("(b) Volume of a drop which is determined by the optimal frequency for each nozzle",style = "margin-bottom: 5px;")
        ),
        
      h4("Example on the Arias:",style = "margin-bottom: -5px; margin-top: 20px;color:steelblue;text-decoration: underline;"),
          fluidRow(
            column(3,offset =-5,style='padding-bottom:50px',
                   HTML("<center><h4>1 sorted drop (cell) is <b>~1.1nl</b> with the <b>70um nozzle</b></h4></center>"),
                   HTML('<center><img src="70umnozzledrops.png" height="500"></center>')),
            column(3,offset =-5,style='padding-bottom:50px',
                   HTML("<center><h4>1 sorted drop (cell) is <b>~3.2nl</b> with the <b>100um nozzle</b></h4></center>"),
                   HTML('<center><img src="100umnozzledrops.png" height="500"></center>')),
            column(3,offset =-5,style='padding-bottom:50px',
                    HTML("<center><h4>1 sorted drop (cell) is <b>~5.6nl</b> with the <b>130um nozzle</b></h4></center>"),
                    HTML('<center><img src="130umnozzledrops.png" height="500"></center>'))
                        )
                         ),
  
  tags$div(class = "header", checked = NA,
           p("Coded by",tags$a(href = "shiny.rstudio.com/tutorial", "Benjamin Ramcharitar"),"at",
                  tags$a(href = "https://fccf.mskcc.org/", "MSKCC FCCF"))
  )
  
  
                          )
            
            
           )
  )

)













server <- function(input, output) {
  tube15 <- image_read('https://heritageanimalhealth.com/9106-large_default/centrifuge-tube-with-cap-disposable-15ml-100ct.jpg')
  tube5<-image_read('https://m.media-amazon.com/images/I/41KCToD5tbL._SL1000_.jpg')
  tubeEPPEN<-image_read('http://www.clker.com/cliparts/g/u/N/o/v/G/eppendorf-tube-hi.png')

  ###animation function
  output$example_image <- renderPlot({
    s <- input$obs
    s2 <- input$obs2
    s3<-input$obs3
    s4 <- input$obs4
    s4<- s4/2
    
    if(input$obs<7&&input$tube=="15ml"){
      s<-s*12}
    else if(input$obs>=7&&input$tube=="15ml"){
      s<-s*14.5 
    }
    
    else if(input$obs2<3&&input$tube=="5ml") {
      s2<-s2*120
      
    }
    else if(input$obs2>=3&&input$tube=="5ml") {
      s2<-s2*145
      
    }
    req(s)
    
 if(input$tube=="15ml"){
    image_ggplot(tube15)+ 
      annotate("rect", xmin=200, xmax=260, ymin=120+s, ymax=345, alpha=0.2, fill="blue")+
      annotate("polygon", x = c(200, 230, 260), y = c(125, 50, 125),fill="red",alpha=0.2)+
      annotate("rect", xmin=200, xmax=260, ymin=120, ymax=120+s, alpha=0.2, fill="red")
 }
 else if (input$tube=="5ml"){
   image_ggplot(tube5)+
     annotate("rect", xmin=440, xmax=565, ymin=120+s2, ymax=778, alpha=0.1, fill="blue")+
     annotate("polygon", x = c(435, 460,500,540, 550,565), y = c(100,60, 40,55,60, 100),fill="red",alpha=0.2)+
     annotate("rect", xmin=440, xmax=565, ymin=100, ymax=120+s2, alpha=0.2, fill="red")
 }
 else if (input$tube=="1.5ml"){
 
if(s3>0.8){
  s3<-(0.8*150)+s3*10 
} else{    
s3<-s3*150
}
   
   image_ggplot(tubeEPPEN)+
     annotate("polygon", x = c(240-s3/3.3,260,280,300,310+s3/3.3), y = c(50+(s3*2),0,0,20, 50+(s3*2)),fill="red",alpha=0.2)+
     annotate("polygon", x = c(215,215,220,240-s3/4,320+s3/4,335,340,340), y = c(400,300,200,50+s3*2,50+s3*2,200,300, 400),
              fill="blue",alpha=0.2)
   
   
   

 }
else if (input$tube=="96 well"){
  ggplot()+annotate("rect", xmin=0, xmax=1000, ymin=0, ymax=1000, alpha=0.1,fill="white",color="white")+
    annotate("rect", xmin=450, xmax=600, ymin=300, ymax=450,fill="white",color="black",lwd=2)+theme_void()+
    annotate("rect", xmin=450, xmax=600, ymin=300+s4, ymax=425, alpha=0.1, fill="blue")+
    annotate("rect", xmin=450, xmax=600, ymin=300, ymax=300+s4, alpha=0.2, fill="red")
    }
    
    
  })
  
  
  ###text functions
  output$textcollection <- renderText({
    
    
    if (input$nozzle2=="70um"&&input$tube=="15ml"&&input$instrumentcollection=="Aria & S6") {
      collect=(14-input$obs)*983607
      collect<-ceiling(collect)
      collect<-format(collect,big.mark=",",scientific=FALSE)
      HTML(paste("You can collect approximately up to",tags$span(style="color:#476bfc;font-weight:bold", collect, sep = " "),"cells in a 15 ml tube",
                 "with",tags$span(style="color:#ff66b5;font-weight:bold", input$obs,"ml"),"of collection media using the 700um nozzle"))
      
    }
    
    else if (input$nozzle2=="100um"&&input$tube=="15ml"&&input$instrumentcollection=="Aria & S6") {
      collect=(14-input$obs)*298879
      collect<-ceiling(collect)
      collect<-format(collect,big.mark=",",scientific=FALSE)
      HTML(paste("You can collect approximately up to",tags$span(style="color:#476bfc;font-weight:bold", collect, sep = " "),"cells in a 15 ml tube",
                 "with",tags$span(style="color:#ff66b5;font-weight:bold", input$obs,"ml"),"of collection media using the 100um nozzle"))
      
    }
    else if (input$nozzle2=="130um"&&input$tube=="15ml"&&input$instrumentcollection=="Aria & S6"){
      
      collect=(14-input$obs)*138539
      collect<-ceiling(collect)
      collect<-format(collect,big.mark=",",scientific=FALSE)
      HTML(paste("You can collect approximately up to",tags$span(style="color:#476bfc;font-weight:bold", collect, sep = " "),"cells in a 15 ml tube",
                 "with",tags$span(style="color:#ff66b5;font-weight:bold", input$obs,"ml"),"of collection media using the 130um nozzle"))
      
    }
    else if (input$nozzle2=="70um"&&input$tube=="5ml"&&input$instrumentcollection=="Aria & S6"){
      collect=(4.5-input$obs2)*983607
      collect<-ceiling(collect)
      collect<-format(collect,big.mark=",",scientific=FALSE)
      HTML(paste("You can collect approximately up to",tags$span(style="color:#476bfc;font-weight:bold", collect, sep = " "),"cells in a 5 ml tube",
                 "with",tags$span(style="color:#ff66b5;font-weight:bold", input$obs2,"ml"),"of collection media using the 70um nozzle"))
      
      
    }
    
    else if (input$nozzle2=="100um"&&input$tube=="5ml"&&input$instrumentcollection=="Aria & S6"){
      collect=(4.5-input$obs2)*298879
      collect<-ceiling(collect)
      collect<-format(collect,big.mark=",",scientific=FALSE)
      HTML(paste("You can collect approximately up to",tags$span(style="color:#476bfc;font-weight:bold", collect, sep = " "),"cells in a 5 ml tube",
                 "with",tags$span(style="color:#ff66b5;font-weight:bold", input$obs2,"ml"),"of collection media using the 100um nozzle"))
      
      
    }
    else if (input$nozzle2=="130um"&&input$tube=="5ml"&&input$instrumentcollection=="Aria & S6"){
      collect=(4.5-input$obs2)*138539
      collect<-ceiling(collect)
      collect<-format(collect,big.mark=",",scientific=FALSE)
      HTML(paste("You can collect approximately up to",tags$span(style="color:#476bfc;font-weight:bold", collect, sep = " "),"cells in a 5 ml tube",
                 "with",tags$span(style="color:#ff66b5;font-weight:bold", input$obs2,"ml"),"of collection media using the 100um nozzle"))
      
      
    }
    
    else if (input$nozzle2=="70um"&&input$tube=="1.5ml"&&input$instrumentcollection=="Aria & S6"){
      collect=(1.3-input$obs3)*983607
      collect<-ceiling(collect)
      collect<-format(collect,big.mark=",",scientific=FALSE)
      HTML(paste("You can collect approximately up to",tags$span(style="color:#476bfc;font-weight:bold", collect, sep = " "),"cells in a 1.5 ml tube",
                 "with",tags$span(style="color:#ff66b5;font-weight:bold", input$obs3,"ml"),"of collection media using the 70um nozzle"))
      
      
      
    }
    
    else if (input$nozzle2=="100um"&&input$tube=="1.5ml"&&input$instrumentcollection=="Aria & S6"){
      collect=(1.3-input$obs3)*298879
      collect<-ceiling(collect)
      collect<-format(collect,big.mark=",",scientific=FALSE)
      HTML(paste("You can collect approximately up to",tags$span(style="color:#476bfc;font-weight:bold", collect, sep = " "),"cells in a 1.5 ml tube",
                 "with",tags$span(style="color:#ff66b5;font-weight:bold", input$obs3,"ml"),"of collection media using the 100um nozzle"))
      
      
    }
    else if (input$nozzle2=="130um"&&input$tube=="1.5ml"&&input$instrumentcollection=="Aria & S6"){
      collect=(1.3-input$obs3)*138539
      collect<-ceiling(collect)
      collect<-format(collect,big.mark=",",scientific=FALSE)
      HTML(paste("You can collect approximately up to",tags$span(style="color:#476bfc;font-weight:bold", collect, sep = " "),"cells in a 1.5 ml tube",
                 "with",tags$span(style="color:#ff66b5;font-weight:bold", input$obs3,"ml"),"of collection media using the 130um nozzle"))
      
    
      
    }
    
    else if (input$nozzle2=="70um"&&input$tube=="96 well"&&input$instrumentcollection=="Aria & S6"){
      collect=((250-input$obs4)*983607/1000)
      collect<-ceiling(collect)
      collect<-format(collect,big.mark=",",scientific=FALSE)
      
      HTML(paste("You can collect approximately up to",tags$span(style="color:#476bfc;font-weight:bold", collect, sep = " "),"cells in a well",
                 "with",tags$span(style="color:#ff66b5;font-weight:bold", input$obs4,"ul"),"of collection media using the 70um nozzle"))
      
    }
    
    else if (input$nozzle2=="100um"&&input$tube=="96 well"&&input$instrumentcollection=="Aria & S6"){
      collect=((250-input$obs4)*298879/1000)
      collect<-ceiling(collect)
      collect<-format(collect,big.mark=",",scientific=FALSE)
      HTML(paste("You can collect approximately up to",tags$span(style="color:#476bfc;font-weight:bold", collect, sep = " "),"cells in a well",
                 "with",tags$span(style="color:#ff66b5;font-weight:bold", input$obs4,"ul"),"of collection media using the 100um nozzle"))
      
      
    }
    else if (input$nozzle2=="130um"&&input$tube=="96 well"&&input$instrumentcollection=="Aria & S6"){
      collect=((250-input$obs4)*138539/1000)
      collect<-ceiling(collect)
      collect<-format(collect,big.mark=",",scientific=FALSE)
      HTML(paste("You can collect approximately up to",tags$span(style="color:#476bfc;font-weight:bold", collect, sep = " "),"cells in a well",
                 "with",tags$span(style="color:#ff66b5;font-weight:bold", input$obs4,"ul"),"of collection media using the 130um nozzle"))
    
    }
    
    
    
###SONY 15ml    
    
    else if (input$nozzle2=="70um"&&input$SonyMode=="Purity"&&input$tube=="15ml"&&input$instrumentcollection=="Sony SH800"){
      collect=(14-input$obs)*588235
      collect<-ceiling(collect)
      collect<-format(collect,big.mark=",",scientific=FALSE)
      HTML(paste("You can collect approximately up to",tags$span(style="color:#476bfc;font-weight:bold", collect, sep = " "),"cells in a 15 ml tube",
                 "with",tags$span(style="color:#ff66b5;font-weight:bold", input$obs,"ml"),"of collection media using the 70um chip"))
      
    }   
    
    else if (input$nozzle2=="70um"&&input$SonyMode=="Ultra-Purity"&&input$tube=="15ml"&&input$instrumentcollection=="Sony SH800"){
      collect=(14-input$obs)*625000
      collect<-ceiling(collect)
      collect<-format(collect,big.mark=",",scientific=FALSE)
      HTML(paste("You can collect approximately up to",tags$span(style="color:#476bfc;font-weight:bold", collect, sep = " "),"cells in a 15 ml tube",
                 "with",tags$span(style="color:#ff66b5;font-weight:bold", input$obs,"ml"),"of collection media using the 70um chip"))
      
    }
    
    else if (input$nozzle2=="100um"&&input$SonyMode=="Purity"&&input$tube=="15ml"&&input$instrumentcollection=="Sony SH800"){
      collect=(14-input$obs)*353634.57
      collect<-ceiling(collect)
      collect<-format(collect,big.mark=",",scientific=FALSE)
      HTML(paste("You can collect approximately up to",tags$span(style="color:#476bfc;font-weight:bold", collect, sep = " "),"cells in a 15 ml tube",
                 "with",tags$span(style="color:#ff66b5;font-weight:bold", input$obs,"ml"),"of collection media using the 100um chip"))
      
    }   
    
    else if (input$nozzle2=="100um"&&input$SonyMode=="Ultra-Purity"&&input$tube=="15ml"&&input$instrumentcollection=="Sony SH800"){
      collect=(14-input$obs)*357142.85
      collect<-ceiling(collect)
      collect<-format(collect,big.mark=",",scientific=FALSE)
      HTML(paste("You can collect approximately up to",tags$span(style="color:#476bfc;font-weight:bold", collect, sep = " "),"cells in a 15 ml tube",
                 "with",tags$span(style="color:#ff66b5;font-weight:bold", input$obs,"ml"),"of collection media using the 100um chip"))
      
    }
    
    else if (input$nozzle2=="130um"&&input$SonyMode=="Purity"&&input$tube=="15ml"&&input$instrumentcollection=="Sony SH800"){
      collect=(14-input$obs)*65645.5
      collect<-ceiling(collect)
      collect<-format(collect,big.mark=",",scientific=FALSE)
      HTML(paste("You can collect approximately up to",tags$span(style="color:#476bfc;font-weight:bold", collect, sep = " "),"cells in a 15 ml tube",
                 "with",tags$span(style="color:#ff66b5;font-weight:bold", input$obs,"ml"),"of collection media using the 130um chip"))
      
    }   
    
    else if (input$nozzle2=="130um"&&input$SonyMode=="Ultra-Purity"&&input$tube=="15ml"&&input$instrumentcollection=="Sony SH800"){
      collect=(14-input$obs)*43913.1
      collect<-ceiling(collect)
      collect<-format(collect,big.mark=",",scientific=FALSE)
      HTML(paste("You can collect approximately up to",tags$span(style="color:#476bfc;font-weight:bold", collect, sep = " "),"cells in a 15 ml tube",
                 "with",tags$span(style="color:#ff66b5;font-weight:bold", input$obs,"ml"),"of collection media using the 130um chip"))
      
    }
    
###SONY 5ml
    
    
    else if (input$nozzle2=="70um"&&input$SonyMode=="Purity"&&input$tube=="5ml"&&input$instrumentcollection=="Sony SH800"){
      collect=(4.5-input$obs2)*588235
      collect<-ceiling(collect)
      collect<-format(collect,big.mark=",",scientific=FALSE)
      HTML(paste("You can collect approximately up to",tags$span(style="color:#476bfc;font-weight:bold", collect, sep = " "),"cells in a 5 ml tube",
                 "with",tags$span(style="color:#ff66b5;font-weight:bold", input$obs,"ml"),"of collection media using the 70um chip"))
      
    }   
    
    else if (input$nozzle2=="70um"&&input$SonyMode=="Ultra-Purity"&&input$tube=="5ml"&&input$instrumentcollection=="Sony SH800"){
      collect=(4.5-input$obs2)*625000
      collect<-ceiling(collect)
      collect<-format(collect,big.mark=",",scientific=FALSE)
      HTML(paste("You can collect approximately up to",tags$span(style="color:#476bfc;font-weight:bold", collect, sep = " "),"cells in a 5 ml tube",
                 "with",tags$span(style="color:#ff66b5;font-weight:bold", input$obs,"ml"),"of collection media using the 70um chip"))
      
    }
    
    else if (input$nozzle2=="100um"&&input$SonyMode=="Purity"&&input$tube=="5ml"&&input$instrumentcollection=="Sony SH800"){
      collect=(4.5-input$obs2)*353634.57
      collect<-ceiling(collect)
      collect<-format(collect,big.mark=",",scientific=FALSE)
      HTML(paste("You can collect approximately up to",tags$span(style="color:#476bfc;font-weight:bold", collect, sep = " "),"cells in a 5 ml tube",
                 "with",tags$span(style="color:#ff66b5;font-weight:bold", input$obs,"ml"),"of collection media using the 100um chip"))
      
    }   
    
    else if (input$nozzle2=="100um"&&input$SonyMode=="Ultra-Purity"&&input$tube=="5ml"&&input$instrumentcollection=="Sony SH800"){
      collect=(4.5-input$obs2)*357142.85
      collect<-ceiling(collect)
      collect<-format(collect,big.mark=",",scientific=FALSE)
      HTML(paste("You can collect approximately up to",tags$span(style="color:#476bfc;font-weight:bold", collect, sep = " "),"cells in a 5 ml tube",
                 "with",tags$span(style="color:#ff66b5;font-weight:bold", input$obs,"ml"),"of collection media using the 100um chip"))
      
    }
    
    else if (input$nozzle2=="130um"&&input$SonyMode=="Purity"&&input$tube=="5ml"&&input$instrumentcollection=="Sony SH800"){
      collect=(4.5-input$obs2)*65645.5
      collect<-ceiling(collect)
      collect<-format(collect,big.mark=",",scientific=FALSE)
      HTML(paste("You can collect approximately up to",tags$span(style="color:#476bfc;font-weight:bold", collect, sep = " "),"cells in a 5 ml tube",
                 "with",tags$span(style="color:#ff66b5;font-weight:bold", input$obs,"ml"),"of collection media using the 130um chip"))
      
    }   
    
    else if (input$nozzle2=="130um"&&input$SonyMode=="Ultra-Purity"&&input$tube=="5ml"&&input$instrumentcollection=="Sony SH800"){
      collect=(4.5-input$obs2)*43913.1
      collect<-ceiling(collect)
      collect<-format(collect,big.mark=",",scientific=FALSE)
      HTML(paste("You can collect approximately up to",tags$span(style="color:#476bfc;font-weight:bold", collect, sep = " "),"cells in a 5 ml tube",
                 "with",tags$span(style="color:#ff66b5;font-weight:bold", input$obs,"ml"),"of collection media using the 130um chip"))
      
    }
    
    
    
    
###SONY 1.5ML
    
    
    else if (input$nozzle2=="70um"&&input$SonyMode=="Purity"&&input$tube=="1.5ml"&&input$instrumentcollection=="Sony SH800"){
      collect=(1.3-input$obs3)*588235
      collect<-ceiling(collect)
      collect<-format(collect,big.mark=",",scientific=FALSE)
      HTML(paste("You can collect approximately up to",tags$span(style="color:#476bfc;font-weight:bold", collect, sep = " "),"cells in a 1.5 ml tube",
                 "with",tags$span(style="color:#ff66b5;font-weight:bold", input$obs,"ml"),"of collection media using the 70um chip"))
      
    }   
    
    else if (input$nozzle2=="70um"&&input$SonyMode=="Ultra-Purity"&&input$tube=="1.5ml"&&input$instrumentcollection=="Sony SH800"){
      collect=(1.3-input$obs3)*625000
      collect<-ceiling(collect)
      collect<-format(collect,big.mark=",",scientific=FALSE)
      HTML(paste("You can collect approximately up to",tags$span(style="color:#476bfc;font-weight:bold", collect, sep = " "),"cells in a 1.5 ml tube",
                 "with",tags$span(style="color:#ff66b5;font-weight:bold", input$obs,"ml"),"of collection media using the 70um chip"))
      
    }
    
    else if (input$nozzle2=="100um"&&input$SonyMode=="Purity"&&input$tube=="1.5ml"&&input$instrumentcollection=="Sony SH800"){
      collect=(1.3-input$obs3)*353634.57
      collect<-ceiling(collect)
      collect<-format(collect,big.mark=",",scientific=FALSE)
      HTML(paste("You can collect approximately up to",tags$span(style="color:#476bfc;font-weight:bold", collect, sep = " "),"cells in a 1.5 ml tube",
                 "with",tags$span(style="color:#ff66b5;font-weight:bold", input$obs,"ml"),"of collection media using the 100um chip"))
      
    }   
    
    else if (input$nozzle2=="100um"&&input$SonyMode=="Ultra-Purity"&&input$tube=="1.5ml"&&input$instrumentcollection=="Sony SH800"){
      collect=(1.3-input$obs3)*357142.85
      collect<-ceiling(collect)
      collect<-format(collect,big.mark=",",scientific=FALSE)
      HTML(paste("You can collect approximately up to",tags$span(style="color:#476bfc;font-weight:bold", collect, sep = " "),"cells in a 1.5 ml tube",
                 "with",tags$span(style="color:#ff66b5;font-weight:bold", input$obs,"ml"),"of collection media using the 100um chip"))
      
    }
    
    
    else if (input$nozzle2=="130um"&&input$SonyMode=="Purity"&&input$tube=="1.5ml"&&input$instrumentcollection=="Sony SH800"){
      collect=(1.3-input$obs3)*65645.5
      collect<-ceiling(collect)
      collect<-format(collect,big.mark=",",scientific=FALSE)
      HTML(paste("You can collect approximately up to",tags$span(style="color:#476bfc;font-weight:bold", collect, sep = " "),"cells in a 1.5 ml tube",
                 "with",tags$span(style="color:#ff66b5;font-weight:bold", input$obs,"ml"),"of collection media using the 130um chip"))
      
    }   
    
    else if (input$nozzle2=="130um"&&input$SonyMode=="Ultra-Purity"&&input$tube=="1.5ml"&&input$instrumentcollection=="Sony SH800"){
      collect=(1.3-input$obs3)*43913.1
      collect<-ceiling(collect)
      collect<-format(collect,big.mark=",",scientific=FALSE)
      HTML(paste("You can collect approximately up to",tags$span(style="color:#476bfc;font-weight:bold", collect, sep = " "),"cells in a 1.5 ml tube",
                 "with",tags$span(style="color:#ff66b5;font-weight:bold", input$obs,"ml"),"of collection media using the 130um chip"))
      
    }
    
    
    
###SONY 96-well
    
    
    else if (input$nozzle2=="70um"&&input$SonyMode=="Purity"&&input$tube=="96 well"&&input$instrumentcollection=="Sony SH800"){
      collect=((250-input$obs4)*588235/1000)
      collect=(1.3-input$obs3)*588235
      collect<-ceiling(collect)
      collect<-format(collect,big.mark=",",scientific=FALSE)
      HTML(paste("You can collect approximately up to",tags$span(style="color:#476bfc;font-weight:bold", collect, sep = " "),"cells in a 1.5 ml tube",
                 "with",tags$span(style="color:#ff66b5;font-weight:bold", input$obs,"ml"),"of collection media using the 70um chip"))
      
    }   
    
    else if (input$nozzle2=="70um"&&input$SonyMode=="Ultra-Purity"&&input$tube=="96 well"&&input$instrumentcollection=="Sony SH800"){
      collect=((250-input$obs4)*625000/1000)
      collect<-ceiling(collect)
      collect<-format(collect,big.mark=",",scientific=FALSE)
      HTML(paste("You can collect approximately up to",tags$span(style="color:#476bfc;font-weight:bold", collect, sep = " "),"cells in a 1.5 ml tube",
                 "with",tags$span(style="color:#ff66b5;font-weight:bold", input$obs,"ul"),"of collection media using the 70um chip"))
      
    }
    
    else if (input$nozzle2=="100um"&&input$SonyMode=="Purity"&&input$tube=="96 well"&&input$instrumentcollection=="Sony SH800"){
      collect=((250-input$obs4)*353634.57/1000)
   
      collect<-ceiling(collect)
      collect<-format(collect,big.mark=",",scientific=FALSE)
      HTML(paste("You can collect approximately up to",tags$span(style="color:#476bfc;font-weight:bold", collect, sep = " "),"cells in a well",
                 "with",tags$span(style="color:#ff66b5;font-weight:bold", input$obs,"ul"),"of collection media using the 100um chip"))
      
    }   
    
    else if (input$nozzle2=="100um"&&input$SonyMode=="Ultra-Purity"&&input$tube=="96 well"&&input$instrumentcollection=="Sony SH800"){
      collect=((250-input$obs4)*357142.85/1000)
      collect<-ceiling(collect)
      collect<-format(collect,big.mark=",",scientific=FALSE)
      HTML(paste("You can collect approximately up to",tags$span(style="color:#476bfc;font-weight:bold", collect, sep = " "),"cells in a well",
                 "with",tags$span(style="color:#ff66b5;font-weight:bold", input$obs,"ul"),"of collection media using the 100um chip"))
      
    }
    
    
    else if (input$nozzle2=="130um"&&input$SonyMode=="Purity"&&input$tube=="96 well"&&input$instrumentcollection=="Sony SH800"){
      collect=((250-input$obs4)*65645.5/1000)
      
      collect<-ceiling(collect)
      collect<-format(collect,big.mark=",",scientific=FALSE)
      HTML(paste("You can collect approximately up to",tags$span(style="color:#476bfc;font-weight:bold", collect, sep = " "),"cells in a well",
                 "with",tags$span(style="color:#ff66b5;font-weight:bold", input$obs,"ul"),"of collection media using the 130um chip"))
      
    }   
    
    else if (input$nozzle2=="130um"&&input$SonyMode=="Ultra-Purity"&&input$tube=="96 well"&&input$instrumentcollection=="Sony SH800"){
      collect=((250-input$obs4)*43913.1/1000)
      collect<-ceiling(collect)
      collect<-format(collect,big.mark=",",scientific=FALSE)
      HTML(paste("You can collect approximately up to",tags$span(style="color:#476bfc;font-weight:bold", collect, sep = " "),"cells in a well",
                 "with",tags$span(style="color:#ff66b5;font-weight:bold", input$obs,"ul"),"of collection media using the 130um chip"))
      
    }
    

    
    
    
  })
  
  
#################Calculator Calculations Below
###!!!Change log:
###-->3/22/22 removed 15 minutes extra time from 70um and 100um nozzle sorts, kept 15 minutes extra setup time for 130um 
# in case of extra startup/fluidics troubleshooting
  
  output$text <- renderText({
    
  if (input$nozzle=="70um") {
      #nozzle1=(((input$samples*5)+(input$cells*1)+30)/60)
       nozzle1=(((input$samples*5)+(input$cells*1)+30))
       nozzle1<-hms(minutes=nozzle1)
       nozzle1<-str_extract(nozzle1, "[^:]*:[^:]*")
      
      paste("Total Time Needed: ", nozzle1, " (HH:MM)")
      
      
    }
    
    
    
    else if (input$nozzle=="100um") {
      #nozzle2=(((input$samples*5)+(input$cells*3)+30)/60)
      nozzle2=(((input$samples*5)+(input$cells*3)+30))
      nozzle2<-hms(minutes=nozzle2)
      nozzle2<-str_extract(nozzle2, "[^:]*:[^:]*")
      paste("Total Time Needed: ", nozzle2, " (HH:MM)")
      
    }
    
    
    else if (input$nozzle=="130um") {
     #nozzle3=(((input$samples*5)+(input$cells*7.5)+30+15))/60/24
      nozzle3=(((input$samples*5)+(input$cells*7.5)+30+15))
      nozzle3<-hms(minutes=nozzle3)
      nozzle3<-str_extract(nozzle3, "[^:]*:[^:]*")
  
      paste0("Total Time Needed: ", nozzle3," (HH:MM)")
      
      
    }
    
    else if (input$nozzle=="plates") {
      #nozzle3=(((input$samples)+(input$numberplates*5)+30+15)/60)
      
      
      nozzle3=(((input$samples)+(input$numberplates*5)+30+15))
      nozzle3<-hms(minutes=nozzle3)
      nozzle3<-str_extract(nozzle3, "[^:]*:[^:]*")
      paste("Total Time Needed: ", nozzle3," (HH:MM)")
      
      
    }
    
    
    
  })
  
  output$my_ui<-renderUI({
    if(input$instrument=='Aria')
      img(src='aria.png', height = '350px')
    else if(input$instrument=='S6')
      img(src='S6.png', height = '350px')
    
  })
  
  output$text2 <- renderText({
    if (input$nozzle=="plates") {
      HTML('NOTE: The calculated time assumes that the sort is not limited by the frequency of the population being sorted.
           For instance, for a rare population, time to sort may exceed 10min per plate.')
    }
  })
  

}



shinyApp(ui = ui, server = server)
