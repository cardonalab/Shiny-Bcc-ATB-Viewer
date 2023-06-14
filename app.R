library(shiny)
library(ggplot2)
library(bslib)
library(ggvis)
library(readxl)
library(dplyr)
library(reactlog)
library(shinyBS)
library(DT)
library(ggrepel)

fitData <- read_excel("./YOUR_DATA_HERE.xlsx")

# Define UI ----
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "minty", secondary = "#85C7F5"),
  titlePanel(div(HTML("<b><em>B. cenocepacia</em> Antibiotic-Genetic Interaction Viewer</b>"))),
  
  fluidRow(
    column(2,
           wellPanel(
             p("In this app you can explore the genome-wide effect of transposon gene disruptions on antibiotic susceptibility."),
             
             p("This app accesses data from: Hogan et al. 2022. BioArxiv. DOI: 10.1101/2022.11.01.510852"),
             
             p("All points displayed on the plots show significant interactions (p < 0.05), but are not corrected for multiple testing."),
             
             br(),
             
             hr(),
             
             selectInput("dropATB", h5("Select Antibiotic:"), choices = c("AMP", "AZT",
                                                                             "CFD", "CAZ-H",
                                                                             "CAZ-L", "TAZ",
                                                                             "CTT", "MEM",
                                                                             "AVI-H", "AVI-L",
                                                                             "AVI/CAZ",
                                                                             "CYC", "FOS",
                                                                             "BAC", "FR-9",
                                                                             "CHIR", "PF-04",
                                                                             "CHX", "PMB",
                                                                             "ERY", "NOV",
                                                                             "RIF")),

             
             hr(),
             
             sliderInput("slideFit", h5("Select Fitness Score Threshold:"),
                         min = 0, max = 5, value = 0.5, step = 0.1),
             
             p("*Absolute fitness value relative to DMSO control"),
             
             br(),
             
             hr(),
             
             downloadButton("downloadData", "Download All Drugs Dataset .csv",
                            style="color: #fff; background-color: #67D5A2; border-color: #999999")
           )),
      column(10,
           fluidRow(
             column(7,
                    wellPanel(plotOutput("plot1", width =685, height =575, click = "plot1_click")))
             ,
             column(5,
                    wellPanel(h5("Points Near Click"), verbatimTextOutput("click_info")),
                    br(),
                    wellPanel(h5("Antibiotic Abbreviations"),
                        strong("DMSO"), span(" = Dimethyl sulfoxide (vector control)"), br(),
                        strong("AMP"), span(" = Ampicillin ;"), strong("AZT"), span(" = Aztreonam"), br(),
                        strong("CFD"), span(" = Cefiderocol ;"), strong("CAZ-H/L"), span(" = Ceftazidime (High/Low Dose)"), br(),
                        strong("TAZ"), span(" = Tazobactam ;"), strong(" CTT"), span(" = Cefotetan"), br(),
                        strong("MEM"), span(" = Meropenem ;"), strong("AVI-H/L"), span(" = Avibactam (High/Low Dose)"), br(),
                        strong("AVI/CAZ"), span("= Avibactam/Ceftazidime ;"), strong("CYC"), span(" = Cycloserine"), br(),
                        strong("FOS"), span(" = Fosfomycin ;"), strong("BAC"), span(" = Bacitracin"), br(),
                        strong("FR-9"), span(" = FR-900098 ;"), strong("CHIR"), span(" = CHIR-090"), br(),
                        strong("PF-04"), span(" = PF-04753299 ;"), strong("CHX"), span(" = Chlorhexidine"), br(),
                        strong("PMB"), span(" = Polymyxin B ;"), strong("ERY"), span(" = Erythromycin"), br(),
                        strong("NOV"), span(" = Novobiocin ;"), strong("RIF"), span(" = Rifampicin"), br()
                    )
                  )
             
           ),
           br(),
           fluidRow(
             column(6,
                  wellPanel(
                    h5(textOutput("DownTableHeader")),
                    h6("**Click header to sort by variable**"),
                    div(DT::dataTableOutput("DownTable"),  style = "font-size:80%")
                    
                  ))
           ,
           column(6,
                  wellPanel(
                    h5(textOutput("UpTableHeader")),
                    h6("**Click header to sort by variable**"),
                    div(DT::dataTableOutput("UpTable"),  style = "font-size:80%")
                  )
           )
           ),
      )
    )
  )

        
# Define server logic ----
server <- function(input, output) {
  
  selectedData <- reactive({
    fitData %>% filter(Cond2 == input$dropATB)
  })
  
  thresholdData <- reactive({
    fitData %>% filter(Cond2 == input$dropATB) %>% filter(between(FitRelDMSO, -1*input$slideFit,input$slideFit))
  })

  DownTableData <- reactive ({
    fitData %>% filter(Cond2 == input$dropATB) %>% filter(FitRelDMSO < -1*input$slideFit) %>% select(sysName,gene,Fit_C1,Fit_C2,FitRelDMSO,Annotation)
  })
  
  UpTableData <- reactive ({
    fitData %>% filter(Cond2 == input$dropATB) %>% filter(FitRelDMSO > input$slideFit) %>% select(sysName,gene,Fit_C1,Fit_C2,FitRelDMSO,Annotation)
  })
  
  ClickData <- reactive ({
    fitData %>% filter(Cond2 == input$dropATB) %>% select(sysName,gene,Fit_C1,Fit_C2,FitRelDMSO,Annotation) #%>% rename(LocusTag = sysName, FitDMSO = Fit_C1, FitATB = Fit_C2, FitDiff = FitRelDMSO, Annotation = Annotation)
  })
  
  #nrows in uptable data
  UpTableRows <- renderText({
    paste0(nrow(UpTableData()), " Genes")
  })

  #nrows in downtable data
  DownTableRows <- renderText({
    paste0(nrow(DownTableData()), " Genes")
  })    

  #define plot to use selected data
  output$plot1<-renderPlot({
    
    ggplot(selectedData(), aes(x = Fit_C1, y = Fit_C2))+
      geom_vline(xintercept = 0, size = 0.5, colour = "grey 70")+
      geom_hline(yintercept = 0, size = 0.5, colour = "grey 70")+
      geom_point(alpha = 0.8, aes(fill = MoreLessATB, size = Abs_FitRelDMSO), shape = 21)+
      scale_fill_manual(values=c("goldenrod1","purple3"))+
      geom_point(thresholdData(), mapping=aes(size = Abs_FitRelDMSO, x = Fit_C1, y = Fit_C2), colour = "grey10", shape = 21, alpha = 1, fill = "grey60")+
      scale_size_continuous(breaks = c(1,2,3,4,5), limits = c(0,6), range = c(0,8))+
      theme_bw()+
      xlab("Gene Fitness (DMSO Control)")+
      ylab("Gene Fitness (ATB)")+
      theme(axis.title = element_text(size = 22), axis.text = element_text(size = 18, colour = "black"), 
            panel.grid.minor = element_blank(),  
            strip.background = element_blank(),
            strip.text.x = element_blank(), legend.key.size = unit(0.75, 'cm'), legend.title = element_text(size=16, face = "bold"),
            legend.text = element_text(size=17))+
      guides(fill = guide_legend(override.aes = list(size = 6)))+
      labs(fill = "Less/More Fit\n     in ATB", size = "Fitness Relative\n   to DMSO")+
      scale_x_continuous(limits = c(-12, 10))+
      scale_y_continuous(limits = c(-12, 10))+
      coord_cartesian(ylim=c(-6.5,3.5), xlim=c(-6.5,3.5))+
      
      annotate("segment", x = -7, xend = 4, y = -7, yend = 4, colour = "grey60", linetype = "dashed", size = 1)+
      annotate("text", label=input$dropATB, x= -5.5, y =3.25, size = 15, fontface = 2)+
      
      annotate("text", label=UpTableRows(), x= -5.5, y =2.5, size = 8, fontface = 2, colour = "purple3")+
      annotate("text", label=DownTableRows(), x= -5.5, y =2, size = 8, fontface = 2, colour = "goldenrod1")+
      
      #negative segment
      annotate("segment", colour = "goldenrod1", linetype = "solid", size = 1, 
               x = -7+input$slideFit/2, xend = 4+input$slideFit/2, y = -7-input$slideFit/2, yend = 4-input$slideFit/2)+
      
      #positive segment
      annotate("segment", colour = "purple3", linetype = "solid", size = 1, 
               x = -7-input$slideFit/2, xend = 4-input$slideFit/2, y = -7+input$slideFit/2, yend = 4+input$slideFit/2)
    
    })
  
  #click info
  output$click_info <- renderPrint({
    nearPoints(ClickData(), input$plot1_click, addDist = FALSE, threshold = 10)
  })
  
  #more susceptible data table header
  output$DownTableHeader <- renderText({ 
    paste("Mutants (Genes) With Lower Fitness in", input$dropATB)
  })
  
  #actual more susceptible data table
  output$DownTable = renderDT({
    datatable(DownTableData(), colnames = c("Locus Tag", "Gene","Fitness (DMSO)","Fitness (ATB)", "Fitness (DMSO vs ATB)", "Gene Function"),
              options = list(
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#EFD44C', 'color': 'white'});",
                  "}")))
  })
  
  #less susceptible data table header
  output$UpTableHeader <- renderText({ 
    paste("Mutants (Genes) With Higher Fitness in", input$dropATB)
  })
  
  #actual less susceptible data table
  output$UpTable = renderDT({
    datatable(UpTableData(), colnames = c("Locus Tag", "Gene","Fitness (DMSO)","Fitness (ATB)", "Fitness (DMSO vs ATB)", "Gene Function"),
              options = list(
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#B255DB', 'color': 'white'});",
                  "}")))
  })
  
  #output suppl data file (not only selected)  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("SupplData1_AllATBs_fitness", Sys.Date(),".csv")
    },
    content = function(file) {
      write.csv(fitData, file)
    })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
