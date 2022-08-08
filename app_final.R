#Name: Indicators for MERRR Portfolio
#Author: Grant Aitken
#Created: 04/02/2021
#Type: Data visualisation
#Written on: RStudio
#Written for: RStudio version 1.3.159 
#Output: Shiny application

#Descripion: The MERRR page is a multi-tab shiny app that is intended to give a high 
#level overview of all of the indicators that are associated with the MERRR portfolio.
#The dashboard will be evolved over time and additional indicators (defined within the framework or new) will be added in due course.
#This will be grouped appropriately to split across the five MERRR chapters,
#and in the first instance will be looking to provide trends.


#On the tab which contains the interactive dropdown there will be three options to choose from:
# - 1) RRR Chapter
# - 2) RRR Outcome
# = 3) RRR Indicator

#There will be five other tabs which contain metadata embedded under each relevant heading:
# - 1) Prevention & Early Intervention
# - 2) Recovery Oriented Systems of Care (ROSC)
# - 3) A Public Health Approach to Justice 
# - 4) Children, Young People & Families
# - 5) Health & Social Harms

#Where more than one year is available data is shown by line chart
#Where only one year is available data is shown by bar chart
#Where applicable, data will also be shown for Age, Sex and Deprivation. 
#On some occasions splits will be slightly different (e.g. drug type)
#Some headers will be the same but allow the user to alternate between different measures (e.g. rates, percentages, numbers)

library(shiny)
library(dplyr)
library(plotly)
library(shinyWidgets)
library(forcats)
library(DT)
library(stringr)
library(shinyBS)
library(bsplus)
library(ggplot2)
library(gridExtra)
library(readxl)
library(readr)
library(shinymanager)
library(rsconnect)

##############################################.
############## Data Input ----
##############################################.

#Only use below if need to go into Excel file to make changes before publication
MERRR_ <- read_excel("data/MERRRFinal.xlsx")

saveRDS(MERRR_, "data/MERRRFinal.rds")

MERRR1 <- readRDS("data/MERRRFinal.rds")

#SG reviewed dataset and suggested removal of certain SG sourced indicators, largely for data validity reasons. 
#These are listed as 'No' so filter to include 'Yes' values only
#Once happy with validity of data in future change to Yes for relevant indicators

MERRR <- subset(MERRR1, Valid=="Yes")

#Create a shortened version removing superfluous columns for data table display later

MERRR2 <- MERRR %>%select(RRR_Conc,
                          Indicator_Text,
                          Definition,
                          Inequalities_Type,
                          Period,
                          Measure,
                          Number)


#Create credentials for password protection. Can be removed for open access publication.

credentials_df <- data.frame(
  user = c("PHS2021"), # mandatory
  password = c("MERRR_2%21"), # mandatory
  stringsAsFactors = FALSE)

saveRDS(credentials_df, "shiny_app/admin/credentials.rds")


#Beginning of whole script


{

  

#First set credentials for password protection. Can be removed for open access publication.
credentials <- readRDS("shiny_app/admin/credentials.rds")
  
  
  
#Beginning of Server script
  
##############################################.
############## Server ----
##############################################.  
  
server  <-  function(input, output, session)
{

#Shinymanager Authorisation. Can be removed for open access publication.
  
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials))
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  #Graph information text output
  output$text_output<-renderUI({ 
    p(HTML("Show/hide table - show data in a table below the chart."),
      
      p(HTML("At the top-right corner of the chart, you will see a toolbar with four buttons:"),
        br(),
        tags$ul(
          tags$li(
            icon("camera"),
            tags$b("Download plot as a png"),
            " - click this button to save the graph as an image
            (please note that Internet Explorer does not support this
            function)."
          ),
          tags$li(
            icon("search"),
            tags$b("Zoom"),
            " - zoom into the graph by clicking this button and then
            clicking and dragging your mouse over the area of the
            graph you are interested in."
          ),
          tags$li(
            icon("move", lib = "glyphicon"),
            tags$b("Pan"),
            " - adjust the axes of the graph by clicking this button
            and then clicking and moving your mouse in any direction
            you want."
          ),
          tags$li(
            icon("home"),
            tags$b("Reset axes"),
            " - click this button to return the axes to their
            default range."
          )
        ),
        HTML("Categories can be shown/hidden by clicking on labels
             in the legend to the right of each chart.")
      ))
  })
  
  
  
  #Now create reactive filters which allow selection of RRR Chapter, then RRR Outcome and finally RRR Indicator
  
  output$chapter_ui <- renderUI({
    
    #Selection for RRR Outcome depends on what RRR chapter has been selected
    outcomes_summary <- sort(MERRR$RRR_Outcome_Conc[MERRR$RRR_Chapter == input$RRR_Chapter])
    
    selectizeInput("RRR_Outcome_Conc", 
                   label = shiny::HTML("Please scroll to select a RRR Outcome <br/> <span style='font-weight: 
                                       400'>(or start typing text to search)</span></p>"), 
                   choices = outcomes_summary, 
                   selected = "",
                   options = list(size=3, 
                                  `live-search`=TRUE))
    
  })
  
  
  output$indicator_ui <- renderUI({
    
    #Selection for RRR indicator depends on what RRR Outcome has been selected
    indicator_summary <- sort(MERRR$RRR_Conc[MERRR$RRR_Outcome_Conc == input$RRR_Outcome_Conc])
    
    selectizeInput("RRR_Conc", 
                   label = shiny::HTML("Please scroll to select a RRR Indicator <br/> 
                                       <span style='font-weight: 400'>(or start typing text to search)</span></p>"),  
                   choices = indicator_summary, 
                   selected = "",
                   options = list(size=3, 
                                  `live-search`=TRUE))
    
  })
  
  
  #Now plot the graph based on the user input.
  #First we create a subset of the data based on user input selected above
  
  MERRR_1_new <- reactive({MERRR[MERRR$RRR_Conc == input$RRR_Conc,]})
  
  #Now to plot the actual graphs, with labels
  output$prevention_plot <- renderPlotly({
    #first the tooltip label
    #To sort out issues with decimal places, only display number if user selects variable with number only (0 d.p). 
    #Otherwise show definition (1 d.p) and number.
    
    if (MERRR_1_new()$Definition == "Number") { 

        tooltip_MERRR1 <- paste0(
      MERRR_1_new()$Inequalities_Type,
      "<br>","Year: ", MERRR_1_new()$Period,
      "<br>","Number: ", formatC(MERRR_1_new()$Number, big.mark = ",",digits = 0,format = 'f'),
      "<br>","Data Source: ", MERRR_1_new()$Data_Source
    )
      
    }
    
    else {
    
      tooltip_MERRR1 <- paste0(
        MERRR_1_new()$Inequalities_Type,
        "<br>","Year: ", MERRR_1_new()$Period,
        "<br>", paste0(unique(MERRR_1_new()$Definition)),": ",formatC(MERRR_1_new()$Measure, big.mark = ",",digits = 1,format = 'f'),
        "<br>","Number: ", formatC(MERRR_1_new()$Number, big.mark = ",",digits = 0,format = 'f'),
        "<br>","Data Source: ", MERRR_1_new()$Data_Source
      )
    }  
        
    #Now for colours and shapes to go into graph (adhering to accessibility guidance)
    #Have a maximum of nine lines for chart. If some indicators have more than nine lines then split indicator appropriately
    
    trend_palette <- c("#000000", "#3f3685", "#d3ceda", "#0078d4", "#c73918", "#83bb26",
                       "#1e7f84", "#9b4393", "#948da3")
    
    trend_scale <- c(setNames(trend_palette, unique(MERRR_1_new()$Inequalities_Type)[1:9]))
    trend_col <- trend_scale[1:9]    
    
    symbols_palette <-  c('circle', 'diamond', 'cross', 'triangle-up', 'square', 'diamond-open',
                          'square-open','triangle-up-open', 'triangle-down')
    symbols_scale <- c(setNames(symbols_palette, unique(MERRR_1_new()$Inequalities_Type)[1:9]))
    symbols_trend <- symbols_scale[1:9]
    
    #If producing a bar chart for indicators order from largest to smallest value
    Measure_sort <- factor(MERRR_1_new()$Inequalities_Type, levels = 
                             unique(MERRR_1_new()$Inequalities_Type)[order(MERRR_1_new()$Measure, decreasing = TRUE)])
    
    font_plots <- list(family = '"Helvetica Neue", Helvetica, Arial, sans-serif')
    
    #Create the main body of the chart.
    
    #First plot a trend chart if we have more than one year available
    if (MERRR_1_new()$Trend == "Yes") { 
      
      plot_ly(
        data = MERRR_1_new(),
        #plot
        x = ~  Period,
        y = ~  Measure,
        color = ~  Inequalities_Type,
        colors = trend_col,
        symbol = ~  Inequalities_Type,
        symbols = symbols_trend,
        #tooltip
        text = tooltip_MERRR1,
        hoverinfo = "text",
        #type
        type = 'scatter',
        mode = 'lines+markers',
        marker = list(size = 8),
        height = 650,
        width = 1000
      ) %>%
        
        #add in title to the chart
        layout(title = list(
          text=
            paste0(  
              MERRR_1_new()$Indicator_Text, " (", MERRR_1_new()$Text, ")" 
            ),font = list(size = 18)),
          annotations = list(),
          margin = list(l = 90, r = 60, b = 30, t = 90),           
          yaxis = list(title=paste0(c(rep("&nbsp;", 45),
                                      "<br>",
                                      unique(MERRR_1_new()$Definition),
                                      rep("&nbsp;", 20),
                                      rep("\n&nbsp;", 3)),
                                    collapse = ""),
                       rangemode="tozero", fixedrange=TRUE, size = 4, 
                       tickfont = list(size=14), titlefont = list(size=14)),
          xaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                        "<br>",
                                        "<br>",
                                        "Year",
                                        rep("&nbsp;", 20),
                                        rep("\n&nbsp;", 3)),
                                      collapse = ""),
                       tickfont = list(size=14), titlefont = list(size=14), 
                       showline = TRUE, tickangle = 0, fixedrange=TRUE),
          font=list(family = '"Helvetica Neue", Helvetica, Arial, sans-serif'),
          showlegend = TRUE,
          legend = list(x = 100, xanchor="right", y = 0.8, yanchor="top",
                        bgcolor = 'rgba(255, 255, 255, 0)',
                        bordercolor = 'rgba(255, 255, 255, 0)')) %>%
        
        #Remove unnecessary buttons from the modebar.
        config(displayModeBar = TRUE, 
               modeBarButtonsToRemove = list('select2d', 'lasso2d', 'zoomIn2d',
                                             'zoomOut2d', 'autoScale2d',
                                             'toggleSpikelines',
                                             'hoverCompareCartesian',
                                             'hoverClosestCartesian'),
               displaylogo = F,  editable = F)
      
    }
    
    
    #If a trend is not available for indicator plot a bar chart
    else if (MERRR_1_new()$Trend == "No") {
      
      plot_ly(
        data = MERRR_1_new(),
        #plot
        x = ~  Measure_sort,
        y = ~  Measure,
        #tooltip
        text = tooltip_MERRR1,
        hoverinfo = "text",
        #type
        type = 'bar',
        marker = list(color = "#0078d4"),
        height = 650,
        width = 1000
      ) %>%
        
        #add in title to the chart
        layout(title = list(
          text=
            paste0(  
              MERRR_1_new()$Indicator_Text, " (", MERRR_1_new()$Text, ")" 
            ),font = list(size = 18)),
          annotations = list(),
          margin = list(l = 90, r = 60, b = 30, t = 90),           
          yaxis = list(title=paste0(c(rep("&nbsp;", 45),
                                      "<br>",
                                      unique(MERRR_1_new()$Definition),
                                      rep("&nbsp;", 20),
                                      rep("\n&nbsp;", 3)),
                                    collapse = ""),
                       rangemode="tozero", fixedrange=TRUE, size = 4,
                       tickfont = list(size=14), titlefont = list(size=14)),
          xaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                        "<br>",
                                        rep("&nbsp;", 20),
                                        rep("\n&nbsp;", 3)),
                                      collapse = ""),
                       tickfont = list(size=14), titlefont = list(size=14), 
                       showline = TRUE, tickangle = 270, fixedrange=TRUE),
          font=font_plots,
          showlegend = FALSE,
          legend = list(bgcolor = 'rgba(255, 255, 255, 0)',
                        bordercolor = 'rgba(255, 255, 255, 0)')) %>%
        
        #Remove unnecessary buttons from the modebar.
        
        config(displayModeBar = TRUE, 
               modeBarButtonsToRemove = list('select2d', 'lasso2d', 'zoomIn2d',
                                             'zoomOut2d', 'autoScale2d',
                                             'toggleSpikelines',
                                             'hoverCompareCartesian',
                                             'hoverClosestCartesian'),
               displaylogo = F,  editable = F)
      
    }})
  
  
  #Allow for data table to be downloaded
  output$MERRR_data <- downloadHandler(    filename = 'MERRR_datafile.csv',
                                           content = function(filename) {write.csv(MERRR_1_new(), filename, row.names=FALSE)})
  
  
  #Allow for data table to be viewed within UI page
  #Create reactivity and select data from shortened dataset which has removed superfluous columns
  MERRR_1_new2 <- reactive({MERRR2[MERRR2$RRR_Conc == input$RRR_Conc,]})
  
  output$table_trend <- renderDataTable({
    datatable(MERRR_1_new2(),
              style = 'bootstrap',
              class = 'table-bordered table-condensed',
              rownames = FALSE,
              options = list(pageLength = 10,
                             dom = 'tip'),              
              colnames = c("Indicator selected", "Indicator Text", "Definition of Measure", "Group being observed", "Year", "Measure","Number")
    )         %>% 
      formatRound(columns = 6,digits = 1)
  })
  
  ##Now for text output under plot associated with indicator
  output$Text1 <- renderText(unique({MERRR_1_new()$Text1}))
  output$Text2 <- renderText(unique({MERRR_1_new()$Text2}))
  output$Text3 <- renderText(unique({MERRR_1_new()$Text3}))
  
  #Now for text confirming embargoed until 25th May. Can be removed for open access publication.
  output$embargotext <- renderText({"** Official Statistics; please treat as restricted until publication. Embargoed until 09:30 Tuesday 25th May 2021. **"})
  #Now for text confirming overview text will likely change, depending on MERRR report feedback. Can be removed for open access publication.
  output$overviewtext <- renderText({"** This section is under review and we expect these text statements to change in line with feedback on the MERRR Report. **"})
  
}
#End of Server script
  
  
  

#Beginning of UI script

##############################################.
############## User Interface ----
##############################################.
#Use secure_app function for password protection. Can be removed for open access publication. 
  
  ui <- secure_app(navbarPage(windowTitle = "Monitoring and Evaluating Rights, Respect & Recovery (MERRR)",
                 title = div(tags$a(img(src="phs-logo.png", width=120, alt = "Public Health Scotland logo"), 
                                    href= "https://www.publichealthscotland.scot/",
                                    target = "_blank"), style = "position: relative; top: -10px;"), 
                 
                 header = tags$head(
                   includeCSS("www/styles.css"), # CSS styles
                   HTML("<html lang='en'>"),
                   tags$link(rel="shortcut icon", href="favicon_phs.ico")
                 ),
                 
                 
                 
                 #Landing page will provide brief background about MERRR, change in drugs policy, 
                 #link to preliminary report and emphasising how the portal will change over time                           
                 tabPanel(title = "Home", icon = icon("home"), 
                          h1("Monitoring and Evaluating Rights, Respect & Recovery (MERRR)"),
                          h2("Welcome to the MERRR Portal"), 
                          #embargoed statement. Remove for open access publication on 25th May
                          h3(span(textOutput("embargotext"), style="color:red")),
                          p(
                            "Rights, respect and recovery: Scotland's strategy to improve health by preventing
                              and reducing alcohol and drug use, harm and related deaths (subsequently referred to as ",
                            tags$a(href="http://www.gov.scot/publications/rights-respect-recovery/",
                              "Rights, Respect and Recovery", target="_blank"), 
                              "(RRR) was published by the Scottish Government in 2018.
                              During the course of the development of the strategy, the Scottish Government invited
                              NHS Health Scotland (now Public Health Scotland) to develop a monitoring and evaluation framework for Rights, Respect and
                              Recovery (subsequently referred to as the ",
                              tags$a(href="http://healthscotland.scot/publications/monitoring-and-evaluation-framework-for-rights-respect-and-recovery/",
                                   "MERRR Framework)", target="_blank"),
                            "to assess the implementation, progress and outcomes of the strategy. "
                          ),
                          
                          p(
                            "This MERRR Framework detailed a set of indicators (subsequently referred to as the indicator
                              set) aligned to outcomes featured in a ",
                            tags$a(href="http://www.healthscotland.scot/media/3039/theory-of-change-model-for-the-merrr-framework.xlsx",
                                   "theory of change", target="_blank"),
                            " (TOC). These indicators are a key component of the MERRR Framework and will be displayed here to
                              provide insight into the implementation and impact of RRR. "
                          ),
                          

                          p(
                            "This MERRR Portal was created to allow users to interactively explore the various different indicators, and their associated inequalities, produced by the MERRR Portfolio.
                              The indicators contained within this portal are to be viewed in combination with the",            
                            tags$a(href="https://publichealthscotland.scot/downloads/monitoring-and-evaluating-rights-respect-and-recovery-merrr/",
                                   "MERRR Report.", target="_blank"),
                            "This report provides a summary of the indicators by relevant MERRR section and for all of the MERRR sections together."
                          ),
                          
                          
                          p(
                            "As of May 2021, we report on data for 87 of the 110 indicators defined within the MERRR Framework. We were unable to on report data for the remaining 23 indicators as
                             this information is not possible to capture, information is not available yet, information is available but not 
                             reported on here due to data quality issues or information is available but was not deemed suitable for publication."
                          ),
                          
                          
                          p(
                            "This portal is split into multiple sections. The section after this home page displays an interactive dashboard which supplements the MERRR Report. 
                             The next five sections of this portal group these indicators by their relevant RRR Chapter and RRR Outcome, 
                             as defined in the MERRR Framework, and provide additional supplementary metadata for these chapters. The chapters which make up MERRR Portfolio are as follows:"),
                          tags$ul(
                            tags$li("Chapter 1 - Prevention & Early Intervention"),
                            tags$li("Chapter 2 - Recovery Oriented Systems of Care (ROSC)"),
                            tags$li("Chapter 3 - A Public Health Approach to Justice"),
                            tags$li("Chapter 4 - Children, Young People & Families"),
                            tags$li("Chapter 5 - Health & Social Harms")
                          ), 
                          
                          p(
                            "To navigate to each of these sections, please select a relevant tab from the top of this page."
                          ),
                          
                          p(
                            "We aim to update this portal frequently with the most up-to-date information as it becomes available. As a result, the interpretation, narrative and metadata provided within the MERRR Portal
                             are subject to change. Over time, additional indicators defined in the MERRR Framework, where we have not been able to obtain information from thus far, 
                             may be incorporated into this portal. New indicators, not defined as part of the MERRR Framework, may arise and may also be incorporated into this portal.",
                          ),
                          
                          p(
                            "If you experience any problems using this dashboard or have further questions relating to the data please contact our Evaluation team mailbox:",
                            tags$a(href="mailto:phs.evaluationteam@phs.scot",
                                   "phs.evaluationteam@phs.scot.", target="_blank")
                          ),
                          
                          tags$a(href = '#top',  
                                 icon("circle-arrow-up", lib= "glyphicon"),"Back to top of page")
                          ),
                 
                 
                 
                 
                 #Next create a new tab which provides the data visualisation, data table and data download for all indicators.
                 tabPanel("Interactive Dashboard", icon = icon("area-chart"),       
                          
                          h2("Interactive Dashboard for MERRR Portfolio"), 
                          
                          p(
                            "An interactive chart is provided here which allows the user to select an indicator of interest. For details on how to select an indicator, 
                             how the chart functions and how to view or download data associated with the chart, please select one of the Data Selection, Chart Functions and Table Functions titles 
                             below to see additional information."
                          ),
                          
                          wellPanel(column(12,
                                           bsCollapse(id = "collapse_commentary", open = "Panel 1", #PanelSet id
                                                      bsCollapsePanel("Data Selection",
                                                                      tagList(
                                                                        p("The charts can be modified using the drop down boxes. 
                                                                         These indicators are ordered by RRR chapter and information for each indicator is 
                                                                         displayed by selecting (from left to right) the following tabs:",
                                                                          tags$ul(
                                                                            tags$li(tags$b("Please select a RRR Chapter:"), "Chapter associated with the RRR Portfolio;"),
                                                                            tags$li(tags$b("Please scroll to select a RRR Outcome:"), "Outcome associated with the selected RRR Chapter; and,"),
                                                                            tags$li(tags$b("Please scroll to select a RRR Indicator:"), "Indicator associated with the selected RRR Outcome and grouping associated with RRR Indicator (if applicable).")
                                                                          ),
                                                                          p("Click on the drop down boxes to bring up a full list of Chapters, Outcomes or Indicators.
                                                                           The information provided in the drop down box can be filtered by searching for relevant text. 
                                                                           To do this, first click on the drop down box, select backspace 
                                                                           and start typing text.")))),
                                                      bsCollapsePanel("Chart Functions",
                                                                      tagList(
                                                                        p("At the top-right corner of the chart, you will see a ",
                                                                          icon("camera"), "icon: use this to save an image of the chart ",
                                                                          HTML(paste0("(",tags$b("not"))), 
                                                                          tags$b("available in Internet "),
                                                                          HTML(paste0(tags$b("Explorer"),").")),
                                                                          br(),br(),
                                                                          "Indicator groupings can be hidden (and shown again) by clicking on labels in the legend to the right of the chart.",
                                                                          br(),br(),
                                                                          "Hovering the mouse over any data point will bring up a tooltip box which displays:",
                                                                          tags$ul(
                                                                            tags$li("The group of interest being monitored (normally Scotland but may also be broken down by age, gender or deprivation);"),
                                                                            tags$li("Year of interest;"),
                                                                            tags$li("Data measure (normally a rate or percentage);"),
                                                                            tags$li("Number associated with indicator (if applicable);"),
                                                                            tags$li("Data source (where information for the indicator was obtained).")
                                                                          )))),
                                                      bsCollapsePanel("Table Functions",
                                                                      tagList(
                                                                        p("To view the data please select the 'Show/hide table' button at the bottom of the chart.
                                                                         This will display a maximum of 10 entries. To view more entries select the 'Next' button under this table 
                                                                         or select a number to navigate to a page of entries.
                                                                         To download and view all of the data for a selected indicator into an Excel CSV file:",
                                                                          tags$ul(
                                                                            tags$li(
                                                                              icon("download", lib = "glyphicon"),
                                                                              tags$b("Download data"),
                                                                              " - click the download icon under the chart to download the data in this format."
                                                                            )))))
                                           ))),
                          br(),
                          br(),
                          
                          
                          #Insert the reactive filters. 
                          #As outcome is dependent on chapter, and indicator is dependent on outcome,
                          #these dropdowns have to to be set up in the server as a 
                          #reactive object and then placed into the UI. 
                          
                          #Default to select first indicator of first outcome of first chapter (prevention)
                          column(4,
                                 selectizeInput(
                                   inputId = "RRR_Chapter",
                                   label = "Please select a RRR Chapter",
                                   choices= c("1) Prevention & Early Intervention", "2) Recovery Oriented Systems of Care", 
                                              "3) A Public Health Approach to Justice", 
                                              "4) Children, Young People & Families", "5) Health & Social Harms"),
                                   selected = "1) Prevention & Early Intervention"
                                 )
                          ),
                          
                          column(4,
                                 uiOutput("chapter_ui")
                          ),
                          
                          column(4,
                                 uiOutput("indicator_ui") 
                          ),   
                          
                          
                          #Now plot the graph, data table and data downloads
                          #First plot
                          mainPanel(
                            width = 12,
                            plotlyOutput("prevention_plot",                                          
                                         width = "95%",
                                         height = "95%"),
                            
                            #Interactive text which summarisies the main points for the indicator selected
                            p(
                              "Main points:",
                              tags$ul(
                                tags$li(textOutput("Text1")),
                                tags$li(textOutput("Text2")),
                                tags$li(textOutput("Text3"))
                              )),
                            
                            
                            #Now data table
                            HTML("<button data-toggle = 'collapse' href = '#trend' 
                                  class='btn-secondary'>
                                  <strong>Show/hide table</strong></button>"),
                            
                            
                            HTML("<div id = 'trend' class = 'collapse'> "),
                            dataTableOutput("table_trend",
                                            width = "95%"),
                            HTML("</div>"),
                            
                            #finally data download button
                            p(
                              "The full data for this indicator can be downloaded here:    ", 
                              #still have issue with this button: downloads csv but puts all of the information into one cell....
                              downloadButton("MERRR_data", 'Download data', class = "down")
                            ),
                            
                            tags$a(href = '#top',  
                                   icon("circle-arrow-up", lib= "glyphicon"),"Back to top of page")
                            )
                          
                          ),
                 
                 
                 
                 #Now start to split dashboard into tabs relevant to each RRR Chapter
                 #First create a tab in the dashboard which will provide information about chapter 1 
                 tabPanel("1) Prevention & Early Intervention", icon = icon("hand-paper"),
                          
                          h2("Chapter 1 - Prevention and Early Intervention"), 
                          
                          p(
                            "The overall outcome for this part of the strategy is for fewer people to develop problem alcohol and drug use. 
                               The original TOC model identifies interim outcomes to achieve this overall outcome. These are:",
                            tags$ul(
                              tags$li("Reducing inequalities experienced by people who are at risk of developing problems with alcohol and drugs."),
                              tags$li("Ensuring education provision for children and young people is aligned with evidence and
                                       best practices to improve young people's capacity to make informed choices."),
                              tags$li("Increasing the number of people who are at risk of alcohol or drug problems being linked to
                                       positive environments and opportunities to increase individual and community wellbeing,
                                       resilience, and social connectedness.")),
                               "Please select one of the indicator titles below for information on the indicator data source, caveats and next update."
                          ),
                          
                          hr(style = "border-top: 2px solid #000000;"),
                          
                          #Indicators for outcome a
                          h3("Outcome a) Reduce inequalities experienced by people who are at risk of developing problems with alcohol and drugs "), 
                          
                          wellPanel(column(12,
                                           bsCollapse(id = "prev_outcome_a", open = "Panel 1", #PanelSet id
                                                      bsCollapsePanel("a) Rating of neighbourhood by Scottish Index of Multiple Deprivation (SIMD) – gap between first and fifth quintile.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the Scottish Household Survey (SHS). 
                                                                          From 2012 the three Scottish Government interviewer-led population surveys have coordinated sample designs. 
                                                                          The sample is disproportionately stratified by Local Authority. Samples of the general population exclude prisons, 
                                                                          hospitals and military bases. The Royal Mail's small user Postcode Address File (PAF) 
                                                                          is used as the sample frame for the address selection."),
                                                                        p("The social interview is carried out using Computer Aided Personal Interviewing (CAPI), and is in two parts; 
                                                                          Household and Random Adult. There is a follow-up component comprising of a 'Physical Survey' of the dwelling, 
                                                                          conducted by a professional surveyor through a visual inspection of the dwelling. The weighting procedures 
                                                                          incorporate a selection weighting stage to address the unequal selection probabilities and calibration weighting 
                                                                          to correct for non-bias. Calibration weighting derives weights such that the weighted survey totals match known population totals."),
                                                                        p("Percentages reported here are grouped by if the respondent rated their neighbourhood as a ‘Very good’ or ‘Very/fairly good’ place to 
                                                                          live and are available from 2009 to 2019. As well as showing national figures, data are available for SIMD quintile and the difference 
                                                                          shown was for the 20% most and 20% least deprived areas in Scotland."),
                                                                        p("The next report will contain 2020 figures and is scheduled to be published in October 2021."),
                                                                        p(
                                                                          tags$a(href="https://shs.theapsgroup.scot/2019/",
                                                                                 "More information on SHS can be found here", target="_blank"))
                                                                        )),
                                                      bsCollapsePanel("b) Child poverty rates in local authority area.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the Her Majesty’s Revenue and Customs (HMRC). 
                                                                          They provide information on the number and proportion of children living in Relative and 
                                                                          Absolute low income Before Housing Costs by local area across the United Kingdom."),
                                                                        p("These statistics draw data from the database “RAPID” (Registration and Population Interaction Database) 
                                                                          which provides a single coherent view of citizens’ interactions with Department of Work and Pensions (DWP) 
                                                                          and HMRC within a tax year for the UK. RAPID provides a basis for analyses of children, the family unit, 
                                                                          and gross personal incomes (benefits/tax credits, employment, self-employment, occupational pensions) 
                                                                          from which estimates of the number of children in low income families can be derived calibrated to 
                                                                          Households Below Average Income (HBAI) regional estimates on Absolute and Relative definitions."),
                                                                        p("RAPID is based on 100% extracts of various DWP benefit systems and is supplemented with 100% data 
                                                                          extracts from HMRC systems. RAPID collates information on individual activities (and the income generated 
                                                                          from those activities) within each tax year, including benefit, employment and in-work benefit interactions, 
                                                                          for example Tax Credits and Housing Benefit. Children have been identified from HMRC Child Benefit scans."),
                                                                        p("Percentages and numbers of children aged under 16 living in Relative (live in families with an income less 
                                                                          than 60% of the median income in that year) or Absolute (live in households with income below 60% of 
                                                                          (inflation-adjusted) median income for 2010/11 base year) low income families are available for each of the 
                                                                          32 Local Authorities in Scotland and are available from 2014/15 to 2018/19 (with 2018/19 figures being 
                                                                          provisional and subject to revision in subsequent releases)."),
                                                                        p("The next report will contain 2019/20 figures and is scheduled to be published in June 2021."),
                                                                        p(
                                                                          tags$a(href="https://www.gov.uk/government/statistics/children-in-low-income-families-local-area-statistics-201415-to-201819/",
                                                                                 "More information on HMRC children in low income families estimates can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("c) Child poverty rates nationally.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the Her Majesty’s Revenue and Customs (HMRC). 
                                                                          They provide information on the number and proportion of children living in Relative and 
                                                                          Absolute low income Before Housing Costs by local area across the United Kingdom."),
                                                                        p("These statistics draw data from the database “RAPID” (Registration and Population Interaction Database) 
                                                                          which provides a single coherent view of citizens’ interactions with Department of Work and Pensions (DWP) 
                                                                          and HMRC within a tax year for the UK. RAPID provides a basis for analyses of children, the family unit, 
                                                                          and gross personal incomes (benefits/tax credits, employment, self-employment, occupational pensions) 
                                                                          from which estimates of the number of children in low income families can be derived calibrated to 
                                                                          Households Below Average Income (HBAI) regional estimates on Absolute and Relative definitions."),
                                                                        p("RAPID is based on 100% extracts of various DWP benefit systems and is supplemented with 100% data 
                                                                          extracts from HMRC systems. RAPID collates information on individual activities (and the income generated 
                                                                          from those activities) within each tax year, including benefit, employment and in-work benefit interactions, 
                                                                          for example Tax Credits and Housing Benefit. Children have been identified from HMRC Child Benefit scans."),
                                                                        p("Percentages and numbers of children aged under 16 living in Relative (live in families with an income less 
                                                                          than 60% of the median income in that year) or Absolute (live in households with income below 60% of 
                                                                          (inflation-adjusted) median income for 2010/11 base year) low income families are available from 2014/15 to 2018/19 
                                                                          (with 2018/19 figures being provisional and subject to revision in subsequent releases). 
                                                                          Figures for Scotland were obtained by aggregating estimates from all of the 32 Local Authorities in Scotland."),
                                                                        p("The next report will contain 2019/20 figures and is scheduled to be published in June 2021."),
                                                                        p(
                                                                          tags$a(href="https://www.gov.uk/government/statistics/children-in-low-income-families-local-area-statistics-201415-to-201819/",
                                                                                 "More information on HMRC children in low income families estimates can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("d) Delivery of Fairer Scotland Action Plan.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the Fairer Scotland Action Plan (FSAP) annual progress report. 
                                                                          This report is built on five high-level ambitions that the Scottish Government continues to focus on in 
                                                                          the period to 2030: a fairer Scotland for all; ending child poverty; a strong start for all young people; 
                                                                          fairer working lives; and a thriving third age. "),
                                                                        p("Similar FSAP progress reports were published from 2016 to 2019. Each report contains the 50 actions that 
                                                                          the Scottish Government will take across the parliamentary term. These actions have been split across seven themes:",
                                                                        tags$ul(
                                                                          tags$li("Employment (including youth unemployment)"),
                                                                          tags$li("Education and childcare"),
                                                                          tags$li("Housing"),
                                                                          tags$li("Social security and benefit uptake"),
                                                                          tags$li("Social Policy"),
                                                                          tags$li("Equality"),
                                                                          tags$li("Health (including mental health)."))),
                                                                        p("The most recent report offers a quick overview of the current status of the actions:",
                                                                      tags$ul(
                                                                        tags$li("Completed (Action or recommendation completed)"),
                                                                        tags$li("In progress (Progress on the action or recommendation)"),
                                                                        tags$li("Continuous programme of work (Some recommendations or actions ask us to ‘do more’ or ‘improve’ with no clear end point. 
                                                                                These are defined as continuous)."))),
                                                                        p("Figures here show progress for each of these actions as of the end of 2020. Numbers and percentages are grouped by 
                                                                          the three different current status of the actions levels and are not disaggregated further by the seven themes. 
                                                                          Percentages have been calculated by summing the total number of action plans actions for each category 
                                                                          (completed/in progress/continuous programme of work) and dividing this number against the total number of action plans actions."),
                                                                        p("As the 2020 report was the first report to provide a quick overview of the current status of each action, 
                                                                          it has not been possible measure progress of these actions over time thus far."),
                                                                        p("The next report will provide a further update on progress and is scheduled to be published in December 2021."),
                                                                        p(
                                                                          tags$a(href="https://www.gov.scot/publications/fairer-scotland-action-plan-progress-report-2020/",
                                                                                 "More information on the FSAP can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("e) Actions taken from the collaboration between SG, Public Health Scotland, NHS Boards and local authorities to reduce health inequalities report.",
                                                                      tagList(
                                                                        p("It has not been possible to source data for this indicator and it is unclear when data will become available."))),
                                                      bsCollapsePanel("f) Deprivation gap in initial school leavers entering positive destinations.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the Attainment and Initial Leaver Destinations report. 
                                                                          This publication provides information on the attainment and initial destinations of school leavers in Scotland, 
                                                                          reports initial destinations for young people approximately three months after the end of the school year, 
                                                                          includes attainment in National Qualifications achieved throughout all stages of a pupil’s schooling and covers 
                                                                          all school leavers from publically funded mainstream schools."),
                                                                        p("A pupil is counted as a school leaver if they have a leaver record on the Opportunities for All shared data set, 
                                                                          a pupil census record for the same academic year, and no pupil census record in the following academic year. 
                                                                          The initial destinations data provide information on the outcomes for young people approximately three months after 
                                                                          the end of the academic year (1st Monday in October) while the follow-up publication provides information on the 
                                                                          outcomes of young people approximately nine months after the end of the academic year (1st Monday in April). 
                                                                          These collections should be seen as complementary to one another but it should be noted that various factors 
                                                                          may affect the results at different time periods."),
                                                                        p("The school leaver destinations data is matched to the pupil census and to Scottish Qualification Authority (SQA) 
                                                                          data so that pupil characteristics and attainment data can be linked to the destinations. Only leavers with a 
                                                                          match to the pupil census are included in the analysis within this publication. This means that some leavers are 
                                                                          excluded from the analysis. If a pupil does not have a Scottish Index of Multiple Deprivation (SIMD) category 
                                                                          in the pupil census data, the SIMD category of the pupil’s school is used."),
                                                                        p("Pupils were categorised as entering a Positive destination if the entered Higher Education, Further Education, 
                                                                          Training, Employment, Voluntary Work, Activity Agreement or Personal Skills Development. Pupils were categorised 
                                                                          as entering an Other destination if they were recorded as Unemployed seeking, Unemployed Not Seeking or Unknown."),
                                                                        p("Percentages of school leavers entering positive destinations are available from 2009/10 to 2019/20. 
                                                                          Data are also available by 20% most and least deprived areas of Scotland, with no further disaggregation available by gender."),
                                                                        p("The next report will contain 2020/21 figures and is scheduled to be published in February 2022."),
                                                                        p(
                                                                          tags$a(href="https://www.gov.scot/publications/summary-statistics-attainment-initial-leaver-destinations-no-3-2021-edition/",
                                                                                 "More information on the Attainment and Initial Leaver Destinations report can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("g) Deprivation gap in Annual Participation Measure.",
                                                                        tagList(
                                                                          p("Data for this indicator were sourced from the Annual Participation Measure (APM) report. This publication reports on learning, 
                                                                          training and work activity of 16-19 year olds in Scotland. The APM has been adopted in the Scottish Government’s National Performance Framework 
                                                                          as the measure of young people’s participation and has replaced the school leaver destination follow up as the source of the national indicator 
                                                                          (Percentage of young adults (16-19 year olds) participating in education, training or employment)."),
                                                                          p("The APM uses a shared data set to report on the economic and employment activity of the wider 16-19 year old cohort, including those at school. 
                                                                          This data set is managed by Skills Development Scotland (SDS) and held on the agency’s Customer Support System (CSS). 
                                                                          Central to the maintenance of the shared data set is the sharing of information between partners to identify what young people are doing in ‘real time’ 
                                                                          between the ages of 16-19. It also allows SDS and partners to improve service delivery and provide a more tailored offer, 
                                                                          helping to identify the right time to engage with customers."),
                                                                          p("Young people were categorised as Participating if they were in Education (School pupil, Higher education or Further education), Employment 
                                                                          (Full time, Part time, Self-employment or a Modern apprenticeship) or Training and other development (SDS Employability fund, Activity agreement, 
                                                                          Other training Personal Skills Development or Voluntary work). Young people were categorised as Not Participating if they were as Unemployed 
                                                                          (but seeking employment or training) or Unemployed and not seeing employment or training (due to being economically inactive, ill health or were in custody)."),
                                                                          p("Numbers and percentages of young people aged 16-19 are available from 2016 to 2020, with each year being classified as financial year 1st April to 31st March 
                                                                          (so, for example, 2020 corresponds to the period 1st April 2019 to 31st March 2020) rather than a calendar year. 
                                                                          Data are also available by 20% most and least deprived areas of Scotland. Further data are available by Local Authority and gender but are not reported on here."),
                                                                          p("The next report will contain 2021 figures and is scheduled to be published in August 2021."),
                                                                          p(
                                                                            tags$a(href="https://www.skillsdevelopmentscotland.co.uk/publications-statistics/statistics/annual-participation-measure/?page=1&statisticCategoryId=7&order=date-desc/",
                                                                                   "More information on the APM can be found here", target="_blank"))
                                                                          ))
                                           ))),
                          
                          #Indicators for outcome b
                          h3("Outcome b) Education provision for children and young people is more in line with evidence and best practices"), 
                          
                          p(
                            "It has not been possible to source data for this indicator as it is currently under development. Any outputs are not likely to become available until 2022 at the earliest."
                          ),
                          
                          #Indicators for outcome c
                          h3("Outcome c) Increase in the number of people at risk of alcohol or drug problems linked to positive environments and opportunities"), 
                          
                          wellPanel(column(12,
                                           bsCollapse(id = "prev_outcome_c", open = "Panel 1", #PanelSet id
                                                      bsCollapsePanel("a) Percentage of S2 and S4 pupils who participated in sports clubs, gyms, exercise or dance groups in the last 12 months.",
                                                                    tagList(
                                                                      p("Data for this indicator were sourced from the Scottish Adolescent Lifestyle & Substance Use Survey (SALSUS). 
                                                                        This survey is a continuation of a long established series of national surveys on smoking, drinking and drug use. 
                                                                        These were carried out jointly in Scotland and England between 1982 and 2000, to provide a national picture of young peoples' 
                                                                        behaviours within the context of other lifestyle, health and social factors. Since 2002, Scotland has developed its own, 
                                                                        more tailored SALSUS survey, a self-completion survey administered by teachers in a mixed ability class, under exam conditions. 
                                                                        Data were weighted by Local Authority, age, sex, school sector (state/independent), school denomination and by urban/rural classification."),
                                                                      p("As with any survey, there are a number of limitations of the data. The data are from a sample of the population as opposed to a census and, 
                                                                        therefore, subject to sampling error. Data can also be incomplete due to non-response of schools, classes and pupils. As with all surveys, 
                                                                        the results can only tell us what respondents say that they do, think or feel. We have to assume that their answers are honest and accurate, 
                                                                        especially given SALSUS covers sensitive topic areas."),
                                                                      p("Accuracy is also an issue as self-report and accuracy of answers will vary by pupil and the sample only covers pupils in mainstream secondary education. 
                                                                        The sample excludes pupils in special schools, secure residential units and those who are home schooled. 
                                                                        Additionally, pupils who are supposed to attend mainstream schools but don't (e.g. absent through truancy or exclusion) are potentially 
                                                                        less likely to have taken part. Findings from studies show that absenteeism due to truanting and exclusion is correlated with substance use."),
                                                                      p("Data for this indicator were recorded as percentage of S2 (13 year old) and S4 (15 year old) pupils who responded ‘Yes’ when asked if they had 
                                                                        participated in sports clubs, gyms, exercise or dance groups in the last 12 months."),
                                                                      p("As well as for all pupils who had participated in some form of exercise, data for this indicator were also split by if the respondent 
                                                                        stated they had used drugs at some point in the past, or if the respondent stated they have never used drugs."),
                                                                      p("As this information is not currently published in SALSUS, analyses were obtained by requesting 2013, 2015 and 2018 SALSUS data from the UK 
                                                                        Data Service (UKDS) separately and by running analyses in R."),
                                                                      p("Although a publication date has yet to be confirmed, the next SALSUS will likely be published in 2022, with subsequent SALSUS datasets 
                                                                        added onto the UKDS shortly after. It is anticipated that information will still be available for this indicator."),
                                                                      p(
                                                                        tags$a(href="https://www.gov.scot/publications/scottish-schools-adolescent-lifestyle-substance-use-survey-salsus-drug-use-report-2018/",
                                                                               "More information on the SALSUS can be found here", target="_blank"))
                                                      )),
                                                      bsCollapsePanel("b) Number and percentage of young people from the lowest SIMD quintile in initial school leavers entering positive destinations.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the Attainment and Initial Leaver Destinations report. 
                                                                          This publication provides information on the attainment and initial destinations of school leavers in Scotland, 
                                                                          reports initial destinations for young people approximately three months after the end of the school year, 
                                                                          includes attainment in National Qualifications achieved throughout all stages of a pupil’s schooling and covers 
                                                                          all school leavers from publically funded mainstream schools."),
                                                                        p("A pupil is counted as a school leaver if they have a leaver record on the Opportunities for All shared data set, 
                                                                          a pupil census record for the same academic year, and no pupil census record in the following academic year. 
                                                                          The initial destinations data provide information on the outcomes for young people approximately three months after 
                                                                          the end of the academic year (1st Monday in October) while the follow-up publication provides information on the 
                                                                          outcomes of young people approximately nine months after the end of the academic year (1st Monday in April). 
                                                                          These collections should be seen as complementary to one another but it should be noted that various factors 
                                                                          may affect the results at different time periods."),
                                                                        p("The school leaver destinations data is matched to the pupil census and to Scottish Qualification Authority (SQA) 
                                                                          data so that pupil characteristics and attainment data can be linked to the destinations. Only leavers with a 
                                                                          match to the pupil census are included in the analysis within this publication. This means that some leavers are 
                                                                          excluded from the analysis. If a pupil does not have a Scottish Index of Multiple Deprivation (SIMD) category 
                                                                          in the pupil census data, the SIMD category of the pupil’s school is used."),
                                                                        p("Pupils were categorised as entering a Positive destination if the entered Higher Education, Further Education, 
                                                                          Training, Employment, Voluntary Work, Activity Agreement or Personal Skills Development. Pupils were categorised 
                                                                          as entering an Other destination if they were recorded as Unemployed seeking, Unemployed Not Seeking or Unknown."),
                                                                        p("Percentages and numbers of school leavers recorded as being from the 20% most deprived areas of Scotland entering 
                                                                          positive destinations are available from 2009/10 to 2019/20. No further disaggregation is available by gender."),
                                                                        p("The next report will contain 2020/21 figures and is scheduled to be published in February 2022."),
                                                                        p(
                                                                          tags$a(href="https://www.gov.scot/publications/summary-statistics-attainment-initial-leaver-destinations-no-3-2021-edition/",
                                                                                 "More information on the Attainment and Initial Leaver Destinations report can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("c) Number and percentage of young people from the lowest SIMD quintile in annual participation measure.",
                                                                      p("Data for this indicator were sourced from the Annual Participation Measure (APM) report. This publication reports on learning, 
                                                                          training and work activity of 16-19 year olds in Scotland. The APM has been adopted in the Scottish Government’s National Performance Framework 
                                                                          as the measure of young people’s participation and has replaced the school leaver destination follow up as the source of the national indicator 
                                                                          (Percentage of young adults (16-19 year olds) participating in education, training or employment)."),
                                                                      p("The APM uses a shared data set to report on the economic and employment activity of the wider 16-19 year old cohort, including those at school. 
                                                                          This data set is managed by Skills Development Scotland (SDS) and held on the agency’s Customer Support System (CSS). 
                                                                          Central to the maintenance of the shared data set is the sharing of information between partners to identify what young people are doing in ‘real time’ 
                                                                          between the ages of 16-19. It also allows SDS and partners to improve service delivery and provide a more tailored offer, 
                                                                          helping to identify the right time to engage with customers."),
                                                                      p("Young people were categorised as Participating if they were in Education (School pupil, Higher education or Further education), Employment 
                                                                          (Full time, Part time, Self-employment or a Modern apprenticeship) or Training and other development (SDS Employability fund, Activity agreement, 
                                                                          Other training Personal Skills Development or Voluntary work). Young people were categorised as Not Participating if they were as Unemployed 
                                                                          (but seeking employment or training) or Unemployed and not seeing employment or training (due to being economically inactive, ill health or were in custody)."),
                                                                      p("Numbers and percentages of young people aged 16-19 are available from 2016 to 2020, with each year being classified as financial year 1st April to 31st March 
                                                                          (so, for example, 2020 corresponds to the period 1st April 2019 to 31st March 2020) rather than a calendar year. 
                                                                          Data are also available by 20% most and least deprived areas of Scotland. Further data are available by Local Authority and gender but are not reported on here."),
                                                                      p("The next report will contain 2021 figures and is scheduled to be published in August 2021."),
                                                                      p(
                                                                        tags$a(href="https://www.skillsdevelopmentscotland.co.uk/publications-statistics/statistics/annual-participation-measure/?page=1&statisticCategoryId=7&order=date-desc/",
                                                                               "More information on the APM can be found here", target="_blank")))
                                           ))),
                          
                          
                          
                          #Indicators for outcome d
                          h3("Outcome d) Young people's capacity to make informed choices is improved"), 
                          
                          wellPanel(column(12,
                                           bsCollapse(id = "prev_outcome_d", open = "Panel 1", #PanelSet id
                                                      bsCollapsePanel("a) Young people’s attitude towards the risks of drug use.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the Scottish Adolescent Lifestyle & Substance Use Survey (SALSUS). 
                                                                          This survey is a continuation of a long established series of national surveys on smoking, drinking and drug use. 
                                                                          These were carried out jointly in Scotland and England between 1982 and 2000, to provide a national picture of young peoples' 
                                                                          behaviours within the context of other lifestyle, health and social factors. Since 2002, Scotland has developed its own, 
                                                                          more tailored SALSUS survey, a self-completion survey administered by teachers in a mixed ability class, under exam conditions. 
                                                                          Data were weighted by Local Authority, age, sex, school sector (state/independent), school denomination and by urban/rural classification."),
                                                                        p("As with any survey, there are a number of limitations of the data. The data are from a sample of the population as opposed to a census and, 
                                                                          therefore, subject to sampling error. Data can also be incomplete due to non-response of schools, classes and pupils. As with all surveys, 
                                                                          the results can only tell us what respondents say that they do, think or feel. We have to assume that their answers are honest and accurate, 
                                                                          especially given SALSUS covers sensitive topic areas."),
                                                                        p("Accuracy is also an issue as self-report and accuracy of answers will vary by pupil and the sample only covers pupils in mainstream secondary education. 
                                                                          The sample excludes pupils in special schools, secure residential units and those who are home schooled. 
                                                                          Additionally, pupils who are supposed to attend mainstream schools but don't (e.g. absent through truancy or exclusion) are potentially 
                                                                          less likely to have taken part. Findings from studies show that absenteeism due to truanting and exclusion is correlated with substance use."),
                                                                        p("Pupils were asked for their views on the acceptability of trying cannabis, cocaine or sniffing glue. Data for this indicator were recorded as percentage of S2 
                                                                          (13 year old) and S4 (15 year old) pupils who responded ‘Yes’ when asked if they thought it was ’OK’ for someone their age to try each of these drugs. 
                                                                          Data for this indicator were obtained from the 2010, 2013, 2015 and 2018 versions of the SALSUS and were not possible to disaggregate by deprivation. 
                                                                          Data are available by gender but are not reported on here."),
                                                                        p("Although a publication date has yet to be confirmed, the next SALSUS will likely be published in 2022. 
                                                                          It is anticipated that information will still be available for this indicator."),
                                                                        p(
                                                                          tags$a(href="https://www.gov.scot/publications/scottish-schools-adolescent-lifestyle-substance-use-survey-salsus-drug-use-report-2018/",
                                                                                 "More information on the SALSUS can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("b) Young people’s reported wellbeing.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the Scottish Adolescent Lifestyle & Substance Use Survey (SALSUS). 
                                                                          This survey is a continuation of a long established series of national surveys on smoking, drinking and drug use. 
                                                                          These were carried out jointly in Scotland and England between 1982 and 2000, to provide a national picture of young peoples' 
                                                                          behaviours within the context of other lifestyle, health and social factors. Since 2002, Scotland has developed its own, 
                                                                          more tailored SALSUS survey, a self-completion survey administered by teachers in a mixed ability class, under exam conditions. 
                                                                          Data were weighted by Local Authority, age, sex, school sector (state/independent), school denomination and by urban/rural classification."),
                                                                        p("As with any survey, there are a number of limitations of the data. The data are from a sample of the population as opposed to a census and, 
                                                                          therefore, subject to sampling error. Data can also be incomplete due to non-response of schools, classes and pupils. As with all surveys, 
                                                                          the results can only tell us what respondents say that they do, think or feel. We have to assume that their answers are honest and accurate, 
                                                                          especially given SALSUS covers sensitive topic areas."),
                                                                        p("Accuracy is also an issue as self-report and accuracy of answers will vary by pupil and the sample only covers pupils in mainstream secondary education. 
                                                                          The sample excludes pupils in special schools, secure residential units and those who are home schooled. 
                                                                          Additionally, pupils who are supposed to attend mainstream schools but don't (e.g. absent through truancy or exclusion) are potentially 
                                                                          less likely to have taken part. Findings from studies show that absenteeism due to truanting and exclusion is correlated with substance use."),
                                                                        p("Data for this indicator were recorded as percentage of S2 (13 year old) and S4 (15 year old) pupils who responded ‘Very Good’ or ‘Good’ 
                                                                          when asked how their health was in general. These pupils were also asked if they have a physical or mental health condition or illness lasting or 
                                                                          expected to last 12 months or more, with percentages recorded by those who responded ‘Yes’. Data for both these sub-indicators were split by if the 
                                                                          respondent stated they had used drugs at some point in the last month, had used drugs at some point in the past or if they have never used drugs."),
                                                                        p("As information based on these definitions and split by these groups is not currently published in SALSUS, analyses were obtained by requesting 2013, 
                                                                          2015 and 2018 SALSUS data from the UK Data Service (UKDS) separately and by running analyses in R. "),
                                                                        p("Although a publication date has yet to be confirmed, the next SALSUS will likely be published in 2022, with subsequent SALSUS datasets 
                                                                         added onto the UKDS shortly after. It is anticipated that information will still be available for this indicator."),
                                                                        p(
                                                                          tags$a(href="https://www.gov.scot/publications/scottish-schools-adolescent-lifestyle-substance-use-survey-salsus-drug-use-report-2018/",
                                                                                 "More information on the SALSUS can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("c) Number of children and young people using drugs.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the Scottish Adolescent Lifestyle & Substance Use Survey (SALSUS). 
                                                                          This survey is a continuation of a long established series of national surveys on smoking, drinking and drug use. 
                                                                          These were carried out jointly in Scotland and England between 1982 and 2000, to provide a national picture of young peoples' 
                                                                          behaviours within the context of other lifestyle, health and social factors. Since 2002, Scotland has developed its own, 
                                                                          more tailored SALSUS survey, a self-completion survey administered by teachers in a mixed ability class, under exam conditions. 
                                                                          Data were weighted by Local Authority, age, sex, school sector (state/independent), school denomination and by urban/rural classification."),
                                                                        p("As with any survey, there are a number of limitations of the data. The data are from a sample of the population as opposed to a census and, 
                                                                          therefore, subject to sampling error. Data can also be incomplete due to non-response of schools, classes and pupils. As with all surveys, 
                                                                          the results can only tell us what respondents say that they do, think or feel. We have to assume that their answers are honest and accurate, 
                                                                          especially given SALSUS covers sensitive topic areas."),
                                                                        p("Accuracy is also an issue as self-report and accuracy of answers will vary by pupil and the sample only covers pupils in mainstream secondary education. 
                                                                          The sample excludes pupils in special schools, secure residential units and those who are home schooled. 
                                                                          Additionally, pupils who are supposed to attend mainstream schools but don't (e.g. absent through truancy or exclusion) are potentially 
                                                                          less likely to have taken part. Findings from studies show that absenteeism due to truanting and exclusion is correlated with substance use."),
                                                                        p("Pupils were provided with a list of drugs (including their commonly used street names) and asked if they had used each of them. 
                                                                          This information was used to create an overall measure of any drug use ‘in the last month’, ‘in the last year’ (including in the last month), 
                                                                          ‘ever’ (including in the last month and last year) and ‘never’. Data for this indicator were recorded as percentage of S2 (13 year old) and S4 
                                                                          (15 year old) pupils who responded ‘Yes’ when asked if they had used any of these drugs."),
                                                                        p("Data for this indicator were obtained from the 2010, 2013, 2015 and 2018 versions of the SALSUS and were not possible to disaggregate by deprivation. 
                                                                          Data are available by gender but are not reported on here. Numbers are also provided in this dashboard but these should be interpreted with caution, 
                                                                          as a decrease may represent fewer pupils sampled across survey years rather than an actual decrease in drug taking behaviour."),
                                                                        p("Although a publication date has yet to be confirmed, the next SALSUS will likely be published in 2022. 
                                                                          It is anticipated that information will still be available for this indicator."),
                                                                        p(
                                                                          tags$a(href="https://www.gov.scot/publications/scottish-schools-adolescent-lifestyle-substance-use-survey-salsus-drug-use-report-2018/",
                                                                                 "More information on the SALSUS can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("d) Number of young people using alcohol.",
                                                                      tagList(
                                                                        p("Moved indicator to Outcome e) of the Children, Young People & Families Chapter. Please navigate here for further details."))),
                                                      bsCollapsePanel("e) Number of young people indicating problematic use.",
                                                                        tagList(
                                                                          p("Data for this indicator were sourced from the Scottish Adolescent Lifestyle & Substance Use Survey (SALSUS). 
                                                                            This survey is a continuation of a long established series of national surveys on smoking, drinking and drug use. 
                                                                            These were carried out jointly in Scotland and England between 1982 and 2000, to provide a national picture of young peoples' 
                                                                            behaviours within the context of other lifestyle, health and social factors. Since 2002, Scotland has developed its own, 
                                                                            more tailored SALSUS survey, a self-completion survey administered by teachers in a mixed ability class, under exam conditions. 
                                                                            Data were weighted by Local Authority, age, sex, school sector (state/independent), school denomination and by urban/rural classification."),
                                                                          p("As with any survey, there are a number of limitations of the data. The data are from a sample of the population as opposed to a census and, 
                                                                            therefore, subject to sampling error. Data can also be incomplete due to non-response of schools, classes and pupils. As with all surveys, 
                                                                            the results can only tell us what respondents say that they do, think or feel. We have to assume that their answers are honest and accurate, 
                                                                            especially given SALSUS covers sensitive topic areas."),
                                                                          p("Accuracy is also an issue as self-report and accuracy of answers will vary by pupil and the sample only covers pupils in mainstream secondary education. 
                                                                            The sample excludes pupils in special schools, secure residential units and those who are home schooled. 
                                                                            Additionally, pupils who are supposed to attend mainstream schools but don't (e.g. absent through truancy or exclusion) are potentially 
                                                                            less likely to have taken part. Findings from studies show that absenteeism due to truanting and exclusion is correlated with substance use."),
                                                                          p("Pupils were asked if they ever felt they needed to get help because they were using drugs. Data for this indicator were recorded as percentage of S2 
                                                                            (13 year old) and S4 (15 year old) pupils who responded ‘Yes’. Data for this indicator were obtained from the 2010, 2013, 2015 and 2018 versions of the 
                                                                            SALSUS and were not possible to disaggregate by deprivation or gender."),
                                                                          p("Although a publication date has yet to be confirmed, the next SALSUS will likely be published in 2022, with subsequent SALSUS datasets 
                                                                            added onto the UKDS shortly after. It is anticipated that information will still be available for this indicator."),
                                                                          p(
                                                                            tags$a(href="https://www.gov.scot/publications/scottish-schools-adolescent-lifestyle-substance-use-survey-salsus-drug-use-report-2018/",
                                                                                   "More information on the SALSUS can be found here", target="_blank"))
                                                                        )),
                                                      bsCollapsePanel("f) Number and rate of young people admitted to hospital for drug-related admissions.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from routinely collected Scottish Morbidity Records (SMR) SMR01 and SMR04 national datasets. 
                                                                           Patients are defined as unique individuals discharged from hospital within the financial year and are counted only once within each time period. 
                                                                           New patients are defined as patients discharged from hospital within the financial year who did not have a drug-related hospital discharge within the last 10 years."),
                                                                        p("Some caution is necessary when using these data as problematic substance use may only be suspected and may not always be recorded by the hospital, 
                                                                           and where problematic substance use was recorded, it may not be possible to identify which drug(s) may be involved."),
                                                                        p("During October and November 2018, ISD conducted a customer consultation about a proposed change in the definition of a drug-related hospital stay to include 
                                                                           admissions due to drug poisoning/overdose. This proposed change would widen the range of stays captured in these statistics in order to measure drug-related 
                                                                           hospital activity more comprehensively. As users agreed with the proposed change, hospital admissions resulting from drug poisoning/overdose are included 
                                                                           within the definition of a drug-related hospital stay reported in these statistics. 
                                                                           This change widened the range of stays captured in these statistics, measuring drug-related hospital activity more comprehensively."),
                                                                        p("A European Age-sex Standardised Rate (EASR) was calculated because hospital activity rates may vary with the age-sex structure of the populations. 
                                                                           The European Standard Population (ESP) was used to calculate EASRs within the most recent publications. The ESP was originally introduced in 1976 and was revised in 2013. 
                                                                           Before 2014 the publication used ESP1976 to calculate EASRs. Since 2014, ESP2013 has been used to calculate EASRs for all years (including those before 2012/13). 
                                                                           Therefore, findings from publications since February 2014 are not comparable with earlier publications."),
                                                                        p("Rates were only available for those aged under 15 and subsequent 10 year age bands thereafter. 
                                                                           Therefore, rates for individuals aged under 25 are not shown. Figures for the number of individuals aged under 25 are based on an aggregation of individuals aged under 15 and individuals aged 15-24 
                                                                          (for all, males and females). As disclosure control protocol has been applied to the disaggregation age-gender figures, there may be some slight discrepancies compared with figures based on a direct calculation."),
                                                                        p("Data are available from 2009/10 to 2018/19. Socioeconomic deprivation data were not available to be split further by 
                                                                           Scottish Index of Multiple Deprivation (SIMD) by each of the gender or age groups."),
                                                                        p("The next report will contain 2019/20 figures and is scheduled to be published in November 2021."),
                                                                        p(
                                                                          tags$a(href="https://beta.isdscotland.org/find-publications-and-data/lifestyle-and-behaviours/substance-use/drug-related-hospital-statistics/",
                                                                                 "More information on drug-related hospital statistics can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("g) Number and rate of young people admitted to hospital for alcohol-related admissions.",
                                                                      tagList(
                                                                        p("Moved indicator to Outcome e) of the Children, Young People & Families Chapter. Please navigate here for further details.")))
                                           ))),
                          
                          
                          
                          
                          #Indicators for outcome e
                          h3("Outcome e) Increase in individual and community wellbeing, resilience, and social connectedness"), 
                          
                          wellPanel(column(12,
                                           bsCollapse(id = "prev_outcome_e", open = "Panel 1", #PanelSet id
                                                      bsCollapsePanel("a) Rating of neighbourhood as a place to live (including by SIMD) – perceptions, strengths, engagement with 
                                                       local community, social isolation, and feelings of loneliness.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the Scottish Household Survey (SHS). 
                                                                           From 2012 the three Scottish Government interviewer-led population surveys have coordinated sample designs. 
                                                                           The sample is disproportionately stratified by Local Authority. Samples of the general population exclude prisons, hospitals and military bases. 
                                                                           The Royal Mail's small user Postcode Address File (PAF) is used as the sample frame for the address selection."),
                                                                        p("The social interview is carried out using Computer Aided Personal Interviewing (CAPI), and is in two parts; Household and Random Adult. 
                                                                           There is a follow-up component comprising of a 'Physical Survey' of the dwelling, conducted by a professional surveyor through a visual inspection of the dwelling. 
                                                                           The weighting procedures incorporate a selection weighting stage to address the unequal selection probabilities and calibration weighting to correct for non-bias. 
                                                                           Calibration weighting derives weights such that the weighted survey totals match known population totals."),
                                                                        p("Percentages reported here are grouped by if the individual responded ‘Agree strongly’ or ‘Agree fairly strongly’ to the following statements about their neighbourhood:",
                                                                          tags$ul(
                                                                            tags$li("Kindness: This is a neighbourhood where people are kind to each other."),
                                                                            tags$li("Trust: This is a neighbourhood where most people can be trusted."),
                                                                            tags$li("Opportunities: There are welcoming places and opportunities to meet new people."),
                                                                            tags$li("Socialise: There are places where people can meet up and socialise."),
                                                                            tags$li("Get on well: This is a neighbourhood where people from different backgrounds get on well together."),
                                                                            tags$li("Take action: This is a neighbourhood where local people take action to help improve the neighbourhood."),
                                                                            tags$li("Belong to community."))),
                                                                        p("Data are available from 2018 to 2019 for all of these indicators apart from belonging to community, where data are available from 2013 to 2019. 
                                                                           As well as showing national figures, data are available for SIMD quintile and the difference shown was for the 20% most and 20% least deprived areas in Scotland."),
                                                                        p("The next report will contain 2020 figures and is scheduled to be published in October 2021."),
                                                                        p(
                                                                          tags$a(href="https://shs.theapsgroup.scot/2019/",
                                                                                 "More information on the SHS can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("b) Feelings of safety in neighbourhood.",
                                                                      tagList(
                                                                        p("Data for these indicators were sourced from the Scottish Crime and Justice Survey (SCJS), a large-scale social survey which asks people about their experiences and perceptions of crime. 
                                                                           The SCJS gathers information from a representative sample rather than from the whole population and so results are always estimates, not precise figures. 
                                                                           This means that the results are subject to a margin of error which can have an impact on how changes in the numbers should be interpreted, especially in the short term."),
                                                                        p("Respondents were asked to respond ‘Very safe’, ‘Fairly safe’, ‘A bit unsafe’, ‘Very unsafe’ or ‘Don't know’ when asked ‘Do you feel safe walking alone after dark in your local area at night?’. 
                                                                           Respondents were also asked to provide one of these responses when asked ‘Do you feel safe alone in your at home at night?’. Figures presented here are a combination of those who 
                                                                           responded they felt ‘Very safe’ or ‘Fairly safe’."),
                                                                        p("The figures in this report are based on percentages rather than numbers sampled within the SCJS. 
                                                                           These figures should be interpreted with caution. It is possible that if there has been a large increase in numbers of crimes committed within these categories then the number of 
                                                                           crimes which involved alcohol could increase even if alcohol involvement has decreased."),
                                                                        p("Data for this indicator are available for most years from 2009/10 to 2018/19. These questions were not asked as part of the 2011/12, 2013/14 or 
                                                                           2015/16 surveys so data were not reported on for these years. Data were also split by if the respondent was male, female or was resident in one of the 15% most deprived areas in Scotland."),
                                                                        p("The next report will contain 2019/20 figures and is scheduled to be published in June 2021."),
                                                                        p(
                                                                          tags$a(href="https://www.gov.scot/collections/scottish-crime-and-justice-survey/",
                                                                                 "More information on the SCJS can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("c) Rating of drugs being a problem in neighbourhood.",
                                                                      tagList(
                                                                        p("Data for these indicators were sourced from the Scottish Crime and Justice Survey (SCJS), a large-scale social survey which asks people about their experiences and perceptions of crime. 
                                                                           The SCJS gathers information from a representative sample rather than from the whole population and so results are always estimates, not precise figures. 
                                                                           This means that the results are subject to a margin of error which can have an impact on how changes in the numbers should be interpreted, especially in the short term."),
                                                                        p("Respondents were asked to respond ‘Very common’, ‘Fairly common’, ‘A bit common’, ‘Very uncommon’ or ‘Don't know’ when asked ‘How common are drug dealing and drug abuse problems were within your local area?’. 
                                                                           Figures presented here are a combination of those who responded it was a ‘Very common’ or ‘Fairly common’ problem."),
                                                                        p("It is not always necessary to have direct personal experience of an issue to know about it or perceive it as a problem in an area. What people may define as an issue is related to their own perceptions, beliefs and definitions. 
                                                                           For instance, one respondent may consider witnessing drug abuse or dealing as experiencing the issue, while another respondent may only report experience of this problem if they have personally been offered drugs."),
                                                                        p("The figures in this report are based on percentages rather than numbers sampled within the SCJS. These figures should be interpreted with caution. It is possible that if there has been a large increase in numbers of 
                                                                           crimes committed within these categories then the number of crimes which involved alcohol could increase even if alcohol involvement has decreased."),
                                                                        p("Data for this indicator are available for most years from 2009/10 to 2018/19. These questions were not asked as part of the 2011/12, 2013/14 or 
                                                                           2015/16 surveys so data were not reported on for these years. Data were also split by if the respondent was male, female or was resident in one of the 15% most deprived areas in Scotland."),
                                                                        p("The next report will contain 2019/20 figures and is scheduled to be published in June 2021."),
                                                                        p(
                                                                          tags$a(href="https://www.gov.scot/collections/scottish-crime-and-justice-survey/",
                                                                                 "More information on the SCJS can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("d) Level of self-reported stigma related to drug use among people who inject drugs.",
                                                                      tagList(
                                                                        p("Data for people who inject drugs (PWID) were sourced from the Needle Exchange Surveillance Initiative (NESI), 
                                                                           which aims to measure and monitor injecting risk behaviours among people who inject drugs in Scotland. 
                                                                           A cross-sectional, voluntary, anonymous, bio-behavioural survey approach was used to recruit and interview PWID. 
                                                                           Trained interviewers recruited participants from selected agencies and pharmacies that provided injecting equipment; 
                                                                           these settings may also provide other harm-reduction services, such as prescribed methadone. 
                                                                           Approximately 60% of PWID approached to participate in each survey consent to do so, equating to around 2,500 PWID per sweep, 
                                                                           10–15% of the PWID population in Scotland."),
                                                                        p("Clients attending these services were invited to take part if they had injected drugs on at least one occasion either recently or in the past, 
                                                                           and if it was the first time they had participated in the current survey. Participants completed a short interviewer-administered questionnaire 
                                                                           and then provided a voluntary blood spot sample for anonymous testing for blood-borne virus markers."),
                                                                        p("Recruitment of people who have ever injected in the past, but not in the previous six months, 
                                                                           was limited to 20–30% of participants during each survey. In addition, the number of individuals reporting injection of image 
                                                                           and performance enhancing drugs (IPED) alone was capped at 5% of total recruitment at each site."),
                                                                        p("PWID were asked to respond Yes or No to if they experienced stigma and discrimination due to their:",
                                                                          tags$ul(
                                                                            tags$li("Drug use"),
                                                                            tags$li("HIV status (Only includes those who are infected with HIV who have disclosed their infection)"),
                                                                            tags$li("HCV status (Only includes those who are HCV antibody positive who have disclosed their infection (past or current) 
                                                                                     and did not select 'Not applicable' in answer to this question)"),
                                                                            tags$li("Other reason (e.g. mental health, appearance, criminal record/history, homeless and begging, drug treatment)."))),
                                                                        p("Data for this indicator are available for 2017-18 only. The survey period accounts for data over two combined calendar years rather than financial year. 
                                                                           As an example, the 2017–18 figure refers to a combination of the two calendar years of 2017 and 2018 
                                                                           together rather than the financial year from 1 April 2017 to 31 March 2018."),
                                                                        p("Data on self-reported health were requested from the Needle Exchange Surveillance Initiative via a bespoke information request and formatted. 
                                                                           The next report, scheduled to be published at some point during 2021, is unlikely to contain 2019-20 figures for stigma so 
                                                                           another information request will have to be raised."),
                                                                        p(
                                                                          tags$a(href="https://www.hps.scot.nhs.uk/web-resources-container/needle-exchange-surveillance-initiative-nesi-2008-09-to-2017-18/",
                                                                                 "More information on the NESI can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("e) Social capital (and constituent parts – social networks, community cohesion, community empowerment 
                                                       and social participation) ratings by quintile.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the Scottish Household Survey (SHS). 
                                                                           From 2012 the three Scottish Government interviewer-led population surveys have coordinated sample designs. 
                                                                           The sample is disproportionately stratified by Local Authority. Samples of the general population exclude prisons, hospitals and military bases. 
                                                                           The Royal Mail's small user Postcode Address File (PAF) is used as the sample frame for the address selection."),
                                                                        p("The social interview is carried out using Computer Aided Personal Interviewing (CAPI), and is in two parts; Household and Random Adult. 
                                                                           There is a follow-up component comprising of a 'Physical Survey' of the dwelling, conducted by a professional surveyor through a visual inspection of the dwelling. 
                                                                           The weighting procedures incorporate a selection weighting stage to address the unequal selection probabilities and calibration weighting to correct for non-bias. 
                                                                           Calibration weighting derives weights such that the weighted survey totals match known population totals."),
                                                                        p("Social capital is the resource of social networks, community cohesion, social participation, trust and empowerment. 
                                                                           The social capital index monitors aggregate changes in levels of social capital since 2013."),
                                                                        p("The Scottish Government developed an index based on four social capital themes: 
                                                                           1) social networks, 2) community cohesion, 3) social participation, and 4) community empowerment. 
                                                                           Under these headings, data from 18 survey questions from the SHS is tracked over time to show aggregate change in the four themes, 
                                                                           and for an overall measure of change in social capital nationally since 2013."),
                                                                        p("To account for the different magnitude of scores, each of the questions has been indexed and set to the value 100 for the base year 2013, 
                                                                           and the percentage changes in subsequent years, relative to the base year."),
                                                                        p("Four themes and associated survey questions are shown here:",
                                                                          tags$ul(
                                                                            tags$li("Social Networks: Could rely on neighbour to help; Could count on neighbour to keep eye on home;
                                                                                     Could turn to someone in neighbourhood for advice; Would help neighbour."),
                                                                            tags$li("Community Cohesion: Neighbourhood rating; Neighbourhood belonging; neighbourhood safety."),
                                                                            tags$li("Community Empowerment: I can influence decisions and people take action to improve the area."),
                                                                            tags$li("Social Participation: In the last 12 months, has given up time to help any groups, clubs or organisations in an unpaid capacity; 
                                                                                     In the last 12 months, has given unpaid help to other people or to improve your local environment, that is apart from any help given through a group, 
                                                                                     club or organisations."))),
                                                                        p("The indexed measures of variables within each theme are averages to provide an index score for each theme. 
                                                                           The four scores for each theme are averaged to provide an overall indicator. This means each theme has equal weight in the calculation of the overall score."),
                                                                        p("Data are available from 2013 to 2018 for all of these indicators. For the overall national index score, data are available from 2013 to 2019. 
                                                                           The next report will contain 2019 figures (with 2020 figure available for the overall score) and is scheduled to be published in October 2021."),
                                                                        p(
                                                                          tags$a(href="https://shs.theapsgroup.scot/2019/",
                                                                                 "More information on the SHS can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("f) Output from the expert group convened to examine stigma.",
                                                                      tagList(
                                                                        p("It has not been possible to source data for this indicator and it is unclear when data will become available.")))
                                           ))),
                          
                          tags$a(href = '#top',  
                                 icon("circle-arrow-up", lib= "glyphicon"),"Back to top of page")
                          
                 ),
                 
                 
                 
                 
                 #Now create a new tab in the dashboard which will provide information about chapter 2
                 tabPanel("2) Recovery Oriented Systems of Care", icon = icon("hands-helping"),
                          #This part of the page will provide information about chapter 2
                          
                          h2("Chapter 2 - Recovery Oriented Systems of Care"), 
                          
                          p(
                            "The overall outcome for this part of the strategy is for people to be able to access and benefit from
                                effective, integrated, person-centred support to achieve their own type of recovery. The original TOC model
                                identifies interim outcomes to achieve this overall outcome. These are:",
                            tags$ul(
                              tags$li("Grow and expand Scotland's recovery communities into wider community settings."),
                              tags$li("Improve access to and quality of treatment services, including harm-reduction and low-threshold
                                 services, other support services and community supports."),
                              tags$li("Increase availability and use of advocacy by those who require it at every stage of their recovery."),
                              tags$li("Increase person-centred approaches across treatment and recovery services and the range of
                                health and social care services which work with people with alcohol and drug problems.")),
                            "Please select one of the indicator titles below for information on the indicator data source, caveats and next update."
                          ),
                          
                          
                          hr(style = "border-top: 2px solid #000000;"),
                          
                          #Indicators for outcome a
                          h3("Outcome a: Grow and expand Scotland’s recovery communities into wider community settings"), 
                          
                          wellPanel(column(12,
                                           bsCollapse(id = "rosc_outcome_a", open = "Panel 1", #PanelSet id
                                                      bsCollapsePanel("a) Number and location (local authority area and setting) of recovery communities across the country.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the Scottish Recovery Consortium (SRC) Register of Affiliated Recovery Communities. 
                                                                           The SRC supports, represents and connects recovery across Scotland from grassroots to government, from individuals to international organisations. 
                                                                           The SRC works nationally across Scotland to develop and provide a variety of offerings including events, training, representation and community development."),
                                                                        p("The SRC have been working to support the development of existing and emerging recovery communities from across Scotland. 
                                                                           The SRC has prioritised work within specific localities whilst also supporting existing and emerging communities to either continue to develop their 
                                                                           community or to initiate the development of a recovery community. Recovery communities can be grouped into one of five categories:",
                                                                          tags$ul(
                                                                            tags$li("Established: Recovery Communities that are imbedded into a ROSC model within local areas, established Recovery Communities have clear policies, 
                                                                                     procedures and structures in place that offer a programme of activities for both volunteers and participants. Some of the below recovery communities 
                                                                                     are more established than others but all are growing steadily."),
                                                                            tags$li("Growing/Planning: Recovery communities that are in development, there is activity within and area and are organically growing, but at the moment aren’t 
                                                                                     imbedded within a ROSC model or have clear pathways between treatment and recovery community. Some of the areas below mentioned have groups of people 
                                                                                     with lived experience committed to developing a recovery community."),
                                                                            tags$li("Challenged/No Community: Areas that have lived experience within third sector and statutory services that have willingness to create recovery communities, 
                                                                                     but at this moment in time all recovery activities are provided by services rather than recovery communities."),
                                                                            tags$li("Lived Experience Recovery Organisations (LEROs): Models and methods are driven by mutual support, community engagement and enhancement and a commitment 
                                                                                     to individual and group wellbeing.  An organisation of lived experience committed to recovery with a focus on autonomy."),
                                                                            tags$li("Recovery Groups: Communities that provide recovery activities within the local community, many of these groups are independent of services and 
                                                                                     provide recovery support on a voluntary basis with no ties to government or local funding."))),
                                                                        p("Data were obtained via a bespoke information request raised with SRC. Percentages and numbers of Local Authorities who have a recovery community within each of the five 
                                                                           categories listed above are shown, as are the category linked to each Local Authority. Data were provided (where applicable) on the name of the 
                                                                           Recovery Community/Organisation but are not reported on in this dashboard."),
                                                                        p("Data are unavailable by when each community was started and how many meetings these communities have each year. Data are only provided for the ‘visible’ communities 
                                                                           contained within the SRC register, with no information available for ‘anonymous’ communities (for example, 12-step fellowships)."),
                                                                        p("To obtain an update for the status of these communities by Local Authority a bespoke information request will have to be raised with the SRC in the near future."),
                                                                        p(
                                                                          tags$a(href="https://scottishrecoveryconsortium.org/",
                                                                                 "More information on the SRC can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("b) Alcohol and drug partnership (ADP) investment (financial and otherwise) in local recovery communities.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the 2019/20 Alcohol and Drugs Partnership (ADP) Annual Review templates  
                                                                           but, as figures have yet to be validated, data are not reported on in this dashboard.")
                                                                      )),
                                                      bsCollapsePanel("c) Percentage and number of people in services also involved with mutual aid/peer support/recovery groups.",
                                                                      tagList(
                                                                        p("The MERRR Framework specified the ADP annual report template would be used as the mechanism to capture this information. 
                                                                          After further investigation, it became clear it would not have been possible to source these data from this template. 
                                                                          Indicator to be removed from future reporting.")))
                                           ))),
                          
                          #Indicators for outcome b
                          h3("Outcome b: Improve access to and quality of treatment services, including harm reduction and low-threshold services, other support services and community supports"), 
                          
                          wellPanel(column(12,
                                           bsCollapse(id = "rosc_outcome_b", open = "Panel 1", #PanelSet id
                                                      bsCollapsePanel("a) Drug and alcohol treatment waiting times (primary waiting time).",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the Drug and Alcohol Treatment Waiting Times (DATWT) report. 
                                                                           The Scottish Government set a target that by June 2013, 90% of people referred with their drug or alcohol problem will 
                                                                           wait no longer than three weeks for treatment that supports their recovery. This was one of the national HEAT 
                                                                           (Health improvement, Efficiency, Access, Treatment) targets, number A11. 
                                                                           This target was achieved in June 2013 and has now become a Local Delivery Plan (LDP) standard."),
                                                                        p("Information about waiting times for drug and alcohol treatment is provided by the treatment services and collected in the 
                                                                           DATWT database which went live across Scotland on 1st January 2011. The DATWT database collates information about the 
                                                                           length of time people wait for specialist drug and/or alcohol treatment after they have been referred to treatment services in Scotland."),
                                                                        p("Due to poor data completeness, figures are not available for primary waiting times (defined here as completed waits from referral to assessment). 
                                                                           Therefore, the percentages of waits completed, from referral to treatment, within one, two and three weeks are shown here from 2015/16 to 2019/20. 
                                                                           These are presented for alcohol treatment waiting times, drug treatment waiting times, and drug and alcohol treatment waiting times."),
                                                                        p("The next report will contain 2020/21 figures and is scheduled to be published in June 2021."),
                                                                        p(
                                                                          tags$a(href="https://beta.isdscotland.org/find-publications-and-data/lifestyle-and-behaviours/substance-use/national-drug-and-alcohol-treatment-waiting-times/",
                                                                                 "More information on the DATWT can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("b) Drug and alcohol treatment waiting times (secondary waiting time).",
                                                                      tagList(
                                                                        p("Due to poor data completeness, figures are not available for secondary waiting times (defined here as completed 
                                                                           waits from assessment to treatment). Indicator to be removed from future reporting."))),
                                                      bsCollapsePanel("c) Percentage of people who leave ‘treatment incomplete’ and discharge reason.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the Scottish Drug Misuse Database (SDMD) report. 
                                                                           The SDMD was set up in 1990 to collect information about people in Scotland with problematic drug use. 
                                                                           Services contributing to the SDMD include specialist drug services and some medical services. 
                                                                           Data are collected when individuals make contact with services providing tier 3 and 4 interventions 
                                                                           (i.e. structured community and residential treatment) or reinitiate contact following a gap of at least six months since last attendance."),
                                                                        p("This provides insights into drug treatment needs and the social circumstances and behaviours of people at the 
                                                                           point when they contact services for treatment. The annual report provides information on people presenting for 
                                                                           initial assessment for a new drug treatment episode at specialist drug treatment services in 2019/20."),
                                                                        p("The figures here show the percentage of discharges that were unplanned from the overall number of discharges during treatment. 
                                                                           It presents figures for both the SDMD and the Drug and Alcohol Treatment Waiting Times (DATWT) database. 
                                                                           Figures are presented from 2011/12 to 2019/20. Note that due to data quality issues no figures are presented for SDMD in 2013/14."),
                                                                        p("The next report will contain 2020/21 figures and is scheduled to be published in March 2022."),
                                                                        p(
                                                                          tags$a(href="https://beta.isdscotland.org/find-publications-and-data/lifestyle-and-behaviours/substance-use/scottish-drug-misuse-database/2-march-2021/",
                                                                                 "More information on the SDMD can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("d) Percentage of people completing treatment and discharge reason.",
                                                                      tagList(
                                                                        p("The Scottish Drug Misuse Database (SDMD) publication does not currently report on the number of people successfully completing treatment. 
                                                                           As a result, data is not presented here. Indicator to be removed from future reporting. "))),
                                                      bsCollapsePanel("e) Percentage breakdown of assessment appointment attendance (including reason for not).",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the Drug and Alcohol Treatment Waiting Times (DATWT) report. 
                                                                           The Scottish Government set a target that by June 2013, 90% of people referred with their drug or alcohol problem will 
                                                                           wait no longer than three weeks for treatment that supports their recovery. This was one of the national HEAT 
                                                                           (Health improvement, Efficiency, Access, Treatment) targets, number A11. 
                                                                           This target was achieved in June 2013 and has now become a Local Delivery Plan (LDP) standard."),
                                                                        p("Information about waiting times for drug and alcohol treatment is provided by the treatment services and collected in the 
                                                                           DATWT database which went live across Scotland on 1st January 2011. The DATWT database collates information about the 
                                                                           length of time people wait for specialist drug and/or alcohol treatment after they have been referred to treatment services in Scotland."),
                                                                        p("Individuals attending their first assessment appointment are shown here from 2011/12 to 2019/20. 
                                                                           Numbers and percentages are shown by if the individual: attended their appointment; could not attend (and service was informed); 
                                                                           did not attend (service not informed) and; had their appointment cancelled. They are presented for first alcohol assessment, 
                                                                           first drug assessment, and first drug and alcohol assessment."),
                                                                        p("Data were obtained via a bespoke information request with DATWT. 
                                                                           Another bespoke request to capture 2020/21 information for this indicator will likely have to be raised with DATWT in the near future. 
                                                                           On 1st December 2020 NHS Ayrshire & Arran, NHS Dumfries & Galloway, NHS Grampian, and NHS Western Isles began using the 
                                                                           Drug and Alcohol Information System (DAISy), a new national database for Scotland. The remaining NHS Boards transferred to DAISy in April 2021. 
                                                                           As DAISy holds data in relation to drug and alcohol treatments and waiting times from services throughout Scotland delivering 
                                                                           tier 3 and 4 interventions, this system may be used as the mechanism to capture this information from 2022 onwards."),
                                                                        p(
                                                                          tags$a(href="https://beta.isdscotland.org/find-publications-and-data/lifestyle-and-behaviours/substance-use/national-drug-and-alcohol-treatment-waiting-times/",
                                                                                 "More information on the DATWT can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("f) Percentage breakdown of first treatment appointment attendance (including reason for not).",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the Drug and Alcohol Treatment Waiting Times (DATWT) report. 
                                                                           The Scottish Government set a target that by June 2013, 90% of people referred with their drug or alcohol problem will 
                                                                           wait no longer than three weeks for treatment that supports their recovery. This was one of the national HEAT 
                                                                           (Health improvement, Efficiency, Access, Treatment) targets, number A11. 
                                                                           This target was achieved in June 2013 and has now become a Local Delivery Plan (LDP) standard."),
                                                                        p("Information about waiting times for drug and alcohol treatment is provided by the treatment services and collected in the 
                                                                           DATWT database which went live across Scotland on 1st January 2011. The DATWT database collates information about the 
                                                                           length of time people wait for specialist drug and/or alcohol treatment after they have been referred to treatment services in Scotland."),
                                                                        p("Individuals attending their first treatment appointment are shown here from 2011/12 to 2019/20. 
                                                                           Numbers and percentages are shown by if the individual: attended their appointment; could not attend (and service was informed); 
                                                                           did not attend (service not informed) and; had their appointment cancelled. They are presented for first alcohol treatments, 
                                                                           first drug treatments, and first drug and alcohol treatments."),
                                                                        p("Data were obtained via a bespoke information request with DATWT. 
                                                                           Another bespoke request to capture 2020/21 information for this indicator will likely have to be raised with DATWT in the near future. 
                                                                           On 1st December 2020 NHS Ayrshire & Arran, NHS Dumfries & Galloway, NHS Grampian, and NHS Western Isles began using the 
                                                                           Drug and Alcohol Information System (DAISy), a new national database for Scotland. The remaining NHS Boards transferred to DAISy in April 2021. 
                                                                           As DAISy holds data in relation to drug and alcohol treatments and waiting times from services throughout Scotland delivering 
                                                                           tier 3 and 4 interventions, this system may be used as the mechanism to capture this information from 2022 onwards."),
                                                                        p(
                                                                          tags$a(href="https://beta.isdscotland.org/find-publications-and-data/lifestyle-and-behaviours/substance-use/national-drug-and-alcohol-treatment-waiting-times/",
                                                                                 "More information on the DATWT can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("g) Percentage of reviews completed in line with recommendations, e.g. currently three-month and 12-month reviews.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the Scottish Drug Misuse Database (SDMD) report. 
                                                                           The SDMD was set up in 1990 to collect information about people in Scotland with problematic drug use. 
                                                                           Services contributing to the SDMD include specialist drug services and some medical services. 
                                                                           Data are collected when individuals make contact with services providing tier 3 and 4 interventions 
                                                                           (i.e. structured community and residential treatment) or reinitiate contact following a gap of at least six months since last attendance."),
                                                                        p("This provides insights into drug treatment needs and the social circumstances and behaviours of people at the 
                                                                           point when they contact services for treatment. The annual report provides information on people presenting for 
                                                                           initial assessment for a new drug treatment episode at specialist drug treatment services in 2019/20."),
                                                                        p("The figures here show the percentage of individuals that had a review completed at three months and a review completed at twelve months. 
                                                                           Figures are presented from 2011/12 to 2019/20 for three-month reviews and for 2011/12 to 2017/18 for twelve-month reviews. 
                                                                           Note that due to data quality issues no figures are presented for 2013/14 for both three and twelve month reviews."),
                                                                        p("Figures for three-month reviews were obtained from the SDMD report, with an individual counted if the review date of the follow up record was 
                                                                           between 10 and 14 weeks after the date of the initial assessment. Figures for the twelve-month reviews were obtained via a bespoke information 
                                                                           request with the SDMD, with an individual counted if the review date of the follow up record was between 50 and 54 weeks after the date of the initial assessment. 
                                                                           Caution is necessary when interpreting these figures, especially for twelve-month reviews, as they are likely to be affected by data quality and completeness issues."),
                                                                        p("Direct comparison of percentages of three-month and twelve-month reviews is not appropriate because not every individual presenting for an initial assessment might require to stay 
                                                                           in treatment for twelve months or longer."),
                                                                        p("The next report will contain 2020/21 figures for three-month reviews and is scheduled to be published in March 2022. 
                                                                           Another bespoke request to capture 2019/20 information for twelve-month reviews will likely have to be raised with SDMD in the near future."),
                                                                        p(
                                                                          tags$a(href="https://beta.isdscotland.org/find-publications-and-data/lifestyle-and-behaviours/substance-use/scottish-drug-misuse-database/2-march-2021/",
                                                                                 "More information on the SDMD can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("h) Number of ADPs that report their commissioned treatment service(s) has feedback mechanism in place, and 
                                                                evidence/examples of how lived experience is informing the development, design and delivery of services.",
                                                                    tagList(
                                                                      p("Data for this indicator were sourced from the 2019/20 Alcohol and Drugs Partnership (ADP) Annual Review templates. 
                                                                         Prior to 2019/20, data for this indicator were not routinely collected. A bespoke request was made with the Scottish Government to request 
                                                                         information for this indicator be captured as part of this template when they were circulated to ADPs at the end of 2020. 
                                                                         Information from each of the ADPs who responded were collated together by the Scottish Government."),
                                                                      p("ADPs were asked indicate which approach services used to involve lived/living experience and to mark all approaches that were applied within their ADP. 
                                                                         These approaches were listed as: Feedback/complaints process; Questionnaires/surveys; Focus groups; Lived/living experience group/forum; 
                                                                         Board Representation within services; Board Representation at ADP; or Other."),
                                                                      p("ADPs were also asked if the involvement of people with lived/living experience changed over the course of the 2019/20 financial year. 
                                                                         ADPs were asked to provide one of the following responses to this question: Improved; Stayed the same; Scaled back; or Not in place."),
                                                                      p("A total of 28 of the 31 ADPs supplied information for this indicator, with no information provided from the Aberdeenshire, Clackmannanshire or Stirling ADPs."),
                                                                      p("The next ADP Annual Review templates for the 2020/21 financial year are due to be circulated to the ADPs at the end of 2021. 
                                                                         Another bespoke request to capture information for this indicator will likely have to be raised with the Scottish Government 
                                                                         in advance of this circulation to ensure these data are captured on a consistent basis."),
                                                                      p(
                                                                        tags$a(href="https://www.gov.scot/policies/alcohol-and-drugs/partnership-working/",
                                                                               "More information on the ADP annual reporting can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("i) Number of needles/syringes supplied from injecting equipment provision (IEP) services.",
                                                                    tagList(
                                                                      p("Data were sourced from the Injecting Equipment Provision (IEP) in Scotland report. 
                                                                         IEP outlets are asked to report on the number of attendances, the number of needles and syringes and other injecting equipment 
                                                                         (referred to as ‘paraphernalia’ in previous reports) distributed and, if known, what type of drugs their clients are injecting. 
                                                                         People who inject drugs may attend IEP outlets at any time, whether or not they are undertaking specialist treatment for problematic drug use."),
                                                                      p("Following the introduction of the ISD Scottish Injecting Equipment Provision Database (ISD IEP) and NEO 
                                                                         (a commercially available database used by NHS Boards to manage their IEP activity) in April 2012, reports covering data from 2012/13 to 2016/17 were 
                                                                         largely based on a combination of information from these two data sources. As of March 2017, all mainland NHS Boards used NEO across both pharmacies and agencies. 
                                                                         Therefore, most of the data for the reported years 2017/18 onwards are extracted annually from this system by the data management team in PHS."),
                                                                      p("While the information provided in the IEP report is considered to be accurate, there may be inconsistencies in reporting between NHS Boards as, in some years, 
                                                                         individual IEP outlets only provided estimated figures and not all outlets provided answers for all questions."),
                                                                      p("Data were reported for the number of needles/syringes supplied from IEP services, with needles and syringes are defined as the total number of fixed needle syringes 
                                                                         plus any additional barrels distributed. As well as the total number nationally, figures are also provided by if they were supplied by a 
                                                                         pharmacy or agency (defined here as a non-pharmacy based IEP outlet)."),
                                                                      p("Data are available from 2010/11 to 2019/20. Data were not available to be disaggregated by age, gender or deprivation. 
                                                                         Figures are available by NHS Board but are not reported on here. For these national figures, no data were supplied by 
                                                                         NHS Western Isles during this period. NHS Orkney did not contribute to the figures until 2017/18. NHS Lanarkshire data 
                                                                         were not deemed reliable enough for inclusion in 2013/14 and 2019/20 figures supplied by this board were provisional and are subject to change."),
                                                                      p("The next report will contain 2020/21 figures and is scheduled to be published during summer 2021."),
                                                                      p(
                                                                        tags$a(href="https://beta.isdscotland.org/media/5105/2020-07-28-iep-report.pdf",
                                                                               "More information on the IEP report can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("j) Ratio of IEP outlets per estimated ‘problem drug user’ estimate.",
                                                                      tagList(
                                                                        p("Data were sourced from the Injecting Equipment Provision (IEP) in Scotland report. 
                                                                          IEP outlets are asked to report on the number of attendances, the number of needles and syringes and other injecting equipment 
                                                                          (referred to as ‘paraphernalia’ in previous reports) distributed and, if known, what type of drugs their clients are injecting. 
                                                                          People who inject drugs may attend IEP outlets at any time, whether or not they are undertaking specialist treatment for problematic drug use."),
                                                                        p("Following the introduction of the ISD Scottish Injecting Equipment Provision Database (ISD IEP) and NEO 
                                                                          (a commercially available database used by NHS Boards to manage their IEP activity) in April 2012, reports covering data from 2012/13 to 2016/17 were 
                                                                          largely based on a combination of information from these two data sources. As of March 2017, all mainland NHS Boards used NEO across both pharmacies and agencies. 
                                                                          Therefore, most of the data for the reported years 2017/18 onwards are extracted annually from this system by the data management team in PHS."),
                                                                        p("While the information provided in the IEP report is considered to be accurate, there may be inconsistencies in reporting between NHS Boards as, in some years, 
                                                                          individual IEP outlets only provided estimated figures and not all outlets provided answers for all questions."),
                                                                        p("Data were reported for the number of Problem Drug Users, number of IEP services and a ratio of 1,000 Problem Drug Users per IEP service. 
                                                                          The number of Problem Drug Users were obtained from the Prevalence of Problem Drug Use in Scotland reports (see Chapter 5) Outcome a) Indicator a) for further details)."),
                                                                        p("Data are available from 2010/11 to 2019/20. Data were not available to be disaggregated by age, gender or deprivation. 
                                                                           As the number of Problem Drug Users are not produced annually they were calculated as follows for this indicator:",
                                                                          tags$ul(
                                                                            tags$li("For years 2015/16 to 2019/20: Based on 2015/16 estimates"),
                                                                            tags$li("For years 2012/13 to 2014/15: Based on 2012/13 estimates"),
                                                                            tags$li("For years 2010/11 to 2011/12: Based on 2009/10 estimates."))),
                                                                        p("Figures are available by NHS Board but are not reported on here. For these national figures, no data were supplied by NHS Western Isles during this period (except for 2017/18). 
                                                                           NHS Orkney did not contribute to the figures until 2017/18. The 2019/20 figures supplied by the NHS Lothian board were provisional and are subject to change."),
                                                                        p("The next report will contain 2020/21 figures and is scheduled to be published during summer 2021."),
                                                                        p(
                                                                          tags$a(href="https://beta.isdscotland.org/media/5105/2020-07-28-iep-report.pdf",
                                                                                 "More information on the IEP report can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("k) Number and type of IEP outlet, e.g. pharmacy, clinic, outreach.",
                                                                      tagList(
                                                                        p("Data were sourced from the Injecting Equipment Provision (IEP) in Scotland report. 
                                                                          IEP outlets are asked to report on the number of attendances, the number of needles and syringes and other injecting equipment 
                                                                          (referred to as ‘paraphernalia’ in previous reports) distributed and, if known, what type of drugs their clients are injecting. 
                                                                          People who inject drugs may attend IEP outlets at any time, whether or not they are undertaking specialist treatment for problematic drug use."),
                                                                        p("Following the introduction of the ISD Scottish Injecting Equipment Provision Database (ISD IEP) and NEO 
                                                                          (a commercially available database used by NHS Boards to manage their IEP activity) in April 2012, reports covering data from 2012/13 to 2016/17 were 
                                                                          largely based on a combination of information from these two data sources. As of March 2017, all mainland NHS Boards used NEO across both pharmacies and agencies. 
                                                                          Therefore, most of the data for the reported years 2017/18 onwards are extracted annually from this system by the data management team in PHS."),
                                                                        p("While the information provided in the IEP report is considered to be accurate, there may be inconsistencies in reporting between NHS Boards as, in some years, 
                                                                          individual IEP outlets only provided estimated figures and not all outlets provided answers for all questions."),
                                                                        p("Data were reported for the number and percentages of pharmacy and agency IEP services, with an agency IEP service classified as: 
                                                                           Drug Treatment Service; Stand Alone; Mobile; Peripatetic outreach; or Other (Needle Replacement Service, Street Outreach and Domiciliary grouped together for these analyses)."),
                                                                        p("Data are available from 2010/11 to 2019/20. Data were not available to be disaggregated by age, gender or deprivation. Figures are available by NHS Board but are not reported on here."),
                                                                        p("Agencies may provide more than one type of service provision. As percentages are based on the number of agencies responding to the survey in the relevant year, 
                                                                           the sum of percentages may not equal 100%. Some IEP outlets submitted data which contains all zero figures, and these outlets are not included in this analysis. 
                                                                           Data supplied by NHS Lothian for 2019/20 were provisional and may be subject to a change in future reports."),
                                                                        p("The next report will contain 2020/21 figures and is scheduled to be published during summer 2021."),
                                                                        p(
                                                                          tags$a(href="https://beta.isdscotland.org/media/5105/2020-07-28-iep-report.pdf",
                                                                                 "More information on the IEP report can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("l) Naloxone reach.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the Public Health Scotland (PHS) National Naloxone Programme Scotland monitoring report. 
                                                                           Since the beginning of the National Naloxone Programme in 2011/12, Take Home Naloxone (THN) kits are issued to people at risk of opioid overdose, 
                                                                           their friends and family and service workers in order to help prevent overdose deaths."),
                                                                        p("The Scottish Government commissioned PHS to report on THN kit distribution using monitoring data supplied by NHS Boards. 
                                                                           This report presents information on the number of THN kits issued, with data presented separately for kits issued from community outlets, 
                                                                           kits issued in prisons at the point of prisoner release and kits dispensed via community prescription."),
                                                                        p("While the information provided in the IEP report is considered to be accurate, there may be inconsistencies in reporting between NHS Boards as, in some years, 
                                                                          individual IEP outlets only provided estimated figures and not all outlets provided answers for all questions."),
                                                                        p("Data are available from 2011/12 to 2018/19 for total number of THN kits issued, the number of THN kits issued to those at risk of opioid overdose 
                                                                           and the number of THN kits issued to Problem Drug Users. The total number of THN kits and number of THN kits issued to those at risk of opioid overdose 
                                                                           include first supplies, repeat supplies and spare supplies. The total number of THN kits issued to Problem Drug Users were first supplies only, 
                                                                           meaning these individuals were issued a kit for the first time."),
                                                                        p("The next report will contain figures for 2019/20 and is expected to be published at some point during 2021."),
                                                                        p(
                                                                          tags$a(href="https://beta.isdscotland.org/media/5161/2020-08-11-naloxone-report.pdf",
                                                                                 "More information on the Naloxone report can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("m) Estimated numbers of people receiving methadone.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the Scottish Public Health Observatory (ScotPHO) website. 
                                                                           These data were extracted from the Public Health Scotland Prescribing Information System and are a minimum count 
                                                                           of individuals prescribed methadone hydrochloride 1mg/ml solution, the most commonly prescribed Opioid Substitute Therapy (OST) formulation in Scotland."),
                                                                        p("The number of individuals prescribed specific drugs can be estimated using the Community Health Index (CHI) numbers captured on prescriptions. 
                                                                           However, issues associated with CHI capture for methadone prescriptions means it is not possible to provide a robust count of the number of 
                                                                           individuals prescribed methadone as an OST in Scotland. Therefore, numbers are the minimum number of people in Scotland being prescribed methadone. 
                                                                           Any changes in numbers between years may also represent differences in prescribing practices between NHS Boards."),
                                                                        p("Data are given for all prescription form types, excluding prescriptions prescribed in England or where the prescribing NHS Board is unknown, 
                                                                           captured from paid items. Data are based on prescriptions dispensed by community pharmacists, appliance suppliers and dispensing doctors only 
                                                                           and refer to prescriptions dispensed in the community, not hospitals or hospital based clinics."),
                                                                        p("Data are available from 2011/12 to 2019/20. Data are reported as the estimated (minimum) number of patients receiving methadone in Scotland."),
                                                                        p("The next update will contain figures for 2020/21 and is scheduled to be published in September 2021."),
                                                                        p(
                                                                          tags$a(href="https://www.scotpho.org.uk/behaviour/drugs/data/treatment-for-drug-misuse/",
                                                                                 "More information on ScotPHO can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("n) Prevalence of opioid substitute treatment (OST) engagement among people who inject drugs.",
                                                                      tagList(
                                                                        p("Data for people who inject drugs (PWID) were sourced from the Needle Exchange Surveillance Initiative (NESI), 
                                                                           which aims to measure and monitor injecting risk behaviours among people who inject drugs in Scotland. 
                                                                           A cross-sectional, voluntary, anonymous, bio-behavioural survey approach was used to recruit and interview PWID. 
                                                                           Trained interviewers recruited participants from selected agencies and pharmacies that provided injecting equipment; 
                                                                           these settings may also provide other harm-reduction services, such as prescribed methadone. 
                                                                           Approximately 60% of PWID approached to participate in each survey consent to do so, equating to around 2,500 PWID per sweep, 
                                                                           10–15% of the PWID population in Scotland."),
                                                                        p("Clients attending these services were invited to take part if they had injected drugs on at least one occasion either recently or in the past, 
                                                                           and if it was the first time they had participated in the current survey. Participants completed a short interviewer-administered questionnaire 
                                                                           and then provided a voluntary blood spot sample for anonymous testing for blood-borne virus markers."),
                                                                        p("Recruitment of people who have ever injected in the past, but not in the previous six months, 
                                                                           was limited to 20–30% of participants during each survey. In addition, the number of individuals reporting injection of image 
                                                                           and performance enhancing drugs (IPED) alone was capped at 5% of total recruitment at each site."),
                                                                        p("Data are available from 2010 to 2017-18. Figures are recorded for PWID who reported injecting drugs in the last six months only. 
                                                                           Figures are self-reported and split by those being in receipt of prescribed methadone in the last six months 
                                                                           or being in receipt of prescribed methadone at some point in the past."),
                                                                        p("The survey period accounts for data over two combined calendar years rather than financial year. 
                                                                           As an example, the 2017–18 figure refers to a combination of the two calendar years of 2017 and 2018 
                                                                           together rather than the financial year from 1 April 2017 to 31 March 2018."),
                                                                        p("The next report will contain 2019-20 figures and is scheduled to be published at some point during 2021."),
                                                                        p(
                                                                          tags$a(href="https://www.hps.scot.nhs.uk/web-resources-container/needle-exchange-surveillance-initiative-nesi-2008-09-to-2017-18/",
                                                                                 "More information on the NESI can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("o) Prevalence of illicit benzodiazepine use among people who inject drugs.",
                                                                      tagList(
                                                                        p("The Needle Exchange Surveillance Initiative (NESI) report was defined as the data source in the MERRR Framework to capture this information. 
                                                                          After further investigation, NESI only captures those who inject benzodiazepines, not those 
                                                                          who used benzodiazepines as well as injecting drugs. As injecting numbers are low and not based 
                                                                          on original definition, was advised by the Scottish Drugs Misuse Database (SDMD) team not to report this information within this dashboard. 
                                                                          Was advised would be able to obtain information based on the indicator definition via a bespoke 
                                                                          information request with SDMD in the near future.")))
                                           ))),
                          
                          #Indicators for outcome c
                          h3("Outcome c: Increase availability and use of advocacy by those who require it at every stage of their recovery"), 
                          
                          wellPanel(column(12,
                                           bsCollapse(id = "rosc_outcome_c", open = "Panel 1", #PanelSet id
                                                      bsCollapsePanel("a) Monitor local investment in rights-based advocacy services for people with alcohol and other drug problems.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the 2019/20 Alcohol and Drugs Partnership (ADP) Annual Review templates  
                                                                           but, as figures have yet to be validated, data are not reported on in this dashboard.")
                                                                      )),
                                                      bsCollapsePanel("b) Monitor national investment in rights-based advocacy services for people with alcohol and other drug problems.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the Scottish Government National Development Project Fund. 
                                                                        As this reports on only one fund and is not a representation of all advocacy investment across Scotland data are not reported on in this dashboard.")
                                                                      )),
                                                      bsCollapsePanel("c) SRC monitoring and evaluation of Nation Recovery Advocacy Network output.",
                                                                      tagList(
                                                                        p("This report was published in March 2021 but, as it had been delayed, 
                                                                          it was not possible to summarise and include a high level summary in this dashboard within the publication timescales. 
                                                                          A summary of information for this indicator will be published during the next iteration of this dashboard."),
                                                                        p(
                                                                          tags$a(href="https://www.gov.scot/publications/evaluation-national-development-project-fund-summary/",
                                                                                 "More information on the SRC evaluation summary can be found here", target="_blank"))))
                                           ))),
                          
                          
                          
                          #Indicators for outcome d
                          h3("Outcome d: Increase in person-centred approaches across treatment and recovery services and the range of health and social care services which work with people with alcohol and drug problems"), 
                          
                          wellPanel(column(12,
                                           bsCollapse(id = "rosc_outcome_d", open = "Panel 1", #PanelSet id
                                                      bsCollapsePanel("a) Number and percentage of ADPs self-reporting ROSC embedment.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the 2019/20 Alcohol and Drugs Partnership (ADP) Annual Review templates. 
                                                                           Prior to 2019/20, data for this indicator were not routinely collected. A bespoke request was made with the Scottish Government 
                                                                           to request information for this indicator be captured as part of this template when they were circulated to ADPs at the end of 2020. 
                                                                           Information from each of the ADPs who responded were collated together by the Scottish Government."),
                                                                        p("ADPs were asked to what extent were Recovery Oriented Systems of Care (ROSC) embedded across services within the ADP area. 
                                                                           Three responses were listed as: Fully embedded; partially embedded; and not embedded."),
                                                                        p("A total of 28 of the 31 ADPs supplied information for this indicator, with no information provided from the Aberdeenshire, Clackmannanshire or Stirling ADPs."),
                                                                        p("The next ADP Annual Review templates for the 2020/21 financial year are due to be circulated to the ADPs at the end of 2021. 
                                                                           Another bespoke request to capture information for this indicator will likely have to be raised with the Scottish Government 
                                                                           in advance of this circulation to ensure these data are captured on a consistent basis."),                                                                        
                                                                        p(
                                                                          tags$a(href="https://www.gov.scot/policies/alcohol-and-drugs/partnership-working/",
                                                                                 "More information on the ADP annual reporting can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("b) Number and percentage of ADPs reporting different treatment options available in their area.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the 2019/20 Alcohol and Drugs Partnership (ADP) Annual Review templates. 
                                                                           Prior to 2019/20, data for this indicator were not routinely collected. A bespoke request was made with the Scottish Government 
                                                                           to request information for this indicator be captured as part of this template when they were circulated to ADPs at the end of 2020. 
                                                                           Information from each of the ADPs who responded were collated together by the Scottish Government."),
                                                                        p("ADPs were asked two separate questions to indicate what treatment or screening options were in place to address drug or alcohol harms and to mark all options that applied to their ADP. 
                                                                           For drug harms these options were listed as: Same day prescribing of Opioid Substitute Therapy; Methadone; Buprenorphine sublingual; Buprenorphine depot; Diamorphine; 
                                                                           non-OST treatment options; or Other. For alcohol harms these options were listed as: Fibro scanning; Alcohol related cognitive screening (e.g. for ARBD); Community alcohol detox; 
                                                                           Inpatient alcohol detox; Alcohol hospital liaison; Access to alcohol medication (such as Antabuse or Acamprase); Arrangements for the delivery of alcohol brief interventions in 
                                                                           priority settings; Arrangements of the delivery of ABIs in non-priority settings; or Other."),
                                                                        p("A total of 28 of the 31 ADPs supplied information for this indicator, with no information provided from the Aberdeenshire, Clackmannanshire or Stirling ADPs."),
                                                                        p("The next ADP Annual Review templates for the 2020/21 financial year are due to be circulated to the ADPs at the end of 2021. 
                                                                           Another bespoke request to capture information for this indicator will likely have to be raised with the Scottish Government 
                                                                           in advance of this circulation to ensure these data are captured on a consistent basis."),                                                                        
                                                                        p(
                                                                          tags$a(href="https://www.gov.scot/policies/alcohol-and-drugs/partnership-working/",
                                                                                 "More information on the ADP annual reporting can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("c) Number of different treatment options and their providers reported by each ADP area.",
                                                                      tagList(
                                                                        p("Data are not reported for this indicator as no information is currently available by treatment providers.")
                                                                      )),
                                                      bsCollapsePanel("d) Percentage of people who have received any other interventions (as per Scottish Morbidity Record (SMR) 25b) since last review.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the Scottish Drug Misuse Database (SDMD) report. 
                                                                           The SDMD was set up in 1990 to collect information about people in Scotland with problematic drug use. 
                                                                           Services contributing to the SDMD include specialist drug services and some medical services. 
                                                                           Data are collected when individuals make contact with services providing tier 3 and 4 interventions 
                                                                           (i.e. structured community and residential treatment) or reinitiate contact following a gap of at least six months since last attendance."),
                                                                        p("This provides insights into drug treatment needs and the social circumstances and behaviours of people at the 
                                                                           point when they contact services for treatment. The annual report provides information on people presenting for 
                                                                           initial assessment for a new drug treatment episode at specialist drug treatment services in 2019/20."),
                                                                        p("Figures are shown as the percentage of individuals with any intervention recorded in follow up records from 2011/12 to 2018/19. 
                                                                           The percentage of individuals with one of the top five interventions is also recorded, with the denominator for this percentage 
                                                                           the total number of individuals who had a follow up record rather than the total number of individuals who had a 
                                                                           follow up record that had an intervention recorded only."),
                                                                        p("A total of 21 types of intervention are recorded on SDMD but the top five interventions shown here are for: 
                                                                           Structured preparatory & motivational interventions; Specialist prescribing; Structured psychosocial intervention; Mental health and; Physical health."),
                                                                        p("As individuals can have more than one intervention recorded the sum of interventions can be greater than the number of individuals recorded. 
                                                                           It is only possible to record up to five interventions during a review. 
                                                                           Therefore, is it possible some interventions may have been excluded from these analyses. 
                                                                           Note that due to data quality issues no figures are presented for SDMD in 2013/14."),
                                                                        p("Data were obtained via a bespoke information request with SDMD. Another bespoke request to capture 2019/20 
                                                                           information for this indicator will likely have to be raised with SDMD in the near future."),
                                                                        p(
                                                                          tags$a(href="https://beta.isdscotland.org/find-publications-and-data/lifestyle-and-behaviours/substance-use/scottish-drug-misuse-database/2-march-2021/",
                                                                                 "More information on the SDMD can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("e) Number and percentage of ADPs with an action plan to implement the quality principles.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the 2019/20 Alcohol and Drugs Partnership (ADP) Annual Review templates. 
                                                                           Prior to 2019/20, data for this indicator were not routinely collected. A bespoke request was made with the Scottish Government 
                                                                           to request information for this indicator be captured as part of this template when they were circulated to ADPs at the end of 2020. 
                                                                           Information from each of the ADPs who responded were collated together by the Scottish Government."),
                                                                        p("ADPs were asked if Quality Assurance arrangements were in place for the following services (with examples to include review performance 
                                                                           against targets/success indicators, clinical governance reviews, case file audits, review against delivery of the quality principles) and to 
                                                                           mark all options that applied to their ADP. For Adult Services and Children & Family Services separately (reporting on Adult Services for this indicator), 
                                                                           these options were listed as: Third Sector; Public Sector; and Other. Data for this indicator is displayed by number of ADPs who were noted as having 
                                                                           no arrangements in place or if they had one arrangement in place, two arrangements in place or three arrangements in place. 
                                                                           Data for this indicator are also displayed by number of ADPs who stated they had Third Sector, Public Sector or Other arrangement in place."),
                                                                        p("A total of 28 of the 31 ADPs supplied information for this indicator, with no information provided from the Aberdeenshire, Clackmannanshire or Stirling ADPs."),
                                                                        p("The next ADP Annual Review templates for the 2020/21 financial year are due to be circulated to the ADPs at the end of 2021. 
                                                                           Another bespoke request to capture information for this indicator will likely have to be raised with the Scottish Government 
                                                                           in advance of this circulation to ensure these data are captured on a consistent basis."),                                                                        
                                                                        p(
                                                                          tags$a(href="https://www.gov.scot/policies/alcohol-and-drugs/partnership-working/",
                                                                                 "More information on the ADP annual reporting can be found here", target="_blank"))
                                                                      ))
                                           ))),
                          
                          
                          
                          
                          #Indicators for outcome e
                          h3("Outcome e: Increase the number of people leaving services with outcomes achieved, increased recovery capital and connected to aftercare and community (of choice)"), 
                          
                          wellPanel(column(12,
                                           bsCollapse(id = "rosc_outcome_e", open = "Panel 1", #PanelSet id
                                                      bsCollapsePanel("a) Percentage of people who leave ‘treatment incomplete’ and discharge reason.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the Scottish Drug Misuse Database (SDMD) report. 
                                                                           The SDMD was set up in 1990 to collect information about people in Scotland with problematic drug use. 
                                                                           Services contributing to the SDMD include specialist drug services and some medical services. 
                                                                           Data are collected when individuals make contact with services providing tier 3 and 4 interventions 
                                                                           (i.e. structured community and residential treatment) or reinitiate contact following a gap of at least six months since last attendance."),
                                                                        p("This provides insights into drug treatment needs and the social circumstances and behaviours of people at the 
                                                                           point when they contact services for treatment. The annual report provides information on people presenting for 
                                                                           initial assessment for a new drug treatment episode at specialist drug treatment services in 2019/20."),
                                                                        p("The figures here show the percentage of discharges that were unplanned from the overall number of discharges during treatment. 
                                                                           It presents figures for both the SDMD and the Drug and Alcohol Treatment Waiting Times (DATWT) database. 
                                                                           Figures are presented from 2011/12 to 2019/20. Note that due to data quality issues no figures are presented for SDMD in 2013/14."),
                                                                        p("The next report will contain 2020/21 figures and is scheduled to be published in March 2022."),
                                                                        p(
                                                                          tags$a(href="https://beta.isdscotland.org/find-publications-and-data/lifestyle-and-behaviours/substance-use/scottish-drug-misuse-database/2-march-2021/",
                                                                                 "More information on the SDMD can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("b) Percentage of people completing treatment and discharge reason.",
                                                                      tagList(
                                                                        p("The Scottish Drug Misuse Database (SDMD) publication does not currently report on the number of people successfully completing treatment. 
                                                                           As a result, data is not presented here. Indicator to be removed from future reporting.")))
                                           ))), 
                          
                          
                          #Indicators for outcome f
                          h3("Outcome f: Reduce the often coexisting complex issues related to harmful alcohol and other drug use, e.g. housing, mental health issues, family issues and so on"), 
                          
                          wellPanel(column(12,
                                           bsCollapse(id = "rosc_outcome_f", open = "Panel 1", #PanelSet id
                                                      bsCollapsePanel("a) Percentage of people who have received any other interventions (as per SMR 25b) since last review.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the Scottish Drug Misuse Database (SDMD) report. 
                                                                           The SDMD was set up in 1990 to collect information about people in Scotland with problematic drug use. 
                                                                           Services contributing to the SDMD include specialist drug services and some medical services. 
                                                                           Data are collected when individuals make contact with services providing tier 3 and 4 interventions 
                                                                           (i.e. structured community and residential treatment) or reinitiate contact following a gap of at least six months since last attendance."),
                                                                        p("This provides insights into drug treatment needs and the social circumstances and behaviours of people at the 
                                                                           point when they contact services for treatment. The annual report provides information on people presenting for 
                                                                           initial assessment for a new drug treatment episode at specialist drug treatment services in 2019/20."),
                                                                        p("Figures are shown as the percentage of individuals with any intervention recorded in follow up records from 2011/12 to 2018/19. 
                                                                           The percentage of individuals with one of the top five interventions is also recorded, with the denominator for this percentage 
                                                                           the total number of individuals who had a follow up record rather than the total number of individuals who had a 
                                                                           follow up record that had an intervention recorded only."),
                                                                        p("A total of 21 types of intervention are recorded on SDMD but the top five interventions shown here are for: 
                                                                           Structured preparatory & motivational interventions; Specialist prescribing; Structured psychosocial intervention; Mental health and; Physical health."),
                                                                        p("As individuals can have more than one intervention recorded the sum of interventions can be greater than the number of individuals recorded. 
                                                                           It is only possible to record up to five interventions during a review. 
                                                                           Therefore, is it possible some interventions may have been excluded from these analyses. 
                                                                           Note that due to data quality issues no figures are presented for SDMD in 2013/14."),
                                                                        p("Data were obtained via a bespoke information request with SDMD. Another bespoke request to capture 2019/20 
                                                                           information for this indicator will likely have to be raised with SDMD in the near future."),
                                                                        p(
                                                                          tags$a(href="https://beta.isdscotland.org/find-publications-and-data/lifestyle-and-behaviours/substance-use/scottish-drug-misuse-database/2-march-2021/",
                                                                                 "More information on the SDMD can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("b) Percentage change in accommodation status from any other classification to ‘owner/rented – stable’ (i.e. secure) and vice versa.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the Scottish Drug Misuse Database (SDMD) report. 
                                                                           The SDMD was set up in 1990 to collect information about people in Scotland with problematic drug use. 
                                                                           Services contributing to the SDMD include specialist drug services and some medical services. 
                                                                           Data are collected when individuals make contact with services providing tier 3 and 4 interventions 
                                                                           (i.e. structured community and residential treatment) or reinitiate contact following a gap of at least six months since last attendance."),
                                                                        p("This provides insights into drug treatment needs and the social circumstances and behaviours of people at the 
                                                                           point when they contact services for treatment. The annual report provides information on people presenting for 
                                                                           initial assessment for a new drug treatment episode at specialist drug treatment services in 2019/20."),
                                                                        p("Figures are shown for the number and percentage of individuals’ accommodation status at the time of assessment from 2009/10 to 2019/20. 
                                                                           Figures are shown for those individuals who stated their accommodation status was: Owned/rented; Supported (Supported accommodation and residential rehab); 
                                                                           Homeless (temporary, unstable, hostel, and roofless) or Other. Note that due to data quality issues no figures are presented for SDMD in 2013/14."),
                                                                        p("The next report will contain 2020/21 figures and is scheduled to be published in March 2022."),
                                                                        p(
                                                                          tags$a(href="https://beta.isdscotland.org/find-publications-and-data/lifestyle-and-behaviours/substance-use/scottish-drug-misuse-database/2-march-2021/",
                                                                                 "More information on the SDMD can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("c) Prevalence of homelessness among people who inject drugs.",
                                                                      tagList(
                                                                        p("Data for people who inject drugs (PWID) were sourced from the Needle Exchange Surveillance
                                                                           Initiative (NESI), which aims to measure and monitor injecting risk behaviours among people who
                                                                           inject drugs in Scotland. A cross-sectional, voluntary, anonymous, bio-behavioural survey approach
                                                                           was used to recruit and interview PWID. Trained interviewers recruited participants from selected
                                                                           agencies and pharmacies that provided injecting equipment; these settings may also provide other
                                                                           harm-reduction services, such as prescribed methadone. Approximately 60% of PWID approached
                                                                           to participate in each survey consent to do so, equating to around 2,500 PWID per sweep, 10–15%
                                                                           of the PWID population in Scotland."),
                                                                        p("Clients attending these services were invited to take part if they had injected drugs on at least one
                                                                           occasion either recently or in the past, and if it was the first time they had participated in the current
                                                                           survey. Participants completed a short interviewer-administered questionnaire and then provided
                                                                           a voluntary blood spot sample for anonymous testing for blood-borne virus markers."),
                                                                        p("Recruitment of people who have ever injected in the past, but not in the previous six months,
                                                                           was limited to 20–30% of participants during each survey. In addition, the number of individuals
                                                                           reporting injection of image and performance enhancing drugs (IPED) alone was capped at
                                                                           5% of total recruitment at each site."),
                                                                        p("Figures are recorded for PWID who responded ‘Yes’ or ‘No’ asked if they had been homeless at some point within the last six months 
                                                                           (defined as living in a hostel for the homeless, having no fixed abode, or living on the streets). 
                                                                           No information is available by what form of homeless PWID regarded themselves as or how long PWID stated they were homeless for."),
                                                                        p("Data are available for 2017-18 only. The survey period accounts for data over two combined calendar years rather than financial year.
                                                                          As an example, the 2017–18 figure refers to a combination of the two calendar years of 2017
                                                                          and 2018 together rather than the financial year from 1 April 2017 to 31 March 2018."),
                                                                        p("The next report will contain 2019-20 figures and is scheduled to be published at some point during 2021."),
                                                                        p(
                                                                          tags$a(href="https://www.hps.scot.nhs.uk/web-resources-container/needle-exchange-surveillance-initiative-nesi-2008-09-to-2017-18/",
                                                                                 "More information on the NESI report can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("d) Percentage of those using tobacco referred to cessation support.",
                                                                      tagList(
                                                                        p("It has not been possible to source data for this indicator as the Drugs and Alcohol Information System (DAISy), 
                                                                          where information for this indicator will be sourced, only went live on 1st December 2020. 
                                                                          Not expected to have a robust data extract for this indicator until 2022 at the earliest."))),
                                                      bsCollapsePanel("e) Percentage of clients where routine enquiry undertaken regarding childhood and domestic abuse.",
                                                                      tagList(
                                                                        p("It has not been possible to source data for this indicator as the Drugs and Alcohol Information System (DAISy), 
                                                                          where information for this indicator will be sourced, only went live on 1st December 2020. 
                                                                          Not expected to have a robust data extract for this indicator until 2022 at the earliest.")))
                                           ))),
                          
                          tags$a(href = '#top',  
                                 icon("circle-arrow-up", lib= "glyphicon"),"Back to top of page")
                          
                 ),
                 
                 
                 
                 
                 
                 #New tab and page will provide information about chapter 3
                 tabPanel("3) A Public Health Approach to Justice", icon = icon("balance-scale"),
                          
                          h2("Chapter 3 - A Public Health Approach to Justice"), 
                          
                          p(
                            "The overall outcome for this part of the strategy is for vulnerable people to be diverted from the
                               justice system wherever possible and to ensure those within justice settings are fully supported.
                               The original TOC model identifies interim outcomes to achieve this overall outcome. These are:",
                            tags$ul(
                              tags$li("Improve treatment in justice settings in line with the appropriate standards and guidelines
                                 to increase the effective and consistent use of justice throughcare services."),
                              tags$li("Increase use of diversion from prosecution and alternatives to custody wherever appropriate
                                 to increase the number of people who come into contact with justice agencies and receive
                                 the right support from the appropriate services and sources.")),
                            "Please select one of the indicator titles below for information on the indicator data source, caveats and next update."
                          ),
                          
                          
                          hr(style = "border-top: 2px solid #000000;"),
                          
                          #Indicators for outcome a
                          h3("Outcome a: Improve treatment in justice settings in line with the appropriate standards and guidelines"), 
                          
                          wellPanel(column(12,
                                           bsCollapse(id = "justice_outcome_a", open = "Panel 1", #PanelSet id
                                                      bsCollapsePanel("a) Prison – Percentage of people identified as requiring drug treatment via urine analysis.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the Scottish Prison Service (SPS) prisoner survey. 
                                                                           The biennial survey is undertaken in each of the 15 Scottish prisons and involves a sample of those held in custody in Scotland. 
                                                                           The survey intends to make use of respondents’ perceptions of service-delivery and service-quality in business planning; 
                                                                           provide those in custody with an opportunity to comment on a range of issues that impact on their experience in prison; 
                                                                           allow staff to get a better understanding of how the halls they manage compare to equivalent halls and in so doing 
                                                                           to provide a tangible way to help share items of ‘best practice’; and the SPS, through repetition of the same questions, 
                                                                           to track progress (or the lack of it) across the various dimensions that are included in the Survey."),
                                                                        p("Data are available biennially from 2011 to 2019. 
                                                                           As part of this survey, respondents were asked a series of general questions about drug treatment both before and during their current prison sentence. 
                                                                           Answers have been categorised as percentage of respondents who answered ‘Yes; when asked if they had: Been assessed for drug use; 
                                                                           taken help if they were offered it and; the chance to receive treatment. 
                                                                           There are no figures available by if the respondent had been identified as requiring treatment via urine analysis."),
                                                                        p("The next SPS prisoner survey will contain 2021 figures and is scheduled to be published in 2022."),
                                                                        p(
                                                                          tags$a(href="https://www.sps.gov.uk/Corporate/Publications/Publications.aspx",
                                                                                 "More information on the SPS prisoner survey can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("b) Prison – Number of people referred for drug treatment.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the Drug and Alcohol Treatment Waiting Times (DATWT) report. 
                                                                           The Scottish Government set a target that by June 2013, 90% of people referred with their drug or alcohol problem will 
                                                                           wait no longer than three weeks for treatment that supports their recovery. This was one of the national HEAT 
                                                                           (Health improvement, Efficiency, Access, Treatment) targets, number A11. 
                                                                           This target was achieved in June 2013 and has now become a Local Delivery Plan (LDP) standard."),
                                                                        p("Information about waiting times for drug and alcohol treatment is provided by the treatment services and collected in the 
                                                                           DATWT database which went live across Scotland on 1st January 2011. The DATWT database collates information about the 
                                                                           length of time people wait for specialist drug and/or alcohol treatment after they have been referred to treatment services in Scotland."),
                                                                        p("Data on the number of people within prison referred to drug treatment are available from 2011/12 to 2018/19. 
                                                                           Data are available for 2019/20 but are not shown here due to poor data completeness."),
                                                                        p("Data were obtained via a bespoke information request with DATWT. 
                                                                           Another bespoke request to capture complete 2019/20 information for this indicator will likely have to be raised with DATWT in the near future."),
                                                                        p(
                                                                          tags$a(href="https://beta.isdscotland.org/find-publications-and-data/lifestyle-and-behaviours/substance-use/national-drug-and-alcohol-treatment-waiting-times/",
                                                                                 "More information on the DATWT can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("c) Prison – Drug and alcohol treatment waiting times (access).",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the Drug and Alcohol Treatment Waiting Times (DATWT) report. 
                                                                           The Scottish Government set a target that by June 2013, 90% of people referred with their drug or alcohol problem will 
                                                                           wait no longer than three weeks for treatment that supports their recovery. This was one of the national HEAT 
                                                                           (Health improvement, Efficiency, Access, Treatment) targets, number A11. 
                                                                           This target was achieved in June 2013 and has now become a Local Delivery Plan (LDP) standard."),
                                                                        p("Information about waiting times for drug and alcohol treatment is provided by the treatment services and collected in the 
                                                                           DATWT database which went live across Scotland on 1st January 2011. The DATWT database collates information about the 
                                                                           length of time people wait for specialist drug and/or alcohol treatment after they have been referred to treatment services in Scotland."),
                                                                        p("Due to poor data completeness, figures are not available for access waiting times (defined here as completed waits from referral to assessment). 
                                                                           Therefore, the percentages of waits completed within prison, from referral to treatment, within one, two and three weeks are shown here from 2015/16 to 2019/20. 
                                                                           These are presented for alcohol treatment waiting times, drug treatment waiting times, and drug and alcohol treatment waiting times."),
                                                                        p("The next report will contain 2020/21 figures and is scheduled to be published in June 2021."),
                                                                        p(
                                                                          tags$a(href="https://beta.isdscotland.org/find-publications-and-data/lifestyle-and-behaviours/substance-use/national-drug-and-alcohol-treatment-waiting-times/",
                                                                                 "More information on the DATWT can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("d) Prison – Drug and alcohol treatment waiting times (secondary waiting time).",
                                                                      tagList(
                                                                        p("Due to poor data completeness, figures are not available for secondary waiting times (defined here as completed waits from assessment to treatment). 
                                                                          Indicator to be removed from future reporting."))),
                                                      bsCollapsePanel("e) Prison – Percentage (of those identified via screening) and number of people receiving treatment during sentence).",
                                                                      tagList(
                                                                        p("Data for the first part of this indicator were sourced from the Scottish Prison Service (SPS) prisoner survey. 
                                                                           The biennial survey is undertaken in each of the 15 Scottish prisons and involves a sample of those held in custody in Scotland. 
                                                                           The survey intends to make use of respondents’ perceptions of service-delivery and service-quality in business planning; 
                                                                           provide those in custody with an opportunity to comment on a range of issues that impact on their experience in prison; allow staff 
                                                                           to get a better understanding of how the halls they manage compare to equivalent halls and in so doing to provide a tangible way to 
                                                                           help share items of ‘best practice’; and the SPS, through repetition of the same questions, to track progress (or the lack of it) 
                                                                           across the various dimensions that are included in the Survey."),
                                                                        p("Data for the first part of this indicator (identified as needing treatment) are available biennially from 2011 to 2019. 
                                                                           As part of this survey, respondents were asked a series of general questions about drug treatment both before and during their current prison sentence. 
                                                                           Answers have been categorised as percentage of respondents who answered ‘Yes; when asked if they had received help/treatment for drug use during my sentence. 
                                                                           There are no figures available by if the respondent had been identified as requiring treatment via screening."),
                                                                        p("Data for this indicator were sourced from the Drug and Alcohol Treatment Waiting Times (DATWT) report. 
                                                                           The Scottish Government set a target that by June 2013, 90% of people referred with their drug or alcohol problem will wait no longer than three weeks for treatment that supports their recovery. 
                                                                           This was one of the national HEAT (Health improvement, Efficiency, Access, Treatment) targets, number A11. This target was achieved in June 2013 and has now become a Local Delivery Plan (LDP) standard."),
                                                                        p("Information about waiting times for drug and alcohol treatment is provided by the treatment services and collected in the DATWT database which went live across Scotland on 1st January 2011. 
                                                                           The DATWT database collates information about the length of time people wait for specialist drug and/or alcohol treatment after they have been referred to treatment services in Scotland."),
                                                                        p("Data for the second part of this indicator are available annually from 2015/16 to 2019/20. Numbers of waits completed, from referral to treatment, for each financial year, are shown here. 
                                                                           These are presented for alcohol treatment, drug treatment, and drug and alcohol treatment."),
                                                                        p("The next SPS prisoner survey will contain 2021 figures and is scheduled to be published in 2022. The next DATWT report will contain 2020/21 figures and is scheduled to be published in June 2021."),
                                                                        p(
                                                                          tags$a(href="https://www.sps.gov.uk/Corporate/Publications/Publications.aspx",
                                                                                 "More information on the SPS prisoner survey can be found here", target="_blank")),
                                                                        p(
                                                                          tags$a(href="https://beta.isdscotland.org/find-publications-and-data/lifestyle-and-behaviours/substance-use/national-drug-and-alcohol-treatment-waiting-times/",
                                                                                 "More information on the DATWT can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("f) Prison – Percentage and number of people completing treatment.",
                                                                      tagList(
                                                                        p("The MERRR Framework specified Drug and Alcohol Treatment Waiting Times (DATWT) would be used as the mechanism to capture this information. 
                                                                           After further investigation, was referred to the Scottish Drug Misuse Database (SDMD) team and it became clear it would not have been possible to source these data from either source."),
                                                                        p("Advised not to use SDMD due to complexities around the concept of treatment completion from prison and that categories in the relevant variables could be interpreted in different ways. 
                                                                           Advised that from 2022 onwards the Drug and Alcohol Information System (DAISy) could be used as a mechanism to capture this information."),
                                                                        )),
                                                      bsCollapsePanel("g) Addiction prevalence estimate in prison population.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the Scottish Prison Service (SPS) Addiction Prevalence Testing (APT) statistics and were sourced from the Scottish Public Health Observatory (ScotPHO) website."),
                                                                        p("The APT was conducted across all Scottish prisons annually. During one month of the year, prisoners arriving in custody were voluntarily tested for the presence of illegal or illicit drugs. 
                                                                           Similarly, those leaving custody during the month were tested to assess progress towards the 'reduced or stabilised' offender outcome."),
                                                                        p("The report identifies the percentage and number of individuals who tested positive for any drug between 2009/10 to 2018/19. 
                                                                           The data excludes drugs prescribed as part of a treatment programme, but does include the illicit use of prescription drugs. 
                                                                           Data are presented for the percentage of positive drugs test and both reception and liberation for any drug, and the top four drugs which include; 
                                                                           Cannabis; Benzodiazepines; Opiates and Cannabis. Other drugs such as Amphetamines and Methamphetamines are included in the APT statistics, but are not presented here."),
                                                                        p("A small discrepancy exists between the total column numbers displayed for all prisons and total column numbers that would be derived from separately adding numbers from each prison for a small number of comparisons. 
                                                                           To ensure consistency with the Public Health Scotland Prison Health Information Dashboard numbers presented here are based on the number that would be derived from addition."),
                                                                        p(
                                                                          tags$a(href="https://beta.isdscotland.org/find-publications-and-data/lifestyle-and-behaviours/substance-use/prison-health-information-dashboard/",
                                                                               "This dashboard can be accessed here", target="_blank")),
                                                                        p("In 2019, it was acknowledged by the SPS that the APT process did not take account of the changing trends in drug use, and this was limiting the impact of APT in monitoring substance use and associated interventions. 
                                                                           Due to this, SPS suspended APT to enable a review to be carried out with a view to improving the validity of testing and its connection to subsequent monitoring, intervention and action. A revised APT process is anticipated to commence in late 2021."),
                                                                        p("The SPS did not carry out any APT in 2019 while they reviewed the process. 
                                                                           The next publication date is currently unknown."),
                                                                        p(
                                                                          tags$a(href="https://www.scotpho.org.uk/behaviour/drugs/data/availability-and-prevalence/",
                                                                                 "More information on the SPS APT can be found here", target="_blank"))
                                                                      )),
                                                      
                                                      bsCollapsePanel("h) Inspecting and Monitoring: Standard 9: Health and Wellbeing.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the Her Majesty’s Inspectorate of Prisons for Scotland (HMIPS). 
                                                                           They are required to inspect the 15 prisons across Scotland in order to establish the treatment of, and the conditions for prisoners and to report publicly on the findings."),
                                                                        p("The most recent HMIPS annual report provides a summary of inspections ratings against a total of ten standards for each prison inspected:",
                                                                          tags$ul(
                                                                            tags$li("Standard 1: Lawful and transparent custody"),
                                                                            tags$li("Standard 2: Decency"),
                                                                            tags$li("Standard 3: Personal Safety"),
                                                                            tags$li("Standard 4: Health and wellbeing"),
                                                                            tags$li("Standard 5: Effective, courteous and humane exercise of authority"),
                                                                            tags$li("Standard 6: Respect, autonomy and protection against mistreatment"),
                                                                            tags$li("Standard 7: Purposeful activity"),
                                                                            tags$li("Standard 8: Transitions from custody to life in the community"),
                                                                            tags$li("Standard 9: Equality, dignity and respect"),
                                                                            tags$li("Standard 10: Organisational effectiveness "))),
                                                                        p("The performance of the prison against each of these standards is rated as: Good (practice worthy of sharing); Satisfactory; Generally acceptable (some improvements required); 
                                                                           Poor (accompanied by statement of what needs to be addressed); Unacceptable (requires immediate attention); or Not applicable. 
                                                                           A number of prisons were inspected each financial year, with annual reports available from 2016-17 (for Barlinnie, Kilmarnock and Edinburgh prisons), 
                                                                           2017-18 (for Low Moss, Shotts, Inverness and Greenock prisons), 2018-19 (for Perth, Addiewell, Polmont (Young Offenders Institute) and Grampian (Young Offenders Institute) prisons) 
                                                                           and 2019-20 (for Glenochill, Barlinnie, Edinburgh and Dumfries prisons). Data are also shown for all years and all prisons rather 
                                                                           than for each individual financial year or each individual prison. Data are shown here for Standard 4 only, 
                                                                           with numbers and percentages grouped by how the prisons performed against this standard."),
                                                                        p("The next annual report is scheduled to be published late 2021."),
                                                                        p(
                                                                          tags$a(href="https://www.prisonsinspectoratescotland.gov.uk/publications/inspecting-and-monitoring-standard-9-health-and-wellbeing?page=1",
                                                                                 "More information on HMIPS Standard 4 can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("i) Number of Alcohol Brief Interventions (ABI) undertaken in justice settings (prison, police custody, other).",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from Public Health Scotland (PHS). 
                                                                           NHS Boards are required to collect and collate figures on Alcohol Brief Interventions (ABIs) delivered locally and submit these on a quarterly basis to PHS. 
                                                                           Once PHS have received the four quarterly submissions within a financial year these data are verified with NHS Boards."),
                                                                        p("ABIs are delivered in a variety of settings and the method of recording and route of data submissions for local collation may vary between setting and between the different NHS Boards. 
                                                                           Since 2012/13, NHS Boards were allowed to submit data on ABIs delivered out with the three priority settings of primary care, A&E and antenatal. 
                                                                           By the end of 2015/16, all NHS Boards were submitting at least some data from the wider settings."),
                                                                        p("Data are available from 2015/16 to 2019/20. Data for ABIs delivered in Criminal Justice settings are shown here for total and split by Custody Suites; 
                                                                           Prisons; Social Work and the Police. NHS Grampian provided only aggregate figures for Wider Settings. Therefore, NHS Grampian was omitted for Criminal Justice figures."),
                                                                        p("In response to the COVID-19 pandemic, NHS Boards in Scotland had to alter their service delivery. 
                                                                           This also affected the delivery and reporting of ABIs in priority and wider settings and will have had some impact on ABI provision and recording. 
                                                                           The numbers of ABIs delivered and reported during the final quarter of financial year 2019/20 are lower compared to the same quarter in the previous year, 
                                                                           though it is not currently possible to identify the extent that COVID-19 countermeasures may have contributed to this reduction."),
                                                                        p("The next report will contain 2020/21 figures and is scheduled to be published in June 2021."),
                                                                        p(
                                                                          tags$a(href="https://beta.isdscotland.org/find-publications-and-data/lifestyle-and-behaviours/substance-use/alcohol-brief-interventions/",
                                                                                 "More information on ABIs can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("j) Number of drug-related deaths in the six months following prison release.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the National Drug-Related Deaths Database (NDRDD) Scotland Report. 
                                                                           This database was held by Information Services Division (ISD, now Public Health Scotland (PHS))."),
                                                                        p("The NDRDD was established to collect detailed information regarding the nature and circumstances of people who had a Drug-Related Death (DRD) in Scotland. 
                                                                           The NDRDD analyses deaths in further detail to help provide insights into the lives of those who died and highlight potential areas for intervention."),
                                                                        p("The National Records of Scotland (NRS) publish annual figures on the number of DRDs. 
                                                                           The figures produced by both NRS and NRDDD will not match as the NDRDD uses the date of death to allocate deaths to a particular year whereas NRS use the date the death was registered. 
                                                                           However, as all deaths in Scotland must be registered within eight days, this only affects deaths occurring at the end of each calendar year."),
                                                                        p("DRDs in Scotland are recorded and examined by Critical Incident Monitoring Groups who collaborate with the police and Procurator Fiscal to identify cases in their NHS Board area. 
                                                                           On completion of the post mortem examination, the Critical Incident Monitoring Group and local Data Collection Co-ordinator decide if the case matches the inclusion criteria for the NDRDD and, 
                                                                           if it does, a record is submitted."),
                                                                        p("NDRDD data collection was designed to collect a wide range of data on individuals’ health and social circumstances and the circumstances of their death. 
                                                                           Information is collected from a range of sources including the Scottish Prison Service (SPS), Scottish Ambulance Service (SAS), drug treatment services, GPs and hospitals."),
                                                                        p("Data are available from 2009 to 2016. Data were reported as the number and of DRDs that occurred within six months from release from prison, 
                                                                           and the percentage of all deaths that occurred within six months from release from prison that were drug-related. Data are not available to be disaggregated by age or deprivation. 
                                                                           Data are available by gender but are not shown here."),
                                                                        p("The next publication will contain figures for 2017 and 2018 and is scheduled to be published during summer 2021."),
                                                                        p(
                                                                          tags$a(href="https://www.isdscotland.org/Health-Topics/Drugs-and-Alcohol-Misuse/Publications/2018-06-12/2018-06-12-NDRDD-Report.pdf",
                                                                                 "More information the NDRDD can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("k) Number of drug-related deaths following police custody release.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the National Drug-Related Deaths Database (NDRDD) Scotland Report. 
                                                                           This database was held by Information Services Division (ISD, now Public Health Scotland (PHS))."),
                                                                        p("The NDRDD was established to collect detailed information regarding the nature and circumstances of people who had a Drug-Related Death (DRD) in Scotland. 
                                                                           The NDRDD analyses deaths in further detail to help provide insights into the lives of those who died and highlight potential areas for intervention."),
                                                                        p("The National Records of Scotland (NRS) publish annual figures on the number of DRDs. 
                                                                           The figures produced by both NRS and NRDDD will not match as the NDRDD uses the date of death to allocate deaths to a particular year whereas NRS use the date the death was registered. 
                                                                           However, as all deaths in Scotland must be registered within eight days, this only affects deaths occurring at the end of each calendar year."),
                                                                        p("DRDs in Scotland are recorded and examined by Critical Incident Monitoring Groups who collaborate with the police and Procurator Fiscal to identify cases in their NHS Board area. 
                                                                           On completion of the post mortem examination, the Critical Incident Monitoring Group and local Data Collection Co-ordinator decide if the case matches the inclusion criteria for the NDRDD and, 
                                                                           if it does, a record is submitted."),
                                                                        p("NDRDD data collection was designed to collect a wide range of data on individuals’ health and social circumstances and the circumstances of their death. 
                                                                           Information is collected from a range of sources including the Scottish Prison Service (SPS), Scottish Ambulance Service (SAS), drug treatment services, GPs and hospitals."),
                                                                        p("Data are available from 2009 to 2016. Data were reported as the number and of DRDs that occurred within six months from release from police custody, 
                                                                           and the percentage of all deaths that occurred within six months from release from police custody that were drug-related. Data are not available to be disaggregated by age or deprivation. 
                                                                           Data are available by gender but are not shown here."),
                                                                        p("The next publication will contain figures for 2017 and 2018 and is scheduled to be published during summer 2021."),
                                                                        p(
                                                                          tags$a(href="https://www.isdscotland.org/Health-Topics/Drugs-and-Alcohol-Misuse/Publications/2018-06-12/2018-06-12-NDRDD-Report.pdf",
                                                                                 "More information the NDRDD can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("l) Number of drug-related deaths while in prison.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the Scottish Prison Service (SPS) website. 
                                                                           The SPS are legally obliged to publish information regarding all deaths in custody in calendar years and publish this information on a quarterly basis."),
                                                                        p("From 2019 onwards, there was a change in methodology so the SPS figures now include the ‘medical certificate of cause of death’ 
                                                                           (MCCD) rather than just the cause of death detailed in the Fatal Accident Inquiry (FAI) determination. 
                                                                           Therefore, prior to 2019, deaths captured in this dashboard are ones recorded as listed as being ‘Drug Related’ or ‘Overdose’. 
                                                                           From 2019 onwards, deaths captured in this dashboard listed as being ones where intoxication, drugs, or specific types of drugs were recorded on the MCCD, 
                                                                           even if more than one cause of death was mentioned on this certificate:",
                                                                          tags$ul(
                                                                            tags$li("In 2019, two drug-related deaths were recorded. 
                                                                                     One was listed as ‘Methadone and quetiapine intoxication’ and the other was listed as ‘Prescription medication toxicity with agonal aspiration of vomitus’."),
                                                                            tags$li("In 2020, seven drug-related deaths were recorded. 
                                                                                     Two were listed as ‘Multidrug toxicity’ and the remainder were listed as ‘Combined adverse effects of Etizolam & Buprenorphine’, 
                                                                                     ‘Cocaine, flubromazolam and synthetic cannaboid receptor against intoxication’, ‘Combined adverse effects of Etizolam, 4F-MDMB-BINACA, 5F-MDMB-PICA and Tramadol’,
                                                                                     'Etizolam, Methadone and amitriptyline intoxication' and 'Heroin and Methadone intoxication'."))),
                                                                        p("Data reported in this dashboard are the number of individuals who died of a drug-related death while in prison in Scotland. 
                                                                           Data are available from 2010 to 2020. There are currently eleven prisoner deaths recorded as ‘Awaiting determination’ (one death from 2017 and ten deaths from 2018) 
                                                                           and three prisoner deaths recorded as ‘Unascertained’ (two deaths from 2017 and one death from 2020). 
                                                                           Therefore, these figures may be revised in future dashboard updates."),
                                                                        p("The next update will contain figures for 2021 and is scheduled to be published in February 2022."),
                                                                        p(
                                                                          tags$a(href="https://www.sps.gov.uk/Corporate/Information/PrisonerDeaths.aspx",
                                                                                 "More information on SPS drug-related deaths in prison can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("m) Percentage of people transitioning from prison to community treatment without interruption to care.",
                                                                      tagList(
                                                                        p("It has not been possible to source data for this indicator as the Scottish Drugs Misuse Database team did not 
                                                                          recommend reporting it due to poor completeness of data. Indicator to be removed from future reporting.")))
                                           ))),
                          
                          #Indicators for outcome b
                          h3("Outcome b: Increase use of diversion from prosecution and alternatives to custody wherever appropriate"), 
                          
                          wellPanel(column(12,
                                           bsCollapse(id = "justice_outcome_b", open = "Panel 1", #PanelSet id
                                                      bsCollapsePanel("a) Number of people diverted from prosecution and to drug treatment/education.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the Scottish Government Crime & Justice Social Work (CJSW) statistics report. 
                                                                           This publication presents national-level information on criminal justice social work activity in Scotland. 
                                                                           The report includes data on criminal justice social work services and social work orders, as well as characteristics of the individuals involved."),
                                                                        p("Figures in this bulletin are extracted from live criminal justice social work information management systems and 
                                                                           may differ slightly from those published previously as local administrative systems are updated. 
                                                                           They therefore reflect any changes to social work activity that may have resulted from the change to the presumption against short custodial sentences."),
                                                                        p("Diversion from prosecution schemes have been in existence in Scotland since the early 1980s and 
                                                                           aim to provide support and advice in relation to the underlying causes of offending, such as substance use. 
                                                                           In the late 1990s, the Scottish Office provided funding for a number of pilot schemes, which were rolled out across Scotland in 2000. 
                                                                           Historically, diversion involves relatively low volumes compared to other fiscal disposals such as fines and warnings, or court proceedings."),
                                                                        p("Data are available from 2014-15 to 2019-20 and figures represent financial years. 
                                                                           It is not possible to disaggregate this indicator by age, gender or deprivation."),
                                                                        p("The next CJSW report will contain 2020-21 figures and is scheduled to be published in March 2022."),
                                                                        p(
                                                                          tags$a(href="https://www.gov.scot/publications/criminal-justice-social-work-statistics-scotland-2019-20/pages/1/",
                                                                                 "More information on the CJSW can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("b) Number of people diverted from prosecution and to alcohol treatment programmes.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the Scottish Government Crime & Justice Social Work (CJSW) statistics report. 
                                                                           This publication presents national-level information on criminal justice social work activity in Scotland. 
                                                                           The report includes data on criminal justice social work services and social work orders, as well as characteristics of the individuals involved."),
                                                                        p("Figures in this bulletin are extracted from live criminal justice social work information management systems and 
                                                                           may differ slightly from those published previously as local administrative systems are updated. 
                                                                           They therefore reflect any changes to social work activity that may have resulted from the change to the presumption against short custodial sentences."),
                                                                        p("Diversion from prosecution schemes have been in existence in Scotland since the early 1980s and aim to provide support and advice in relation to the 
                                                                           underlying causes of offending, such as substance use. In the late 1990s, the Scottish Office provided funding for a number of pilot schemes, 
                                                                           which were rolled out across Scotland in 2000. Historically, diversion involves relatively low volumes compared to other fiscal disposals such as fines and warnings, 
                                                                           or court proceedings."),
                                                                        p("Data are available from 2014-15 to 2019-20 and figures represent financial years. 
                                                                           It is not possible to disaggregate this indicator by age, gender or deprivation."),
                                                                        p("The next CJSW report will contain 2020-21 figures and is scheduled to be published in March 2022."),
                                                                        p(
                                                                          tags$a(href="https://www.gov.scot/publications/criminal-justice-social-work-statistics-scotland-2019-20/pages/1/",
                                                                                 "More information on the CJSW can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("c) Number of people diverted from prison custody via Drug Treatment and Testing Order.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the Scottish Government Crime & Justice Social Work (CJSW) statistics report. 
                                                                           This publication presents national-level information on criminal justice social work activity in Scotland. 
                                                                           The report includes data on criminal justice social work services and social work orders, as well as characteristics of the individuals involved."),
                                                                        p("Figures in this bulletin are extracted from live criminal justice social work information management systems and 
                                                                           may differ slightly from those published previously as local administrative systems are updated. 
                                                                           They therefore reflect any changes to social work activity that may have resulted from the change to the presumption against short custodial sentences."),
                                                                        p("The drug treatment and testing order (DTTO) is a high tariff disposal for people with more serious substance use related to their offending, 
                                                                           who might otherwise receive a custodial sentence. This order includes the requirement for regular reviews by the court and that the person 
                                                                           consent to frequent random drug tests throughout the lifetime of the order. On the basis of these regular reviews, the judiciary may, among 
                                                                           other courses of action, vary the conditions of the order (such as the frequency of testing, the type of treatment or the frequency of attendance at treatment), 
                                                                           revoke the order on the basis that satisfactory progress has been made or, in the event of non-compliance, revoke the order and re-sentence the person for the original offence. 
                                                                           DTTOs were rolled out across Scotland in phases between 1999 and 2002. They are available to all courts apart from justice of the peace courts. 
                                                                           In addition, the less intensive DTTO II was introduced more recently in the Highland and Lothian areas (apart from West Lothian) 
                                                                           for people committing lower tariff offences at a relatively early stage in their lives."),
                                                                        p("Data are available from 2014-15 to 2019-20 and figures represent financial years. 
                                                                           It is not possible to disaggregate this indicator by age, gender or deprivation."),
                                                                        p("The next CJSW report will contain 2020-21 figures and is scheduled to be published in March 2022."),
                                                                        p(
                                                                          tags$a(href="https://www.gov.scot/publications/criminal-justice-social-work-statistics-scotland-2019-20/pages/1/",
                                                                                 "More information on the CJSW can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("d) Number of people diverted from prison custody via Community Payback Order (CPO) with alcohol treatment condition.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the Scottish Government Crime & Justice Social Work (CJSW) statistics report. 
                                                                           This publication presents national-level information on criminal justice social work activity in Scotland. 
                                                                           The report includes data on criminal justice social work services and social work orders, as well as characteristics of the individuals involved."),
                                                                        p("Figures in this bulletin are extracted from live criminal justice social work information management systems and 
                                                                           may differ slightly from those published previously as local administrative systems are updated. 
                                                                           They therefore reflect any changes to social work activity that may have resulted from the change to the presumption against short custodial sentences."),
                                                                        p("The Community Payback Order (CPO) was introduced by the Criminal Justice and Licensing (Scotland) Act 2010 and replaced provisions for community service, 
                                                                           probation and supervised attendance orders for offences committed from 1 February 2011 onwards. It is available to all courts, with some restrictions 
                                                                           applying to justice of the peace courts in relation to treatment and programme requirements. There are currently up to nine different requirements 
                                                                           which can be issued with a CPO, the most common of which are unpaid work or other activity and offender supervision. Courts may not impose unpaid work 
                                                                           or other activity on someone under 16 years old while offender supervision must be imposed for someone aged under 18."),
                                                                        p("Data are available from 2014-15 to 2019-20 and figures represent financial years. 
                                                                           It is not possible to disaggregate this indicator by age, gender or deprivation."),
                                                                        p("The next CJSW report will contain 2020-21 figures and is scheduled to be published in March 2022."),
                                                                        p(
                                                                          tags$a(href="https://www.gov.scot/publications/criminal-justice-social-work-statistics-scotland-2019-20/pages/1/",
                                                                                 "More information on the CJSW can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("e) Number of people diverted from prison custody via CPO with drug treatment condition.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the Scottish Government Crime & Justice Social Work (CJSW) statistics report. 
                                                                           This publication presents national-level information on criminal justice social work activity in Scotland. 
                                                                           The report includes data on criminal justice social work services and social work orders, as well as characteristics of the individuals involved."),
                                                                        p("Figures in this bulletin are extracted from live criminal justice social work information management systems and 
                                                                           may differ slightly from those published previously as local administrative systems are updated. 
                                                                           They therefore reflect any changes to social work activity that may have resulted from the change to the presumption against short custodial sentences."),
                                                                        p("The Community Payback Order (CPO) was introduced by the Criminal Justice and Licensing (Scotland) Act 2010 and replaced provisions for community service, 
                                                                           probation and supervised attendance orders for offences committed from 1 February 2011 onwards. It is available to all courts, with some restrictions 
                                                                           applying to justice of the peace courts in relation to treatment and programme requirements. There are currently up to nine different requirements 
                                                                           which can be issued with a CPO, the most common of which are unpaid work or other activity and offender supervision. Courts may not impose unpaid work 
                                                                           or other activity on someone under 16 years old while offender supervision must be imposed for someone aged under 18."),
                                                                        p("Data are available from 2014-15 to 2019-20 and figures represent financial years. 
                                                                           It is not possible to disaggregate this indicator by age, gender or deprivation."),
                                                                        p("The next CJSW report will contain 2020-21 figures and is scheduled to be published in March 2022."),
                                                                        p(
                                                                          tags$a(href="https://www.gov.scot/publications/criminal-justice-social-work-statistics-scotland-2019-20/pages/1/",
                                                                                 "More information on the CJSW can be found here", target="_blank"))
                                                                      ))
                                           ))),
                          
                          #Indicators for outcome c
                          h3("Outcome c: Increase availability and use of advocacy by those who require it at every stage of their recovery"), 
                          
                          wellPanel(column(12,
                                           bsCollapse(id = "justice_outcome_c", open = "Panel 1", #PanelSet id
                                                      bsCollapsePanel("a) Percentage of people accessing preparation for release (from prison) services.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the Scottish Prison Service (SPS) prisoner survey. 
                                                                           The biennial survey is undertaken in each of the 15 Scottish prisons and involves a sample of those held in custody in Scotland. 
                                                                           The survey intends to make use of respondents’ perceptions of service-delivery and service-quality in business planning; 
                                                                           provide those in custody with an opportunity to comment on a range of issues that impact on their experience in prison; 
                                                                           allow staff to get a better understanding of how the halls they manage compare to equivalent halls and in so doing to 
                                                                           provide a tangible way to help share items of ‘best practice’; and the SPS, through repetition of the same questions, 
                                                                           to track progress (or the lack of it) across the various dimensions that are included in the Survey."),
                                                                        p("As part of this survey, respondents were asked if they had accessed services while in prison in order to help them prepare for release. 
                                                                           Of those who responded ‘Yes’ they were asked which service they accessed from the following options: 
                                                                           Housing; Welfare/Benefits; Addictions; Employment; Throughcare; Health; Social Work; Financial advice; Training; Relationships."),
                                                                        p("The Welfare/Benefits, Additions, Throughcare, Health and Other categories were added as additional options in the 2017 and 2019 surveys. 
                                                                           In this dashboard we only report on the Housing, Employment, Financial advice, Training and Relationship options to ensure consistency with previous years, 
                                                                           but any large changes from 2017 onwards should be interpreted with caution due to the addition of these categories."),
                                                                        p("Data are available biennially from 2011 to 2019. Percentages are provided for all prisoners surveyed who responded 
                                                                           ‘Yes’ when asked if they had accessed services in preparation for release from prison. Percentages for respondents which selected a 
                                                                           service they had accessed are presented as a proportion of respondents who responded ‘Yes’ to accessing services rather than a proportion of 
                                                                           all respondents surveyed. As respondents can select more than one option these percentages total greater than 100%. It is not possible to 
                                                                           disaggregate this indicator by age, gender or deprivation."),
                                                                        p("The next SPS prisoner survey will contain 2021 figures and is scheduled to be published in 2022."),
                                                                        p(
                                                                          tags$a(href="https://www.sps.gov.uk/Corporate/Publications/Publications.aspx",
                                                                                 "More information on the SPS prisoner survey can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("b) Prisons’ performance against Inspecting and Monitoring Standard 7 ‘Transitions from custody to life in the community’.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the Her Majesty’s Inspectorate of Prisons for Scotland (HMIPS). 
                                                                           They are required to inspect the 15 prisons across Scotland in order to establish the treatment of, and the conditions for prisoners and to report publicly on the findings."),
                                                                        p("The most recent HMIPS annual report provides a summary of inspections ratings against a total of ten standards for each prison inspected:",
                                                                          tags$ul(
                                                                            tags$li("Standard 1: Lawful and transparent custody"),
                                                                            tags$li("Standard 2: Decency"),
                                                                            tags$li("Standard 3: Personal Safety"),
                                                                            tags$li("Standard 4: Health and wellbeing"),
                                                                            tags$li("Standard 5: Effective, courteous and humane exercise of authority"),
                                                                            tags$li("Standard 6: Respect, autonomy and protection against mistreatment"),
                                                                            tags$li("Standard 7: Purposeful activity"),
                                                                            tags$li("Standard 8: Transitions from custody to life in the community"),
                                                                            tags$li("Standard 9: Equality, dignity and respect"),
                                                                            tags$li("Standard 10: Organisational effectiveness "))),
                                                                        p("The performance of the prison against each of these standards is rated as: Good (practice worthy of sharing); Satisfactory; Generally acceptable (some improvements required); 
                                                                           Poor (accompanied by statement of what needs to be addressed); Unacceptable (requires immediate attention); or Not applicable. 
                                                                           A number of prisons were inspected each financial year, with annual reports available from 2016-17 (for Barlinnie, Kilmarnock and Edinburgh prisons), 
                                                                           2017-18 (for Low Moss, Shotts, Inverness and Greenock prisons), 2018-19 (for Perth, Addiewell, Polmont (Young Offenders Institute) and Grampian (Young Offenders Institute) prisons) 
                                                                           and 2019-20 (for Glenochill, Barlinnie, Edinburgh and Dumfries prisons). Data are also shown for all years and all prisons rather 
                                                                           than for each individual financial year or each individual prison. Data are shown here for Standard 8 only, 
                                                                           with numbers and percentages grouped by how the prisons performed against this standard."),
                                                                        p("The next annual report is scheduled to be published late 2021."),
                                                                        p(
                                                                          tags$a(href="https://www.prisonsinspectoratescotland.gov.uk/publications/inspecting-and-monitoring-standard-7-transitions-custody-life-community/",
                                                                                 "More information on HMIPS Standard 8 can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("c) Number and percentage of people receiving statutory and voluntary throughcare.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the Scottish Government Crime & Justice Social Work (CJSW) statistics report. 
                                                                           This publication presents national-level information on criminal justice social work activity in Scotland. 
                                                                           The report includes data on criminal justice social work services and social work orders, as well as characteristics of the individuals involved."),
                                                                        p("Figures in this bulletin are extracted from live criminal justice social work information management systems and 
                                                                           may differ slightly from those published previously as local administrative systems are updated. 
                                                                           They therefore reflect any changes to social work activity that may have resulted from the change to the presumption against short custodial sentences."),
                                                                        p("Throughcare is the provision of a range of social work and associated services to people serving a prison sentence and their families from the point of 
                                                                           sentence or remand, during the period of imprisonment and following release into the community. People serving more than four years are released under statutory supervision. 
                                                                           Those serving less than four years who are short-term sex offenders under Section 15 of the Management of Offenders (Scotland) Act 2005, 
                                                                           or who are subject to an extended sentence or supervised release order, are also supervised on release. 
                                                                           The objective of throughcare services is public protection, as well as assisting individuals to prepare for release and supporting community reintegration and rehabilitation. 
                                                                           Voluntary throughcare is available to those who are not subject to statutory throughcare, but who request support while in custody or within 12 months of release."),
                                                                        p("Data are available from 2014-15 to 2019-20 and figures represent financial years. 
                                                                           It is not possible to disaggregate this indicator by age, gender or deprivation."),
                                                                        p("The next CJSW report will contain 2020-21 figures and is scheduled to be published in March 2022."),
                                                                        p(
                                                                          tags$a(href="https://www.gov.scot/publications/criminal-justice-social-work-statistics-scotland-2019-20/pages/1/",
                                                                                 "More information on the CJSW can be found here", target="_blank"))
                                                                      ))
                                           ))),
                          
                          
                          
                          #Indicators for outcome d
                          h3("Outcome d: Increase the number of people who come into contact with justice agencies and receive the right support from the appropriate services and sources"), 
                          
                          p(
                            "It has not been possible to source data for this indicator and it is unclear when data will become available."
                          ),
                          
                          tags$a(href = '#top',  
                                 icon("circle-arrow-up", lib= "glyphicon"),"Back to top of page")
                 ),
                 
                 
                 
                 
                 #New tab and page will provide information about chapter 4
                 tabPanel("4) Children, Young People & Families", icon = icon("child"), 
                          
                          h2("Chapter 4 - Children Young People & Families"), 
                          
                          p(
                            "The overall outcome for this part of the strategy is for more children and families affected by
                               alcohol and drug use to be safe, healthy, included and supported. The original TOC model identifies
                               interim outcomes to achieve this overall outcome:",
                            tags$ul(
                              tags$li("More children, families and young people are involved in decisions about their care and about
                                 service design and delivery."),
                              tags$li("More children, families and young people's services are high quality and evidence-based."),
                              tags$li("Improve availability of support to family members who need it."),
                              tags$li("More children, families and young people receive integrated, inclusive, effective services.")),
                            "Please select one of the indicator titles below for information on the indicator data source, caveats and next update."
                          ),
                          
                          
                          hr(style = "border-top: 2px solid #000000;"),
                          
                          #Indicators for outcome a
                          h3("Outcome a: More children, families and young people are involved by services in decisions made about their care and about service design and delivery"), 
                          
                          wellPanel(column(12,
                                           bsCollapse(id = "children_outcome_a", open = "Panel 1", #PanelSet id
                                                      bsCollapsePanel("a) Number and proportion of Corra Partnership Drugs Initiative (PDI)-funded projects that are co-produced with family members.",
                                                                      tagList(
                                                                        p("This Data for this indicator were sourced from Corra Foundation. 
                                                                           This foundation has distributed almost £193 million and made nearly 16,000 grants over the last 35 years to help improve the 
                                                                           lives of individuals and communities experiencing disadvantage all across Scotland and in developing countries."),
                                                                        p("As part of the foundation, the Partnership Drugs Initiative (PDI) gives money to charities that support children and young people in Scotland, 
                                                                           which has totalled around £27.6 million and has supported over 300 projects over the last 20 years. 
                                                                           These charities work to increase opportunities, wellbeing and resilience of children and young people affected by alcohol and or drugs. 
                                                                           Children and young people, along with other experts, are involved in making decisions about PDI grants."),
                                                                        p("Data were obtained via a bespoke information request raised with Corra, with figures shown for the total number of PDI-funded projects by Corra 
                                                                           and the number and percentage of these projects that were co-produced with family members from the onset or that were co-produced with family members 
                                                                           in parts (they were consulted or involved in some processes)."),
                                                                        p("Details were also provided by Organisation Name and Service name but are not shown here. 
                                                                           There are other funds that are managed by Corra (Whole Family Approaches and the Challenge Fund) that may include elements of co-production with families. 
                                                                           Data shown in this dashboard are just for PDI-funded projects only."),
                                                                        p("Grants were distributed from 2016 to 2020. Data shown here are for all years rather than for each individual year the grant was distributed. 
                                                                           To obtain an update for additional grants a bespoke information request will have to be raised with Corra in the near future."),
                                                                        p(
                                                                          tags$a(href="https://www.corra.scot/grants/partnership-drugs-initiative/",
                                                                                 "More information on Corra PDI-funded projects can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("b) Number and proportion of Scottish Families Affected by Alcohol and Drugs (SFAD) helpline callers involved in loved ones’ treatment.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the Scottish Families Affected by Alcohol & Drugs (SFAD) Helpline Report. 
                                                                           SFAD is a national charity that supports anyone concerned about someone else’s alcohol or drug use in Scotland. 
                                                                           They give information and advice to many people and help them with confidence, communication, general wellbeing, and we link them into local support. 
                                                                           SFAD provide support via a helpline, bereavement support service, Telehealth (one-to-one support), and family support services in 
                                                                           Aberdeenshire, East Dunbartonshire, and Forth Valley. SFAD also deliver workforce development through training courses, communications and campaigning work."),
                                                                        p("This report compares helpline statistics for the full calendar year period (1st January to 31st December) of 2019 compared to the full calendar year period of 2020. 
                                                                           In the report the number of helpline contacts were split into one of 16 categories by the relationship of the caller to the person who uses substances: 
                                                                           Parent; Sibling; Child; Grandparent; Grandchild; Husband/wife (including ex); Partner (including ex); In Laws; Aunt/uncle; 
                                                                           Niece/nephew; Cousin; Friend; Work colleague; Professional body; Calling for self and; Other/NFD."),
                                                                        p("Data were obtained via a bespoke information request raised with SFAD to obtain this Helpline Report, with numbers shown in this dashboard as contacts
                                                                           which were Concern for themselves (Calling for self), Professional/Other (Professional Body and Other/NFD grouped together) or Concern for Significant Other (CSO) 
                                                                           (the remaining 13 categories grouped together) for 2019 and 2020. 
                                                                           To obtain an update for helpline calls beyond 2020 a bespoke information request will have to be raised with SFAD in the near future."),
                                                                        p(
                                                                          tags$a(href="https://www.sfad.org.uk/",
                                                                                 "More information the SFAD can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("c) Proportion of ADPs reporting, and providing examples of how, their commissioned services actively involve family members in service design and delivery.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the 2019/20 Alcohol and Drugs Partnership (ADP) Annual Review templates. 
                                                                           Prior to 2019/20, data for this indicator were not routinely collected. A bespoke request was made with the Scottish Government to request 
                                                                           information for this indicator be captured as part of this template when they were circulated to ADPs at the end of 2020. 
                                                                           Information from each of the ADPs who responded were collated together by the Scottish Government."),
                                                                        p("ADPs were asked indicate which approach services used to involve family members and to mark all approaches that were applied within their ADP. 
                                                                           These approaches were listed as: Feedback/complaints process; Questionnaires/surveys; Focus groups; Lived/living experience group/forum; 
                                                                           Board Representation within services; Board Representation at ADP; or Other."),
                                                                        p("ADPs were also asked if the involvement of family members changed over the course of the 2019/20 financial year. 
                                                                           ADPs were asked to provide one of the following responses to this question: Improved; Stayed the same; Scaled back; or Not in place."),
                                                                        p("A total of 28 of the 31 ADPs supplied information for this indicator, with no information provided from the Aberdeenshire, Clackmannanshire or Stirling ADPs."),
                                                                        p("The next ADP Annual Review templates for the 2020/21 financial year are due to be circulated to the ADPs at the end of 2021. 
                                                                           Another bespoke request to capture information for this indicator will likely have to be raised with the Scottish Government 
                                                                           in advance of this circulation to ensure these data are captured on a consistent basis."),
                                                                        p(
                                                                          tags$a(href="https://www.gov.scot/policies/alcohol-and-drugs/partnership-working/",
                                                                                 "More information on the ADP annual reporting can be found here", target="_blank"))
                                                                        ))
                                           ))),
                          
                          #Indicators for outcome b
                          h3("Outcome b: More children, families and young people’s services are high quality and evidence based"), 
                          
                          wellPanel(column(12,
                                           bsCollapse(id = "children_outcome_b", open = "Panel 1", #PanelSet id
                                                      bsCollapsePanel("a) Output from review of services available to family members where the quality principles apply.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the 2019/20 Alcohol and Drugs Partnership (ADP) Annual Review templates. 
                                                                           Prior to 2019/20, data for this indicator were not routinely collected. A bespoke request was made with the Scottish Government to request 
                                                                           information for this indicator be captured as part of this template when they were circulated to ADPs at the end of 2020. 
                                                                           Information from each of the ADPs who responded were collated together by the Scottish Government."),
                                                                        p("ADPs were asked Quality Assurance arrangements in place for the following services (with examples to include review performance against targets/success indicators, 
                                                                           clinical governance reviews, case file audits, review against delivery of the quality principles) and to mark all options that applied to their ADP. 
                                                                           For Adult Services and Children & Family Services separately (reporting on Children & Family Services for this indicator), these options were listed as: 
                                                                           Third Sector; Public Sector; and Other. Data for this indicator is displayed by number of ADPs who were noted as having no arrangements in place or if they 
                                                                           had one arrangement in place, two arrangements in place or three arrangements in place. Data for this indicator are also displayed by number of ADPs 
                                                                           who stated they had Third Sector, Public Sector or Other arrangement in place."),
                                                                        p("A total of 28 of the 31 ADPs supplied information for this indicator, with no information provided from the Aberdeenshire, Clackmannanshire or Stirling ADPs."),
                                                                        p("The next ADP Annual Review templates for the 2020/21 financial year are due to be circulated to the ADPs at the end of 2021. 
                                                                           Another bespoke request to capture information for this indicator will likely have to be raised with the Scottish Government 
                                                                           in advance of this circulation to ensure these data are captured on a consistent basis."),
                                                                        p(
                                                                          tags$a(href="https://www.gov.scot/policies/alcohol-and-drugs/partnership-working/",
                                                                                 "More information on the ADP annual reporting can be found here", target="_blank"))
                                                                        )),
                                                      bsCollapsePanel("b) Local reports/reviews/inspections of services offered to children, young people and/or families affected by alcohol and other drug problems.",
                                                                      tagList(
                                                                        p("The MERRR Framework specified the ADP annual report template would be used as the mechanism to capture this information. 
                                                                          After further investigation, it became clear it would not have been possible to source these data from this template. 
                                                                          Indicator to be removed from future reporting.")))
                                           ))),
                          
                          #Indicators for outcome c
                          h3("Outcome c: Improve availability of support to family members who need it"), 
                          
                          wellPanel(column(12,
                                           bsCollapse(id = "children_outcome_c", open = "Panel 1", #PanelSet id
                                                      bsCollapsePanel("a) Number of services and their settings for children affected by alcohol and other drug problems.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the 2019/20 Alcohol and Drugs Partnership (ADP) Annual Review templates. 
                                                                           Prior to 2019/20, data for this indicator were not routinely collected. A bespoke request was made with the Scottish Government to request 
                                                                           information for this indicator be captured as part of this template when they were circulated to ADPs at the end of 2020. 
                                                                           Information from each of the ADPs who responded were collated together by the Scottish Government."),
                                                                        p("ADPs were asked to respond ‘Yes’ or ‘No’ when asked if they have specific treatment and support services for children and young people (under the age of 25) 
                                                                           affected by alcohol and/or drug problems of a parent/carer or other adult. ADPs were also asked if services for children and young people, 
                                                                           with alcohol and/or drugs problems, changed over the course of the 2019/20 financial year. 
                                                                           ADPs were asked to provide one of the following responses to this question: Improved; Stayed the same; Scaled back; or Not in place. 
                                                                           ADPs were also asked if services for children and young people, affected by alcohol and/or drugs problems, changed over the course of the 2019/20 financial year. 
                                                                           ADPs were asked to provide one of the following responses to this question: Improved; Stayed the same; Scaled back; or Not in place."),
                                                                        p("A total of 28 of the 31 ADPs supplied information for this indicator, with no information provided from the Aberdeenshire, Clackmannanshire or Stirling ADPs."),
                                                                        p("The next ADP Annual Review templates for the 2020/21 financial year are due to be circulated to the ADPs at the end of 2021. 
                                                                           Another bespoke request to capture information for this indicator will likely have to be raised with the Scottish Government 
                                                                           in advance of this circulation to ensure these data are captured on a consistent basis."),
                                                                        p(
                                                                          tags$a(href="https://www.gov.scot/policies/alcohol-and-drugs/partnership-working/",
                                                                                 "More information on the ADP annual reporting can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("b) Number of services available for families affected by alcohol and other drug problems.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the Scottish Families Affected by Alcohol & Drugs (SFAD) Helpline Report. 
                                                                           SFAD is a national charity that supports anyone concerned about someone else’s alcohol or drug use in Scotland. 
                                                                           They give information and advice to many people and help them with confidence, communication, general wellbeing, and we link them into local support. 
                                                                           SFAD provide support via a helpline, bereavement support service, Telehealth (one-to-one support), and family support services in 
                                                                           Aberdeenshire, East Dunbartonshire, and Forth Valley. SFAD also deliver workforce development through training courses, communications and campaigning work."),
                                                                        p("SFAD directory numbers are for the full calendar year period (1st January to 31st December) of 2019 compared to the full calendar year period of 2020. 
                                                                           In the directory the number of services provided by organisations which offer a variety of family support in each ADP area were grouped into one of the following categories:"),
                                                                        tags$ul(
                                                                          tags$li("Evidence Based Intervention: Community Reinforcement and Family Training (CRAFT) & specific, measurable, achievable, relevant, and time-bound (SMART)."),
                                                                          tags$li("Peer Support: one to one or group peer support."),
                                                                          tags$li("Counselling: A block of counselling sessions specific to someone impacted by a loved one’s substance use, short term support."),
                                                                          tags$li("Children Affected by Parental Substance Misuse (CAPSM): This is a variety of Young Persons’ support. The majority of them require Social Work involvement or referred 
                                                                                   from another professional not quite the same as the Routes Young Persons’ Project and what SFAD would hope would be available."),
                                                                          tags$li("Kinship: Care specific to families impacted by substance use. This support tends to be around their caring requirements not “family support” as SFAD sees it."),
                                                                          tags$li("Carers: Each ADP area will have a carers centre however not all advertise that they offer support to carers relative to drug and/or alcohol support. 
                                                                                   This support tends to be around their caring requirements and not “family support” as SFAD sees it.")),
                                                                        p("Data were obtained via a bespoke information request raised with SFAD to obtain numbers from the SFAD directory, with figures shown in this dashboard as number of
                                                                          services provided by organisations which provide family support in 2019 and 2020. To obtain an update for services beyond 2020 a bespoke information request will have to be raised with SFAD in the near future."),
                                                                        p(
                                                                          tags$a(href="https://www.sfad.org.uk/",
                                                                                 "More information the SFAD can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("c) Proportion of ADP investment in services available to children, young people and family members affected by alcohol and other drug problems.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the 2019/20 Alcohol and Drugs Partnership (ADP) Annual Review templates  
                                                                           but, as figures have yet to be validated, data are not reported on in this dashboard.")
                                                                      ))
                                           ))),
                          
                          
                          
                          #Indicators for outcome d
                          h3("Outcome d: More children, families and young people receive integrated, inclusive, effective services"), 
                          
                          wellPanel(column(12,
                                           bsCollapse(id = "children_outcome_d", open = "Panel 1", #PanelSet id
                                                      bsCollapsePanel("a) Number of ADPs providing examples of how all of their services provide family-inclusive practice.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the 2019/20 Alcohol and Drugs Partnership (ADP) Annual Review templates. 
                                                                           Prior to 2019/20, data for this indicator were not routinely collected. A bespoke request was made with the Scottish Government to request 
                                                                           information for this indicator be captured as part of this template when they were circulated to ADPs at the end of 2020. 
                                                                           Information from each of the ADPs who responded were collated together by the Scottish Government."),
                                                                        p("ADPs were asked if provide details on if they had adult services to support family-inclusive practice and to mark all options that applied to their ADP. 
                                                                           For Family member in treatment services these options were listed as: Advice; Mutual aid; Mentoring; Social Activities; Personal Development; Advocacy; 
                                                                           Support for victims of gender based violence; and Other. For Family member not in treatment services these options were listed as the same."),
                                                                        p("This indicator displays how many of these options are available per ADP. For either family member in treatment or 
                                                                           family member not in treatment these are displayed as: Three or less; Four; Five; or Six or more."),
                                                                        p("A total of 28 of the 31 ADPs supplied information for this indicator, with no information provided from the Aberdeenshire, Clackmannanshire or Stirling ADPs."),
                                                                        p("The next ADP Annual Review templates for the 2020/21 financial year are due to be circulated to the ADPs at the end of 2021. 
                                                                           Another bespoke request to capture information for this indicator will likely have to be raised with the Scottish Government 
                                                                           in advance of this circulation to ensure these data are captured on a consistent basis."),
                                                                        p(
                                                                          tags$a(href="https://www.gov.scot/policies/alcohol-and-drugs/partnership-working/",
                                                                                 "More information on the ADP annual reporting can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("b) Number of ADPs providing examples of how their services provide family support.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the 2019/20 Alcohol and Drugs Partnership (ADP) Annual Review templates. 
                                                                           Prior to 2019/20, data for this indicator were not routinely collected. A bespoke request was made with the Scottish Government to request 
                                                                           information for this indicator be captured as part of this template when they were circulated to ADPs at the end of 2020. 
                                                                           Information from each of the ADPs who responded were collated together by the Scottish Government."),
                                                                        p("ADPs were asked to respond ‘Yes’ or ‘No’ when asked if they have specific support services available for adult family members. 
                                                                           ADPs were also asked if services for adult family members change in the 2019/20 financial year. 
                                                                           ADPs were asked to provide one of the following responses to this question: Improved; Stayed the same; Scaled back; or Not in place."),
                                                                        p("A total of 28 of the 31 ADPs supplied information for this indicator, with no information provided from the Aberdeenshire, Clackmannanshire or Stirling ADPs."),
                                                                        p("The next ADP Annual Review templates for the 2020/21 financial year are due to be circulated to the ADPs at the end of 2021. 
                                                                           Another bespoke request to capture information for this indicator will likely have to be raised with the Scottish Government 
                                                                           in advance of this circulation to ensure these data are captured on a consistent basis."),
                                                                        p(
                                                                          tags$a(href="https://www.gov.scot/policies/alcohol-and-drugs/partnership-working/",
                                                                                 "More information on the ADP annual reporting can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("c) Proportion of addiction treatment staff who attend, when invited, Child Protection Case Conference.",
                                                                      tagList(
                                                                        p("The MERRR Framework specified the ADP annual report template would be used as the mechanism to capture this information. 
                                                                          After further investigation, it became clear it would not have been possible to source these data from this template. 
                                                                          Indicator to be removed from future reporting."))),
                                                      bsCollapsePanel("d) Number of ADPs providing examples of, and level of investment in, joint service commissioning between ADP and Child Protection Committee.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the 2019/20 Alcohol and Drugs Partnership (ADP) Annual Review templates. 
                                                                           Prior to 2019/20, data for this indicator were not routinely collected. A bespoke request was made with the Scottish Government to request 
                                                                           information for this indicator be captured as part of this template when they were circulated to ADPs at the end of 2020. 
                                                                           Information from each of the ADPs who responded were collated together by the Scottish Government."),
                                                                        p("ADPs were asked to respond ‘Yes’ or ‘No’ when asked if they feed into or contribute toward the integrated children’s service plan. 
                                                                           ADPs were also asked to provide free text details on how priorities are reflected in children’s service planning. 
                                                                           We collate responses for this indicator by if they made any reference to the Child Protection Committee in their free text response to this request.
                                                                           No information is available by level of investment."),
                                                                        p("A total of 28 of the 31 ADPs supplied information for this indicator, with no information provided from the Aberdeenshire, Clackmannanshire or Stirling ADPs."),
                                                                        p("The next ADP Annual Review templates for the 2020/21 financial year are due to be circulated to the ADPs at the end of 2021. 
                                                                           Another bespoke request to capture information for this indicator will likely have to be raised with the Scottish Government 
                                                                           in advance of this circulation to ensure these data are captured on a consistent basis."),
                                                                        p(
                                                                          tags$a(href="https://www.gov.scot/policies/alcohol-and-drugs/partnership-working/",
                                                                                 "More information on the ADP annual reporting can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("e) Number of ADPs providing examples, and the number of examples, of joint training between ADP and Child Protection Committee (and the staff under their auspices).",
                                                                      tagList(
                                                                        p("The MERRR Framework specified the ADP annual report template would be used as the mechanism to capture this information. 
                                                                          After further investigation, it became clear it would not have been possible to source these data from this template. 
                                                                          Indicator to be removed from future reporting."))),
                                                      bsCollapsePanel("f) Number of ADPs providing examples of how the needs of children affected by parental substance misuse are reflected in local integrated children’s services plans.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the 2019/20 Alcohol and Drugs Partnership (ADP) Annual Review templates. 
                                                                           Prior to 2019/20, data for this indicator were not routinely collected. A bespoke request was made with the Scottish Government to request 
                                                                           information for this indicator be captured as part of this template when they were circulated to ADPs at the end of 2020. 
                                                                           Information from each of the ADPs who responded were collated together by the Scottish Government."),
                                                                        p("ADPs were asked to respond ‘Yes’ or ‘No’ when asked if they feed into or contribute toward the integrated children’s service plan."),
                                                                        p("A total of 28 of the 31 ADPs supplied information for this indicator, with no information provided from the Aberdeenshire, Clackmannanshire or Stirling ADPs."),
                                                                        p("The next ADP Annual Review templates for the 2020/21 financial year are due to be circulated to the ADPs at the end of 2021. 
                                                                           Another bespoke request to capture information for this indicator will likely have to be raised with the Scottish Government 
                                                                           in advance of this circulation to ensure these data are captured on a consistent basis."),
                                                                        p(
                                                                          tags$a(href="https://www.gov.scot/policies/alcohol-and-drugs/partnership-working/",
                                                                                 "More information on the ADP annual reporting can be found here", target="_blank"))
                                                                      ))
                                           ))),
                          
                          
                          #Indicators for outcome e
                          h3("Outcome e) Young people's capacity to make informed choices is improved"), 
                          #These alcohol indicators have come from Chapter 1 Outcome d. Need statement here explaining why
                          p(
                            "A decision was made to take alcohol indicators from Prevention & Early intervention chapter and place them here as RRR doesn’t cover a prevention 
                            aspect for alcohol and determined it was more suitable to place these indicators under the section involving children and young people."
                          ),
                          wellPanel(column(12,
                                           bsCollapse(id = "children_outcome_e", open = "Panel 1", #PanelSet id
                                                      bsCollapsePanel("a) Young people’s reported wellbeing.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the Scottish Adolescent Lifestyle & Substance Use Survey (SALSUS). 
                                                                          This survey is a continuation of a long established series of national surveys on smoking, drinking and drug use. 
                                                                          These were carried out jointly in Scotland and England between 1982 and 2000, to provide a national picture of young peoples' 
                                                                          behaviours within the context of other lifestyle, health and social factors. Since 2002, Scotland has developed its own, 
                                                                          more tailored SALSUS survey, a self-completion survey administered by teachers in a mixed ability class, under exam conditions. 
                                                                          Data were weighted by Local Authority, age, sex, school sector (state/independent), school denomination and by urban/rural classification."),
                                                                        p("As with any survey, there are a number of limitations of the data. The data are from a sample of the population as opposed to a census and, 
                                                                          therefore, subject to sampling error. Data can also be incomplete due to non-response of schools, classes and pupils. As with all surveys, 
                                                                          the results can only tell us what respondents say that they do, think or feel. We have to assume that their answers are honest and accurate, 
                                                                          especially given SALSUS covers sensitive topic areas."),
                                                                        p("Accuracy is also an issue as self-report and accuracy of answers will vary by pupil and the sample only covers pupils in mainstream secondary education. 
                                                                          The sample excludes pupils in special schools, secure residential units and those who are home schooled. 
                                                                          Additionally, pupils who are supposed to attend mainstream schools but don't (e.g. absent through truancy or exclusion) are potentially 
                                                                          less likely to have taken part. Findings from studies show that absenteeism due to truanting and exclusion is correlated with substance use."),
                                                                        p("Data for this indicator were recorded as percentage of S2 (13 year old) and S4 (15 year old) pupils who responded ‘Very Good’ or ‘Good’ 
                                                                          when asked how their health was in general. These pupils were also asked if they have a physical or mental health condition or illness lasting or 
                                                                          expected to last 12 months or more, with percentages recorded by those who responded ‘Yes’.
                                                                          Data for both these sub-indicators were split by if the respondent stated they had consumed alcohol some point in the last week, 
                                                                          consumed alcohol at some point in the past or if they have never consumed alcohol."),
                                                                        p("As information based on these definitions and split by these groups is not currently published in SALSUS, analyses were obtained by requesting 2013, 
                                                                          2015 and 2018 SALSUS data from the UK Data Service (UKDS) separately and by running analyses in R."),
                                                                        p("Although a publication date has yet to be confirmed, the next SALSUS will likely be published in 2022, with subsequent SALSUS datasets 
                                                                         added onto the UKDS shortly after. It is anticipated that information will still be available for this indicator."),
                                                                        p(
                                                                          tags$a(href="https://www.gov.scot/publications/scottish-schools-adolescent-lifestyle-substance-use-survey-salsus-drug-use-report-2018/",
                                                                                 "More information on the SALSUS can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("b) Number of young people using alcohol.",
                                                                      tagList(
                                                                        p("Data for this indicator were sourced from the Scottish Adolescent Lifestyle & Substance Use Survey (SALSUS). 
                                                                           This survey is a continuation of a long established series of national surveys on smoking, drinking and drug use. 
                                                                           These were carried out jointly in Scotland and England between 1982 and 2000, to provide a national picture of young peoples' 
                                                                           behaviours within the context of other lifestyle, health and social factors. Since 2002, Scotland has developed its own, 
                                                                           more tailored SALSUS survey, a self-completion survey administered by teachers in a mixed ability class, under exam conditions. 
                                                                           Data were weighted by Local Authority, age, sex, school sector (state/independent), school denomination and by urban/rural classification."),
                                                                        p("As with any survey, there are a number of limitations of the data. The data are from a sample of the population as opposed to a census and, 
                                                                           therefore, subject to sampling error. Data can also be incomplete due to non-response of schools, classes and pupils. As with all surveys, 
                                                                           the results can only tell us what respondents say that they do, think or feel. We have to assume that their answers are honest and accurate, 
                                                                           especially given SALSUS covers sensitive topic areas."),
                                                                        p("Accuracy is also an issue as self-report and accuracy of answers will vary by pupil and the sample only covers pupils in mainstream secondary education. 
                                                                           The sample excludes pupils in special schools, secure residential units and those who are home schooled. 
                                                                           Additionally, pupils who are supposed to attend mainstream schools but don't (e.g. absent through truancy or exclusion) are potentially 
                                                                           less likely to have taken part. Findings from studies show that absenteeism due to truanting and exclusion is correlated with substance use."),
                                                                        p("Pupils were provided details about their alcohol use behaviour. 
                                                                           This information was used to create an overall measure of alcohol consumption ‘in the last month’, ‘in the last year’ (including in the last month), 
                                                                           ‘ever’ (including in the last month and last year) and ‘never’. Data for this indicator were recorded as percentage of S2 (13 year old) and S4 (15 year old) 
                                                                           pupils who responded ‘Yes’ when asked if they had drank in the last week, drank ever, been drunk in the last week or been drunk ever."),
                                                                        p("Data for this indicator were obtained from the 2010, 2013, 2015 and 2018 versions of the SALSUS and were not possible to disaggregate by deprivation. 
                                                                           Data are available by gender but are not reported on here. Numbers are also provided in this dashboard but these should be interpreted with caution, 
                                                                           as a decrease may represent fewer pupils sampled across survey years rather than an actual decrease in alcohol consumption behaviour."),
                                                                        p("Although a publication date has yet to be confirmed, the next SALSUS will likely be published in 2022. 
                                                                          It is anticipated that information will still be available for this indicator."),
                                                                        p(
                                                                          tags$a(href="https://www.gov.scot/publications/scottish-schools-adolescent-lifestyle-substance-use-survey-salsus-drug-use-report-2018/",
                                                                                 "More information on the SALSUS can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("c) Number and rate of young people admitted to hospital for alcohol-related admissions.",
                                                                      tagList(
                                                                        p("Data were collected from two routinely collected national datasets. Scottish Morbidity Records 01
                                                                           (SMR01) was the source for general acute inpatient and day-case hospital activity for specialties
                                                                           other than mental health, maternity, neonatal and geriatric long-stay. SMR04 was the source for
                                                                           psychiatric inpatient and day-case hospital activity."),
                                                                        p("A patient refers to the number of unique individuals treated within the financial year and was
                                                                           counted only once. New patients are defined as patients admitted to hospital who did not
                                                                           previously have an alcohol-related hospital admission within the last 10 years. Hospital stays
                                                                           are not shown in this report as these are primarily an activity-based measure."),
                                                                        p("In 1997, ISD moved from using the International Statistical Classification of Diseases and Related
                                                                           Health Problems, Ninth revision (ICD-9) to the 10th revision (ICD-10). The change introduced a
                                                                           number of new alcohol-related codes so time trends for individual alcohol-related conditions
                                                                           started in 1997/98."),
                                                                        p("A European Age-sex Standardised Rate (EASR) was calculated because hospital activity rates may
                                                                           vary with the age-sex structure of the populations. The European Standard Population (ESP) was
                                                                           used to calculate EASRs within the most recent publications. The ESP was originally introduced in
                                                                           1976 and was revised in 2013. Before 2014 the publication used ESP1976 to calculate EASRs. Since
                                                                           2014, ESP2013 has been used to calculate EASRs for all years (including those before 2012/13).
                                                                           Therefore, findings from publications since February 2014 are not comparable with earlier
                                                                           publications."),
                                                                        p("Rates are only available by those aged under 15 and subsequent 5 year age bands thereafter. 
                                                                           Therefore, we do not present rates by those aged under 25. 
                                                                           Numbers for under 25s reported on here are based on an aggregation of those aged under 15 and those aged 15-24 (for all, males and females). 
                                                                           As disclosure control protocol has been applied to age-gender disaggregation there may be slight discrepancies 
                                                                           for aggregated totals compared to if numbers had been calculated directly."),
                                                                        p("Figures are shown for general acute hospitals in Scotland rather than combining general acute and psychiatric hospitals.
                                                                           This decision was made due to data incompleteness for SMR04 records for 2019/20, with no indication to when these data will be complete."),
                                                                        p("Data are available from 2009/10 to 2019/20. 
                                                                           Socioeconomic deprivation data were not available to be split further by Scottish Index of Multiple Deprivation (SIMD) by each of the gender or age groups."),
                                                                        p("The next report will contain 2020/21 figures and is scheduled to be published in November 2021."),
                                                                        p(
                                                                          tags$a(href="https://beta.isdscotland.org/find-publications-and-data/lifestyle-and-behaviours/substance-use/alcohol-related-hospital-statistics/",
                                                                                 "More information on alcohol-related hospital statistics can be found here", target="_blank"))
                                                                      ))
                                           ))),
                          
                          tags$a(href = '#top',  
                                 icon("circle-arrow-up", lib= "glyphicon"),"Back to top of page")
                          
                 ),
                 
                 
                 
                 #New tab and page will provide information about chapter 5
                 tabPanel("5) Health & Social Harms", icon = icon("hospital"), 
                          
                          h2("Chapter 5 - Health & Social Harms"), 
                          
                          p(
                            "The overall outcome for this part of the strategy is for the original TOC model for the other four chapters 
                               to be realised in order to achieve two overall outcomes:",
                            tags$ul(
                              tags$li("Reduce alcohol- and drug-related health harms"),
                              tags$li("Reduce alcohol- and drug-related social harms")),
                            "Please select one of the indicator titles below for information on the indicator data source, caveats and next update."
                          ),
                          
                          hr(style = "border-top: 2px solid #000000;"),
                          
                          #Indicators for outcome a
                          h3("Outcome a: Reduce alcohol- and drug-related health harms"), 
                          
                          wellPanel(column(12,
                                           bsCollapse(id = "harms_outcome_a", open = "Panel 1", #PanelSet id
                                                      bsCollapsePanel("a) Prevalence of problem drug use.",
                                                                      tagList(
                                                                        p("This indicator defines Problematic Drug Use as the problematic use of opioids (including illicit and prescribed methadone use) 
                                                                           and/or the illicit use of benzodiazepines, and implies routine and prolonged use as opposed to recreational and occasional drug use."),
                                                                        p("Data collected for this indicator relied upon a ‘capture-recapture’ methodology used to estimate
                                                                          the number of individuals with problem drug use in the Prevalence of Problem Drug Use in Scotland
                                                                          report. This form of analysis required the collection of two or more data sources which in some
                                                                          way identify individuals with problem drug use. Specialist Drug Treatment Services, Drug-Related
                                                                          Hospital Admissions, Criminal Justice Social Work Reports, and Police Data were the data sources
                                                                          used for modelling."),
                                                                        p("Information Services Division (ISD, now Public Health Scotland (PHS)) were advised that for 2015/16, police data compatible with
                                                                          the definitions required for the study could not be extracted from current databases in the same
                                                                          way as it had been in previous years. Following an impact assessment of methodological revisions,
                                                                          ISD concluded that working with three data sources still allowed for the effective production
                                                                          of drug prevalence estimates, and the project proceeded on that basis. These changes to the
                                                                          methodological approach for the 2015/16 study mean direct comparisons with results from
                                                                          previous studies are difficult to fully interpret."),
                                                                        p("An experimental statistical methods document was published in June 2020 which explored the
                                                                          expansion of this definition to capture problematic use of other substances. Three definitions of
                                                                          problem drug use were considered as part of this report:",
                                                                          tags$ul(
                                                                            tags$li("Definition 1: Opioids (including illicit and prescribed methadone use) and/or the illicit use of benzodiazepines."),
                                                                            tags$li("Definition 2: As definition 1, plus illicit use of cocaine (including crack cocaine) and amphetamines (including amphetamine type substances)"),
                                                                            tags$li("Definition 3: As definition 2, plus illicit use of cannabis (including synthetic cannabinoids)"))),
                                                                        p("The statistical methods used to estimate the prevalence based on the wider definitions were
                                                                          the same as for the original reports, namely capture-recapture using log-linear modelling.
                                                                          In agreement with our stakeholders, we may explore these expanded definitions in future reporting,
                                                                          as cocaine injecting has been a key driver in the HIV outbreak in Glasgow and, with a marked rise
                                                                          in the use of both cocaine powder and crack cocaine across Scotland, poly-drug use is also an
                                                                          increasing problem. Services have not yet fully adapted to meet the needs engendered by these
                                                                          changing trends."),
                                                                        p("Data are available for 2009/10, 2012/13 and 2015/16 only. The next report will contain 2018/19 figures and is scheduled to be published in 2022."),
                                                                        p(
                                                                          tags$a(href="https://beta.isdscotland.org/find-publications-and-data/lifestyle-and-behaviours/substance-use/prevalence-of-problem-drug-use-in-scotland-201516-estimates/",
                                                                                 "More information on prevalence of problem drug use can be found here", target="_blank")),
                                                                      )),
                                                      bsCollapsePanel("b) Alcohol-related hospital statistics.",
                                                                      tagList(
                                                                        p("Data were collected from two routinely collected national datasets. Scottish Morbidity Records 01
                                                                          (SMR01) was the source for general acute inpatient and day-case hospital activity for specialties
                                                                          other than mental health, maternity, neonatal and geriatric long-stay. SMR04 was the source for
                                                                          psychiatric inpatient and day-case hospital activity."),
                                                                        p("A patient refers to the number of unique individuals treated within the financial year and was
                                                                          counted only once. New patients are defined as patients admitted to hospital who did not
                                                                          previously have an alcohol-related hospital admission within the last 10 years. Hospital stays
                                                                          are not shown in this report as these are primarily an activity-based measure."),
                                                                        p("In 1997, ISD moved from using the International Statistical Classification of Diseases and Related
                                                                          Health Problems, Ninth revision (ICD-9) to the 10th revision (ICD-10). The change introduced a
                                                                          number of new alcohol-related codes so time trends for individual alcohol-related conditions
                                                                          started in 1997/98."),
                                                                        p("A European Age-sex Standardised Rate (EASR) was calculated because hospital activity rates may
                                                                          vary with the age-sex structure of the populations. The European Standard Population (ESP) was
                                                                          used to calculate EASRs within the most recent publications. The ESP was originally introduced in
                                                                          1976 and was revised in 2013. Before 2014 the publication used ESP1976 to calculate EASRs. Since
                                                                          2014, ESP2013 has been used to calculate EASRs for all years (including those before 2012/13).
                                                                          Therefore, findings from publications since February 2014 are not comparable with earlier
                                                                          publications."),
                                                                        p("Figures are shown for general acute hospitals in Scotland rather than combining general acute and psychiatric hospitals.
                                                                          This decision was made due to data incompleteness for SMR04 records for 2019/20, with no indication to when these data will be complete."),
                                                                        p("Data are available from 2009/10 to 2019/20. Socioeconomic deprivation data were only available by Scottish Index of Multiple Deprivation
                                                                          (SIMD) decile and the difference shown was for the 10% most and 10% least deprived areas in
                                                                          Scotland."),
                                                                        p("The next report will contain 2020/21 figures and is scheduled to be published in November 2021."),
                                                                        p(
                                                                          tags$a(href="https://beta.isdscotland.org/find-publications-and-data/lifestyle-and-behaviours/substance-use/alcohol-related-hospital-statistics/",
                                                                                 "More information on alcohol-related hospital statistics can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("c) Alcohol-specific deaths.",
                                                                      tagList(
                                                                        p("Data were taken from the National Records of Scotland (NRS). The NRS framework specified
                                                                          that this indicator was to be reported on using the new definition, which produces what may be
                                                                          described as the number of ‘alcohol-specific’ deaths. These are deaths which are known to be direct
                                                                          consequences of problem alcohol use, meaning they are wholly attributable to problem alcohol
                                                                          use. Such figures were first published in 2017, using a definition which was introduced following a
                                                                          consultation organised by the Office for National Statistics (ONS), and in 2018 the NRS extended
                                                                          the series available on this website back to 1979."),
                                                                        p("Before 2017, the NRS reported the number of ‘alcohol-related’ deaths. These are deaths from
                                                                          a selection of causes which are related to alcohol consumption. Most of those causes are wholly
                                                                          attributable to problem alcohol use (such as medical conditions for which every death was caused
                                                                          by alcohol), but some are only partially attributable to problem alcohol use (such as medical
                                                                          conditions for which only a proportion of deaths are caused by alcohol)."),
                                                                        p("Data are available from 2009 to 2019 and are not shown by age as at least 96% of deaths occurred for those aged 35 and over, a figure
                                                                          consistent from 2009 to 2019."),
                                                                        p("Socioeconomic deprivation data are available by SIMD quintile and the difference shown was for the 20% most and 20% least deprived areas in Scotland. 
                                                                          Two methods of allocating datazones (and therefore deaths) to SIMD quintiles are presented:",                                                                                                                                                    
                                                                          tags$ul(
                                                                            tags$li("NRS Method – The SIMD quintiles are based on datazone counts. Quintile 1 therefore contains the 20% most deprived datazones in Scotland.",
                                                                                    tags$a(href="https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/vital-events/deaths/age-standardised-death-rates-calculated-using-the-esp/",
                                                                                           "Sourced from the NRS", target="_blank")),
                                                                            tags$li("PHS Method – The SIMD quintiles are based on population counts. Quintile 1 therefore contains the 20% of the Scottish population that live in the most deprived datazones in Scotland. 
                                                                                    Sourced from the PHS Monitoring and Evaluating Scotland’s Alcohol Strategy (MESAS) team."))),
                                                                        p("Both methods are shown here to display discrepancies that can arise between different methodologies and to ensure consistency with both the annual MESAS publication, which uses the PHS Method, 
                                                                          and NRS drug-related deaths figures within this dashboard, which uses the NRS Method (see Indicator f) under this outcome)."),
                                                                        p(
                                                                          tags$a(href="https://www.isdscotland.org/Products-and-Services/GPD-Support/Deprivation/SIMD/",
                                                                                 "More information on the methodology behind these approaches and their differences can be found here", target="_blank")),
                                                                        p("The next report will contain 2020 figures and is scheduled to be published in June 2021."),
                                                                        p(
                                                                          tags$a(href="https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/vital-events/deaths/alcohol-deaths/",
                                                                                 "More information on alcohol-specific deaths can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("d) Alcohol-related trauma.",
                                                                      tagList(
                                                                        p("Data were collected from the Scottish Trauma Audit Group (STAG), which presents detailed
                                                                          information of the patient journeys of patients with severe injury (trauma) that have passed
                                                                          through the Scottish healthcare system. Severity of injuries were classified based on the Injury
                                                                          Severity Score (ISS), a method for describing patients with single or multiple injuries and evaluating
                                                                          emergency care. This score was used to standardise the severity of injuries sustained during a
                                                                          trauma to allow a more accurate prediction of morbidity and mortality outcomes after traumatic
                                                                          injuries. The ISS uses the Abbreviated Injury Scale (AIS), an anatomically based consensus-derived
                                                                          global severity scoring system, in combination with a body region to calculate severity of trauma.
                                                                          So a minor injury to the hand will have a far lower ISS than a critical injury to the head. Patients
                                                                          were placed into one of three ISS categories: major (ISS>15); moderate (ISS 9–15); and minor."),
                                                                        p("Data on whether alcohol played a role in trauma injuries were recorded if evidence existed that
                                                                          either the trauma patient or another contributor to the trauma had ingested alcohol. STAG data
                                                                          do not record how much alcohol was consumed by either the trauma patient or trauma contributor
                                                                          so we are unable to completely determine how much of a factor alcohol intoxication was for the
                                                                          injury sustained."),
                                                                        p("STAG data are subject to ongoing validation and must be regarded as dynamic. Therefore if this
                                                                          analysis was to be re-run at a later stage data and results may be subject to change. STAG data
                                                                          collection is currently recognised as a priority as changes are made to enhance trauma care in
                                                                          Scotland. There was major investment in 2018 and cross-hospital working has been introduced (in
                                                                          larger NHS Health Boards) to minimise the risk of data gaps in the future. This led to an increase in
                                                                          the overall number of trauma cases recorded over a year (3,774 in 2017 to 4,459 in 2018). Due to
                                                                          increasing data completeness and having more hospitals participating in the audit, numbers will
                                                                          increase over time and we will have a greater number of trauma incidents being recorded. For these
                                                                          reasons we do not present numbers in this chapter as they are likely reflective of increasing data
                                                                          completeness rather than represent a real increase in the number of trauma incidents."),
                                                                        p("A bespoke information request was raised to include deprivation. These data were sent in October 2020 
                                                                          and are included in this dashboard. Socioeconomic deprivation is available by
                                                                          (SIMD) quintile and the difference shown was for the 20% most and 20% least deprived areas in Scotland.
                                                                          All other data were pulled and formatted from the Tableau dashboards. Data published in 2020 were 
                                                                          available by age and drugs but are not reported on here."),
                                                                        p("Data are available from 2013 to 2019. The next report will contain 2020 figures and is scheduled to be published in June 2021. 
                                                                          Data are not expected to be published by deprivation in 2021 so another bespoke information request will 
                                                                          have to be raised to obtain this information."),
                                                                        p(
                                                                          tags$a(href="https://www.stag.scot.nhs.uk/",
                                                                                 "More information on STAG can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("e) Drug-related hospital statistics.",
                                                                      tagList(
                                                                        p("As with alcohol-related hospital statistics, data for this indicator were sourced from routinely
                                                                          collected SMR01 and SMR04 national datasets. Patients are defined as unique individuals discharged 
                                                                          from hospital within the financial year and are counted only once within each time period. 
                                                                          New patients are defined as patients discharged from hospital within the financial year who did not 
                                                                          have a drug-related hospital discharge within the last 10 years."),
                                                                        p("Some caution is necessary when using these data as problematic substance use may only be suspected
                                                                          and may not always be recorded by the hospital, and where problematic substance use was recorded, it
                                                                          may not be possible to identify which drug(s) may be involved."),
                                                                        p("During October and November 2018, ISD conducted a customer consultation about a proposed
                                                                          change in the definition of a drug-related hospital stay to include admissions due to drug poisoning/
                                                                          overdose. This proposed change would widen the range of stays captured in these statistics in order
                                                                          to measure drug-related hospital activity more comprehensively. As users agreed with the proposed
                                                                          change, hospital admissions resulting from drug poisoning/overdose are included within the
                                                                          definition of a drug-related hospital stay reported in these statistics. 
                                                                          This change widened the range of stays captured in these statistics, measuring drug-related hospital activity more comprehensively."),
                                                                        p("A European Age-sex Standardised Rate (EASR) was calculated because hospital activity rates may
                                                                          vary with the age-sex structure of the populations. The European Standard Population (ESP) was
                                                                          used to calculate EASRs within the most recent publications. The ESP was originally introduced in
                                                                          1976 and was revised in 2013. Before 2014 the publication used ESP1976 to calculate EASRs. Since
                                                                          2014, ESP2013 has been used to calculate EASRs for all years (including those before 2012/13).
                                                                          Therefore, findings from publications since February 2014 are not comparable with earlier
                                                                          publications."),
                                                                        p("Data are available from 2009/10 to 2018/19. Socioeconomic deprivation data were available by 
                                                                          SIMD quintile and the difference shown was for the 20% most and 20% least deprived areas in Scotland."),
                                                                        p("The next report will contain 2019/20 figures and is scheduled to be published in November 2021."),
                                                                        p(
                                                                          tags$a(href="https://beta.isdscotland.org/find-publications-and-data/lifestyle-and-behaviours/substance-use/drug-related-hospital-statistics/",
                                                                                 "More information on drug-related hospital statistics can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("f) Drug-related deaths.",
                                                                      tagList(
                                                                        p("Data for this indicator were taken from the National Records of Scotland (NRS) and were produced 
                                                                          using a definition of ‘drug-related deaths’ which was introduced in 2001 for the ‘baseline’ figures 
                                                                          for the UK Drugs Strategy. The ‘baseline’ definition covers the following cause of death where either:",
                                                                          tags$ul(
                                                                            tags$li("the underlying cause of death has been coded to a sub-set of ‘mental and behavioural disorders due to psychoactive substance use’ categories; or"),
                                                                            tags$li("it has been coded under a sub-set of poisoning, self-poisoning and assault categories
                                                                              and where a drug listed under the Misuse of Drugs Act (1971) was known to be present
                                                                              in the body at the time of death (even if the pathologist did not consider the drug to
                                                                              have had any direct contribution to the death)."))),
                                                                        p("Deaths where a drug listed under the Misuse of Drugs Act was likely to be present only as part
                                                                          of a compound analgesic/cold remedy or deaths where the direct cause of death was secondary
                                                                          infections or later complications of drug use were excluded from the final figures."),
                                                                        p("If a drug’s legal status changes, NRS aims to count it on the basis of its classification on the day
                                                                          the person died. Deaths are coded according to the ICD-10, which has been used by NRS since
                                                                          the start of 2000."),
                                                                        p("This indictor covers the years from 2009 to 2019. Socioeconomic deprivation data were available by SIMD quintile and the difference shown was
                                                                          for the 20% most and 20% least deprived areas in Scotland. Data by deprivation were requested
                                                                          from NRS via a bespoke information request and formatted. The SIMD quintiles are based on datazone counts. 
                                                                          Quintile 1 therefore contains the 20% most deprived datazones in Scotland."),
                                                                        p("The next report will contain 2020 figures."),
                                                                        p(
                                                                          tags$a(href="https://www.nrscotland.gov.uk/statistics-and-data/future-publications/",
                                                                                 "Its publication date will be given here", target="_blank")),
                                                                        p(
                                                                          tags$a(href="https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/vital-events/deaths/drug-related-deaths-in-scotland/",
                                                                                 "More information on drug-related deaths can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("g) Level of self-reported health rating among people who inject drugs.",
                                                                      tagList(
                                                                        p("Data for people who inject drugs (PWID) were sourced from the Needle Exchange Surveillance
                                                                          Initiative (NESI), which aims to measure and monitor injecting risk behaviours among people who
                                                                          inject drugs in Scotland. A cross-sectional, voluntary, anonymous, bio-behavioural survey approach
                                                                          was used to recruit and interview PWID. Trained interviewers recruited participants from selected
                                                                          agencies and pharmacies that provided injecting equipment; these settings may also provide other
                                                                          harm-reduction services, such as prescribed methadone. Approximately 60% of PWID approached
                                                                          to participate in each survey consent to do so, equating to around 2,500 PWID per sweep, 10–15%
                                                                          of the PWID population in Scotland."),
                                                                        p("Clients attending these services were invited to take part if they had injected drugs on at least one
                                                                          occasion either recently or in the past, and if it was the first time they had participated in the current
                                                                          survey. Participants completed a short interviewer-administered questionnaire and then provided
                                                                          a voluntary blood spot sample for anonymous testing for blood-borne virus markers."),
                                                                        p("Recruitment of people who have ever injected in the past, but not in the previous six months,
                                                                          was limited to 20–30% of participants during each survey. In addition, the number of individuals
                                                                          reporting injection of image and performance enhancing drugs (IPED) alone was capped at
                                                                          5% of total recruitment at each site."),
                                                                        p("For self-reported health indicators, participants were then asked three questions relating to their health
                                                                          (mobility, self-care, and if they could do usual activities) and were asked to note which of the five
                                                                          categories best corresponded to them: no problems, slight problems, moderate problems, severe
                                                                          problems or were unable to perform task. Participants were then asked two questions about
                                                                          pain/discomfort and anxiety/depression and asked to note which of the five categories
                                                                          best corresponded to them: none, slight, moderate, severe or extreme. Results are given as the percentage of PWID who responded as
                                                                          moderate or worse to the health category questions. No definitions were given as to what would
                                                                          qualify as a slight, moderate, severe or extreme problem. Therefore the perception of severity of
                                                                          problem may vary by individual perspective or pain threshold."),
                                                                        p("Data are available for 2017-18 only. The survey period accounts for data over two combined calendar years rather than financial year.
                                                                          As an example, the 2017–18 figure refers to a combination of the two calendar years of 2017
                                                                          and 2018 together rather than the financial year from 1 April 2017 to 31 March 2018."),
                                                                        p("Data on self-reported health were requested from the Needle Exchange Surveillance Initiative
                                                                          via a bespoke information request and formatted. The next report, scheduled to be published at some point during 2021,
                                                                          is unlikely to contain 2019-20 figures for self-reported health so another bespoke information request will have to be raised."),
                                                                        p(
                                                                          tags$a(href="https://www.hps.scot.nhs.uk/web-resources-container/needle-exchange-surveillance-initiative-nesi-2008-09-to-2017-18/",
                                                                                 "More information on the NESI report can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("h) Prevalence of naloxone carriage in people who inject drugs.",
                                                                      tagList(
                                                                        p("Data for people who inject drugs (PWID) were sourced from the Needle Exchange Surveillance
                                                                          Initiative (NESI), which aims to measure and monitor injecting risk behaviours among people who
                                                                          inject drugs in Scotland. A cross-sectional, voluntary, anonymous, bio-behavioural survey approach
                                                                          was used to recruit and interview PWID. Trained interviewers recruited participants from selected
                                                                          agencies and pharmacies that provided injecting equipment; these settings may also provide other
                                                                          harm-reduction services, such as prescribed methadone. Approximately 60% of PWID approached
                                                                          to participate in each survey consent to do so, equating to around 2,500 PWID per sweep, 10–15%
                                                                          of the PWID population in Scotland."),
                                                                        p("Clients attending these services were invited to take part if they had injected drugs on at least one
                                                                          occasion either recently or in the past, and if it was the first time they had participated in the current
                                                                          survey. Participants completed a short interviewer-administered questionnaire and then provided
                                                                          a voluntary blood spot sample for anonymous testing for blood-borne virus markers."),
                                                                        p("Recruitment of people who have ever injected in the past, but not in the previous six months,
                                                                          was limited to 20–30% of participants during each survey. In addition, the number of individuals
                                                                          reporting injection of image and performance enhancing drugs (IPED) alone was capped at
                                                                          5% of total recruitment at each site."),
                                                                        p("For 2011–12, the low prevalence of Naloxone carriage (8%) and low number of PWID responding
                                                                          to this question (175) compared to subsequent years (745 in 2013–14, 1,295 in 2015–16 and
                                                                          1,299 in 2017–18) accurately reflects how at the time this was a new intervention (the National
                                                                          Naloxone Programme started in 2011). But figures should still be interpreted with caution when
                                                                          comparing other years’ data with that recorded from 2011–12."),
                                                                        p("Data are available from 2011-12 to 2017-18. The survey period accounts for data over two combined calendar years rather than financial year.
                                                                          As an example, the 2017–18 figure refers to a combination of the two calendar years of 2017
                                                                          and 2018 together rather than the financial year from 1 April 2017 to 31 March 2018."),
                                                                        p("The next report will contain 2019-20 figures and is scheduled to be published at some point during 2021."),
                                                                        p(
                                                                          tags$a(href="https://www.hps.scot.nhs.uk/web-resources-container/needle-exchange-surveillance-initiative-nesi-2008-09-to-2017-18/",
                                                                                 "More information on the NESI report can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("i) Prevalence of needle/syringe re-use among people who inject drugs.",
                                                                      tagList(
                                                                        p("Data for people who inject drugs (PWID) were sourced from the Needle Exchange Surveillance
                                                                          Initiative (NESI), which aims to measure and monitor injecting risk behaviours among people who
                                                                          inject drugs in Scotland. A cross-sectional, voluntary, anonymous, bio-behavioural survey approach
                                                                          was used to recruit and interview PWID. Trained interviewers recruited participants from selected
                                                                          agencies and pharmacies that provided injecting equipment; these settings may also provide other
                                                                          harm-reduction services, such as prescribed methadone. Approximately 60% of PWID approached
                                                                          to participate in each survey consent to do so, equating to around 2,500 PWID per sweep, 10–15%
                                                                          of the PWID population in Scotland."),
                                                                        p("Clients attending these services were invited to take part if they had injected drugs on at least one
                                                                          occasion either recently or in the past, and if it was the first time they had participated in the current
                                                                          survey. Participants completed a short interviewer-administered questionnaire and then provided
                                                                          a voluntary blood spot sample for anonymous testing for blood-borne virus markers."),
                                                                        p("Recruitment of people who have ever injected in the past, but not in the previous six months,
                                                                          was limited to 20–30% of participants during each survey. In addition, the number of individuals
                                                                          reporting injection of image and performance enhancing drugs (IPED) alone was capped at
                                                                          5% of total recruitment at each site."),
                                                                        p("Data are available from 2008-09 to 2017-18. The survey period accounts for data over two combined calendar years rather than financial year.
                                                                          As an example, the 2017–18 figure refers to a combination of the two calendar years of 2017
                                                                          and 2018 together rather than the financial year from 1 April 2017 to 31 March 2018."),
                                                                        p("The next report will contain 2019-20 figures and is scheduled to be published at some point during 2021."),
                                                                        p(
                                                                          tags$a(href="https://www.hps.scot.nhs.uk/web-resources-container/needle-exchange-surveillance-initiative-nesi-2008-09-to-2017-18/",
                                                                                 "More information on the NESI report can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("j) Prevalence of recent non-fatal overdose among people who inject drugs.",
                                                                      tagList(
                                                                        p("Data for people who inject drugs (PWID) were sourced from the Needle Exchange Surveillance
                                                                          Initiative (NESI), which aims to measure and monitor injecting risk behaviours among people who
                                                                          inject drugs in Scotland. A cross-sectional, voluntary, anonymous, bio-behavioural survey approach
                                                                          was used to recruit and interview PWID. Trained interviewers recruited participants from selected
                                                                          agencies and pharmacies that provided injecting equipment; these settings may also provide other
                                                                          harm-reduction services, such as prescribed methadone. Approximately 60% of PWID approached
                                                                          to participate in each survey consent to do so, equating to around 2,500 PWID per sweep, 10–15%
                                                                          of the PWID population in Scotland."),
                                                                        p("Clients attending these services were invited to take part if they had injected drugs on at least one
                                                                          occasion either recently or in the past, and if it was the first time they had participated in the current
                                                                          survey. Participants completed a short interviewer-administered questionnaire and then provided
                                                                          a voluntary blood spot sample for anonymous testing for blood-borne virus markers."),
                                                                        p("Recruitment of people who have ever injected in the past, but not in the previous six months,
                                                                          was limited to 20–30% of participants during each survey. In addition, the number of individuals
                                                                          reporting injection of image and performance enhancing drugs (IPED) alone was capped at
                                                                          5% of total recruitment at each site."),
                                                                        p("Data are available for 2017-18 only. The survey period accounts for data over two combined calendar years rather than financial year.
                                                                          As an example, the 2017–18 figure refers to a combination of the two calendar years of 2017
                                                                          and 2018 together rather than the financial year from 1 April 2017 to 31 March 2018."),
                                                                        p("The next report will contain 2019-20 figures and is scheduled to be published at some point during 2021."),
                                                                        p(
                                                                          tags$a(href="https://www.hps.scot.nhs.uk/web-resources-container/needle-exchange-surveillance-initiative-nesi-2008-09-to-2017-18/",
                                                                                 "More information on the NESI report can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("k) The gradient in the burden of alcohol and substance use disorders.",
                                                                      tagList(
                                                                        p("Data were taken from the Scottish Burden of Disease (SBOD) study to estimate the difference
                                                                          between ideal and actual health in a country or region at a specific point in time. Individuals can
                                                                          suffer non-fatal health loss due to suffering disability attributable to a disease, or suffer fatal health
                                                                          loss which is early death due to a disease."),
                                                                        p("To quantify the total burden, non-fatal and fatal health loss are combined to produce a single
                                                                          metric called the Disability-Adjusted Life Year (DALY).These are calculated by combining estimates
                                                                          from two individual metrics: Years of Life Lost (YLL) due to premature mortality and Years Lived
                                                                          with Disability (YLD). YLL measures the years of life lost due to premature deaths, the fatal
                                                                          component of burden of disease. YLLs are calculated by subtracting the age at each alcohol
                                                                          dependence (or drug-use disorder) death from the expected remaining life expectancy for a
                                                                          person at that age. YLDs are estimated using disease and injury prevalence estimates, levels
                                                                          of severity and disability weights."),
                                                                        p("All SBOD data presented for alcohol dependence (or drug-use disorders) are three-year averages
                                                                          for the period 2014 to 2016. A three-year period was used to smooth out most of the effect if the
                                                                          mortality or morbidity of a single year happens to be unusual. For this reason we do not show trend
                                                                          data from 2014 to 2016."),
                                                                        p("Rankings were based on the descending order of DALYs, YLDs and YLLs out of a total of 68 causes.
                                                                          For mental health and substance use disorders DALYs, YLDs and YLLs were ranked out of a total of
                                                                          seven causes."),
                                                                        p("Socioeconomic deprivation data were available by SIMD quintile. Data were pulled and formatted
                                                                          from SBOD Injuries and Risk Factors 2016 reports."),
                                                                        p("The next report will contain 2019 figures and is scheduled to be published at some point during 2021."),
                                                                        p(
                                                                          tags$a(href="https://www.scotpho.org.uk/comparative-health/burden-of-disease/overview/",
                                                                                 "More information on SBOD can be found here", target="_blank"))
                                                                      ))
                                           ))),
                          
                          
                          
                          #Indicators for outcome b
                          h3("Outcome b: Reduce alcohol- and drug-related social harms"), 
                          
                          wellPanel(column(12,
                                           bsCollapse(id = "harms_outcome_b", open = "Panel 1", #PanelSet id
                                                      bsCollapsePanel("a) Percentage of victims of violent crime who suspect offender was under influence of drugs.",
                                                                      tagList(
                                                                        p("Data for these indicators were sourced from the Scottish Crime and Justice Survey (SCJS), a
                                                                          large-scale social survey which asks people about their experiences and perceptions of crime.
                                                                          The SCJS gathers information from a representative sample rather than from the whole population
                                                                          and so results are always estimates, not precise figures. This means that the results are subject
                                                                          to a margin of error which can have an impact on how changes in the numbers should be
                                                                          interpreted, especially in the short term."),
                                                                        p("Data for these indicators refer to if the crime was committed against the victim in the previous
                                                                          12 months from when they were surveyed rather than if they have ever been the victim of violent crime. 
                                                                          There was no indication as to if they suspected the offender was under the
                                                                          influence at the time of the crime or if they retrospectively suspected the offender was under
                                                                          the influence at the time of their interview."),
                                                                        p("The figures in this report are based on percentages rather than numbers sampled within the SCJS.
                                                                          These figures should be interpreted with caution. It is possible that if there has been a large increase
                                                                          in numbers of crimes committed within these categories then the number of crimes which involved
                                                                          drugs could increase even if drug involvement has decreased."),
                                                                        p("Data for this indicator are available for most years from 2009/10 to 2018/19. 
                                                                          These questions were not asked as part of the 2011/12, 2013/14 or 2015/16 surveys so data were not reported on for these years."),
                                                                        p("The next report will contain 2019/20 figures and is scheduled to be published in June 2021."),
                                                                        p(
                                                                          tags$a(href="https://www.gov.scot/collections/scottish-crime-and-justice-survey/",
                                                                                 "More information on the SCJS can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("b) Percentage who report drug dealing/‘abuse’ as most common issue in their local area.",
                                                                      tagList(
                                                                        p("Data for this indicator were taken from the Scottish Household Survey (SHS) and responses of
                                                                          ‘fairly’ and ‘very’ were aggregated together when the respondent was asked ‘How common is
                                                                          drug misuse or dealing within your local neighbourhood?’ as were responses of 'Yes' when asked
                                                                          'Have you experienced drug misuse or dealing within your local neighbourhood?'"),
                                                                        p("The SHS can only produce estimates and these estimates are limited by a number of factors such
                                                                          as sample coverage and sampling variability (the number of cases that analysis is based on and
                                                                          the bias in the achieved sample). The SHS is also limited in the amount of detail it can collect about
                                                                          some topics (such as economic activity). Users especially need to be mindful of the sampling errors
                                                                          for analysis when this is based on breakdowns by deprivation."),
                                                                        p("It is not always necessary to have direct personal experience of an issue to know about it or perceive
                                                                          it as a problem in an area. What people may define as an issue is related to their own perceptions,
                                                                          beliefs and definitions. For instance, one respondent may consider witnessing drug misuse or
                                                                          dealing as experiencing the issue, while another respondent may only report experience of this
                                                                          problem if they have personally been offered drugs."),
                                                                        p("Data are available from 2012 to 2019. Socioeconomic deprivation data were available by SIMD quintile 
                                                                          and the difference shown was for the 20% most and 20% least deprived areas in Scotland."),
                                                                        p("The next report will contain 2020 figures and is scheduled to be published in September 2021."),
                                                                        p(
                                                                          tags$a(href="https://shs.theapsgroup.scot/2019/",
                                                                                 "More information on the SHS can be found here", target="_blank"))
                                                                        
                                                                      )),
                                                      bsCollapsePanel("c) Prevalence of drug injecting in public places among people who inject drugs.",
                                                                      tagList(
                                                                        p("Data for people who inject drugs (PWID) were sourced from the Needle Exchange Surveillance
                                                                          Initiative (NESI), which aims to measure and monitor injecting risk behaviours among people who
                                                                          inject drugs in Scotland. A cross-sectional, voluntary, anonymous, bio-behavioural survey approach
                                                                          was used to recruit and interview PWID. Trained interviewers recruited participants from selected
                                                                          agencies and pharmacies that provided injecting equipment; these settings may also provide other
                                                                          harm-reduction services, such as prescribed methadone. Approximately 60% of PWID approached
                                                                          to participate in each survey consent to do so, equating to around 2,500 PWID per sweep, 10–15%
                                                                          of the PWID population in Scotland."),
                                                                        p("Clients attending these services were invited to take part if they had injected drugs on at least one
                                                                          occasion either recently or in the past, and if it was the first time they had participated in the current
                                                                          survey. Participants completed a short interviewer-administered questionnaire and then provided
                                                                          a voluntary blood spot sample for anonymous testing for blood-borne virus markers."),
                                                                        p("Recruitment of people who have ever injected in the past, but not in the previous six months,
                                                                          was limited to 20–30% of participants during each survey. In addition, the number of individuals
                                                                          reporting injection of image and performance enhancing drugs (IPED) alone was capped at
                                                                          5% of total recruitment at each site."),
                                                                        p("A public place is defined as a stairwell/close, squat/abandoned house, public toilet, car park or an outdoor space 
                                                                          (such as park, alleyway or street))."),
                                                                        p("Data are available for 2017-18 only. The survey period accounts for data over two combined calendar years rather than financial year.
                                                                          As an example, the 2017–18 figure refers to a combination of the two calendar years of 2017
                                                                          and 2018 together rather than the financial year from 1 April 2017 to 31 March 2018."),
                                                                        p("Data on public injecting were requested from the Needle Exchange Surveillance Initiative via a bespoke information request and formatted. 
                                                                           The next report, scheduled to be published at some point during 2021, is unlikely to contain 2019-20 figures for public injecting 
                                                                           so another information request will have to be raised."),
                                                                        p(
                                                                          tags$a(href="https://www.hps.scot.nhs.uk/web-resources-container/needle-exchange-surveillance-initiative-nesi-2008-09-to-2017-18/",
                                                                                 "More information on the NESI report can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("d) Number of drug-related offences (possession and intent to supply).",
                                                                      tagList(
                                                                        p("Data for this indicator were based on the number and type of drug seizures made by Police
                                                                          Scotland. It does not include information on drugs seized by the UK Border Force or British Transport
                                                                          Police, or as a result of Police Scotland activity which led to drugs being seized outwith Scotland."),
                                                                        p("Police Scotland manually check drug supply-based crimes each year, with these records accounting
                                                                          for the clear majority of drugs seized in terms of quantity. The Scottish Government requests data
                                                                          on drug supply-related crimes from Police Scotland’s Analysis and Performance Unit (APU)."),
                                                                        p("This was supplemented by a sample led by Scottish Government statisticians of the remaining
                                                                          and far more numerous crimes of drug possession. The sample consists of a random selection of
                                                                          400 crime records per year, stratified by police division to reflect the distribution of drug possession
                                                                          crimes across Scotland. This sample-based approach avoids the need to manually extract data for
                                                                          every crime record relating to a drugs possession crime, which would be a very resource-intensive
                                                                          process."),
                                                                        p("The quantity of drugs seized can fluctuate considerably each year and does not necessarily move
                                                                          in line with the number of seizures made. While most drug seizures consist of relatively small
                                                                          quantities (usually possession-related crimes), annual quantities of drugs seized can be greatly
                                                                          influenced by a small number of large seizures (usually supply-related crimes)."),
                                                                        p("Changes in both possession and supply of drugs over time could show alterations in the policing
                                                                          of drug laws (such as lower rates of stop and search since 2014/15) rather than changes in the
                                                                          prevalence of associated crime. Therefore, changes to these figures could be interpreted more as
                                                                          a measure of police practice than drug use or availability."),
                                                                        p("Data by drug class are available from 2014/15 to 2017/18. The Recorded Crime in Scotland 2019/20 
                                                                          report contains data by drug type from 2014/15 to 2018/19 and is reported on here."),
                                                                        p("The next Drug seizures and offender characteristics report will contain 2018/19 figures for drug class 
                                                                          and is scheduled to be published in April 2021."),
                                                                        p(
                                                                          tags$a(href="https://www.gov.scot/publications/drug-seizures-offender-characteristics-2017-18/",
                                                                                 "More information on drug-related offences can be found here", target="_blank"))
                                                                        
                                                                      )),
                                                      bsCollapsePanel("e) Number of homicides where motive for homicide was drug-related.",
                                                                      tagList(
                                                                        p("A ‘drug-related’ homicide was defined as a homicide motivated by a need to obtain drugs or money
                                                                          for drugs, a homicide of a consumer or supplier of drugs, a homicide committed in order to steal
                                                                          proceeds of the drugs trade or a homicide as a consequence of rivalry between users and/or dealers
                                                                          within the drugs trade."),
                                                                        p("Data for this indicator were based on a snapshot of Police Scotland’s live homicide database at
                                                                          an agreed date. Since it is a ‘live’ system, amendments to Police Scotland’s database can arise
                                                                          after the data has been submitted to the Scottish Government (such as a serious assault may be
                                                                          reclassified to a culpable homicide sometime after the crime was first recorded by the police). To
                                                                          allow for these changes, retrospective revisions have been made to earlier data for all years since
                                                                          the formation of Police Scotland (2013/14 onwards)."),
                                                                        p("When producing the 2017/18 statistics, there was a tendency for the first publication of figures for
                                                                          a particular year to be revised upwards in subsequent bulletins – due to developments with specific
                                                                          cases (such as victim of serious assault dies of their injuries sometime after the incident, which could
                                                                          subsequently lead to the case being reclassified to a culpable homicide)."),
                                                                        p("The numbers of homicides suspected to have been for a drug-related motive were broadly stable
                                                                          from 2009/10 to 2015/16, not rising above 10 for this period. Police Scotland advised the reported
                                                                          increase in homicide cases with a drug-related motive since then may be due at least in part to
                                                                          an ongoing improvement in recording practice (better identification of where motives can include
                                                                          a drug-related element). As pre-2016/17 figures are likely to be underestimated, caution is to be
                                                                          exercised when comparing this variable with earlier years."),
                                                                        p("Data are available from 2009/10 to 2019/20. 
                                                                          The next report will contain 2020/21 figures and is scheduled to be published in October 2021."),
                                                                        p(
                                                                          tags$a(href="https://www.gov.scot/publications/homicide-scotland-2018-19/",
                                                                                 "More information on drug-related homicides can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("f) Alcohol-related crime (victim reports offender and/or self under influence).",
                                                                      tagList(
                                                                        tagList(
                                                                          p("Data for these indicators were sourced from the Scottish Crime and Justice Survey (SCJS), a
                                                                          large-scale social survey which asks people about their experiences and perceptions of crime.
                                                                          The SCJS gathers information from a representative sample rather than from the whole population
                                                                          and so results are always estimates, not precise figures. This means that the results are subject
                                                                          to a margin of error which can have an impact on how changes in the numbers should be
                                                                          interpreted, especially in the short term."),
                                                                          p("Data for these indicators refer to if the crime was committed against the victim in the previous
                                                                          12 months from when they were surveyed rather than if they have ever been the victim of assault. 
                                                                          There was no indication as to if they suspected the offender was under the
                                                                          influence at the time of the crime or if they retrospectively suspected the offender was under
                                                                          the influence at the time of their interview."),
                                                                          p("The figures in this report are based on percentages rather than numbers sampled within the SCJS.
                                                                          These figures should be interpreted with caution. It is possible that if there has been a large increase
                                                                          in numbers of crimes committed within these categories then the number of crimes which involved
                                                                          alcohol could increase even if alcohol involvement has decreased."),
                                                                          p("Data for this indicator are available for most years from 2009/10 to 2018/19. 
                                                                            These questions were not asked as part of the 2011/12, 2013/14 or 2015/16 surveys so data were not reported on for these years. "),
                                                                          p("The next report will contain 2019/20 figures and is scheduled to be published in June 2021."),
                                                                          p(
                                                                            tags$a(href="https://www.gov.scot/collections/scottish-crime-and-justice-survey/",
                                                                                   "More information on the SCJS can be found here", target="_blank"))
                                                                        )))
                                           ))),
                          
                          tags$a(href = '#top',  
                                 icon("circle-arrow-up", lib= "glyphicon"),"Back to top of page")
                          
                          )

                
                  ) #End of UI script

                 ) #End of secure_app. Can be removed for open access publication.

} #End of whole script

shinyApp(ui = ui, server = server)

