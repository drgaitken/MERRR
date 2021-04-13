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

##############################################.
############## Data Input ----
##############################################.

#Only use below if need to go into Excel file to make changes before publication
#MERRR_ <- read_excel("data/MERRRFinal.xlsx")

#saveRDS(MERRR_, "data/MERRRFinal.rds")

MERRR <- readRDS("data/MERRRFinal.rds")

#Create a shortened version removing superfluous columns for data table display later

MERRR2 <- MERRR %>%select(RRR_Conc,
                          Indicator_Text,
                          Definition,
                          Inequalities_Type,
                          Period,
                          Measure,
                          Number)


#Create credentials for password protection

credentials_df <- data.frame(
  user = c("MERRR2021"), # mandatory
  password = c("MERRR_2%21"), # mandatory
  stringsAsFactors = FALSE)

saveRDS(credentials_df, "shiny_app/admin/credentials.rds")

#Beginning of script


{

  
#Beginning of Server script

##############################################.
############## Server ----
##############################################.

#First set credentials for password protection  
credentials <- readRDS("shiny_app/admin/credentials.rds")
  
  
#Beginning of server
server  <-  function(input, output, session)
{

  # Shinymanager Auth
  
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
    tooltip_MERRR1 <- paste0(
      MERRR_1_new()$Inequalities_Type,
      "<br>","Year: ", MERRR_1_new()$Period,
      "<br>", paste0(unique(MERRR_1_new()$Definition)),": ",formatC(MERRR_1_new()$Measure, big.mark = ",",digits = 1,format = 'f'),
      "<br>","Number: ", formatC(MERRR_1_new()$Number, big.mark = ",",digits = 0,format = 'f'),
      "<br>","Data Source: ", MERRR_1_new()$Data_Source
    )
    
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
        width = 1200,
        height = 650
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
                       showline = TRUE, tickangle = 270, fixedrange=TRUE),
          font=list(family = '"Helvetica Neue", Helvetica, Arial, sans-serif'),
          showlegend = TRUE,
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
        width = 1200,
        height = 650
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
  
  
}
#End of Server script
  
  
  

#Beginning of UI script

##############################################.
############## User Interface ----
##############################################.
#Use secure_app function for password protection
  
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
                          
                          p(
                            "Rights, respect and recovery: Scotland's strategy to improve health by preventing
                              and reducing alcohol and drug use, harm and related deaths (subsequently referred
                              to as Rights, Respect and Recovery (RRR)) was published by Scottish Government in 2018.
                              During the course of the development of the strategy, Scottish Government invited
                              NHS Health Scotland (now Public Health Scotland) to develop a monitoring and evaluation framework for Rights, Respect and
                              Recovery (subsequently referred to as the ",
                            HTML(paste0('<b><a href="http://healthscotland.scot/publications/monitoring-and-evaluation-framework-for-rights-respect-and-recovery/">MERRR Framework</a></b>)')),
                            "to assess the implementation, progress and outcomes of the strategy. "
                          ),
                          
                          p(
                            "This MERRR Framework detailed a set of indicators (subsequently referred to as the indicator
                              set) aligned to outcomes featured in a theory of change (TOC). These indicators are
                              a key component of the MERRR Framework and will be displayed here to
                              provide insight into the implementation and impact of MERRR. "
                          ),
                          
                          
                          p(
                            "This MERRR Portal was created to allow users to interactively explore the various different indicators, and their associated inequalities, produced by the MERRR Portfolio.
                              The indicators contained within this portal are to be viewed in combination with the", #update this once have webpage link for report                            
                            HTML(paste0('<b><a href="http://healthscotland.scot/publications/monitoring-and-evaluation-framework-for-rights-respect-and-recovery/">MERRR Report</a></b>.')),
                            "As of May 2021, we report on data for 92 out of 110 indicators defined within the MERRR Framework. We were unable to report data for the remaining 18 indicators as
                             this information is not possible to capture, information is not available yet or information is available but not 
                             reported on here due to data quality issues."
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
                            "The final section of this portal summarises of all the information contained within this portal.
                              To natigate to each of these sections, please select a relevant tab from the top of this page."
                          ),
                          
                          p(
                            "We aim to update this portal frequently with the most up-to-date information as it becomes available. As a result, the interpretation and narrative provided within the MERRR Report
                             is subject to change. Over time, additional indicators defined in the MERRR Framework, where we have not been able to obtain information from thus far, 
                             may be incorporated into this portal. New indicators, not defined as part of the MERRR Framework, may arise and may also be incorporated into this portal.",
                          ),
                          
                          p(
                            "If you experience any problems using this dashboard or have further questions relating to the data, please contact our Evaluation team mailbox at:",
                            HTML(paste0('<b> <a href="mailto:phs.evaluationteam@phs.scot">phs.evaluationteam@phs.scot</a></b>.'))
                          )),
                 
                 
                 
                 
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
                                                                        p("The charts can be modified using the drop down box. 
                                                   These indicators are ordered by RRR chapter and information for each indicator is 
                                                                displayed by selecting (from left to right) the following tabs:",
                                                                          tags$ul(
                                                                            tags$li("RRR Chapter: Chapter associated with the RRR Portfolio;"),
                                                                            tags$li("RRR Outcome: Outcome associated with the selected RRR Chapter; and,"),
                                                                            tags$li("RRR Indicator: Indicator associated with the selected RRR Outcome and grouping associated with RRR Indicator (if applicable).")
                                                                          ),
                                                                          p("Click on the drop down boxes to bring up a full list of Chapters, Outcomes or indicators.
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
                                                                            tags$li("The group of interest being monitiored (where applicable and normally broken down by age, gender or deprivation);"),
                                                                            tags$li("Year of interest;"),
                                                                            tags$li("Data measure (normally a percentage, rate or number);"),
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
                                                                              " - click the download icon under the chart to download the data in this format "
                                                                            )))))
                                           ))),
                          br(),
                          hr(style = "border-top: 2px solid #000000;"),
                          
                          
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
                            h4("Main points:"), 
                            p(
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
                            
                          )),
                 
                 
                 
                 #Now start to split dashboard into tabs relevant to each RRR Chapter
                 #First create a tab in the dashboard which will provide information about chapter 1 
                 tabPanel("1) Prevention & Early Intervention", icon = icon("hand-paper"),
                          
                          h2("Chapter 1 - Prevention and Early Intervention"), 
                          
                          p(
                            "The overall outcome for this part of the strategy is for fewer people to develop problem alcohol and drug use. 
                               The TOC model identifies interim outcomes to achieve this overall outcome. These are:",
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
                                                                        p("Data comes from Scottish Household survey...blurb here....next updated in June 2021"))),
                                                      bsCollapsePanel("b) Child poverty rates in local authority area.",
                                                                      tagList(
                                                                        p("Data comes from HMRC...blurb here....next updated in June 2021"))),
                                                      bsCollapsePanel("c) Child poverty rates nationally.",
                                                                      tagList(
                                                                        p("Data comes from HMRC...blurb here....next updated in June 2021"))),
                                                      bsCollapsePanel("d) Delivery of Fairer Scotland Action Plan.",
                                                                      tagList(
                                                                        p("Data comes from Fairer Scotland Action plan...blurb here....next updated in June 2021"))),
                                                      bsCollapsePanel("e) Actions taken from the collaboration between SG, Public Health Scotland, NHS Boards and local authorities to reduce health inequalities report.",
                                                                      tagList(
                                                                        p("No information available.... reason why...."))),
                                                      bsCollapsePanel("f) Deprivation gap in initial school leavers entering positive destinations.",
                                                                      tagList(
                                                                        p("Data comes from school leavers survey...blurb here....next updated in June 2021"))),
                                                      bsCollapsePanel("g) Deprivation gap in Annual Participation Measure.",
                                                                      tagList(
                                                                        p("Data comes from APM report...blurb here....next updated in June 2021.")))
                                           ))),
                          
                          #Indicators for outcome b
                          h3("Outcome b) Education provision for children and young people is more in line with evidence and best practices"), 
                          
                          p(
                            "Blurb here about why nothing for this indicator."
                          ),
                          
                          #Indicators for outcome c
                          h3("Outcome c) Increase in the number of people at risk of alcohol or drug problems linked to positive environments and opportunities"), 
                          
                          wellPanel(column(12,
                                           bsCollapse(id = "prev_outcome_c", open = "Panel 1", #PanelSet id
                                                      bsCollapsePanel("a) Percentage of S2 and S4 pupils who participated in sports clubs, gyms, exercise or dance groups in the last 12 months.",
                                                                      tagList(
                                                                        p("Data comes from SALSUS...blurb here....next updated in June 2021"))),
                                                      bsCollapsePanel("b) Number and percentage of young people from the lowest SIMD quintile in initial school leavers entering positive destinations.",
                                                                      tagList(
                                                                        p("Data comes from school leaver survey...blurb here....next updated in June 2021"))),
                                                      bsCollapsePanel("c) Number and percentage of young people from the lowest SIMD quintile in annual participation measure.",
                                                                      tagList(
                                                                        p("Data comes from APM...blurb here....next updated in June 2021")))
                                           ))),
                          
                          
                          
                          #Indicators for outcome d
                          h3("Outcome d) Young people's capacity to make informed choices is improved"), 
                          
                          wellPanel(column(12,
                                           bsCollapse(id = "prev_outcome_d", open = "Panel 1", #PanelSet id
                                                      bsCollapsePanel("a) Young people’s attitude towards the risks of drug use.",
                                                                      tagList(
                                                                        p("Data comes from SALSUS...blurb here....next updated in June 2021"))),
                                                      bsCollapsePanel("b) Young people’s reported wellbeing.",
                                                                      tagList(
                                                                        p("Data comes from SALSUS...blurb here....next updated in June 2021"))),
                                                      bsCollapsePanel("c) Number of children and young people using drugs.",
                                                                      tagList(
                                                                        p("Data comes from SALSUS...blurb here....next updated in June 2021"))),
                                                      bsCollapsePanel("d) Number of young people using alcohol.",
                                                                      tagList(
                                                                        p("Not reported here. Focussing on drugs because......"))),
                                                      bsCollapsePanel("e) Actions taken from the collaboration between SG, Public Health Scotland, NHS Boards and local authorities to reduce health inequalities report.",
                                                                      tagList(
                                                                        p("Data comes from SALSUS...blurb here....next updated in June 2021"))),
                                                      bsCollapsePanel("f) Number and rate of young people admitted to hospital for drug-related admissions.",
                                                                      tagList(
                                                                        p("Data comes from school leavers survey...blurb here....next updated in June 2021"))),
                                                      bsCollapsePanel("g) Number and rate of young people admitted to hospital for alcohol-related admissions.",
                                                                      tagList(
                                                                        p("Not reported here. Focussing on drugs because......")))
                                           ))),
                          
                          
                          
                          
                          #Indicators for outcome d
                          h3("Outcome e) Increase in individual and community wellbeing, resilience, and social connectedness"), 
                          
                          wellPanel(column(12,
                                           bsCollapse(id = "prev_outcome_e", open = "Panel 1", #PanelSet id
                                                      bsCollapsePanel("a) Rating of neighbourhood as a place to live (including by SIMD) – perceptions, strengths, engagement with 
                                                       local community, social isolation, and feelings of loneliness",
                                                                      tagList(
                                                                        p("Data comes from SHS...blurb here....next updated in June 2021"))),
                                                      bsCollapsePanel("b) Feelings of safety in neighbourhood.",
                                                                      tagList(
                                                                        p("Data comes from SCJS...blurb here....next updated in June 2021"))),
                                                      bsCollapsePanel("c) Rating of drugs being a problem in neighbourhood.",
                                                                      tagList(
                                                                        p("Data comes from SALSUS...blurb here....next updated in June 2021"))),
                                                      bsCollapsePanel("d) Level of self-reported stigma related to drug use among people who inject drugs.",
                                                                      tagList(
                                                                        p("Data comes from NESI...blurb here....next updated in June 2021"))),
                                                      bsCollapsePanel("e) Social capital (and constituent parts – social networks, community cohesion, community empowerment 
                                                       and social participation) ratings by quintile.",
                                                                      tagList(
                                                                        p("Data comes from SALSUS...blurb here....next updated in June 2021"))),
                                                      bsCollapsePanel("f) Output from the expert group convened to examine stigma.",
                                                                      tagList(
                                                                        p("Not reported here because")))
                                           )))
                          
                 ),
                 
                 
                 
                 
                 #Now create a new tab in the dashboard which will provide information about chapter 2
                 tabPanel("2) Recovery Oriented Systems of Care", icon = icon("hands-helping"),
                          #This part of the page will provide information about chapter 2
                          
                          h2("Chapter 2 - Recovery Oriented Systems of Care"), 
                          
                          p(
                            "The overall outcome for this part of the strategy is for people to be able to access and benefit from
                                effective, integrated, person-centred support to achieve their own type of recovery. The TOC model
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
                                                                        p("Data comes from SRC...blurb here....next updated in June 2021"))),
                                                      bsCollapsePanel("b) Alcohol and drug partnership (ADP) investment (financial and otherwise) in local recovery communities.",
                                                                      tagList(
                                                                        p("Data comes from ADP Annual reports...blurb here....next updated in June 2021"))),
                                                      bsCollapsePanel("c) Percentage and number of people in services also involved with mutual aid/peer support/recovery groups.",
                                                                      tagList(
                                                                        p("Data not available because......")))
                                           ))),
                          
                          #Indicators for outcome b
                          h3("Outcome b: Improve access to and quality of treatment services, including harm reduction and low-threshold services, other support services and community supports"), 
                          
                          wellPanel(column(12,
                                           bsCollapse(id = "rosc_outcome_b", open = "Panel 1", #PanelSet id
                                                      bsCollapsePanel("a) Drug and alcohol treatment waiting times (primary waiting time).",
                                                                      tagList(
                                                                        p("Data comes from DATWT...blurb here....next updated in June 2021"))),
                                                      bsCollapsePanel("b) Drug and alcohol treatment waiting times (secondary waiting time).",
                                                                      tagList(
                                                                        p("Data not available because......"))),
                                                      bsCollapsePanel("c) Percentage of people who leave ‘treatment incomplete’ and discharge reason.",
                                                                      tagList(
                                                                        p("Data comes from SDMD...blurb here....next updated in June 2021"))),
                                                      bsCollapsePanel("d) Percentage of people completing treatment and discharge reason.",
                                                                      tagList(
                                                                        p("Data comes from DATWT...blurb here....next updated in June 2021"))),
                                                      bsCollapsePanel("e) Percentage breakdown of assessment appointment attendance (including reason for not).",
                                                                      tagList(
                                                                        p("Text here....."))),
                                                      bsCollapsePanel("f) Percentage breakdown of first treatment appointment attendance (including reason for not).",
                                                                      tagList(
                                                                        p("Text here....."))),
                                                      bsCollapsePanel("g) Percentage of reviews completed in line with recommendations, e.g. currently three-month and 12-month reviews.",
                                                                      tagList(
                                                                        p("Text here....."))),
                                                      bsCollapsePanel("h) Number of ADPs that report their commissioned treatment service(s) has feedback mechanism in place, and 
                                                                evidence/examples of how lived experience is informing the development, design and delivery of services.",
                                                                      tagList(
                                                                        p("Text here....."))),
                                                      bsCollapsePanel("i) Number of needles/syringes supplied from injecting equipment provision (IEP) services.",
                                                                      tagList(
                                                                        p("Text here....."))),
                                                      bsCollapsePanel("j) Ratio of IEP outlets per estimated ‘problem drug user’ estimate.",
                                                                      tagList(
                                                                        p("Text here....."))),
                                                      bsCollapsePanel("k) Number and type of IEP outlet, e.g. pharmacy, clinic, outreach.",
                                                                      tagList(
                                                                        p("Text here....."))),
                                                      bsCollapsePanel("l) Naloxone reach.",
                                                                      tagList(
                                                                        p("Text here....."))),
                                                      bsCollapsePanel("m) Estimated numbers of people receiving methadone.",
                                                                      tagList(
                                                                        p("Text here....."))),
                                                      bsCollapsePanel("n) Prevalence of opioid substitute treatment (OST) engagement among people who inject drugs.",
                                                                      tagList(
                                                                        p("Text here....."))),
                                                      bsCollapsePanel("o) Prevalence of illicit benzodiazepine use among people who inject drugs.",
                                                                      tagList(
                                                                        p("Text here.....")))
                                           ))),
                          
                          #Indicators for outcome c
                          h3("Outcome c: Increase availability and use of advocacy by those who require it at every stage of their recovery"), 
                          
                          wellPanel(column(12,
                                           bsCollapse(id = "rosc_outcome_c", open = "Panel 1", #PanelSet id
                                                      bsCollapsePanel("a) Monitor local investment in rights-based advocacy services for people with alcohol and other drug problems.",
                                                                      tagList(
                                                                        p("Text here...."))),
                                                      bsCollapsePanel("b) Monitor national investment in rights-based advocacy services for people with alcohol and other drug problems.",
                                                                      tagList(
                                                                        p("Text here...."))),
                                                      bsCollapsePanel("c) SRC monitoring and evaluation of Nation Recovery Advocacy Network output.",
                                                                      tagList(
                                                                        p("Text here....")))
                                           ))),
                          
                          
                          
                          #Indicators for outcome d
                          h3("Outcome d: Increase in person-centred approaches across treatment and recovery services and the range of health and social care services which work with people with alcohol and drug problems"), 
                          
                          wellPanel(column(12,
                                           bsCollapse(id = "rosc_outcome_d", open = "Panel 1", #PanelSet id
                                                      bsCollapsePanel("a) Number and percentage of ADPs self-reporting ROSC embedment.",
                                                                      tagList(
                                                                        p("Text here...."))),
                                                      bsCollapsePanel("b) Number and percentage of ADPs reporting different treatment options available in their area.",
                                                                      tagList(
                                                                        p("Text here...."))),
                                                      bsCollapsePanel("c) Number of different treatment options and their providers reported by each ADP area.",
                                                                      tagList(
                                                                        p("Text here...."))),
                                                      bsCollapsePanel("d) Percentage of people who have received any other interventions (as per Scottish Morbidity Record (SMR) 25b) since last review.",
                                                                      tagList(
                                                                        p("Text here...."))),
                                                      bsCollapsePanel("e) Number and percentage of ADPs with an action plan to implement the quality principles.",
                                                                      tagList(
                                                                        p("Text here....")))
                                           ))),
                          
                          
                          
                          
                          #Indicators for outcome e
                          h3("Outcome e: Increase the number of people leaving services with outcomes achieved, increased recovery capital and connected to aftercare and community (of choice)"), 
                          
                          wellPanel(column(12,
                                           bsCollapse(id = "rosc_outcome_e", open = "Panel 1", #PanelSet id
                                                      bsCollapsePanel("a) Percentage of people who leave ‘treatment incomplete’ and discharge reason.",
                                                                      tagList(
                                                                        p("Text here...."))),
                                                      bsCollapsePanel("b) Percentage of people completing treatment and discharge reason.",
                                                                      tagList(
                                                                        p("Text here....")))
                                           ))), 
                          
                          
                          #Indicators for outcome f
                          h3("Outcome f: Reduce the often coexisting complex issues related to harmful alcohol and other drug use, e.g. housing, mental health issues, family issues and so on"), 
                          
                          wellPanel(column(12,
                                           bsCollapse(id = "rosc_outcome_f", open = "Panel 1", #PanelSet id
                                                      bsCollapsePanel("a) Percentage of people who have received any other interventions (as per SMR 25b) since last review.",
                                                                      tagList(
                                                                        p("Text here...."))),
                                                      bsCollapsePanel("b) Percentage change in accommodation status from any other classification to ‘owner/rented – stable’ (i.e. secure) and vice versa.",
                                                                      tagList(
                                                                        p("Text here...."))),
                                                      bsCollapsePanel("c) Prevalence of homelessness among people who inject drugs.",
                                                                      tagList(
                                                                        p("Text here...."))),
                                                      bsCollapsePanel("d) Percentage of those using tobacco referred to cessation support.",
                                                                      tagList(
                                                                        p("Text here...."))),
                                                      bsCollapsePanel("e) Percentage of clients where routine enquiry undertaken regarding childhood and domestic abuse.",
                                                                      tagList(
                                                                        p("Text here....")))
                                           )))
                          
                 ),
                 
                 
                 
                 
                 
                 #New tab and page will provide information about chapter 3
                 tabPanel("3) A Public Health Approach to Justice", icon = icon("balance-scale"),
                          
                          h2("Chapter 3 - A Public Health Approach to Justice"), 
                          
                          p(
                            "The overall outcome for this part of the strategy is for vulnerable people to be diverted from the
                               justice system wherever possible and to ensure those within justice settings are fully supported.
                               The TOC model identifies interim outcomes to achieve this overall outcome. These are:",
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
                                                                        p("Text here....."))),
                                                      bsCollapsePanel("b) Prison – Number of people referred for drug treatment.",
                                                                      tagList(
                                                                        p("Text here....."))),
                                                      bsCollapsePanel("c) Prison – Drug and alcohol treatment waiting times (access).",
                                                                      tagList(
                                                                        p("Text here.....1"))),
                                                      bsCollapsePanel("d) Prison – Drug and alcohol treatment waiting times (secondary waiting time).",
                                                                      tagList(
                                                                        p("Text here....."))),
                                                      bsCollapsePanel("e) Prison – Percentage (of those identified via screening) and number of people receiving treatment during sentence).",
                                                                      tagList(
                                                                        p("Text here....."))),
                                                      bsCollapsePanel("f) Prison – Percentage and number of people completing treatment.",
                                                                      tagList(
                                                                        p("Text here....."))),
                                                      bsCollapsePanel("g) Addiction prevalence estimate in prison population.",
                                                                      tagList(
                                                                        p("Text here....."))),
                                                      bsCollapsePanel("h) Inspecting and Monitoring: Standard 9: Health and Wellbeing.",
                                                                      tagList(
                                                                        p("Text here....."))),
                                                      bsCollapsePanel("i) Number of Alcohol Brief Interventions (ABI) undertaken in justice settings (prison, police custody, other).",
                                                                      tagList(
                                                                        p("Text here....."))),
                                                      bsCollapsePanel("j) Number of drug-related deaths in the six months following prison release.",
                                                                      tagList(
                                                                        p("Text here....."))),
                                                      bsCollapsePanel("k) Number of drug-related deaths following police custody release.",
                                                                      tagList(
                                                                        p("Text here....."))),
                                                      bsCollapsePanel("l) Number of drug-related deaths while in prison.",
                                                                      tagList(
                                                                        p("Text here....."))),
                                                      bsCollapsePanel("m) Percentage of people transitioning from prison to community treatment without interruption to care.",
                                                                      tagList(
                                                                        p("Text here.....")))
                                           ))),
                          
                          #Indicators for outcome b
                          h3("Outcome b: Increase use of diversion from prosecution and alternatives to custody wherever appropriate"), 
                          
                          wellPanel(column(12,
                                           bsCollapse(id = "justice_outcome_b", open = "Panel 1", #PanelSet id
                                                      bsCollapsePanel("a) Number of people diverted from prosecution and to drug treatment/education.",
                                                                      tagList(
                                                                        p("Text here....."))),
                                                      bsCollapsePanel("b) Number of people diverted from prosecution and to alcohol treatment programmes.",
                                                                      tagList(
                                                                        p("Text here....."))),
                                                      bsCollapsePanel("c) Number of people diverted from prison custody via Drug Treatment and Testing Order.",
                                                                      tagList(
                                                                        p("Text here....."))),
                                                      bsCollapsePanel("d) Number of people diverted from prison custody via Community Payback Order (CPO) with alcohol treatment condition.",
                                                                      tagList(
                                                                        p("Text here....."))),
                                                      bsCollapsePanel("e) Number of people diverted from prison custody via CPO with drug treatment condition.",
                                                                      tagList(
                                                                        p("Text here.....")))
                                           ))),
                          
                          #Indicators for outcome c
                          h3("Outcome c: Increase availability and use of advocacy by those who require it at every stage of their recovery"), 
                          
                          wellPanel(column(12,
                                           bsCollapse(id = "justice_outcome_c", open = "Panel 1", #PanelSet id
                                                      bsCollapsePanel("a) Percentage of people accessing preparation for release (from prison) services.",
                                                                      tagList(
                                                                        p("Text here...."))),
                                                      bsCollapsePanel("b) Prisons’ performance against Inspecting and Monitoring Standard 7 ‘Transitions from custody to life in the community’.",
                                                                      tagList(
                                                                        p("Text here...."))),
                                                      bsCollapsePanel("c) Number and percentage of people receiving statutory and voluntary throughcare.",
                                                                      tagList(
                                                                        p("Text here....")))
                                           ))),
                          
                          
                          
                          #Indicators for outcome d
                          h3("Outcome d: Increase the number of people who come into contact with justice agencies and receive the right support from the appropriate services and sources"), 
                          
                          p(
                            "Blurb here about why nothing for this indicator."
                          )
                 ),
                 
                 
                 
                 
                 #New tab and page will provide information about chapter 4
                 tabPanel("4) Children, Young People & Families", icon = icon("child"), 
                          
                          h2("Chapter 4 - Children Young People & Families"), 
                          
                          p(
                            "The overall outcome for this part of the strategy is for more children and families affected by
                               alcohol and drug use to be safe, healthy, included and supported. The TOC model identifies
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
                                                                        p("Text here....."))),
                                                      bsCollapsePanel("b) Number and proportion of Scottish Families Affected by Alcohol and Drugs (SFAAD) helpline callers involved in loved ones’ treatment.",
                                                                      tagList(
                                                                        p("Text here....."))),
                                                      bsCollapsePanel("c) Proportion of ADPs reporting, and providing examples of how, their commissioned services actively involve family members in service design and delivery.",
                                                                      tagList(
                                                                        p("Text here.....1")))
                                           ))),
                          
                          #Indicators for outcome b
                          h3("Outcome b: More children, families and young people’s services are high quality and evidence based"), 
                          
                          wellPanel(column(12,
                                           bsCollapse(id = "children_outcome_b", open = "Panel 1", #PanelSet id
                                                      bsCollapsePanel("a) Output from review of services available to family members where the quality principles apply.",
                                                                      tagList(
                                                                        p("Text here....."))),
                                                      bsCollapsePanel("b) Local reports/reviews/inspections of services offered to children, young people and/or families affected by alcohol and other drug problems.",
                                                                      tagList(
                                                                        p("Text here.....")))
                                           ))),
                          
                          #Indicators for outcome c
                          h3("Outcome c: Improve availability of support to family members who need it"), 
                          
                          wellPanel(column(12,
                                           bsCollapse(id = "children_outcome_c", open = "Panel 1", #PanelSet id
                                                      bsCollapsePanel("a) Number of services and their settings for children affected by alcohol and other drug problems.",
                                                                      tagList(
                                                                        p("Text here...."))),
                                                      bsCollapsePanel("b) Number of services available for families affected by alcohol and other drug problems.",
                                                                      tagList(
                                                                        p("Text here...."))),
                                                      bsCollapsePanel("c) Proportion of ADP investment in services available to children, young people and family members affected by alcohol and other drug problems.",
                                                                      tagList(
                                                                        p("Text here....")))
                                           ))),
                          
                          
                          
                          #Indicators for outcome d
                          h3("Outcome d: More children, families and young people receive integrated, inclusive, effective services"), 
                          
                          wellPanel(column(12,
                                           bsCollapse(id = "children_outcome_d", open = "Panel 1", #PanelSet id
                                                      bsCollapsePanel("a) Number of ADPs providing examples of how all of their services provide family-inclusive practice.",
                                                                      tagList(
                                                                        p("Text here....."))),
                                                      bsCollapsePanel("b) Number of ADPs providing examples of how their services provide family support.",
                                                                      tagList(
                                                                        p("Text here....."))),
                                                      bsCollapsePanel("c) Proportion of addiction treatment staff who attend, when invited, Child Protection Case Conference.",
                                                                      tagList(
                                                                        p("Text here....."))),
                                                      bsCollapsePanel("d) Number of ADPs providing examples of, and level of investment in, joint service commissioning between ADP and Child Protection Committee.",
                                                                      tagList(
                                                                        p("Text here....."))),
                                                      bsCollapsePanel("e) Number of ADPs providing examples, and the number of examples, of joint training between ADP and Child Protection Committee (and the staff under their auspices).",
                                                                      tagList(
                                                                        p("Text here....."))),
                                                      bsCollapsePanel("f) Number of ADPs providing examples of how the needs of children affected by parental substance misuse are reflected in local integrated children’s services plans.",
                                                                      tagList(
                                                                        p("Text here.....")))
                                           ))),
                          
                          
                          #Indicators for outcome e
                          h3("Outcome e) Young people's capacity to make informed choices is improved"), 
                          #These alcohol indicators have come from Chapter 1 Outcome d. Need statement here explaining why
                          wellPanel(column(12,
                                           bsCollapse(id = "children_outcome_e", open = "Panel 1", #PanelSet id
                                                      bsCollapsePanel("a) Young people’s reported wellbeing.",
                                                                      tagList(
                                                                        p("Text here....."))),
                                                      bsCollapsePanel("b) Number of young people using alcohol.",
                                                                      tagList(
                                                                        p("Text here....."))),
                                                      bsCollapsePanel("c) Number and rate of young people admitted to hospital for alcohol-related admissions.",
                                                                      tagList(
                                                                        p("Text here.....")))
                                           )))
                          
                 ),
                 
                 
                 
                 #New tab and page will provide information about chapter 5
                 tabPanel("5) Health & Social Harms", icon = icon("hospital"), 
                          
                          h2("Chapter 5 - Health & Social Harms"), 
                          
                          p(
                            "The overall outcome for this part of the strategy is for the TOC model for the other four chapters 
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
                                                                        p("Data collected for this indicator relied upon a ‘capture-recapture’ methodology used to estimate
                                                                          the number of individuals with problem drug use in the Prevalence of Problem Drug Use in Scotland
                                                                          report. This form of analysis required the collection of two or more data sources which in some
                                                                          way identify individuals with problem drug use. Specialist Drug Treatment Services, Drug-Related
                                                                          Hospital Admissions, Criminal Justice Social Work Reports, and Police Data were the data sources
                                                                          used for modelling."),
                                                                        p("Information Services Division (ISD) were advised that for 2015/16, police data compatible with
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
                                                                        p("The next report will contain 2018/19 figures and is scheduled to be published in 2022."),
                                                                        p(
                                                                          tags$a(href="https://www.isdscotland.org/Health-Topics/Drugs-and-Alcohol-Misuse/Publications/",
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
                                                                        p("Socioeconomic deprivation data were only available by Scottish Index of Multiple Deprivation
                                                                          (SIMD) decile and the difference shown was for the 10% most and 10% least deprived areas in
                                                                          Scotland."),
                                                                        p("The next report will contain 2020/21 figures and is scheduled to be published in November 2021."),
                                                                        p(
                                                                          tags$a(href="https://www.isdscotland.org/Health-Topics/Drugs-and-Alcohol-Misuse/Publications/",
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
                                                                        p("Data are not shown by age as at least 96% of deaths occurred for those aged 35 and over, a figure
                                                                          consistent from 2009 to 2019."),
                                                                        p("Socioeconomic deprivation data were available by SIMD
                                                                          quintile and the difference shown was for the 20% most and 20% least deprived areas in Scotland. 
                                                                          Data by deprivation were requested from the NRS and formatted."),
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
                                                                          Severity Score (ISS), a method for describing patients with multiple injuries and evaluating
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
                                                                        p("An information request was raised to include deprivation. These data were sent in December 2020 
                                                                          and are included in this dashboard. Socioeconomic deprivation is available by
                                                                          (SIMD) quintile and the difference shown was for the 20% most and 20% least deprived areas in Scotland.
                                                                          All other data were pulled and formatted from the Tableau dashboards. Data published in 2020 were 
                                                                          available by age and drugs but are not reported on here."),
                                                                        p("The next report will contain 2020 figures and is scheduled to be published in September 2021. 
                                                                          Data are not expected to be published by deprivation in 2021 so another information request will 
                                                                          have to be raised to obtain this information."),
                                                                        p(
                                                                          tags$a(href="https://www.stag.scot.nhs.uk/",
                                                                                 "More information on STAG can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("e) Drug-related hospital statistics.",
                                                                      tagList(
                                                                        p("As with alcohol-related hospital statistics, data for this indicator were sourced from routinely
                                                                          collected SMR01 and SMR04 national datasets. A patient refers to the number of unique individuals
                                                                          treated within the financial year and was counted only once. New patients are defined as patients
                                                                          admitted to hospital who did not previously have a drug-related hospital admission within the last
                                                                          10 years."),
                                                                        p("Some caution is necessary when using these data as problem substance use may only be suspected
                                                                          and may not always be recorded by the hospital, and where problem substance use was recorded, it
                                                                          may not be possible to identify which drug(s) may be involved."),
                                                                        p("In 1997, ISD moved from using the International Statistical Classification of Diseases and Related
                                                                          Health Problems, Ninth Revision (ICD-9) to the 10th revision (ICD-10). The change introduced a
                                                                          number of new drug-related codes."),
                                                                        p("During October and November 2018, ISD conducted a customer consultation about a proposed
                                                                          change in the definition of a drug-related hospital stay to include admissions due to drug poisoning/
                                                                          overdose. This proposed change would widen the range of stays captured in these statistics in order
                                                                          to measure drug-related hospital activity more comprehensively. As users agreed with the proposed
                                                                          change, hospital admissions resulting from drug poisoning/overdose are included within the
                                                                          definition of a drug-related hospital stay reported in these statistics."),
                                                                        p("A European Age-sex Standardised Rate (EASR) was calculated because hospital activity rates may
                                                                          vary with the age-sex structure of the populations. The European Standard Population (ESP) was
                                                                          used to calculate EASRs within the most recent publications. The ESP was originally introduced in
                                                                          1976 and was revised in 2013. Before 2014 the publication used ESP1976 to calculate EASRs. Since
                                                                          2014, ESP2013 has been used to calculate EASRs for all years (including those before 2012/13).
                                                                          Therefore, findings from publications since February 2014 are not comparable with earlier
                                                                          publications."),
                                                                        p("Socioeconomic deprivation data were available by SIMD quintile and the difference shown was for
                                                                          the 20% most and 20% least deprived areas in Scotland."),
                                                                        p("The next report will contain 2019/20 figures and is scheduled to be published in November 2021."),
                                                                        p(
                                                                          tags$a(href="https://www.isdscotland.org/Health-Topics/Drugs-and-Alcohol-Misuse/Publications/",
                                                                                 "More information on drug-related hospital statistics can be found here", target="_blank"))
                                                                      )),
                                                      bsCollapsePanel("f) Drug-related deaths.",
                                                                      tagList(
                                                                        p("Data for this indicator were taken from the National Records of Scotland (NRS) and were produced 
                                                                          using a definition of ‘drug-related deaths’ which was introduced in 2001 for the ‘baseline’ figures 
                                                                          for the UK Drugs Strategy. The ‘baseline’ definition covers the following cause of death where:",
                                                                          tags$ul(
                                                                            tags$li("the underlying cause of death has been coded to a sub-set of ‘mental and behavioural disorders due to psychoactive substance use’ categories"),
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
                                                                        p("Socioeconomic deprivation data were available by SIMD quintile and the difference shown was
                                                                          for the 20% most and 20% least deprived areas in Scotland. Data by deprivation were requested
                                                                          from NRS via a bespoke information request and formatted."),
                                                                        p("The next report will contain 2020 figures and is scheduled to be published in June 2021."),
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
                                                                        p("For self-reported health indicators, participants were asked to rate their current health status on a
                                                                          scale from 1 to 100, where 1 corresponded to worst health imaginable and 100 corresponded to
                                                                          the best health imaginable. Participants were then asked three questions relating to their health
                                                                          (mobility, self-care, and if they could do usual activities) and were asked to note which of the five
                                                                          categories best corresponded to them: no problems, slight problems, moderate problems, severe
                                                                          problems or were unable to perform task. Participants were then asked two questions about
                                                                          pain/discomfort and anxiety/depression and asked to note which of the five categories
                                                                          best corresponded to them: none, slight, moderate, severe or extreme. Results are given as the
                                                                          average score for all PWID who responded and the percentage of PWID who responded as
                                                                          moderate or worse to the health category questions. No definitions were given as to what would
                                                                          qualify as a slight, moderate, severe or extreme problem. Therefore the perception of severity of
                                                                          problem may vary by individual perspective or pain threshold."),
                                                                        p("The survey period accounts for data over two combined calendar years rather than financial year.
                                                                          As an example, the 2017–18 figure refers to a combination of the two calendar years of 2017
                                                                          and 2018 together rather than the financial year from 1 April 2017 to 31 March 2018."),
                                                                        p("Data on self-reported health were requested from the Needle Exchange Surveillance Initiative
                                                                          via a bespoke information request and formatted. The next report, scheduled to be published at some point during 2021,
                                                                          is unlikely to contain 2019-20 figures for self-reported health so another information request will have to be raised."),
                                                                        p(
                                                                          tags$a(href="https://www.hps.scot.nhs.uk/web-resources-container/needle-exchange-surveillance-initiative-nesi-2008-09-to-2017-18/",
                                                                                 "More information on NESI can be found here", target="_blank"))
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
                                                                        p("The survey period accounts for data over two combined calendar years rather than financial year.
                                                                          As an example, the 2017–18 figure refers to a combination of the two calendar years of 2017
                                                                          and 2018 together rather than the financial year from 1 April 2017 to 31 March 2018."),
                                                                        p("The next report will contain 2019-20 figures and is scheduled to be published at some point during 2021."),
                                                                        p(
                                                                          tags$a(href="https://www.hps.scot.nhs.uk/web-resources-container/needle-exchange-surveillance-initiative-nesi-2008-09-to-2017-18/",
                                                                                 "More information on NESI can be found here", target="_blank"))
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
                                                                        p("The survey period accounts for data over two combined calendar years rather than financial year.
                                                                          As an example, the 2017–18 figure refers to a combination of the two calendar years of 2017
                                                                          and 2018 together rather than the financial year from 1 April 2017 to 31 March 2018."),
                                                                        p("The next report will contain 2019-20 figures and is scheduled to be published at some point during 2021."),
                                                                        p(
                                                                          tags$a(href="https://www.hps.scot.nhs.uk/web-resources-container/needle-exchange-surveillance-initiative-nesi-2008-09-to-2017-18/",
                                                                                 "More information on NESI can be found here", target="_blank"))
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
                                                                        p("The survey period accounts for data over two combined calendar years rather than financial year.
                                                                          As an example, the 2017–18 figure refers to a combination of the two calendar years of 2017
                                                                          and 2018 together rather than the financial year from 1 April 2017 to 31 March 2018."),
                                                                        p("The next report will contain 2019-20 figures and is scheduled to be published at some point during 2021."),
                                                                        p(
                                                                          tags$a(href="https://www.hps.scot.nhs.uk/web-resources-container/needle-exchange-surveillance-initiative-nesi-2008-09-to-2017-18/",
                                                                                 "More information on NESI can be found here", target="_blank"))
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
                          
                          
                          
                          #Indicators for outcome B
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
                                                                        p("Data for this indicator was pulled and formatted from each individual SCJS from 2008/09 to 2017/18."),
                                                                        p("The next report will contain 2019/20 figures and is scheduled to be published in June 2021."),
                                                                        p(
                                                                          tags$a(href="https://www2.gov.scot/Topics/Statistics/Browse/Crime-Justice/crime-and-justice-survey/publications/",
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
                                                                        p("Socioeconomic deprivation data were available by SIMD quintile and the difference shown
                                                                          was for the 20% most and 20% least deprived areas in Scotland."),
                                                                        p("The next report will contain 2019/20 figures and is scheduled to be published in September 2021."),
                                                                        p(
                                                                          tags$a(href="https://www2.gov.scot/Topics/Statistics/16002/",
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
                                                                        p("The survey period accounts for data over two combined calendar years rather than financial year.
                                                                          As an example, the 2017–18 figure refers to a combination of the two calendar years of 2017
                                                                          and 2018 together rather than the financial year from 1 April 2017 to 31 March 2018."),
                                                                        p("The next report will contain 2019-20 figures and is scheduled to be published at some point during 2021."),
                                                                        p(
                                                                          tags$a(href="https://www.hps.scot.nhs.uk/web-resources-container/needle-exchange-surveillance-initiative-nesi-2008-09-to-2017-18/",
                                                                                 "More information on NESI can be found here", target="_blank"))
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
                                                                        p("The Recorded Crime in Scotland 2019/20 report contains drug type figures for 2018/19 and is reported on here."),
                                                                        p("The next Drug seizures and offender characteristics report report will contain 2018/19 figures for drug class and is scheduled to be published in April 2021."),
                                                                        p(
                                                                          tags$a(href="https://www.gov.scot/publications/drug-seizures-offender-characteristics-2017-18/",
                                                                                 "More information on the drug-related offences can be found here", target="_blank"))
                                                                        
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
                                                                        p("The next report will contain 2020/21 figures and is scheduled to be published in October 2021."),
                                                                        p(
                                                                          tags$a(href="https://www.gov.scot/publications/homicide-scotland-2018-19/",
                                                                                 "More information on the drug-related homicides can be found here", target="_blank"))
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
                                                                          p("Data for this indicator was pulled and formatted from each individual SCJS from 2008/09 to 2017/18."),
                                                                          p("The next report will contain 2019/20 figures and is scheduled to be published in June 2021."),
                                                                          p(
                                                                            tags$a(href="https://www2.gov.scot/Topics/Statistics/Browse/Crime-Justice/crime-and-justice-survey/publications/",
                                                                                   "More information on the SCJS can be found here", target="_blank"))
                                                                        )))
                                           )))),
                 
                 
                 
                 tabPanel("Overall Summary", icon = icon("clipboard-check"),
                          
                          h2("Overall Summary"), 
                          
                          p(
                            "The overall summary for MERRR (as of May 2021) is as follows",
                            tags$ul(
                              tags$li("Point one."),
                              tags$li("Point two."),
                              tags$li("Point three")),
                            "Final summary here....."
                          )),
                 
                 
                 
                 tags$a(href = '#top',  
                        icon("circle-arrow-up", lib= "glyphicon"),"Back to top of page")
                  )
#End of UI script
                 )
#End of secure_app 


#End of script
}

shinyApp(ui = ui, server = server)

