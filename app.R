#Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(plyr)
library(boastUtils)
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyverse)
# library(ggiraph)
# library(ggiraphExtra)
library(plotly)
library(shinythemes)
library(boastUtils)
library(DT)

## Load in data
SampleData2 <- read_excel("./SampleData2.xlsx")
source("popPicker.R")
maxPaths<-3

# Define UI for App ----
ui <- list(
  ## Create the app page ----
  dashboardPage(
    skin = "green",
    ### Create the app header ----
    dashboardHeader(
      title = "Multiplicative Interaction", # You may use a shortened form of the title here
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "Multiplicative Interaction")
      ),
      tags$li(
        class = "dropdown",
        tags$a(href = 'https://shinyapps.science.psu.edu/',
               icon("home")
        )
      )
    ),
    ### Create the sidebar/left navigation menu ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("tachometer-alt")),
        menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
        menuItem("Example", tabName = "example", icon = icon("book-open-reader")),
        menuItem("Explore", tabName = "explore", icon = icon("wpexplorer")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        # boastUtils::sidebarFooter()
        boastUtils::psu_eberly_logo("reversed")
      )
    ),
    ### Create the content ----
    dashboardBody(
      tabItems(
        #### Set up the Overview Page ----
        tabItem(
          tabName = "overview",
          withMathJax(),
          h1("Multiplicative Interaction"), # This should be the full name.
          br(),
          p("In this app, you will learn three parts in Interaction Term Regression Models:"),
          p("1. Interactions between binary regression."),
          p("2. Interaction between binary variable and a continuous variable"),
          p("3. Interaction between continuous regressors."),
          br(),
          h2("Instructions"),
          p("1. Review any prerequiste ideas using the Prerequistes tab."),
          p("2. Play the game to test how far you've come."),
          br(),
          
          ##### Go Button--location will depend on your goals
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "go1",
              label = "GO!",
              size = "large",
              icon = icon("bolt"),
              style = "default"
            )
          ),
          ##### Create two lines of space----
          br(),
          br(),
          h2("Acknowledgements"),
          p(
            "This version of the app was developed and coded by Neil J.
            Hatfield  and Robert P. Carey, III.",
            br(),
            "We would like to extend a special thanks to the Shiny Program
            Students.",
            br(),
            br(),
            "Cite this app as:",
            br(),
            citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 5/19/2022 by NJH.")
          )
        ),
        #### Set up the Prerequisites Page ----
        tabItem(
          tabName = "prerequisites",
          withMathJax(),
          h2("Prerequisites"),
          p("In order to get the most out of this app, please review the
            following:"),
          tags$ul(
            tags$li("A linear regression model with multiplicative interaction for a response,",tags$em("Y")," with explanatory variables \\(X_1\\) and \\(X_2\\) takes the form:"),
            p("\\[E(Y) = \\beta_0 + \\beta_1 X_1 + \\beta_2 X_2 + \\beta_3(X_1 X_2)\\]"),
            p("So that the intercept \\(\\beta_0\\)  is the expected response when both \\(X_1\\) and \\(X_2\\) are zero;"),
            p("\\(\\beta_1\\) is the increase in the average value of ",tags$em("Y")," per unit of \\(X_1\\) when \\(\\beta_3\\) and/or \\(X_2\\) is zero, (if you hold \\(X_2\\) fixed at some non-zero level \\(X_2\\) then the average value of ",tags$em("Y")," changes by \\(\\beta_1\\) + \\(\\beta_3\\ (X_2\\) per unit of \\(X_1\\));"),
            p("\\(\\beta_2\\) is the increase in the average value of ",tags$em("Y")," per unit of \\(X_2\\) when \\(\\beta_3\\) and/or \\(X_1\\) is zero, (if you hold \\(X_1\\) fixed at some non-zero level \\(X_1\\) then the average value of ",tags$em("Y")," changes by \\(\\beta_2\\) + \\(\\beta_3\\ (X_1\\) per unit of \\(X_2\\))."),
            p("Interpretation: When there is an interaction in the model, the degree to which a change in one
of the X variables affects the expectation of ",tags$em("Y")," depends on the other X variable. The coefficient
\\(\\beta_3\\) tells you how strong that dependency is."),
tags$li("Special case: Suppose \\(X_2\\) is a binary variable that =1 if an event “A” happens and = 0 if it doesn’t.
            \\(E(Y) = (\\beta_0 + \\beta_2)+ (\\beta_1 + \\beta_3) X_1\\) when A happens and
            \\(E(Y) = \\beta_0 + \\beta_1 X_1\\) when it doesn't"),
          ),
tags$ul(
  tags$li("Special case of the special case: Suppose both \\(X_1\\) and \\(X_2\\) are binary variables and \\(X_1\\)=1 if an event “B” happens and = 0 if it doesn’t."),
  p("In this case"),
  p("\\(E(Y) = (\\beta_0 + \\beta_1  + \\beta_2  + \\beta_3)\\) when A and B both happen"),
  p("\\(E(Y) = (\\beta_0 + \\beta_1 )\\) when B happens but A doesn't"),
  p("\\(E(Y) = (\\beta_0 + \\beta_2 )\\) when A happens but B doesn't"),
  p("\\(E(Y) = \\beta_0\\) when neither A nor B happen"),
),
br(),

box(
  title = strong("Interpretation on Special case"),
  status = "primary",
  collapsible = TRUE,
  collapsed = TRUE,
  width = '100%',
  "The average value of Y is related to by one linear equation when A happens and by a different line when it doesn’t."
),
box(
  title = strong("Interpretation on special case of the special case"),
  status = "primary",
  collapsible = TRUE,
  collapsed = TRUE,
  width = '100%',
  "The average value of ",tags$em("Y")," changes by a constant amount that depends on whether the events A and B happen or not."
),
        ),


#### Set up an Example Page ----
tabItem(
  tabName = "example",
  withMathJax(),
  h2("Example of Multiplicative Interaction"),
  p("Interaction term were used when you believe the response 
    of one predictor will change with the change of the other predictor. 
    Then we can multiplied these factors together to form interaction 
    terms."),
  p("Instruction: 
    Select the types of interaction you want to explore, then look 
    at the plot and try to identify if there is interaction for predicting 
    depth correspond to the following summary table of coefficients.
    In addition, in this context, we treat Depth as dependent variable, which
    is the value we want to estimate. The other four variables are predictors.
    Absolute Distance from Meridian and Diameter are continuous variable,
    Distance from Equator and Region are binary variable"),
  br(),
  
  fluidRow(
    column(
      width = 4,
      wellPanel(
        selectInput(
          inputId = "interactionType",
          label = "Type of interaction",
          choices = c(
            "Binary & Binary" = 1,
            "Binary & Continuous" = 2,
            "Continuous & Continuous" = 3
          )
        ),
        # selectInput(
        #   inputId = "Horizontal",
        #   label = "Horizontal",
        #   choices = c("Region(binary)",
        #               "Distance_from_Equator(binary)",
        #               "Diameter(continuous)",
        #               "Absolute_Distance_from_Meridian(continuous)")
        # ),
        # selectInput(
        #   inputId = "Color",
        #   label = "Color",
        #   choices = c("Region(binary)",
        #               "Distance_from_Equator(binary)",
        #               "Diameter(continuous)",
        #               "Absolute_Distance_from_Meridian(continuous)"),
        # )
      ),
    ),
    column(
      width = 8,
      # plotOutput("plot1")
      uiOutput("interactionPlotUI"),
      dataTableOutput("exampleSummary"),
      uiOutput("dataInterpretation"),
      br(),
    )
  )
),


#### Set up an Explore Page ----


#### Set up the References Page ----
tabItem(
  tabName = "references",
  withMathJax(),
  h2("References"),
  p("You'll need to fill in this page with all of the appropriate
            references for your app."),
  p(
    class = "hangingindent",
    "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny.
            (v0.61). [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
  ),
  br(),
  br(),
  br(),
  boastUtils::copyrightInfo()
)
      )
    )
  )
)


# Define server logic ----
server <- function(input, output, session) {
  
  ## Set up Info button ----
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        type = "info",
        title = "Information",
        text = "This App Template will help you get started building your own app"
      )
    }
  )
  
  ## Example Page Elements ----
  
  ### Update Horizontal and Color ----
  observeEvent(input$interactionType, {
    
    freezeReactiveValue(input, "Horizontal")
    freezeReactiveValue(input, "Color")
    
    if (input$interactionType == 1) {
      updateSelectInput(inputId = "Horizontal",  choices = "Distance from Equator")
      updateSelectInput(inputId = "Color",  choices = "Region")
    } else if (input$interactionType == 2) {
      updateSelectInput(inputId = "Horizontal", choices = c("Diameter(m)"))
      updateSelectInput(inputId = "Color",choices = c("Distance from Equator"))
    } else if (input$interactionType == 3) {
      updateSelectInput(inputId = "Horizontal", choices = c("Diameter(m)"))
      updateSelectInput(inputId = "Color", choices = c("Absolute Distance from Meridian"))
    }

    ### Plot ----
    output$interactionPlotUI <- renderUI({
      if (input$interactionType == 1 ) {
        plotOutput("plot1")
      } else if (input$interactionType == 2 ) {
        plotOutput("plot2")
      } else if (input$interactionType == 3 ) {
        plotOutput("plot3")
      }
    })
    
    
    ### exampleModel1
    if(input$interactionType == 1) {
      output$plot1<-renderPlot(
        expr = {
          
          ggplot(
            data = SampleData2,
            mapping = aes(
              x = Distance_from_Equator,
              y = Depth,
              color = Region,
              group = Region
            )
          )+
            geom_point(size=3) +
            labs(title= "Depth and Distance from Equator",
                 x= "Distance from Equator",
                 y= "Depth(m)",
                 color="Region"
            ) +
            stat_summary(fun = "mean", geom = "point") +
            stat_summary(fun = "mean", geom = "line")+
            theme(
              legend.position = "bottom",
              text=element_text(size=15))
          
        })
    }
    
    
    ### exampleModel2
    if(input$interactionType == 2) {
      output$plot2<-renderPlot({
        
        ggplot(
          data = SampleData2,
          mapping = aes(
            x = Diameter,
            y = Depth,
            color = Distance_from_Equator,
            group = Distance_from_Equator
          )
        )+
          geom_point(size=3) +
          labs(title= "Depth and Diameter",
               x= "Diameter(m)",
               y= "Depth(m)",
               color="Distance from Equator"
          ) +
          geom_point() + geom_smooth(method = "lm", fill = NA)+
          theme(legend.position = "bottom",
                text=element_text(size=15))
        
      })
    }
    
    ## exampleModel3
    if(input$interactionType == 3) {
      output$plot3<-renderPlot({
        
        ggplot(
          data = SampleData2,
          mapping = aes(
            x = Diameter,
            y = Depth,
            color = Absolute_Distance_from_Meridian,
            group = Absolute_Distance_from_Meridian
          )
        )+
          geom_point(size=3) +
          labs(title= "Depth and Diameter",
               x= "Diameter(m)",
               y= "Depth(m)",
               color="Absolute Distance from Meridian"
          ) +
          geom_point() + geom_smooth(method = "lm", fill = NA)+
          theme(legend.position = "bottom",
                text=element_text(size=15))
      })
    }

  ### Summary Table ----
  observeEvent(
    eventExpr = input$interactionType,
    handlerExpr = {
      if(input$interactionType == 1) {
        exampleModel <- lm(
          formula = Depth ~ Distance_from_Equator * Region,
          data = SampleData2)
      } else if(input$interactionType == 2 ) {
        exampleModel <- lm(
          formula = Depth ~ Diameter * Distance_from_Equator,
          data = SampleData2)
      } else if(input$interactionType == 3) {
        exampleModel <- lm(
          formula= Depth ~ Diameter * Absolute_Distance_from_Meridian,
          data = SampleData2)
      }
      exampleCoeff <- round(summary(exampleModel)$coefficients, digits = 4)
      
      output$exampleSummary <- DT::renderDataTable(
        expr = exampleCoeff,
        caption = "Model Coefficients",
        style = "bootstrap4",
        rownames = TRUE,
        options = list(
          responsive = TRUE,
          scrollX = TRUE,
          ordering = FALSE,
          paging = FALSE,
          lengthChange = FALSE,
          pageLength = 10,
          searching = FALSE,
          info = FALSE,
          columnDefs = list(
            list(className = "dt-center", targets = 1:ncol(exampleCoeff))
          )
        )
      )
    },
    ignoreNULL = TRUE
  )

   


## DataInterpretation
observeEvent(
  eventExpr = input$interactionType,
  handlerExpr = {
    output$dataInterpretation <- renderUI({
      if (input$interactionType == 1) {
        p("Based on the coeffitient table, we can get the fitted value 
          into our model: 
          \\[E(Y) = 5.7944 - 0.5351 X_1 - 0.3428 X_2 + 1.7255(X_1 X_2)\\]
          To test whether 
          the interaction between Advertising and Competitors is significant, we make a 
          hypothesis that H0:\\(\\beta_3\\)=0 vs H1:\\(\\beta_3\\)≠0. The test statistic for this 
          hypothesis is 0.644, and the p-value is 0.5246, so we fail to reject 
          the null hypothesis, indicating the interaction term is not significant 
          in this case.")
      }else if (input$interactionType == 2) {
        p("Based on the coeffitient table, we can get the fitted value 
          into our model:
          \\[E(Y) = 0.03838 + 0.13798 X_1 - 2.72064 X_2 + 0.07432(X_1 X_2)\\]
          We can do the same thing we did in the last one, we make a hypothesis 
          that H0:\\(\\beta_3\\)=0 vs H1:\\(\\beta_3\\)≠0.The test statistic for this hypothesis 
          is 2.717, and the p-value is 0.0112. Since the p-value is small,
          we can reject the null hypothesis, indicating the interaction 
          term is significant in the binary & continuous case.")
      } else if (input$interactionType == 3) {
        p("Based on the coeffitient table, we can get the fitted value 
          into our model:
          \\[E(Y) = 0.4975 + 0.1268 X_1 - 0.0240 X_2 + 0.0006(X_1 X_2)\\]
          Same as what we did previously, we make a hypothesis that 
          H0:\\(\\beta_3\\)=0 vs H1:\\(\\beta_3\\)≠0.
          The test statistic for this hypothesis is 2.246, and the p-value is 
          0.0328. Since the p-value is small, we can reject the null hypothesis,
          indicating the interaction term is significant in the continuous 
          & continuous case.")
      }
    })
  }
)
 
           
  })
}      


## Explore Page ----

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)

