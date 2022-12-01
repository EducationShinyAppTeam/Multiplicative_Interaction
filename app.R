#Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(ggplot2)
library(readxl)
library(DT)

## Load in data
SampleData2 <- read_excel("SampleData2.xlsx")

Crater_Data2 <- read_excel("Crater_Data2.xlsx")
source("predictionData.R")
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
        boastUtils::surveyLink(name = "Multiplicative_Interaction")
      ),
      tags$li(
        class = "dropdown",
        tags$a(href = 'https://shinyapps.science.psu.edu/',
               icon("house")
        )
      )
    ),
    ### Create the sidebar/left navigation menu ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("gauge-high")),
        menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
        menuItem("Example", tabName = "example", icon = icon("book-open-reader")),
        menuItem("Explore", tabName = "explore", icon = icon("wpexplorer")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
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
          p("In this app, you will learn three types of interaction term regression models:"),
          p("1. Interactions between binary regressors;"),
          p("2. Interaction between binary variable and a continuous variable;"),
          p("3. Interaction between continuous regressors."),
          br(),
          h3("Instructions"),
          p("1. Review any prerequiste ideas using the Prerequistes tab."),
          p("2. Examine the lunar crater example to see the illustration of
            the ideas."),
          p("3. Explore the concepts through the simulation."),
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
          p( "This version of the app was developed and coded by Xinyue Tang ",
             br(),
             br(),
             "Cite this app as:",
             br(),
             citeApp(),
             br(),
             br(),
             div(class = "updated", "Last Update: 11/29/2022 by XYT.")
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
            tags$li("A linear regression model with multiplicative interaction
                    for a response,",tags$em("Y")," with explanatory variables
                    \\(X_1\\) and \\(X_2\\) takes the form:"),
            p("\\[E(Y) = \\beta_0 + \\beta_1 X_1 + \\beta_2 X_2 +
              \\beta_3(X_1 X_2)\\]"),
            p("So that the intercept \\(\\beta_0\\)  is the expected response
              when both \\(X_1\\) and \\(X_2\\) are zero;"),
            p("\\(\\beta_1\\) is the increase in the average value of ",
              tags$em("Y")," per unit of \\(X_1\\) when \\(\\beta_3\\) and/or
              \\(X_2\\) is zero, (if you hold \\(X_2\\) fixed at some non-zero
              level \\(X_2\\) then the average value of ",tags$em("Y"),"
              changes by \\(\\beta_1\\) + \\(\\beta_3\\ (X_2\\) per unit
              of \\(X_1\\));"),
            p("\\(\\beta_2\\) is the increase in the average value of ",
              tags$em("Y")," per unit of \\(X_2\\) when \\(\\beta_3\\)
              and/or \\(X_1\\) is zero, (if you hold \\(X_1\\) fixed at
              some non-zero level \\(X_1\\) then the average value of ",
              tags$em("Y")," changes by \\(\\beta_2\\) + \\(\\beta_3\\
              (X_1\\) per unit of \\(X_2\\))."),
            p("Interpretation: When there is an interaction in the model,
              the degree to which a change in one of the X variables affects
              the expectation of ",tags$em("Y")," depends on the other X
              variable. The coefficient \\(\\beta_3\\) tells you how strong
              that dependency is."),
            tags$li("Special case: Suppose \\(X_2\\) is a binary variable that =1 if an
        event “A” happens and = 0 if it doesn’t. \\(E(Y) = (\\beta_0 +
        \\beta_2)+ (\\beta_1 + \\beta_3) X_1\\)
        when A happens and\\(E(Y) = \\beta_0 + \\beta_1 X_1\\)
        when it doesn't"),
          ),
        tags$ul(
          tags$li("Special case of the special case: Suppose both \\(X_1\\) and
          \\(X_2\\) are binary variables and \\(X_1\\)=1 if an event “B”
          happens and = 0 if it doesn’t."),
          p("In this case"),
          p("\\(E(Y) = (\\beta_0 + \\beta_1  + \\beta_2  + \\beta_3)\\) when
    A and B both happen"),
    p("\\(E(Y) = (\\beta_0 + \\beta_1 )\\) when B happens but A doesn't"),
    p("\\(E(Y) = (\\beta_0 + \\beta_2 )\\) when A happens but B doesn't"),
    p("\\(E(Y) = \\beta_0\\) when neither A nor B happen"),
        ),
    br(),

    box(
      title = strong("Interpretation of the special case when \\(x_2\\) is binary"),
      status = "primary",
      collapsible = TRUE,
      collapsed = TRUE,
      width = '100%',
      "The average value of Y is related to by one linear equation when A happens
  and by a different line when it doesn’t."
    ),
  box(
    title = strong("Interpretation on special case of the special case when both
                 \\(x_1\\) and \\(x_2\\) are binary "),
    status = "primary",
    collapsible = TRUE,
    collapsed = TRUE,
    width = '100%',
    "The average value of ",tags$em("Y")," changes by a constant amount that
  depends on whether the events A and B happen or not."
  ),
        ),


  #### Set up an Example Page ----
  tabItem(
    tabName = "example",
    withMathJax(),
    h2("Example of Multiplicative Interaction"),
    p("While very large lunar craters are visible from Earth, only recently
      have extensive databases been created of small craters.  In this app,
      we look at the problem of predicting the depth of such craters based on
      different characteristics including: its diameter; whether it is in the
      flat dark looking Mare or the Highland areas (see picture); whether it
      is near or far from the moon’s equator; and it’s distance from the Lunar
      prime meridian. In particular, beyond the main effects of each of these
      possible predictors, we examine which pairs of them show significant
      interaction in predictive models."),


    tags$figure(
      align = "center",
      tags$img(
        src = "Picture1.png",
        width = 400,
      )
    ),


    # p("One type of interaction is multiplicative where the factors are multiplied
    # together to form the interaction term. We study the depth of the lunar
    # craters and how that might be predicted by whether the crater is close or far
    # from the equator; whether the crater is in a Highland area or in a Mare;
    # Diameter of the crater and the distance from lunar meridian."),
    br(),
    h3("Instructions"),
    p("Select the type of interaction you want to explore, then view the
    corresponding summary table of coefficients. and try to identify
    if there is interaction for predicting depth.
    In addition, in this context, we treat Depth as dependent variable, which
    is the value we want to estimate. The other four variables are predictors.
    Absolute Distance from Meridian and Diameter are continuous variables,
    Distance from Equator (near or far) and Region type are binary variables."),
    br(),

    fluidRow(
      column(
        width = 4,
        wellPanel(
          selectInput(
            inputId = "interactionType",
            label = "Type of interaction",
            choices = c(
              # " ",
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
  tabItem(
    tabName = "explore",
    withMathJax(),

    h2("Explore"),
    p("Here, we will explore multiplicative interaction if a y variable same for
    response variable Y, predicted by binary variable Distance from Equator
    and Region Type and continuous variable Diameter and Absolute
    Distance from Meridian."),
    br(),
    h3("Instructions"),
    p("Use the Generate New Sample button to see plots for a different random
      sample of 24 crators. One plot shows the fitted lines with an interaction
      term and the other plot shows the fitted lines without an interaction term.
      When there is a continuous variable on the x-axis you can move the slider
      to help focus on particular values and how the interaction and no
      interaction models differ."),
    br(),
    fluidPage(
      tabsetPanel(
        id = "whichtype",
        type = "tabs",
        ##### Binary & Binary ----
        tabPanel(
          title = "Binary & Binary",
          br(),
          column(
            width = 4,
            offset = 0,
            wellPanel(
              #### input part----
              tags$strong("Type of interaction"),
              # radioButtons(
              #   inputId = " Interactions between binary regressors",
              #   label = NULL,
              #   choices = c(" Interactions between binary regressors"),
              #   selected = " Interactions between binary regressors",
              #   width = '100%'
              # ),
              p("Interactions between binary regressors"),
              p("Distance from Equator and Region Type are two binary regressors."),
              p("Close to Equator = 0"),
              p("Far from Equator = 1"),
              p("Highland = 0"),
              p("Mare = 1"),
              # tags$strong("x value"),
              # sliderInput(
              #   inputId = "x11",
              #   label = "Distance from Meridian",
              #   min = 0,
              #   max = 1,
              #   step = 1,
              #   value =0
              # ),
              br(),
              bsButton(
                inputId = "newSample",
                label = "Generate New Sample",
                icon = icon("retweet"),
                size = "large"
              )
            )
          ),
          #### output part----
          column(
            width = 8,
            plotOutput("graphDisplay1" ),
            plotOutput("graphDisplay11" ),
            dataTableOutput("exploreSummary1"),
            br(),
          )
        ),
        ##### Binary & Continuous ----
        tabPanel(
          title = "Binary & Continuous",
          br(),
          column(
            width = 4,
            offset = 0,
            wellPanel(
              #### input part----
              tags$strong("Type of interaction"),
              # radioButtons(
              #   inputId = "Interaction between binary variable and a continuous variable",
              #   label = NULL,
              #   choices = c("Interaction between binary variable and a continuous variable"),
              #   selected = "Interaction between binary variable and a continuous variable",
              #   width = '100%'
              # ),
              p("Interaction between binary variable and a continuous variable"),
              # tags$strong("x value"),
              sliderInput(
                inputId = "x3",
                label = "Diameter",
                min = 20,
                max = 170,
                step = 4,
                value = 20
              ),
              br(),

              bsButton(
                inputId = "newSample2",
                label = "Generate New Sample",
                icon = icon("retweet"),
                size = "large"
              ),
            ),
          ),
          #### output part----
          column(
            width = 8,
            plotOutput("graphDisplay2" ),
            plotOutput("graphDisplay22" ),
            dataTableOutput("exploreSummary2"),
            br(),
          ),

        ),

        ##### Continuous & Continuous ----
        tabPanel(
          title = "Continuous & Continuous",
          br(),
          column(
            width = 4,
            offset = 0,
            wellPanel(
              #### input part----
              tags$strong("Type of interaction"),
              # radioButtons(
              #   inputId = "Interaction between two continuous regressors",
              #   label = NULL,
              #   choices = c("Interaction between two continuous regressors"),
              #   selected = "Interaction between two continuous regressors",
              #   width = '100%'
              # ),
              p("Interaction between two continuous regressors"),
              #x3
              # tags$strong("x value"),
              sliderInput(
                inputId = "x33",
                label = "diameter",
                min = 20,
                max = 170,
                step = 4,
                value = 20
              ),
              br(),


              # Generate sample button
              bsButton(
                inputId = "newSample3",
                label = "Generate New Sample",
                icon = icon("retweet"),
                size = "large"
              ),

            )
          ),
          #### output part----
          column(
            width = 8,
            plotOutput("graphDisplay3" ),
            plotOutput("graphDisplay33" ),
            dataTableOutput("exploreSummary3"),
            br(),
          )
        ),
      )
    )
  ),


  #### Set up the References Page ----
  tabItem(
    tabName = "references",
    withMathJax(),
    h2("References"),
    p(
      class = "hangingindent",
      "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny.
            (v0.61). [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
    ),
    p(
      class = "hangingindent",
      "Carey, R. and Hatfield, N. (2022). boastUtils: BOAST utlities.
            (v 0.1.12.3). [R package]. Available from
            https://github.com/EducationShinyAppTeam/boastUtils"
    ),
    p(
      class = "hangingindent",
      "Chang, W., and Borges Ribeiro, B. (2021). shinydashboard: Create
            dashboards with 'shiny'. (v 0.7.2) [R package]. Available from
            https://CRAN.R-project.org/package=shinydashboard"
    ),
    p(
      class = "hangingindent",
      "Chang, W., Cheng J., Allaire, J., Sievert, C., Schloerke, B., Xie, Y.,
            Allen, J., McPherson, J., Dipert, A., and Borges, B. (2021). shiny:
            Web application framework for R. (v 1.7.1). [R package]. Available
            from https://CRAN.R-project.org/package=shiny"
    ),
    p(
      class = "hangingindent",
      "Sun, S., et al. “Investigation of the Depth and Diameter Relationship
   of Subkilometer-Diameter Lunar Craters.??? Icarus, vol. 309, 15 July 2018,
   pp. 61???68., https://doi.org/10.1016/j.icarus.2018.02.031. "
    ),
   p(
     class = "hangingindent",
     "Victor, P., Fanny, M. and David, G. (2018).
            shinyWidgets: Custom Inputs Widgets for Shiny.
            R package version 0.4.3."
   ),
   p(
     class = "hangingindent",
     "Wickham, H. (2016). ggplot2: Elegant graphics for data analysis.
            Springer-Verlag:New York. (v 3.3.6) [R package]. Available from
            https://ggplot2.tidyverse.org"
   ),
   p(
     class = "hangingindent",
     "Wickham, H., Bryan, J. (2022). readxl: Read Excel Files.
    https://readxl.tidyverse.org, https://github.com/tidyverse/readxl."
   ),
   p(
     class = "hangingindent",
     "Xie, Y., Sarma, A., Vogt, A., knitr: Provides a general=purpose tool for
            dynamic report generation in R using Literate Programming techniques."
   ),
   p(
     class = "hangingindent",
     "Yihui, X., Joe, C. and Xianying, T. (2022). DT: A Wrapper of the
            JavaScript Library 'DataTables'. R package version 0.21.
            https://CRAN.R-project.org/package=DT"
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
  boastUtils::typesetMath(session = session)

  ## Set up Info button ----
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        type = "info",
        title = "Information",
        text = "This App will help you get familiar with multiplicative interaction by showing
        plot of with interaction and without interaction. "
      )
    }
  )

  ### explore button
  observeEvent(
    eventExpr = input$go1,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "prerequisites"
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
            geom_point(size=2.5) +
            labs(title= "Depth and Distance from Equator",
                 x= "Distance from Equator",
                 y= "Depth(m)",
                 color="Region Type"
            ) +
            stat_summary(fun = "mean", geom = "point") +
            stat_summary(fun = "mean", geom = "line")+
            theme_bw()+
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
          theme_bw()+
          geom_point() + geom_smooth(method = "lm", fill = NA)+
          theme(legend.position = "bottom",
                text=element_text(size=15))

      })
    }

    ## exampleModel3
    test1 <- lm(Depth ~ Diameter*Absolute_Distance_from_Meridian, data = SampleData2)

    if(input$interactionType == 3) {
      output$plot3<-renderPlot({

        test2 <- makePredictionData(test1)
        ggplot(
          data = SampleData2,
          mapping = aes(
            x = Diameter,
            y = Depth,
            color = Absolute_Distance_from_Meridian,
            group = Absolute_Distance_from_Meridian
          )
        )+
          geom_jitter()+
          scale_color_gradient(low = "steelblue3", high = "navyblue") +
          theme_bw()+
          geom_smooth(
            inherit.aes = FALSE,
            data = test2,
            mapping = aes(
              x = Diameter,
              y = Depth,
              color = Absolute_Distance_from_Meridian,
              group = Absolute_Distance_from_Meridian
            ),
            formula = "y~x",
            method = "lm"
          )+
          geom_point(size=3) +
          labs(title= "Depth and Diameter",
               x= "Diameter (m)",
               y= "Depth (m)",
               color="Absolute Distance from Meridian"
          ) +
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

        row.names(exampleCoeff) <- sapply(
          X = row.names(exampleCoeff),
          FUN = function(x) {
            gsub(
              pattern = "[[:punct:]]",
              replacement = " ",
              x = x
            )
          }
        )

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




    ### Data Interpretation ----
    observeEvent(
      eventExpr = input$interactionType,
      handlerExpr = {
        output$dataInterpretation <- renderUI({
          if (input$interactionType == 1) {
            withMathJax(p("Based on the coefficient table, we can get the fitted value
          into our model:
          \\[E(\\widehat{Y}) = 5.7944 - 0.5351 X_1 - 0.3428 X_2 + 1.7255(X_1 X_2)\\]
          To test whether the interaction is significant, we study a
          hypothesis that \\(H_0\\): \\(\\beta_3=0\\) vs \\(H_1\\):
          \\(\\beta_3\\neq 0\\). The test statistic for this
          hypothesis is 0.644, and the p-value is 0.5246, so we fail to reject
          the null hypothesis, indicating the interaction term is not significant
          in this case."))
          }else if (input$interactionType == 2) {
            p("Based on the coefficient table, we can get the fitted value
          into our model:
           \\[E(\\widehat{Y}) = 0.03838 + 0.13798 X_1 - 2.72064 X_2 + 0.07432(X_1 X_2)\\]
          To test whether the interaction is significant, we study a hypothesis
          that \\(H_0\\): \\(\\beta_3=0\\) vs \\(H_1\\): \\(\\beta_3\\neq 0\\).
          The test statistic for this hypothesis
          is 2.717, and the p-value is 0.0112. Since the p-value is small,
          we can reject the null hypothesis, indicating the interaction
          term is significant in the binary & continuous interaction case.")
          } else if (input$interactionType == 3) {
            p("Based on the coefficient table, we can get the fitted value
          into our model:
           \\[E(\\widehat{Y}) = 0.4975 + 0.1268 X_1 - 0.0240 X_2 + 0.0006(X_1 X_2)\\]
          To test whether the interaction is significant, we study a
          hypothesis that, we make a hypothesis that
          \\(H_0\\): \\(\\beta_3=0\\) vs \\(H_1\\): \\(\\beta_3\\neq 0\\) .
          The test statistic for this hypothesis is 2.246, and the p-value is
          0.0328. Since the p-value is small, we can reject the null hypothesis,
          indicating the interaction term is significant in the continuous
          & continuous interaction case.")
          }
        })
        boastUtils::typesetMath(session = session)
      }
    )
    r <- reactiveValues(notice = "")
    ## Explore Page ----

    #### Create Sample Data
    observeEvent(
      eventExpr = input$newSample,
      handlerExpr = {
        r$tempData <- Crater_Data2[sample(nrow(Crater_Data2), size = 24),]


      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )

    #### Display sample plot1
    output$graphDisplay1 <- renderPlot(

      expr = {
        validate(
          need(
            expr = !is.null(r$tempData),
            message = "Click on Generate New Sample to create a plot WITH interaction"
          )
        )

        tempData=r$tempData
        test11<- lm(Depth ~ Distance_from_Equator + Region+
                      Distance_from_Equator:Region,
                    data = tempData)
        test12<- makePredictionData(test11)

        ggplot(
          data = tempData,
          mapping = aes(
            x = Distance_from_Equator,
            y = Depth,
            color = Region,
            group = Region
          )
        )+
          # geom_jitter()+
          theme_bw()+
          geom_smooth(
            inherit.aes = FALSE,
            data = test12,
            mapping = aes(
              x = Distance_from_Equator,
              y = Depth,
              color = Region,
              group = Region
            ),
            formula = "y~x",
            method = "lm"
          )+
          geom_point(size=2.5) +
          labs(title= "Depth and Distance from Equator WITH interaction",
               x= "Distance from Equator",
               y= "Depth (m)",
               color="Region Type"
          ) +
          stat_summary(fun = "mean", geom = "point") +
          stat_summary(fun = "mean", geom = "line")+
          theme(legend.position = "bottom",
                text=element_text(size=15))+
          geom_vline(xintercept =as.numeric(input$x3),color="red")

      }
    )

    output$graphDisplay11 <- renderPlot(

      expr = {
        validate(
          need(
            expr = !is.null(r$tempData),
            message = "Click on Generate New Sample to create a plot
            WITHOUT interaction"
          )
        )


        tempData=r$tempData

        fit1<- lm(Depth ~ Distance_from_Equator + Region,
                  data = tempData)
        Depth_pred1 <- predict(fit1, tempData)
        ggplot(tempData, aes(x=Distance_from_Equator,
                             y=Depth,
                             color = Region,
                             group = Region))+
          geom_point()+
          theme_bw()+
          geom_point(size=2.5) +
          geom_line(aes(y = Depth_pred1))+
          labs(title= "Depth and Distance from Equator WITHOUT interaction",
               x= "Distance from Equator",
               y= "Depth (m)",
               color="Region Type"
          ) +
          # stat_summary(fun = "mean", geom = "point") +
          # stat_summary(fun = "mean", geom = "line")+
          theme(legend.position = "bottom",
                text=element_text(size=15))+
          geom_vline(xintercept =as.numeric(input$x3),color="red")

      }
    )





    #### Display sample plot 2
    observeEvent(
      eventExpr = input$newSample2,
      handlerExpr = {
        r$tempData <- Crater_Data2[sample(nrow(Crater_Data2), size = 24),]


      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )


    output$graphDisplay2 <- renderPlot(

      expr = {
        validate(
          need(
            expr = !is.null(r$tempData),
            message = "Click on Generate New Sample to create a plot
            WITH interaction"
          )
        )

        tempData=r$tempData
        test21<- lm(Depth ~ Diameter + Distance_from_Equator+
                      Diameter:Distance_from_Equator, data = tempData)
        test22<- makePredictionData(test21)

        ggplot(
          data = tempData,
          mapping = aes(
            x = Diameter,
            y = Depth,
            color = Distance_from_Equator,
            group = Distance_from_Equator
          )
        )+
          geom_jitter()+
          theme_bw()+
          geom_smooth(
            inherit.aes = FALSE,
            data = test22,
            mapping = aes(
              x = Diameter,
              y = Depth,
              color = Distance_from_Equator,
              group = Distance_from_Equator
            ),
            formula = "y~x",
            method = "lm"
          )+
          geom_point(size=2.5) +
          labs(title= "Depth and Diameter WITH interaction",
               x= "Diameter",
               y= "Depth (m)",
               color="Distance from Equator"
          ) +
          geom_point() + geom_smooth(method = "lm", fill = NA)+
          theme(legend.position = "bottom",
                text=element_text(size=15))+
          geom_vline(xintercept =as.numeric(input$x3),color="red")

        # ggplot(
        #   data = tempData,
        #   mapping = aes(
        #     x = Diameter,
        #     y = Depth,
        #     color = Distance_from_Equator,
        #     group = Distance_from_Equator
        #   )
        # )+
        #   geom_point(size=2.5) +
        #   labs(title= "Depth and Distance from Equator with interaction",
        #        x= "Diameter",
        #        y= "Depth (m)",
        #        color="Distance from Equator"
        #   ) +
        #   geom_point() + geom_smooth(method = "lm", fill = NA)+
        #   theme(legend.position = "bottom",
        #         text=element_text(size=15))+geom_vline(xintercept =as.numeric(input$x3),color="red")
      }
    )

    output$graphDisplay22 <- renderPlot(

      expr = {
        validate(
          need(
            expr = !is.null(r$tempData),
            message = "Click on Generate New Sample to create a plot
            WITHOUT interaction"
          )
        )


        tempData=r$tempData

        fit<- lm(Depth ~ Diameter + Distance_from_Equator,
                 data = tempData)
        Depth_pred <- predict(fit, tempData)
        ggplot(tempData,
               aes(
                 x=Diameter,
                 y=Depth,
                 color = Distance_from_Equator))+
          geom_point(size=2.5)+
          labs(title= "Depth and Diameter WITHOUT interaction",
               x= "Diameter(m)",
               y= "Depth(m)",
               color="Distance from Equator"
          ) +
          geom_line(aes(y = Depth_pred))+
          theme_bw()+
          theme(legend.position = "bottom",
                text=element_text(size=15))+
          geom_vline(xintercept =as.numeric(input$x3),color="red")

      }
    )






    #### Display sample plot 3
    observeEvent(
      eventExpr = input$newSample3,
      handlerExpr = {
        r$tempData <- Crater_Data2[sample(nrow(Crater_Data2), size = 24),]


      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )

    output$graphDisplay3 <- renderPlot(
      expr = {
        validate(
          need(
            expr = !is.null(r$tempData),
            message = "Click on Generate New Sample to create a plot
            WITH interaction"
          )
        )

        tempData=r$tempData

        test31 <- lm(Depth ~ Diameter*Absolute_Distance_from_Meridian,
                     data = tempData)
        test41<- makePredictionData(test31)

        ggplot(
          data = tempData,
          mapping = aes(
            x = Diameter,
            y = Depth,
            color = Absolute_Distance_from_Meridian,
            group = Absolute_Distance_from_Meridian
          )
        )+
          geom_jitter()+
          scale_color_gradient(low = "steelblue3", high = "navyblue") +
          theme_bw()+
          geom_smooth(
            inherit.aes = FALSE,
            data = test41,
            mapping = aes(
              x = Diameter,
              y = Depth,
              color = Absolute_Distance_from_Meridian,
              group = Absolute_Distance_from_Meridian
            ),
            formula = "y~x",
            method = "lm"
          )+
          geom_point(size=3) +
          labs(title= "Depth and Diameter WITH interaction",
               x= "Diameter (m)",
               y= "Depth (m)",
               color="Absolute Distance from Meridian"
          ) +
          theme(legend.position = "bottom",
                text=element_text(size=15))+
          geom_vline(xintercept =as.numeric(input$x33),
                     color="red")

      }
    )


    output$graphDisplay33 <- renderPlot(

      expr = {
        validate(
          need(
            expr = !is.null(r$tempData),
            message = "Click on Generate New Sample to create a plot
            WITHOUT interaction"
          )
        )

        tempData=r$tempData


        test32 <- lm(Depth ~ Diameter + Absolute_Distance_from_Meridian,
                     data = tempData)
        test42<- makePredictionData(test32)

        ggplot(
          data = tempData,
          mapping = aes(
            x = Diameter,
            y = Depth,
            color = Absolute_Distance_from_Meridian,
            group = Absolute_Distance_from_Meridian
          )
        )+
          geom_jitter()+
          scale_color_gradient(low = "steelblue3", high = "navyblue") +
          theme_bw()+
          geom_smooth(
            inherit.aes = FALSE,
            data = test42,
            mapping = aes(
              x = Diameter,
              y = Depth,
              color = Absolute_Distance_from_Meridian,
              group = Absolute_Distance_from_Meridian
            ),
            formula = "y~x",
            method = "lm"
          )+
          geom_point(size=3) +
          labs(title= "Depth and Diameter WITHOUT interaction",
               x= "Diameter (m)",
               y= "Depth (m)",
               color="Absolute Distance from Meridian"
          ) +
          theme(legend.position = "bottom",
                text=element_text(size=15))+
          geom_vline(xintercept =as.numeric(input$x33),color="red")

      }
    )
    observeEvent(
      eventExpr = input$newSample,
      handlerExpr = {
        output$exploreSummary1 <- DT::renderDataTable({

          tempData=r$tempData
          test11<- lm(Depth ~ Distance_from_Equator + Region+
                        Distance_from_Equator:Region,
                      data = tempData)
          test12<- predict(test11, tempData)
          fit1<- lm(Depth ~ Distance_from_Equator + Region,
                    data = tempData)
          Depth_pred1 <- predict(fit1, tempData)

          pred<-data.frame(`predict with interaction`=round(test12,2),
                           `predict without interaction`=round(Depth_pred1,2))
          colnames(pred)<-c("predict with interaction",
                            "predict without interaction")

          datatable(
            pred,
            caption = "Model prediction with and without interaction",
            style = "bootstrap4",
            rownames = TRUE,
            options = list(
              responsive = FALSE,
              scrollX = TRUE,
              ordering = FALSE,
              paging = FALSE,
              lengthChange = FALSE,
              pageLength = 10,
              searching = FALSE,
              info = FALSE,
              columnDefs = list(
                list(className = "dt-center", targets = 1:ncol(pred))
              )
            )
          )
        })

        output$exploreSummary2 <- DT::renderDataTable({

          tempData=r$tempData
          test21<- lm(Depth ~ Diameter + Distance_from_Equator+
                        Diameter:Distance_from_Equator, data = tempData)

          test12<- predict(test21, tempData)
          fit<- lm(Depth ~ Diameter + Distance_from_Equator,
                   data = tempData)
          Depth_pred1 <- predict(fit, tempData)

          pred<-data.frame(`predict with interaction`=round(test12,2),
                           `predict without interaction`=round(Depth_pred1,2))
          colnames(pred)<-c("predict with interaction","predict without interaction")
          datatable(
            pred,
            caption = "Model prediction with and without interaction",
            style = "bootstrap4",
            rownames = TRUE,
            options = list(
              responsive = FALSE,
              scrollX = TRUE,
              ordering = FALSE,
              paging = FALSE,
              lengthChange = FALSE,
              pageLength = 10,
              searching = FALSE,
              info = FALSE,
              columnDefs = list(
                list(className = "dt-center", targets = 1:ncol(pred))
              )
            )
          )
        })


        output$exploreSummary3<- DT::renderDataTable({

          tempData=r$tempData
          test31 <- lm(Depth ~ Diameter*Absolute_Distance_from_Meridian,
                       data = tempData)

          pred1<- predict(test31, tempData)
          test32 <- lm(Depth ~ Diameter + Absolute_Distance_from_Meridian,
                       data = tempData)
          pred2 <- predict(test32 , tempData)

          pred<-data.frame(`predict with interaction`=round(pred1,2),
                           `predict without interaction`=round(pred2,2))
          colnames(pred)<-c("predict with interaction","predict without interaction")
          datatable(
            pred,
            caption = "Model prediction with and without interaction",
            style = "bootstrap4",
            rownames = TRUE,
            options = list(
              responsive = FALSE,
              scrollX = TRUE,
              ordering = FALSE,
              paging = FALSE,
              lengthChange = FALSE,
              pageLength = 10,
              searching = FALSE,
              info = FALSE,
              columnDefs = list(
                list(className = "dt-center", targets = 1:ncol(pred))
              )
            )
          )
        })
      })





    ## PREDICTED VALUE TABLE----
    # observeEvent(
    #   eventExpr = input$newSample,
    #   handlerExpr = {
    #     modelwith<- lm(Depth ~ Distance_from_Equator + Region +
    #                    Distance_from_Equator:Region,
    #                    data = r$tempData)
    #     modelwithout<- lm(Depth ~ Distance_from_Equator + Region,
    #                       data = r$tempData)
    #     data1<- data.frame(
    #       Distance_from_Equator = Distance_from_Equator,
    #       Region = Region
    #     )
    #     prediction1 <- predict(modelwith, newdata = data1,
    #                            type = "response", se.fit = FALSE)
    #     prediction2 <- predict(modelwithout, newdata = data1,
    #                            type = "response", se.fit = FALSE)
    #     predictionTable1<- rbind(prediction1, prediction2)
    #
    #     output$exploreSummary1 <- DT::renderDataTable(
    #       expr = predictionTable1,
    #       caption = "Predicted value with and without interaction",
    #       style = "bootstrap4",
    #       rownames = TRUE,
    #       options = list(
    #         responsive = TRUE,
    #         scrollX = TRUE,
    #         ordering = FALSE,
    #         paging = FALSE,
    #         lengthChange = FALSE,
    #         pageLength = 10,
    #         searching = FALSE,
    #         info = FALSE,
    #         columnDefs = list(
    #           list(className = "dt-center", targets = 1:ncol(predictionTable1))
    #         )
    #       )
    #     )
    #     }
    # )
    #
    # ## output table
    # observeEvent(
    #   eventExpr = input$newSample,
    #   handlerExpr = {
    #     output$exploreSummary1 <- DT::renderDataTable({
    #       exampleModel <- lm(
    #         formula = Depth ~ Distance_from_Equator + Region + Distance_from_Equator:Region,
    #         data = r$tempData)
    #       exampleCoeff1<- round(summary(exampleModel)$coefficients, digits = 4)
    #
    #       row.names(exampleCoeff1) <- sapply(
    #         X = row.names(exampleCoeff1),
    #         FUN = function(x) {
    #           gsub(
    #             pattern = "[[:punct:]]",
    #             replacement = " ",
    #             x = x
    #           )
    #         }
    #       )
    #
    #       datatable(
    #         exampleCoeff1,
    #         caption = "Model Coefficients with interaction",
    #         style = "bootstrap4",
    #         rownames = TRUE,
    #         options = list(
    #           responsive = TRUE,
    #           scrollX = TRUE,
    #           ordering = FALSE,
    #           paging = FALSE,
    #           lengthChange = FALSE,
    #           pageLength = 10,
    #           searching = FALSE,
    #           info = FALSE,
    #           columnDefs = list(
    #             list(className = "dt-center", targets = 1:ncol(exampleCoeff1))
    #           )
    #         )
    #       )
    #     })
    #
    #
    #     output$exploreSummary11 <- DT::renderDataTable({
    #       exampleModel <- lm(
    #         formula = Depth ~ Distance_from_Equator + Region,
    #         data =  r$tempData)
    #       exampleCoeff1<- round(summary(exampleModel)$coefficients, digits = 4)
    #
    #       row.names(exampleCoeff1) <- sapply(
    #         X = row.names(exampleCoeff1),
    #         FUN = function(x) {
    #           gsub(
    #             pattern = "[[:punct:]]",
    #             replacement = " ",
    #             x = x
    #           )
    #         }
    #       )
    #
    #       datatable(
    #         exampleCoeff1,
    #         caption = "Model Coefficients without interaction",
    #         style = "bootstrap4",
    #         rownames = TRUE,
    #         options = list(
    #           responsive = TRUE,
    #           scrollX = TRUE,
    #           ordering = FALSE,
    #           paging = FALSE,
    #           lengthChange = FALSE,
    #           pageLength = 10,
    #           searching = FALSE,
    #           info = FALSE,
    #           columnDefs = list(
    #             list(className = "dt-center", targets = 1:ncol(exampleCoeff1))
    #           )
    #         )
    #       )
    #     })
    #
    #
    #
    #
    #     output$exploreSummary2 <- DT::renderDataTable({
    #
    #       exampleModel <- lm(
    #         formula = Depth ~ Diameter * Distance_from_Equator,
    #         data = r$tempData)
    #       exampleCoeff2<- round(summary(exampleModel)$coefficients, digits = 4)
    #
    #       row.names(exampleCoeff2) <- sapply(
    #         X = row.names(exampleCoeff2),
    #         FUN = function(x) {
    #           gsub(
    #             pattern = "[[:punct:]]",
    #             replacement = " ",
    #             x = x
    #           )
    #         }
    #       )
    #
    #       datatable(
    #         exampleCoeff2,
    #         caption = "Model Coefficients with interaction",
    #         style = "bootstrap4",
    #         rownames = TRUE,
    #         options = list(
    #           responsive = FALSE,
    #           scrollX = TRUE,
    #           ordering = FALSE,
    #           paging = FALSE,
    #           lengthChange = FALSE,
    #           pageLength = 10,
    #           searching = FALSE,
    #           info = FALSE,
    #           columnDefs = list(
    #             list(className = "dt-center", targets = 1:ncol(exampleCoeff2))
    #           )
    #         )
    #       )
    #     })
    #
    #     output$exploreSummary22 <- DT::renderDataTable({
    #
    #       exampleModel <- lm(
    #         formula = Depth ~ Diameter + Distance_from_Equator,
    #         data = r$tempData)
    #       exampleCoeff2<- round(summary(exampleModel)$coefficients, digits = 4)
    #
    #       row.names(exampleCoeff2) <- sapply(
    #         X = row.names(exampleCoeff2),
    #         FUN = function(x) {
    #           gsub(
    #             pattern = "[[:punct:]]",
    #             replacement = " ",
    #             x = x
    #           )
    #         }
    #       )
    #
    #       datatable(
    #         exampleCoeff2,
    #         caption = "Model Coefficients without interaction",
    #         style = "bootstrap4",
    #         rownames = TRUE,
    #         options = list(
    #           responsive = TRUE,
    #           scrollX = TRUE,
    #           ordering = FALSE,
    #           paging = FALSE,
    #           lengthChange = FALSE,
    #           pageLength = 10,
    #           searching = FALSE,
    #           info = FALSE,
    #           columnDefs = list(
    #             list(className = "dt-center", targets = 1:ncol(exampleCoeff2))
    #           )
    #         )
    #       )
    #     })
    #     output$exploreSummary3 <- DT::renderDataTable({
    #       exampleModel <- lm(
    #         formula = Depth ~ Diameter * Absolute_Distance_from_Meridian,
    #         data = r$tempData)
    #
    #       exampleCoeff3<- round(summary(exampleModel)$coefficients, digits = 4)
    #
    #       row.names(exampleCoeff3) <- sapply(
    #         X = row.names(exampleCoeff3),
    #         FUN = function(x) {
    #           gsub(
    #             pattern = "[[:punct:]]",
    #             replacement = " ",
    #             x = x
    #           )
    #         }
    #       )
    #       datatable(
    #         exampleCoeff3,
    #         caption = "Model Coefficients with interaction",
    #         style = "bootstrap4",
    #         rownames = TRUE,
    #         options = list(
    #           responsive = TRUE,
    #           scrollX = TRUE,
    #           ordering = FALSE,
    #           paging = FALSE,
    #           lengthChange = FALSE,
    #           pageLength = 10,
    #           searching = FALSE,
    #           info = FALSE,
    #           columnDefs = list(
    #             list(className = "dt-center", targets = 1:ncol(exampleCoeff3))
    #           )
    #         )
    #       )
    #     })
    #     output$exploreSummary33 <- DT::renderDataTable({
    #       exampleModel <- lm(
    #         formula = Depth ~ Diameter + Absolute_Distance_from_Meridian,
    #         data = r$tempData)
    #       exampleCoeff3<- round(summary(exampleModel)$coefficients, digits = 4)
    #       row.names(exampleCoeff3) <- sapply(
    #         X = row.names(exampleCoeff3),
    #         FUN = function(x) {
    #           gsub(
    #             pattern = "[[:punct:]]",
    #             replacement = " ",
    #             x = x
    #           )
    #         }
    #       )
    #       datatable(
    #         exampleCoeff3,
    #         caption = "Model Coefficients without interaction",
    #         style = "bootstrap4",
    #         rownames = TRUE,
    #         options = list(
    #           responsive = TRUE,
    #           scrollX = TRUE,
    #           ordering = FALSE,
    #           paging = FALSE,
    #           lengthChange = FALSE,
    #           pageLength = 10,
    #           searching = FALSE,
    #           info = FALSE,
    #           columnDefs = list(
    #             list(className = "dt-center", targets = 1:ncol(exampleCoeff3))
    #           )
    #         )
    #       )
    #     })
    #   },
    #   ignoreNULL = TRUE,
    #   ignoreInit = TRUE
    # )
  })
}


# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
