p1_ui = fluidPage(
  tabBox(
    width = 11,
    height = 1100,
    tabPanel(
      title = "CH2",
      uiOutput("pdf2")
    ),
    tabPanel(
      title = "CH3",
      uiOutput("pdf3")
    ),
    tabPanel(
      title = "CH4",
      uiOutput("pdf4")
    ),
    tabPanel(
      title = "CH5",
      uiOutput("pdf5")
    ),
    tabPanel(
      title = "CH6",
      uiOutput("pdf6")
    ),
    tabPanel(
      title = "CH7",
      uiOutput("pdf7")
    ),
    tabPanel(
      title = "CH8",
      uiOutput("pdf8")
    ),
    tabPanel(
      title = "CH10",
      uiOutput("pdf10")
    )
  )
)

p2_ui = fluidPage(

  tabBox(
    width = 7,
    height = 800,
    tabPanel(
      title = "Code1",
      uiOutput("code1")
    ),
    tabPanel(
      title = "Code2",
      uiOutput("code2")
    )),
  box(
    title = 'R Code Practice',
    solidHeader = TRUE,
    status = 'primary',
    width = 5,
    height = 800,
    htmlOutput("html"))
)

p3_ui = fluidPage(
  fluidRow(
    box(
      title = "Compound Interest",
      numericInput('p',"Present Value",100),
      numericInput('r',"annual interest rate",0.01),
      numericInput('m',"compounded times a year",2),
      numericInput('t',"after several years",5),
      actionButton('set1',"set", icon("paper-plane"),
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    
    ),
    box(title = 'Value after t years :',textOutput("FunctionPlot")),
    box(title = 'Compound Interest : ',textOutput('FunctionPlot0'))
  ),
  fluidRow(box(
    title = "Present Value",
    numericInput('p1',"Future Payment",100),
    numericInput('r1',"annual interest rate",0.01),
    numericInput('m1',"compounded times a year",2),
    numericInput('t1',"years to be paid",5),
    actionButton('set2',"set", icon("paper-plane"),
                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
  ),
  box(title = 'Present Value : ',textOutput("FunctionPlot1"))
  ),
  fluidRow(box(
    title = "Continuous Compounding",
    numericInput('p2',"Present Value",100),
    numericInput('r2',"annual interest rate",0.01),
    numericInput('t2',"years for compounded continuously",5),
    actionButton('set3',"set", icon("paper-plane"),
                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
  ),
  box(title = 'Value after t years : ',textOutput("FunctionPlot2"))
  )
  )


p3_1_ui = fluidPage(
  
  # Application title
  headerPanel("Column-based integration"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    
    textInput("infun", "Enter function:", value = "dnorm(x)"),    
    numericInput("poly", "Number of columns:", min=1, max=10000, value=20),
    numericInput("fromx", "Starting x:", value=-1.96),
    numericInput("tox", "Ending x:", value=1.96),
    checkboxInput("intcum", "Show cumulative integration", FALSE),
    actionButton('set4',"set", icon("paper-plane"),
                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("intPlot")
  )
)

p3_2_ui = fluidPage(
  fluidRow(
    box(
      title = "Function & Plot",
      textInput("expression","expresssion input (only for x)"),
      numericInput("from1","from",-10),
      numericInput("to1","to",10),
      actionButton("set5","set")
    ),
    box(
      plotOutput("FunctionPlot00")
    )
  ),
  fluidRow(
    box(
      title = "First Derivative",
      textOutput("difFun1a"),
      numericInput("from2","from",-10),
      numericInput("to2","to",10),
      actionButton("set6","set")
    ),
    box(
      textOutput("difFun1"),
      plotOutput("difplot1")
    )
  ),
  fluidRow(
    box(
      title = "Second Derivative",
      textOutput("difFun2a"),
      numericInput("from3","from",-10),
      numericInput("to3","to",10),
      actionButton("set7","set")
    ),
    box(
      textOutput("difFun2"),
      plotOutput("difplot2")
    )
  )
)

p4_ui = fluidPage(
  fluidRow(
    box(
      width = 12,
      textOutput("example1_question"),
    )
  ),
  fluidRow(
    box(
      width = 12,
      br(),
      textOutput("QFun"),
      br(),
      actionButton("Bset","Plot"),
      br(),
      plotOutput("Qplot")
    )
  ),
  fluidRow(
    column(
      width = 6,
      box(
        width = 200,
        textOutput("velocity"),
        actionButton("Vset","Solution")
      )
    ),
    column(
      width = 6,
      box(
        width = 200,
        textOutput("acceleration"),
        actionButton("Aset","Solution")
      )
    )
  ),
  fluidRow(
    column(
      width = 6,
      box(
        width = 200,
        textOutput("velocityFun"),
        plotOutput("Vplot")
      )
    ),
    column(
      width = 6,
      box(
        width = 200,
        textOutput("accelerationFun"),
        plotOutput("Aplot")
      )
    )
  )
)

p5_ui=fluidPage(
  fluidRow(
    box(
      width = 6,
      title = "1",
      status = "primary",
      solidHeader = TRUE,
      tags$h3(textOutput("moontitle")),
      br(),
      tags$h4(textOutput("moontxt1")),
      br()
    ),
    box(
      width = 6,
      title = "2",
      status = "success",
      solidHeader = TRUE,
      tags$h4(textOutput("moontxt2")),
      br(),
      textOutput("moonFun")
    )
  ),
  fluidRow(
    box(
      width = 6,
      title = "3",
      status = "info",
      solidHeader = TRUE,
      tags$h3(textOutput("title1")),
      br(),
      tags$h4(textOutput("tx1")),
      br(),
      uiOutput("MVT")
    ),
    box(
      width = 6,
      title = "4",
      status = "warning",
      solidHeader = TRUE,
      tags$h4(textOutput("moontxt3")),
      br(),
      uiOutput("conFUn5"),
    ),
    box(
      width = 6,
      title = "5",
      status = "danger",
      solidHeader = TRUE,
      tags$h4(textOutput("conclusion1")),
      br(),
      textOutput("conFUn")
    )
  )
)

#P6
p6_ui=fluidPage(
  radioButtons("rb1", "Let f  be the function defined by f (x) = 4x + 8.
  Find f (1), f (- 1), f (a), f (- a), and f (a + 1). 
",
               choiceNames = list(
                 'a. f (1) = 12, f (- 1) = 4, f (a) = a + 8, f(- a) = - a + 8, f (a + 1) = 4a + –4',
                 'b. f (1) = 11, f (- 1) = 5, f (a) = 4a + 8, f(- a) = - 4a + 8',
                 'c. f (1) = 10, f (- 1) = 6, f (a) = 12, f(- a) = 4, f (a + 1) = 4a + 12',
                 'd. f (1) = 12, f (- 1) = 4, f (a) = 4a + 8, f(- a) = 4, f (a + 1) = 4a + 8',
                 'e. f (1) = 12, f (- 1) = 4, f (a) = 4a + 8, f(- a) = - 4a + 8, f (a + 1) = 4a + 12'
               ),
               choiceValues = list(
                 "Wrong", "Wrong", "Wrong",'Wrong',"Correct!"
               ),
               width=1000),
  textOutput("txt1"),
  br(),
  
  radioButtons("rb2", "Determine whether the statement is true or false. 
               If f is a function, then f (a + b) = f (a) + f (b).",
               choiceNames = list(
                 'a. True',
                 'b. False'
               ),
               choiceValues = list(
                 "Correct!",'Wrong'
               ),
               width=1000,
               inline = T),
  textOutput("txt2"),
  br(),
  actionButton("do", "Submit")
  
)

