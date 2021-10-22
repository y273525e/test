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

p4_4_ui = fluidPage(
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
        height = 100,
        verbatimTextOutput("velocityFun"),
        plotOutput("Vplot")
      )
    ),
    column(
      width = 6,
      box(
        width = 200,
        height = 100,
        verbatimTextOutput("accelerationFun"),
        plotOutput("Aplot")
      )
    )
  ),
  uiOutput('html4')
)

p4_2_ui=fluidPage(
  fluidRow(
  box(title = 'Example1',
      width = 7,
      textOutput('ex40201'),
  br(),
  fluidRow(
    column(
      width = 6,
      box(
        width = 200,
        actionButton('ex40201_1set','Solution'),
        verbatimTextOutput('good01'),
        plotOutput('goodplot01')
      )
    ),
    column(
      width = 6,
      box(
        width = 200,
        actionButton('ex40201_2set','Solution'),
        verbatimTextOutput('good02'),
        plotOutput('goodplot02')
      )
    )
  )),
  box(
    width = 5,
    height = 660,
    htmlOutput('html402'))
),
  fluidRow(
    box(title = 'Example2',
        width = 7,
        textOutput('ex40202'),
        br(),
        actionButton('ex40202_1set','Solution'),
        br(),
        verbatimTextOutput('good03')),
    box(
      width = 5,
      height = 650,
      htmlOutput('html403')
  )))

p4_3_ui = fluidPage(
  fluidRow(
    box(
      width = 12,
      title = 'Question 1',
      verbatimTextOutput("peggy_question1"),
    )
  ),
  fluidRow(
    box(
      width = 12,
      br(),
      verbatimTextOutput("peggy_Fun1"),
      br(),
      actionButton("peggy_set1","Plot"),
      br(),
      plotOutput("peggy_plot1")
    )
  ),
  fluidRow(
    column(
      width = 6,
      box(
        width = 200,
        verbatimTextOutput("peggy_que11"),
        actionButton("peggy_set11","Solution")
      )
    ),
    column(
      width = 6,
      box(
        width = 200,
        verbatimTextOutput("peggy_que12"),
        actionButton("peggy_set12","Solution")
      )
    )
  ),
  fluidRow(
    column(
      width = 6,
      box(
        width = 200,
        verbatimTextOutput("peggy_Fun11"),
        htmlOutput("peggy_html11")
      )
    ),
    column(
      width = 6,
      box(
        width = 200,
        verbatimTextOutput("peggy_Fun12"),
        htmlOutput("peggy_html12")
      )
    )
  ),
  fluidRow(
    column(
      width = 12,
      box(
        width = 200,
        verbatimTextOutput("peggy_que13"),
        actionButton("peggy_set13","Solution")
      )
    ),
    column(
      width = 12,
      box(
        width = 200,
        verbatimTextOutput("peggy_Fun13"),
        htmlOutput("peggy_html13")
      )
    )
  ),
  fluidRow(
    box(
      width = 12,
      title = 'Question 2',
      verbatimTextOutput("peggy_question2"),
    )
  ),
  fluidRow(
    box(
      width = 12,
      br(),
      verbatimTextOutput("peggy_Fun2"),
      br(),
      actionButton("peggy_set2","Plot"),
      br(),
      plotOutput("peggy_plot2")
    )
  ),
  fluidRow(
    column(
      width = 6,
      box(
        width = 200,
        verbatimTextOutput("peggy_que21"),
        actionButton("peggy_set21","Solution")
      )
    ),
    column(
      width = 6,
      box(
        width = 200,
        verbatimTextOutput("peggy_que22"),
        actionButton("peggy_set22","Solution")
      )
    )
  ),
  fluidRow(
    column(
      width = 6,
      box(
        width = 200,
        verbatimTextOutput("peggy_Fun21"),
        htmlOutput("peggy_html21")
      )
    ),
    column(
      width = 6,
      box(
        width = 200,
        verbatimTextOutput("peggy_Fun22"),
        htmlOutput("peggy_html22")
      )
    )
  )
)

p4_5_ui <- fluidPage(
  fluidRow(
    box(
      width = 12,
      verbatimTextOutput("example5_question"),
    )
  ),
  fluidRow(
    column(
      width = 6,
      box(
        width = 200,
        verbatimTextOutput("quiz1"),
        actionButton("setn1","Solution")
      ), box(
        width = 200,
        verbatimTextOutput("quiz1ch5"),
        plotOutput("quiz1plot")
      )),
    column(
      width = 6,
      box(
        title = 'R Code',
        solidHeader = TRUE,
        status = 'primary',
        width = 200,
        height = 550,
        htmlOutput("htmln2")))),
  
  fluidRow(
    column(
      width = 6,
      box(
        width = 200,
        verbatimTextOutput("quiz2"),
        actionButton("setn2","Solution")
      )
    ),
    column(
      width = 6,
      box(
        width = 200,
        verbatimTextOutput("quiz3"),
        actionButton("setn3","Solution")
      )
    )),
  fluidRow(
    column(
      width = 6,
      box(
        width = 200,
        verbatimTextOutput("quiz2ch5"),
        plotOutput("quiz2plot")
      )
    ),
    column(
      width = 6,
      box(
        width = 200,
        verbatimTextOutput("quiz3ch5"),
        plotOutput("quiz3plot")
      )
    )
  )
)

p4_6_ui = fluidPage(
  fluidRow(
    box(
      width = 6,
      height = 680,
      textOutput("ddd1_question"),
      br(),
      actionButton("dddd","Plot"),
      actionButton("ddd01","Solution"),
      uiOutput("daplot"),
      br(),
      uiOutput("dsol1plot")
      
    ),
    box(
      title = 'R Code Practice',
      solidHeader = TRUE,
      status = 'primary',
      width = 6,
      height = 680,
      htmlOutput("htmld2"))),
  fluidRow(
    box(
      width = 6,
      box(
        width = 500,
        textOutput("ddd2_question"),
        actionButton("ddd02","Solution")
      )
      ,
      box(
        width = 500,
        uiOutput("ddd02plot")
      )),
    box(
      title = 'R Code Practice',
      solidHeader = TRUE,
      status = 'primary',
      width = 6,
      height = 650,
      htmlOutput("htmld3")))
  
)

p4_8_ui = fluidPage(
  fluidRow(
    box(
      width = 6,
      height = 680,
      textOutput("ch08example1_question"),
      br(),
      actionButton("areaplot","Plot"),
      actionButton("sol01","Solution"),
      uiOutput("aplot"),
      br(),
      uiOutput("sol1plot")
      
    ),
  box(
    title = 'R Code Practice',
    solidHeader = TRUE,
    status = 'primary',
    width = 6,
    height = 680,
    htmlOutput("html2"))),
  fluidRow(
     box(
       width = 6,
      box(
        width = 500,
        textOutput("ch08example2_question"),
        actionButton("sol02","Solution")
      )
    ,
      box(
        width = 500,
        uiOutput("sol02plot")
      )),
  box(
    title = 'R Code Practice',
    solidHeader = TRUE,
    status = 'primary',
    width = 6,
    height = 650,
    htmlOutput("html3")))
  
)

p4_10_ui=fluidPage(
  fluidRow(
    box(
      width = 12,
      textOutput("example10_question"),
    ),
    box(
      width=12,
      htmlOutput("html0003"))
  ),
  
  
  fluidRow(
    column(
      width = 6,
      box(
        width = 200,
        textOutput("Q001"),
        actionButton("V001","Solution")
      )
    ),
    column(
      width = 6,
      box(
        width = 200,
        textOutput("Q002"),
        actionButton("V002","Solution")
      )
    )
  ),
  fluidRow(
    column(
      width = 6,
      box(
        width = 200,
        textOutput("A001")
      )
    ),
    column(
      width = 6,
      box(
        width = 200,
        textOutput("A002")
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
p6_00_ui=fluidPage(
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

p6_2_ui=fluidPage(
  box(
    width = 10,
      box(
        width = 900,
        title='Question 1',
        status = 'primary',
        uiOutput("choice201"),
        br(),
        radioButtons("p_201", "Select the CORRECT answer",
                     choices = c(
                       'a','b','c','d','e'
                     ),
                     width=1000,
                     inline = T,
                     selected = 0
        )
      ),
      box(
        width = 900,
        title='Question 2',
        status = 'primary',
        uiOutput("choice202"),
        br(),
        radioButtons("p_202", "Select the CORRECT answer",
                     choices = c(
                       'a','b'
                     ),
                     width=1000,
                     inline = T,
                     selected = 0)
      ),
      box(
        width = 900,
        title='Question 3',
        status = 'primary',
        uiOutput("choice203"),
        br(),
        radioButtons("p_203", "Select the CORRECT answer",
                     choices = c(
                       'a','b','c','d'
                     ),
                     width=1000,
                     inline = T,
                     selected = 0)
      ),
      box(
        width = 900,
        title='Question 4',
        status = 'primary',
        uiOutput("dddd301"),
        br(),
        radioButtons("d_301", "Select the CORRECT answer",
                     choices = c(
                       'a','b','c','d','e'
                     ),
                     width=1000,
                     inline = T,
                     selected = 0)
      ),
    box(
      width = 900,
      title='Question 5',
      status = 'primary',
      uiOutput("dddd302"),
      br(),
      radioButtons("d_302", "Select the CORRECT answer",
                   choices = c(
                     'a','b','c','d','e'
                   ),
                   width=1000,
                   inline = T,
                   selected = 0)
    ),
    box(
      width = 900,
      title='Question 6',
      status = 'primary',
      uiOutput("dddd303"),
      br(),
      radioButtons("d_303", "Select the CORRECT answer",
                   choices = c(
                     'a','b','c','d','e'
                   ),
                   width=1000,
                   inline = T,
                   selected = 0)
    ),
    box(
      width = 900,
      title='Question 7',
      status = 'primary',
      uiOutput("dddd304"),
      br(),
      radioButtons("d_304", "Select the CORRECT answer",
                   choices = c(
                     'a','b','c','d','e'
                   ),
                   width=1000,
                   inline = T,
                   selected = 0)
    ),
    box(
      width = 900,
      title='Question 8',
      status = 'primary',
      uiOutput("J401"),
      br(),
      radioButtons("d_401", "Select the CORRECT answer",
                   choices = c(
                     'a','b','c','d','e'
                   ),
                   width=1000,
                   inline = T,
                   selected = 0)
    ),
    box(
      width = 900,
      title='Question 9',
      status = 'primary',
      uiOutput("J402"),
      br(),
      radioButtons("d_402", "Select the CORRECT answer",
                   choices = c(
                     'a','b','c','d','e'
                   ),
                   width=1000,
                   inline = T,
                   selected = 0)
    ),
    box(
      width = 900,
      title='Question 10',
      status = 'primary',
      uiOutput("J403"),
      br(),
      radioButtons("d_403", "Select the CORRECT answer",
                   choices = c(
                     'a','b','c','d','e'
                   ),
                   width=1000,
                   inline = T,
                   selected = 0)
    ),
    box(
      width = 900,
      title='Question 11',
      status = 'primary',
      uiOutput("J404"),
      br(),
      radioButtons("d_404", "Select the CORRECT answer",
                   choices = c(
                     'a','b','c','d','e'
                   ),
                   width=1000,
                   inline = T,
                   selected = 0)
    ),
    actionButton("p_do", "Submit"))
  
)
