library(shiny)
require(ggplot2)
require(scales)
require(gridExtra)
require(devtools)
require(mosaic)
require(devtools)
require(shinyforms)
library(rsconnect)
require(shinyglide)
#remotes::install_github("juba/shinyglide")
#rsconnect::deployApp('path/to/your/app')
source("mgf.R")
source("sub_ui.R")
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                     tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                     textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
                     passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                     br(),
                     div(
                         style = "text-align: center;",
                         actionButton("login", "SIGN IN", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                         shinyjs::hidden(
                             div(id = "nomatch",
                                 tags$p("Oops! Incorrect username or password!",
                                        style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                                        class = "text-center"))),
                         br(),
                         br(),
                         tags$code("Username: myuser  Password: mypass"),
                         br(),
                         tags$code("Username: myuser1  Password: mypass1")
                     ))
)

credentials = data.frame(
    username_id = c("myuser", "myuser1"),
    passod   = sapply(c("mypass", "mypass1"),password_store),
    permission  = c("basic", "advanced"), 
    stringsAsFactors = F
)

shinyServer(function(input, output, session){
    
    login = FALSE
    USER <- reactiveValues(login = login)
    
    observe({ 
        if (USER$login == FALSE) {
            if (!is.null(input$login)) {
                if (input$login > 0) {
                    Username <- isolate(input$userName)
                    Password <- isolate(input$passwd)
                    if(length(which(credentials$username_id==Username))==1) { 
                        pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
                        pasverify <- password_verify(pasmatch, Password)
                        if(pasverify) {
                            USER$login <- TRUE
                        } else {
                            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
                            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
                        }
                    } else {
                        shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
                        shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
                    }
                } 
            }
        }    
    })
    
    output$logoutbtn <- renderUI({
        req(USER$login)
        tags$li(a(icon("fa fa-sign-out"), "Logout", 
                  href="javascript:window.location.reload(true)"),
                class = "dropdown", 
                style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
    })
    
    output$sidebarpanel <- renderUI({
        if (USER$login == TRUE ){ 
            sidebarMenu(
                menuItem("主題 : 微積分",tabName = "chapter1",
                         menuItem("講義",tabName = "page1"),
                         menuItem("R程式", tabName="page2"),
                         menuItem("互動式R-Shiny數位教材",tabName = "contact",
                                  menuItem('現金流量',tabName = 'page3'),
                                  menuItem('含上下界積分',tabName = 'page3-1'),
                                  menuItem('微分',tabName = 'page3-2')),
                         menuItem("習題練習", tabName="page4",
                                  menuItem('CH2',tabName='page4-2'),
                                  menuItem('CH3',tabName='page4-3'),
                                  menuItem('CH4',tabName='page4-4'),
                                  menuItem('CH5',tabName='page4-5'),
                                  menuItem('CH6',tabName='page4-6'),
                                  menuItem('CH8',tabName='page4-8'),
                                  menuItem('CH10',tabName='page4-10')),
                         menuItem("科普應用", tabName="page5"),
                         menuItem("測驗", tabName="page6",
                                  menuItem('CH00',tabName='page6-00'),
                                  menuItem('CH2',tabName='page6-2'))
                )
            )
        }
    })
    
    output$body <- renderUI({
        if (USER$login == TRUE ) {
            tabItems(
                tabItem(tabName = "page1", p1_ui),
                tabItem(tabName = "page2", p2_ui),
                tabItem(tabName = "page3", p3_ui),
                tabItem(tabName = 'page3-1',p3_1_ui),
                tabItem(tabName = 'page3-2',p3_2_ui),
                tabItem(tabName = "page4-2", p4_2_ui),
                tabItem(tabName = "page4-3", p4_3_ui),
                tabItem(tabName = "page4-4", p4_4_ui),
                tabItem(tabName = "page4-5", p4_5_ui),
                tabItem(tabName = "page4-6", p4_6_ui),
                tabItem(tabName = "page4-8", p4_8_ui),
                tabItem(tabName = "page4-10", p4_10_ui),
                tabItem(tabName = "page5", p5_ui),
                tabItem(tabName = "page6-00", p6_00_ui),
                tabItem(tabName = "page6-2", p6_2_ui)
            )
            
        }
        else {
            loginpage
        }
    })
    #p1
    output$pdf2 <- renderUI({
        tags$iframe(style="height:1000px; width:100%", src="CH2.pdf")
    })
    output$pdf3 <- renderUI({
        tags$iframe(style="height:1000px; width:100%", src="CH3.pdf")
    })
    output$pdf4 <- renderUI({
        tags$iframe(style="height:1000px; width:100%", src='CH4.pdf')
    })
    output$pdf5 <- renderUI({
        tags$iframe(style="height:1000px; width:100%", src="CH5.pdf")
    })
    output$pdf6 <- renderUI({
        tags$iframe(style="height:1000px; width:100%", src="CH6.pdf")
    })
    output$pdf7 <- renderUI({
        tags$iframe(style="height:1000px; width:100%", src="CH7.pdf")
    })
    output$pdf8 <- renderUI({
        tags$iframe(style="height:1000px; width:100%", src="CH8.pdf")
    })
    output$pdf10 <- renderUI({
        tags$iframe(style="height:1000px; width:100%", src="CH10.pdf")
    })
    
    
    #p2
    output$code1 <- renderUI({
        tags$iframe(style="height:750px; width:100%", src="R-code1.pdf")
    })
    output$code2 <- renderUI({
        tags$iframe(style="height:750px; width:100%", src="R-code2.pdf")
    })
    output$html = renderUI({
        tags$iframe(width="100%", height="750", src='example001.html',seamless=TRUE)
    })
    #p3
    observeEvent(input$set1,{
        output$FunctionPlot = renderText(paste(
            input$p*((1+input$r/input$m)^(input$m*input$t))))
    })
    observeEvent(input$set1,{
        output$FunctionPlot0 = renderText(paste(
            ((1+input$r/input$m)^(input$m*input$t))-1))
    })
    
    observeEvent(input$set2, {
        output$FunctionPlot1 = renderText(paste(
            input$p1/((1+input$r1/input$m1)^(input$m1*input$t1))
            
        ))
    })
    
    observeEvent(input$set3, {
        output$FunctionPlot2 = renderText(paste(
            input$p2*exp(input$r2*input$t2)
        ))
    })
    
    #p3-1
    observeEvent(input$set4, {
        output$intPlot <- renderPlot({
            int.fun<-input$infun
            int.fun<-eval(parse(text=paste('function(x)',int.fun)))
            int.base<-0
            num.poly<-input$poly
            from.x<-input$fromx
            to.x<-input$tox
            if(from.x>to.x) stop('Starting x must be less than ending x')
            
            poly.x<-seq(from.x,to.x,length=num.poly+1)
            
            polys<-sapply(
                1:(length(poly.x)-1),
                function(i){
                    
                    x.strt<-poly.x[i]
                    x.stop<-poly.x[i+1]
                    
                    cord.x<-rep(c(x.strt,x.stop),each=2) 
                    cord.y<-c(int.base,rep(int.fun(mean(c(x.strt,x.stop))),2),int.base) 
                    data.frame(cord.x,cord.y)
                    
                },
                simplify=F
            )
            
            area<-sum(unlist(lapply(
                polys,
                function(x) diff(unique(x[,1]))*diff(unique(x[,2]))
            )))
            txt.val<-paste('Area from',from.x,'to',to.x,'=',round(area,4),collapse=' ')
            
            y.col<-rep(unlist(lapply(polys,function(x) max(abs(x[,2])))),each=4)
            plot.polys<-data.frame(do.call('rbind',polys),y.col)
            
            p1<-ggplot(data.frame(x=c(from.x,to.x)), aes(x)) + stat_function(fun=int.fun)
            if(num.poly==1){ 
                p1<-p1 + geom_polygon(data=plot.polys,mapping=aes(x=cord.x,y=cord.y),
                                      alpha=0.7,color=alpha('black',0.6))
            }
            else{
                p1<-p1 + geom_polygon(data=plot.polys,mapping=aes(x=cord.x,y=cord.y,fill=y.col,
                                                                  group=y.col),alpha=0.6,color=alpha('black',0.6))
            }
            
            p1<-p1 + ggtitle(txt.val) + theme(legend.position="none") 
            
            if(!input$intcum) print(p1) 
            
            else{
                area.cum<-unlist(sapply(1:num.poly,
                                        function(val){
                                            poly.x<-seq(from.x,to.x,length=val+1)
                                            
                                            polys<-sapply(
                                                1:(length(poly.x)-1),
                                                function(i){
                                                    
                                                    x.strt<-poly.x[i]
                                                    x.stop<-poly.x[i+1]
                                                    
                                                    cord.x<-rep(c(x.strt,x.stop),each=2) 
                                                    cord.y<-c(int.base,rep(int.fun(mean(c(x.strt,x.stop))),2),int.base) 
                                                    data.frame(cord.x,cord.y)
                                                    
                                                },
                                                simplify=F
                                            )
                                            
                                            sum(unlist(lapply(
                                                polys,
                                                function(x) diff(unique(x[,1]))*diff(unique(x[,2]))
                                            )))
                                            
                                        }
                ))
                
                dat.cum<-data.frame(Columns=1:num.poly,Area=area.cum)
                actual<-integrate(int.fun,from.x,to.x)
                
                p2<-ggplot(dat.cum, aes(x=Columns,y=Area)) + geom_point() #+ geom_smooth(span=0.1,se=F)
                p2<-p2 + geom_hline(yintercept=actual$value,lty=2) 
                p2<-p2 + ggtitle(paste('Actual integration',round(actual$value,4),'with absolute error',prettyNum(actual$abs.error)))
                
                print(grid.arrange(p1,p2))
                
            }
            
        },height=500)})    
    
    #p3-3
    observeEvent(input$set5,{
        output$FunctionPlot00 = renderPlot({
            mgf(input$expression, from=input$from1, to=input$to1)
        })
    })
    observeEvent(input$set6,{
        output$difFun1 = renderPrint({
            dif(input$expression)
        })
        output$difFun1a = renderPrint({
            dif(input$expression)
        })
        output$difplot1 = renderPlot({
            mgf(deparse(dif(input$expression)), from=input$from2, to=input$to2)
        })
    })
    observeEvent(input$set7,{
        output$difFun2 = renderPrint({
            ddif(input$expression)
        })
        output$difFun2a = renderPrint({
            ddif(input$expression)
        })
        output$difplot2 = renderPlot({
            mgf(deparse(ddif(input$expression)), from=input$from3, to=input$to3)
        })
    })
    
    #p4-2
    Previous_Button=tags$div(actionButton("Prev_Tab",HTML('<div class="col-sm-4"><i class="fa fa-angle-double-left fa-2x"></i></div>
                                                                  ')))
    Next_Button=div(actionButton("Next_Tab",HTML('<div class="col-sm-4"><i class="fa fa-angle-double-right fa-2x"></i></div>')))

    output$ex40201= renderUI({
        tags$iframe(width = "100%", height = "120", src='pic01.png')
    })
    observeEvent(input$ex40201_1set,{
        output$good01=renderText({
            'f(Ace)=35+0.45x'
        })
        output$goodplot01=renderPlot({
            mgf('35+0.45*x', from=0, to=20)
        })
    })
    observeEvent(input$ex40201_2set,{
        output$good02=renderText({
            'f(Acme)=30+0.5x'
        })
        output$goodplot02=renderPlot({
            mgf('30+0.5*x', from=0, to=20)
        })
    })
    output$html402 = renderUI({
        tags$iframe(width="100%", height="600", src='example01.html',seamless=TRUE)
    })
    output$ex40202= renderUI({
        tags$iframe(width = "100%", height = "120", src='pic02.png')
    })
    
    observeEvent(input$ex40202_1set,{
        output$good03=renderUI({
            tags$iframe(width = "100%", height = "40", src='pic03.png')
        })
    })
    output$html403 = renderUI({
        tags$iframe(width="100%", height="600", src='example02.html',seamless=TRUE)
    })
    output$Next_Previous=renderUI({
        tab_list=input$List_of_tab[-length(input$List_of_tab)]
        nb_tab=length(tab_list)
        if (which(tab_list==input$tabBox_next_previous)==nb_tab)
            column(1,offset=1,Previous_Button)
        else if (which(tab_list==input$tabBox_next_previous)==1)
            column(1,offset = 10,Next_Button)
        else
            div(column(1,offset=1,Previous_Button),column(1,offset=8,Next_Button))
        
    })
    observeEvent(input$Prev_Tab,
                 {
                     tab_list=input$List_of_tab
                     current_tab=which(tab_list==input$tabBox_next_previous)
                     updateTabsetPanel(session,"tabBox_next_previous",selected=tab_list[current_tab-1])
                 }
    )
    observeEvent(input$Next_Tab,
                 {
                     tab_list=input$List_of_tab
                     current_tab=which(tab_list==input$tabBox_next_previous)
                     updateTabsetPanel(session,"tabBox_next_previous",selected=tab_list[current_tab+1])
                 }
    )
    
    #4-3
    Previous_Button1=tags$div(actionButton("Prev_Tab1",HTML('<div class="col-sm-4"><i class="fa fa-angle-double-left fa-2x"></i></div>
                                                                  ')))
    Next_Button1=div(actionButton("Next_Tab1",HTML('<div class="col-sm-4"><i class="fa fa-angle-double-right fa-2x"></i></div>')))
    
    output$peggy_question1 = renderUI({
        tags$iframe(width = "100%", height = "130", src='pic04.png')
    })
    output$peggy_que11 = renderText({"(a) Find the rate of growth of the turtle population when t=2."})
    output$peggy_que12 = renderText({
        "(b) Find the rate of growth of the turtle population when t=4."})
    output$peggy_que13 = renderText({
        "(c) What will be the population 10 yr after the conservation measures are implemented?"})
    output$peggy_Fun1 = renderUI({
        tags$iframe(width = "100%", height = "40", src='pic05.png')
})
    observeEvent(input$peggy_set1,{
        output$peggy_plot1 = renderPlot({
            mgf('2*x^3+4*x^2-5*x+1000', from=0, to=10)
        })})
    observeEvent(input$peggy_set11,{
        output$peggy_Fun11 = renderUI({
            tags$iframe(width = "100%", height = "75", src='pic06.png')
        })
        output$peggy_html11 = renderUI({
            tags$iframe(width="100%", height="360", src='code11.html',seamless=TRUE)
        })
    })
    
    observeEvent(input$peggy_set12,{
        output$peggy_Fun12 = renderUI({
            tags$iframe(width = "100%", height = "75", src='pic07.png')
        })
        output$peggy_html12 = renderUI({
            tags$iframe(width="100%", height="360", src='code12.html',seamless=TRUE)
        })
    })
    
    observeEvent(input$peggy_set13,{
        output$peggy_Fun13 = renderUI({
            tags$iframe(width = "100%", height = "75", src='pic08.png')
        })
        output$peggy_html13 = renderUI({
            tags$iframe(width="100%", height="360", src='code13.html',seamless=TRUE)
        })
    })
    
    ### 4-3Q2 ###
    output$peggy_question2 = renderUI({
        tags$iframe(width="100%", height="79", src='pic09.png')
    })
    output$peggy_que21 = renderText({"(a) Find the percentage per minute when t=3 ( A'(3) )."})
    output$peggy_que22 = renderText({
        "(b) Find the percentage per minute in the second power when t=3 ( A''(3) )."})
    output$peggy_Fun2 = renderUI({
        tags$iframe(width="100%", height="40", src='pic10.png')
    })
    observeEvent(input$peggy_set2,{
        output$peggy_plot2 = renderPlot({
            mgf('-0.00006*x^5+0.00468*x^4-0.1316*x^3+1.915*x^2-17.63*x+100', from=0, to=10)
        })
    })
    observeEvent(input$peggy_set21,{
        output$peggy_Fun21 = renderUI({
            tags$iframe(width="100%", height="75", src='pic11.png')
        })
        output$peggy_html21 = renderUI({
            tags$iframe(width="100%", height="360", src='code21.html',seamless=TRUE)
        })
    })
    
    observeEvent(input$peggy_set22,{
        output$peggy_Fun22 = output$peggy_Fun21 = renderUI({
            tags$iframe(width="100%", height="75", src='pic12.png')
        })
        output$peggy_html22 = renderUI({
            tags$iframe(width="100%", height="360", src='code22.html',seamless=TRUE)
        })
    })
    output$Next_Previous1=renderUI({
        tab_list1=input$List_of_tab1[-length(input$List_of_tab1)]
        nb_tab1=length(tab_list1)
        if (which(tab_list1==input$tabBox_next_previous1)==nb_tab1)
            column(1,offset=1,Previous_Button1)
        else if (which(tab_list1==input$tabBox_next_previous1)==1)
            column(1,offset = 10,Next_Button1)
        else
            div(column(1,offset=1,Previous_Button1),column(1,offset=8,Next_Button1))
        
    })
    observeEvent(input$Prev_Tab1,
                 {
                     tab_list1=input$List_of_tab1
                     current_tab1=which(tab_list1==input$tabBox_next_previous1)
                     updateTabsetPanel(session,"tabBox_next_previous1",selected=tab_list1[current_tab1-1])
                 }
    )
    observeEvent(input$Next_Tab1,
                 {
                     tab_list1=input$List_of_tab1
                     current_tab1=which(tab_list1==input$tabBox_next_previous1)
                     updateTabsetPanel(session,"tabBox_next_previous1",selected=tab_list1[current_tab1+1])
                 }
    )
    
    #p4-4
    #改成215 APPLIED EXAMPLE 4
    Previous_Button2=tags$div(actionButton("Prev_Tab2",HTML('<div class="col-sm-4"><i class="fa fa-angle-double-left fa-2x"></i></div>
                                                                  ')))
    Next_Button2=div(actionButton("Next_Tab2",HTML('<div class="col-sm-4"><i class="fa fa-angle-double-right fa-2x"></i></div>')))
    
    output$example1_question = renderUI({
        tags$iframe(width="100%", height="40", src='pic13.png')
    })
    output$velocity = renderText({"(a) 0 week (at the beginning of training)"})
    output$acceleration = renderText({"(b) 12 weeks"})
    output$QFun = renderUI({
        tags$iframe(width="100%", height="40", src='pic14.png')
    })
    observeEvent(input$Bset,{
        output$Qplot = renderPlot({
            mgf('15-(11*2.72^(-0.1*x))', from=0, to=20)
        })})
    observeEvent(input$Vset,{
        output$velocityFun = renderUI({
            tags$iframe(width="100%", height="110", src='pic15.png')
        })})
    
    observeEvent(input$Aset,{
        output$accelerationFun = renderUI({
            tags$iframe(width="100%", height="110", src='pic16.png')
        })
    })
    output$html4 = renderUI({
        tags$iframe(width="100%", height="600", src='08151.html',seamless=TRUE)
    })
    output$Next_Previous2=renderUI({
        tab_list2=input$List_of_tab2[-length(input$List_of_tab2)]
        nb_tab2=length(tab_list2)
        if (which(tab_list2==input$tabBox_next_previous2)==nb_tab2)
            column(1,offset=1,Previous_Button2)
        else if (which(tab_list2==input$tabBox_next_previous2)==1)
            column(1,offset = 10,Next_Button2)
        else
            div(column(1,offset=1,Previous_Button2),column(1,offset=8,Next_Button2))
        
    })
    observeEvent(input$Prev_Tab2,
                 {
                     tab_list2=input$List_of_tab2
                     current_tab2=which(tab_list2==input$tabBox_next_previous2)
                     updateTabsetPanel(session,"tabBox_next_previous2",selected=tab_list2[current_tab2-1])
                 }
    )
    observeEvent(input$Next_Tab2,
                 {
                     tab_list2=input$List_of_tab2
                     current_tab2=which(tab_list2==input$tabBox_next_previous2)
                     updateTabsetPanel(session,"tabBox_next_previous2",selected=tab_list2[current_tab2+1])
                 }
    )
    
    #p4-5
    output$htmln2 = renderUI({
        tags$iframe(width="90%", height="480", src='examplen5.html',seamless=TRUE)
    })
    
    #####課本習題#####
    output$example5_question = renderText({"Find each of the following Indefinite integrals "})
    output$quiz1 = renderText({"∫(3x-2)dx"})
    output$quiz2 = renderText({"∫(1/x)dx"})
    output$quiz3 = renderText({"∫(e+1)dx"})
    observeEvent(input$setn1,{
        output$quiz1ch5 = renderUI({
            tags$iframe(width="100%", height="230", src='pic17.png')
        })})
    
    observeEvent(input$setn2,{
        output$quiz2ch5 = renderUI({
            tags$iframe(width="100%", height="190", src='pic18.png')
        })})
    
    observeEvent(input$setn3,{
        output$quiz3ch5 = renderUI({
            tags$iframe(width="100%", height="230", src='pic19.png')
        })
    })
    
    #p4-6
    output$ddd1_question = renderText({"Evaluate x e^xdx"})
    output$ddd2_question = renderText({"Evaluate xlnx dx"})
    observeEvent(input$dddd,{
        output$daplot = renderUI({
            tags$iframe(width = "100%", height = "350", src='ex6-1-1.jpg')
        })})
    observeEvent(input$ddd01,{
        output$dsol1plot = renderUI({
            tags$iframe(width = "100%", height = "150", src='ex6-1-c.png')
        })})
    output$htmld2 = renderUI({
        tags$iframe(width="100%", height="600", src='fabian6html01.html',seamless=TRUE)
    })
    observeEvent(input$ddd02,{
        output$ddd02plot = renderUI({
            tags$iframe(width = "100%", height = "120", src='ex6-2-a.png')
        })})
    output$htmld3 = renderUI({
        tags$iframe(width="100%", height="600", src='fabian6html02.html',seamless=TRUE)
    })
    
    
    #p4-8
    output$ch08example1_question = renderText({"Find the area under one arch of y =sin t"})
    output$ch08example2_question = renderText({"Differentiate f(t)=sint÷t"})
    observeEvent(input$areaplot,{
        output$aplot = renderUI({
            tags$iframe(width = "500", height = "450", src='areaplot.png')
        })})
    observeEvent(input$sol01,{
        output$sol1plot = renderUI({
            tags$iframe(width = "500", height = "50", src='ch8sol01.png')
        })})
    output$html2 = renderUI({
        tags$iframe(width="100%", height="600", src='ch08html01.html',seamless=TRUE)
    })
    observeEvent(input$sol02,{
        output$sol02plot = renderUI({
            tags$iframe(width = "500", height = "100", src='ch8sol02.png')
        })})
    output$html3 = renderUI({
        tags$iframe(width="100%", height="600", src='ch08html02.html',seamless=TRUE)
    })
    
    #4-10
    output$Q001 = renderUI({
        tags$iframe(width = "100%", height = "240", src='pic20.png')
    })
    output$Q002 = renderUI({
        tags$iframe(width = "100%", height = "240", src='pic21.png')
    })
    observeEvent(input$V001,{
        output$A001 = renderUI({
            tags$iframe(width = "100%", height = "55", src='pic22.png')
        })})
    observeEvent(input$V002,{
        output$A002 = renderUI({
            tags$iframe(width = "100%", height = "55", src='pic23.png')
        })})
    output$html0003 <- renderUI({
        tags$iframe(src = "example41001.html", width = "100%", height = "600", seamless=TRUE)
    })
    
    
    
    #p5
    output$moontitle = renderText({
        "無限小數與非標準分析學"
    })
    output$moontxt1 = renderText({
        "17世紀時牛頓和萊布尼茲發明了微積分。其中萊布尼茲的「積分符號」∫、
        「極微小差」dx 等兩個符號仍然使用至今。現今的課本會用「極限」解釋，所以
        有些人說 dx 只是符號，不需要實質意義。這兩種觀點都有其意義和重要性。本文將分
        為若干期，從不同觀點探索微積分的靈魂、以及各觀點的應用。"
    })
    output$moontxt2 = renderText({
        "為何有微積分"
    })
    output$moonFun = renderText({
        '自古以來，數學家們就深知自然萬物難以測量，而不如圓形、多邊形、橢圓一般簡潔。
        我們固然可以拿起一把尺開始耐著性子量，例如阿基米德 (Αρχιμήδης ο Συρακούσιος)、劉
        徽、關孝和 (関 孝和) 的割圓術，又譬如古巴比倫人的三角函數表，都是測量的典範和先
        驅。雖然以人類的力量和壽命，必定會留下誤差，但是只要我們願意再量幾次，就可以把
        誤差一直縮小。所以理想上的誤差是「無限小的數 (infinitesimal number)」，與無限大
        相對。17世紀，勒內．笛卡兒 (René Descartes) 發明解析幾何，再次帶動了無限小數的討
        論。這門學問被稱做「分析學 (analysis)」或「數學分析 (mathematical analysis)」。終
        於在同一世紀，艾薩克．牛頓 (Isaac Newton)、哥特佛萊德．萊布尼茲 (Gottfried Leibni
        z) 確立了分析學的基礎。所以他們被尊為微積分的發明者。'
    })
    
    output$conclusion1 = renderText({
        "無限大與無限小"
    })
    output$conFUn = renderText({
        '「無限」的概念太難描繪，但也因此而迷人。「無限大」是什麼呢？無限大減一是什麼？
        無限大減無限大是什麼？「無限大」已經夠難了，「無限小」卻更難。我們或許能想像在
        數線看不到的地方有個盡頭，那裡的數字叫做無限大。無限小卻是硬生生地在我們眼前消失
        。無限小在數線的位置應該和 0 一模一樣，可是無限小又不是 0！'
    })
    
    output$moontxt3 = renderText({
        '任何有限數 x 都能唯一地分成一個實數和一個無限小數的和。其中的實數記為 st(x)，稱
        為 x 的標準部分 (standard part)。'
    })
    output$conFUn5 = renderUI({
        tags$iframe(width = "450", height = "250", src='p83413-f1.png')
    })
    output$title1 = renderText({
        "微積分均值定理"
    })
    output$tx1 = renderText({
        "舉例來說：國道一號從台北到新竹差不多  70  公里，從上國道開始算，到下國道，若只花了  30  分鐘，
        這段路途的平均時速是「每小時  140 公里」，因此根據均值定理，一定有個時間點的時速達到  140 (公里/小時)。"
    })
    output$MVT = renderUI({
        tags$iframe(width = "450", height = "300", src='images.png')
    })
    
    observeEvent(input$do,{
        output$txt1 = renderPrint({
            paste(input$rb1)
        })
        output$txt2 = renderPrint({
            paste(input$rb2)
        })
    })
    
    ### p6 ###
    item<-sample(1:100,10,replace = FALSE)
    x<-list('1'='b','2'='b','3'='c','4'='e','5'='b','6'='d','7'='b','8'='e','9'='d','10'='c',
            '11'='d','12'='c','13'='e','14'='d','15'='a','16'='a','17'='d','18'='c','19'='a','20'='c',
            '21'='b','22'='e','23'='d','24'='a','25'='b','26'='c','27'='d','28'='a','29'='c','30'='e',
            '31'='c','32'='a','33'='e','34'='a','35'='b','36'='a','37'='d','38'='c','39'='a','40'='a',
            '41'='c','42'='c','43'='d','44'='b', '45'='c','46'='c','47'='e','48'='c','49'='d','50'='c',
            '51'='d','52'='d','53'='c','54'='e','55'='e','56'='e','57'='a','58'='e','59'='b','60'='c',
            '61'='b','62'='a','63'='b','64'='a','65'='a','66'='a','67'='c','68'='d','69'='a','70'='c',
            '71'='a','72'='d','73'='a','74'='a','75'='d','76'='c','77'='e','78'='c','79'='a','80'='a',
            '81'='c','82'='e','83'='b','84'='a','85'='a','86'='a','87'='a','88'='a','89'='a','90'='a',
            '91'='b','92'='b','93'='b','94'='b','95'='c','96'='c','97'='c','98'='c','99'='d','100'='e')
    
    output$choice001= renderUI({
        tags$iframe(width = "860", height = "700", src=sprintf('%s.png',item[1]))
    })
    output$choice002= renderUI({
        tags$iframe(width = "860", height = "700", src=sprintf('%s.png',item[2]))
    })
    output$choice003= renderUI({
        tags$iframe(width = "860", height = "700", src=sprintf('%s.png',item[3]))
    })
    output$choice004= renderUI({
        tags$iframe(width = "860", height = "700", src=sprintf('%s.png',item[4]))
    })
    output$choice005= renderUI({
        tags$iframe(width = "860", height = "700", src=sprintf('%s.png',item[5]))
    })
    output$choice006= renderUI({
        tags$iframe(width = "860", height = "700", src=sprintf('%s.png',item[6]))
    })
    output$choice007= renderUI({
        tags$iframe(width = "860", height = "700", src=sprintf('%s.png',item[7]))
    })
    output$choice008= renderUI({
        tags$iframe(width = "860", height = "700", src=sprintf('%s.png',item[8]))
    })
    output$choice009= renderUI({
        tags$iframe(width = "860", height = "700", src=sprintf('%s.png',item[9]))
    })
    output$choice010= renderUI({
        tags$iframe(width = "860", height = "700", src=sprintf('%s.png',item[10]))
    })
    
    lst<-list()
    for (i in c(1:10)){
        for (j in c(1:100)){
            if (item[i]==j){
                lst<-c(lst,x[[j]])
            }
        }
    }
    
    
    observeEvent(input$p_do,{
        showModal(modalDialog(
            title = "Text",
            ui<-fluidPage(
                fluidRow(
                    column(
                        width = 6,
                        box(
                            width = 200,
                            title='Correct Answer',
                            verbatimTextOutput('peggy_show01')
                        )
                    ),
                    column(
                        width = 6,
                        box(
                            width = 200,
                            title='Your Answer',
                            verbatimTextOutput("p_text02")
                        )
                    )
                )),
            easyClose = TRUE,
            footer = modalButton('Close')
        ))
    })
    
    output$peggy_show01 = renderPrint({
        for (i in c(1:10)){
            cat(lst[[i]],sep='\n')
        }
    })
    output$p_text02<-renderText({
        c('',input$p_301,'\n',
          input$p_302,'\n',
          input$p_303,'\n',
          input$p_304,'\n',
          input$p_305,'\n',
          input$p_306,'\n',
          input$p_307,'\n',
          input$p_308,'\n',
          input$p_309,'\n',
          input$p_310)
    })
})

