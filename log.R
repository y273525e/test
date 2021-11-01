library(shiny)
require(ggplot2)
require(scales)
require(gridExtra)
require(devtools)
require(mosaic)
require(devtools)
require(shinyforms)
library(rsconnect)
#rsconnect::deployApp('path/to/your/app')
source("mgf.R")
source('sub_ui.R')

shinyServer(function(input, output, session){
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
  output$ex40201 = renderText({'Ace Truck leases its 10-ft box truck at $35/day and $0.45/mi, 
        whereas Acme Truck leases a similar truck at $30/day and $0.50/mi. Find the daily cost 
        of leasing from each company as a function of x number of miles driven.'})
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
  output$ex40202 = renderText({' For the supply equation p=x^3+2x^2+3 where x is the quantity 
    supplied in units of a thousand and p is the unit price in dollars, determine the price at 
    which the supplier will make 3 units of the commodity available in the market.'})
  
  observeEvent(input$ex40202_1set,{
    output$good03=renderText({'p(x=3)=3^3+2×3^2+3=48'
    })
  })
  output$html403 = renderUI({
    tags$iframe(width="100%", height="600", src='example02.html',seamless=TRUE)
  })
  
  #4-3
  output$peggy_question1 = renderText({
    "A certain species of turtle faces extinction because dealers collect truckloads of turtle eggs to be sold as aphrodisiacs.\nAfter severe conservation measures are implemented, it is hoped that the turtle population will grow according to the rule:\nN(t) = 2t^3 + 4t^2 - 5t + 1000   0<t<10, where N(t) denotes the population at the end of year t."})
  output$peggy_que11 = renderText({"(a) Find the rate of growth of the turtle population when t=2."})
  output$peggy_que12 = renderText({
    "(b) Find the rate of growth of the turtle population when t=4."})
  output$peggy_que13 = renderText({
    "(c) What will be the population 10 yr after the conservation measures are implemented?"})
  output$peggy_Fun1 = renderText({"N(t) = 2t^3 + 4t^2 - 5t + 1000"})
  observeEvent(input$peggy_set1,{
    output$peggy_plot1 = renderPlot({
      mgf('2*x^3+4*x^2-5*x+1000', from=0, to=10)
    })})
  observeEvent(input$peggy_set11,{
    output$peggy_Fun11 = renderText({
      "For the rate of growth when t=2:
            N'(2) = 6*2^2 + 8*2 - 5 = 35"
    })
    output$peggy_html11 = renderUI({
      tags$iframe(width="100%", height="360", src='code11.html',seamless=TRUE)
    })
  })
  
  observeEvent(input$peggy_set12,{
    output$peggy_Fun12 = renderText({
      "For the rate of growth when t=4:
            N'(4) = 6*4^2 + 8*4 - 5 = 123"
    })
    output$peggy_html12 = renderUI({
      tags$iframe(width="100%", height="360", src='code12.html',seamless=TRUE)
    })
  })
  
  observeEvent(input$peggy_set13,{
    output$peggy_Fun13 = renderText({
      "For the population 10 yr after the conservation measures:
            N(10) = 2*10^3 + 4*10^2 - 5t + 1000 = 3350"
    })
    output$peggy_html13 = renderUI({
      tags$iframe(width="100%", height="360", src='code13.html',seamless=TRUE)
    })
  })
  
  ### 4-3Q2 ###
  output$peggy_question2 = renderText({
    "During testing of a certain brand of air purifier, the amount of smoke remaining t min after the start of the test was\nA(t) = -0.00006t^5 + 0.00468t^4 - 0.1316t^3 + 1.915t^2 - 17.63t +100 percent of the original amount."})
  output$peggy_que21 = renderText({"(a) Find the percentage per minute when t=3 ( A'(3) )."})
  output$peggy_que22 = renderText({
    "(b) Find the percentage per minute in the second power when t=3 ( A''(3) )."})
  output$peggy_Fun2 = renderText({"A(t) = -0.00006t^5 + 0.00468t^4 - 0.1316t^3 + 1.915t^2 - 17.63t +100"})
  observeEvent(input$peggy_set2,{
    output$peggy_plot2 = renderPlot({
      mgf('-0.00006*x^5+0.00468*x^4-0.1316*x^3+1.915*x^2-17.63*x+100', from=0, to=10)
    })
  })
  observeEvent(input$peggy_set21,{
    output$peggy_Fun21 = renderText({
      "For the percentage per minute when t=3:
            A'(3) = -0.003*3^4 + 0.01872*3^3 - 0.2632*3^2 + 3.83*3 -17.63 = -9.212"
    })
    output$peggy_html21 = renderUI({
      tags$iframe(width="100%", height="360", src='code21.html',seamless=TRUE)
    })
  })
  
  observeEvent(input$peggy_set22,{
    output$peggy_Fun22 = renderText({
      "For the percentage per minute in the second power when t=3:
            A''(3) = -0.012*3^3 + 0.3744*3^2 - 0.5264*3 + 3.83 = 1.934"
    })
    output$peggy_html22 = renderUI({
      tags$iframe(width="100%", height="360", src='code22.html',seamless=TRUE)
    })
  })
  
  #p4-4
  #改成215 APPLIED EXAMPLE 4
  output$example1_question = renderText({"After t weeks of practice a pole vaulter can vault feet.Find the rate of change of the athlete's jumps after"})
  output$velocity = renderText({"(a) 0 week (at the beginning of training)"})
  output$acceleration = renderText({"(b) 12 weeks"})
  output$QFun = renderText({"H(x) = 15-11e^(-0.1x)"})
  observeEvent(input$Bset,{
    output$Qplot = renderPlot({
      mgf('15-(11*2.72^(-0.1*x))', from=0, to=20)
    })})
  observeEvent(input$Vset,{
    output$velocityFun = renderText({
      "H'(x)=1.1e^(-0.1(x))\n=>For the rate of change after 0 weeks:\nH'(0) = 1.1e^(-0.1(0)) = 1.1e^0 = 1.1"
    })})
  
  observeEvent(input$Aset,{
    output$accelerationFun = renderText({
      "H'(x)=1.1e^(-0.1(x))\n=>After 12 weeks:\nH'(12) = 1.1e^(-0.1(12)) = 1.1e^(-1.2) = 1.1(0.30) = 0.33"
    })
  })
  output$html4 = renderUI({
    tags$iframe(width="100%", height="600", src='08151.html',seamless=TRUE)
  })
  
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
    output$quiz1ch5 = renderText({
      "The answer is :
           3*X^2/2-2X + C
          =3X^2/2-2X + C"
    })})
  
  observeEvent(input$setn2,{
    output$quiz2ch5 = renderText({
      "The answer is :
         ∫1/xdx = ln x + c =
            ln x + C"
    })})
  
  observeEvent(input$setn3,{
    output$quiz3ch5 = renderText({
      "The answer is:
        ∫kdx = kx+c =
        ∫(e+1)dx = (e+1)x + C"
    })
  })
  
  #p4-6
  output$ddd1_question = renderText({"Evaluate x e^xdx"})
  output$ddd2_question = renderText({"Evaluate xlnx dx"})
  observeEvent(input$dddd,{
    output$daplot = renderUI({
      tags$iframe(width = "500", height = "450", src='ex6-1-1.png')
    })})
  observeEvent(input$ddd01,{
    output$dsol1plot = renderUI({
      tags$iframe(width = "400", height = "150", src='ex6-1-a.png')
    })})
  output$htmld2 = renderUI({
    tags$iframe(width="100%", height="600", src='fabian6html01.html',seamless=TRUE)
  })
  observeEvent(input$ddd02,{
    output$ddd02plot = renderUI({
      tags$iframe(width = "400", height = "150", src='ex6-1-a.png')
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
  output$example10_question = renderText({"ANSWER THE QUESTION
 "})
  output$Q001 = renderText({"Sum the first 4 terms of 10, 30, 90, 270, 810, 2430, ..
The values of a, r and n are:

a = 10 (the first term)
r = 3 (the common ratio)
n = 4 (we want to sum the first 4 terms)
 "})
  output$Q002 = renderText({"Sam deposits $50 on the first of each month into an account which 
    arns 0.5% interest each month. To the nearest dollar, how much is in the account right after 
    Sam makes his last deposit on the first day of the fifth year (the 49th month).

The deposits that Sam make and the interest earned on each deposit generate a geometric series
"})
  observeEvent(input$V001,{
    output$A001 = renderText({
      "(4-1)∑(k=0) 10(3)^k=400"
    })})
  observeEvent(input$V002,{
    output$A002 = renderText({
      "Evaluate 8∑(n=3)2(-3)^(n-1)"
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
  output$choice201= renderUI({
    tags$iframe(width = "630", height = "360", src='choice201.png')
  })
  
  output$choice202= renderUI({
    tags$iframe(width = "630", height = "420", src='choice202.png')
  })
  
  output$choice203= renderUI({
    tags$iframe(width = "630", height = "210", src='choice203.png')
  })
  
  output$dddd301= renderUI({
    tags$iframe(width = "550", height = "250", src='dd1.png')
  })
  
  output$dddd302= renderUI({
    tags$iframe(width = "550", height = "250", src='dd2.png')
  })
  
  output$dddd303= renderUI({
    tags$iframe(width = "550", height = "250", src='dd3.png')
  })
  
  output$dddd304= renderUI({
    tags$iframe(width = "550", height = "250", src='dd4.png')
  })
  
  output$J401= renderUI({
    tags$iframe(width = "550", height = "250", src='J401.png')
  })
  
  output$J402= renderUI({
    tags$iframe(width = "550", height = "250", src='J402.png')
  })
  
  output$J403= renderUI({
    tags$iframe(width = "550", height = "250", src='J403.png')
  })
  
  output$J404= renderUI({
    tags$iframe(width = "550", height = "250", src='J404.png')
  })
  
  output$choice301= renderUI({
    tags$iframe(width = "450", height = "450", src='choice301.png')
  })
  
  output$choice302= renderUI({
    tags$iframe(width = "450", height = "620", src='choice302.png')
  })
  
  output$choice303= renderUI({
    tags$iframe(width = "100%", height = "500", src='choice303.png')
  })
  
  output$choice304= renderUI({
    tags$iframe(width = "650", height = "270", src='choice304.png')
  })
  
  output$choice101= renderUI({
    tags$iframe(width = "750", height = "550", src='choice101.png')
  })
  
  output$choice102= renderUI({
    tags$iframe(width = "750", height = "550", src='choice102.png')
  })
  output$choice103= renderUI({
    tags$iframe(width = "750", height = "600", src='choice103.png')
  })
  output$choice104= renderUI({
    tags$iframe(width = "750", height = "600", src='choice104.png')
  })
  
  output$choice501= renderUI({
    tags$iframe(width = "450", height = "650", src='choice501.png')
  })
  
  output$choice502= renderUI({
    tags$iframe(width = "670", height = "550", src='choice502.png')
  })
  
  output$choice503= renderUI({
    tags$iframe(width = "390", height = "550", src='choice503.png')
  })
  
  output$choice801= renderUI({
    tags$iframe(width = "450", height = "300", src='choice801.png')
  })
  
  output$choice802= renderUI({
    tags$iframe(width = "450", height = "300", src='choice802.png')
  })
  output$choice803= renderUI({
    tags$iframe(width = "450", height = "300", src='choice803.png')
  })
  output$choice804= renderUI({
    tags$iframe(width = "450", height = "300", src='choice804.png')
  })
  
  p_f201=function(q201){
    if(q201=='a'){
      return('Correct')
    }
    else{
      return('Wrong')
    }
  }
  
  p_f202=function(q202){
    if(q202=='a'){
      return('Correct')
    }
    else{
      return('Wrong')
    }
    
  }
  
  p_f203=function(q203){
    if(q203=='c'){
      return('Correct')
    }
    else{
      return('Wrong')
    }
    
  }
  
  d_f301=function(q601){
    if(q601=='d'){
      return('Correct')
    }
    else{
      return('Wrong')
    }
  }
  
  d_f302=function(q602){
    if(q602=='b'){
      return('Correct')
    }
    else{
      return('Wrong')
    }
    
  }
  d_f303=function(q603){
    if(q603=='e'){
      return('Correct')
    }
    else{
      return('Wrong')
    }
  }
  d_f304=function(q604){
    if(q604=='c'){
      return('Correct')
    }
    else{
      return('Wrong')
    }
  }
  d_f401=function(q401){
    if(q401=='b'){
      return('Correct')
    }
    else{
      return('Wrong')
    }
  }
  
  d_f402=function(q402){
    if(q402=='b'){
      return('Correct')
    }
    else{
      return('Wrong')
    }
    
  }
  d_f403=function(q403){
    if(q403=='c'){
      return('Correct')
    }
    else{
      return('Wrong')
    }
  }
  d_f404=function(q404){
    if(q404=='e'){
      return('Correct')
    }
    else{
      return('Wrong')
    }
  }
  p_f301=function(q301){
    if(q301=='c'){
      return('Correct')
    }
    else{
      return('Wrong')
    }
  }
  
  p_f302=function(q302){
    if(q302=='a'){
      return('Correct')
    }
    else{
      return('Wrong')
    }
  }
  
  p_f303=function(q303){
    if(q303=='e'){
      return('Correct')
    }
    else{
      return('Wrong')
    }
  }
  
  p_f304=function(q304){
    if(q304=='a'){
      return('Correct')
    }
    else{
      return('Wrong')
    }
  }
  
  p_f101=function(q101){
    if(q101=="c"){
      return('Correct')
    }
    else{
      return('Wrong')
    }
  }
  
  p_f102=function(q102){
    if(q102=="c"){
      return('Correct')
    }
    else{
      return('Wrong')
    }
    
  }
  p_f103=function(q103){
    if(q103=="e"){
      return('Correct')
    }
    else{
      return('Wrong')
    }
    
  }
  p_f104=function(q104){
    if(q104=="c"){
      return('Correct')
    }
    else{
      return('Wrong')
    }
    
  }
  
  p_f501=function(q501){
    if(q501=='b'){
      return('Correct')
    }
    else{
      return('Wrong')
    }
  }
  
  p_f502=function(q502){
    if(q502=='a'){
      return('Correct')
    }
    else{
      return('Wrong')
    }
    
  }
  
  p_f503=function(q503){
    if(q502=='c'){
      return('Correct')
    }
    else{
      return('Wrong')
    }
    
  }
  
  p_f801=function(q801){
    if(q801=='a'){
      return('Correct')
    }
    else{
      return('Wrong')
    }
  }
  
  p_f802=function(q802){
    if(q802=='d'){
      return('Correct')
    }
    else{
      return('Wrong')
    }
    
  }
  p_f803=function(q803){
    if(q803=='a'){
      return('Correct')
    }
    else{
      return('Wrong')
    }
    
  }
  p_f804=function(q804){
    if(q804=='c'){
      return('Correct')
    }
    else{
      return('Wrong')
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
  
  output$peggy_show01 = renderText({
    '1  a\n2  a\n3  c\n4  d\n5  b\n6  e\n7  c\n8  b\n9  b\n10  c\n11  e\n12  c\n13  a\n14  e\n15  a\n16  c\n17  c\n18  e\n19  c\n20  b\n21  a\n22  c\n23  a\n24  d\n25  a\n26  c'
  })
  output$p_text02<-renderText({
    c('',input$p_201,'\n',
      input$p_202,'\n',
      input$p_203,'\n',
      input$d_301,'\n',
      input$d_302,'\n',
      input$d_303,'\n',
      input$d_304,'\n',
      input$d_401,'\n',
      input$d_402,'\n',
      input$d_403,'\n',
      input$d_404,'\n',
      input$p_301,'\n',
      input$p_302,'\n',
      input$p_303,'\n',
      input$p_304,'\n',
      input$p_101,'\n',
      input$p_102,'\n',
      input$p_103,'\n',
      input$p_104,'\n',
      input$p_501,'\n',
      input$p_502,'\n',
      input$p_503,'\n',
      input$p_801,'\n',
      input$p_802,'\n',
      input$p_803,'\n',
      input$p_804)
  })
  
  
  
})


