library(shiny)
require(ggplot2)
require(scales)
require(gridExtra)
require(devtools)
require(mosaic)
source("mgf.R")

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
        tags$iframe(width="100%", height="750", src='example.html',seamless=TRUE)
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
    
    
    #p4
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
            "For the rate of change after 0 weeks:
            H'(0) = 1.1e^(-0.1(0)) = 1.1e^0 = 1.1"
        })})

    observeEvent(input$Aset,{
        output$accelerationFun = renderText({
            "After 12 weeks:
            H'(12) = 1.1e^(-0.1(12)) = 1.1e^(-1.2) = 1.1(0.30) = 0.33"
        })
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
})
