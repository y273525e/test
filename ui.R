library(shiny)
library(shinydashboard)
source("sub_ui.R")
dashboardPage(skin="blue",
              dashboardHeader(
                  title="微積分主題式教學"
              ),
              dashboardSidebar(
                  sidebarMenu(
                    menuItem("主題 : 微積分",tabName = "chapter1",
                             menuItem("講義",tabName = "page1"),
                             menuItem("R程式", tabName="page2"),
                             menuItem("互動式R-Shiny數位教材",tabName = "contact",
                                      menuItem('現金流量',tabName = 'page3'),
                                      menuItem('含上下界積分',tabName = 'page3-1'),
                                      menuItem('微分',tabName = 'page3-2')),
                             menuItem("習題練習", tabName="page4"),
                             menuItem("科普應用", tabName="page5"),
                             menuItem("測驗", tabName="page6"))
                  )
              ),
              dashboardBody(
                  tabItems(
                    tabItem(tabName = "page1", p1_ui),
                    tabItem(tabName = "page2", p2_ui),
                    tabItem(tabName = "page3", p3_ui),
                    tabItem(tabName = 'page3-1',p3_1_ui),
                    tabItem(tabName = 'page3-2',p3_2_ui),
                    tabItem(tabName = "page4", p4_ui),
                    tabItem(tabName = "page5", p5_ui),
                    tabItem(tabName = "page6", p6_ui)
                  )
              )
)
