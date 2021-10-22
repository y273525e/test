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
              ),
              dashboardBody(
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
              )
)
