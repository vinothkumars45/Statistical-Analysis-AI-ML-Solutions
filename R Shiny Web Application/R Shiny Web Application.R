library(shiny)
library(openxlsx)
library(dplyr)
library(DT)
library(shinythemes)
#library(tidyverse)
library(tidyr)
library(forcats)
library(scales)
library(numbers)
library(gganimate)
#library(gifski)
library(png)
library(shinydashboard)
library(shinyWidgets)
library(plotly)

#setwd("C:/Users/vinotsek/Docu/Scorecard Insight_Automation/Attach Rate")
load("./insights.Rdata", envir = .GlobalEnv)



ui<-navbarPage("Attach Rate",
              tabPanel("Theater",dashboardPage(
              dashboardHeader(title = "Metrics by Theatre"),
              dashboardSidebar(
                sidebarMenu(
                  menuItem("Summary",tabName="summary",icon=icon("dashboard"),selected = TRUE),
                  menuItem("Accounts View",tabName="accounts_view",icon=icon("user-alt")),
                  menuItem("Tabular",tabName="tabular",icon=icon("th")),
                  menuItem("Predictive",tabName="predictive",icon=icon("forward")),
                  menuItem("Comparitive",tabName="comparitive",icon=icon("balance-scale")),
                  menuItem("Dollar Per Unit",tabName="dollar_per_unit",icon=icon("dollar-sign")),
                  selectInput('in_reg_theater',"Choose Region/Theater:",choices = CX.L1.L2.UNQ,selected = "Americas")
                )
              ),dashboardBody(
                tabItems(
                  tabItem(tabName = "summary",br(),
                          fluidRow(
                            valueBoxOutput('out_ar_box_1',width = 3),
                            valueBoxOutput('out_unit_box',width = 3),
                            valueBoxOutput('out_list_box',width = 3),
                            valueBoxOutput('out_dol_p_unit_box',width = 3)
                          ),
                          fluidRow(
                            valueBoxOutput('out_ar_yoy_box',width = 3),
                            valueBoxOutput('out_unit_yoy_box',width = 3),
                            valueBoxOutput('out_list_yoy_box',width = 3),
                            valueBoxOutput('out_dol_p_unit_yoy_box',width = 3)
                          ),br(),br(),
                          fluidRow(column(6,br(),htmlOutput('out_AR_Qtr_Target_1'),
                                          htmlOutput('out_AR_Qtr_YoY_1'),
                                          htmlOutput('out_AR_Qtr_YoY_2.1'),
                                          htmlOutput('out_AR_Qtr_YoY_2.2')
                          ),
                          column(6,br(),htmlOutput('out_Units_Shipped'),
                                 htmlOutput('out_Units_Attached'),
                                 htmlOutput('out_List_Price'),
                                 htmlOutput('out_Dollar_Per_Unit')
                          )
                          ),
                          br(),br(),fluidRow(box(title = "Summary Table",status="success",solidHeader = TRUE,
                                                 collapsible = TRUE,width=12,collapsed = TRUE,DT::DTOutput('out_summary_table'))),br(),
                          fluidRow(box(title = "Attach Rate Trends",status="primary",solidHeader = TRUE,
                                       collapsible = TRUE,width=12,collapsed = TRUE,
                                       fluidRow(
                                         column(6,br(),plotOutput('out_hist_quart_ar')),
                                         column(6,
                                                column(4,selectInput('in_mth',"Month:",choices = MTH.UNQ,selected = MTH.UNQ[2])),
                                                column(4,selectInput('in_stack',"Split by",choices = c("Overall","Sales Level","Warranty","Product Band"))),
                                                column(4,selectInput('in_line',"Trend by",choices = c("Overall","Sales Level","Warranty","Product Band"))),
                                                br(),
                                                column(12,plotOutput('out_ar_trend_stack'))
                                         )
                                       )
                          ))
                  ),
                  tabItem(tabName = "accounts_view",br(),
                          fluidRow(
                            valueBoxOutput('out_target_box',width = 3),
                            infoBoxOutput('out_to_go_box',width = 3)
                          ),
                          # box(title = "Waterfall View",status="success",solidHeader = TRUE,
                          #     collapsible = TRUE,width=12,
                          #     #br(),
                          #     plotOutput('out_waterfall'),br(),
                          #     textOutput('out_waterfall_text'),br(),
                          #     htmlOutput('out_cust_distn')
                          # ),
                          br(),br(),
                          box(title = paste("Waterfall View"),
                              status="primary",solidHeader = TRUE,width=12,
                              #h3(paste("Waterfall View - as of",min(sfc_data$Date.Pulled))),
                              fluidRow(
                                column(3,sliderInput(inputId = "in_non_sfc_pct",label = "% conversion unattached",
                                                     min = 0,max = 100,value = 50)),
                                column(3,selectInput('in_ar_qtr',"Attach (+1) QTR:",choices = AR.QTR.UNQ,selected = "Q3FY22"))
                              ),
                              br(),plotOutput('out_waterfall'),br(),
                              textOutput('out_waterfall_text'),br(),
                              htmlOutput('out_cust_distn')
                          ),
                          br(),DT::DTOutput('out_waterfall_sfc_table'),br()
                  ),
                  tabItem(tabName = "tabular",br(),
                          #h3(htmlOutput("out_metric_head")),
                          fluidRow(
                            #column(6,br(),DT::DTOutput('out_top_n_pf'))#,
                            column(6,br(),htmlOutput('out_band_text_high'),
                                   htmlOutput('out_band_text_med'),
                                   htmlOutput('out_band_text_overall')
                            ),
                            column(6,br(),htmlOutput('out_pf_distn')
                            )
                          ),
                          br(),br(),
                          selectInput('in_top_attach',"AR by Top:",choices = c("Attached Quantity","Total Shipments","Default List Price","Open Opportunity"),selected = "Attached Quantity"),
                          #h3("Top Attached Items"),
                          fluidRow(
                            box(title = "Account",status="success",solidHeader = TRUE,width = 6,
                                DT::DTOutput('out_top_acct_attached')),
                            box(title = "Partner",status="primary",solidHeader = TRUE,width = 6,
                                DT::DTOutput('out_top_part_attached'))
                          ),
                          fluidRow(
                            box(title = "Product Family",status="info",solidHeader = TRUE,width = 6,
                                DT::DTOutput('out_top_pf_attached')),
                            box(title = "Business Entity",status="warning",solidHeader = TRUE,width = 6,
                                DT::DTOutput('out_top_BE_attached'))
                          )
                  ),
                  tabItem(tabName = "predictive",br(),
                          valueBoxOutput('out_ar_box_2'),br(),
                          sliderInput(inputId = "in_cust_attach",label = "Attach Threshold - Customer",
                                      min = 0,max = 100,value = 30),
                          fluidRow(
                            box(title = "High vs Low Attach - By Customer",status="success",solidHeader = TRUE,
                                collapsible = TRUE,collapsed = TRUE,
                                DT::DTOutput ('out_cust_high_low')),
                            box(title = "Actual vs Predicted - Attach Rate",status="info",solidHeader = TRUE,
                                collapsible = TRUE,collapsed = TRUE,
                                DT::DTOutput ('out_act_pred_ar'))
                          ),
                          fluidRow(
                            box(title = "Actual vs Predicted - Contribution to AR",status="warning",solidHeader = TRUE,
                                collapsible = TRUE,
                                DT::DTOutput ('out_act_pred_ar_contr')),
                            box(title = "Actual vs Predicted - Potential AR (On 100% conversion)",status="primary",solidHeader = TRUE,
                                collapsible = TRUE,
                                DT::DTOutput ('out_act_pred_ptnl_impact'))
                          ),
                          br(),
                          fluidRow(
                            column(4),
                            column(8,h3("High Attach Probability Customers with Low Current AR %"))
                          ),
                          fluidRow(
                            column(2,br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                                   selectInput('in_segment',"Segment",choices = segment.UNQ),
                                   selectInput('in_band',"Product Band",choices = band.UNQ),
                                   selectInput('in_BE',"Business Entity",choices = BE.UNQ.1)
                            ),
                            column(10,DT::DTOutput ('out_prob_to_attach'))
                          )
                  ),
                  tabItem(tabName = "comparitive",br(),
                          fluidRow(
                            column(2,br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                                   selectInput('in_comp_reg_theater',"Region/Theater to Compare",choices = CX.L1.L2.UNQ, selected = CX.L1.L2.UNQ[2])
                            ),
                            column(10,br(),plotOutput('out_comp_theater_reg'))
                          )
                  ),
                  tabItem(tabName = "dollar_per_unit",br(),
                          fluidRow(
                            column(8,br(),plotOutput('out_dollar_per_unit_trend'))
                          ),
                          br(),DT::DTOutput('out_dollar_per_unit_table'),
                          br()#,imageOutput("out_dollar.p.unit_AR_scatter")
                  )
                )
              ))),
              tabPanel("Business Entity",
                       dashboardPage(
                         dashboardHeader(title = "Metrics by Business Entity"),
                         dashboardSidebar(
                           sidebarMenu(
                             menuItem("Summary",tabName="summary1",icon=icon("dashboard"),selected = TRUE),
                             menuItem("Accounts View",tabName="accounts_view1",icon=icon("user-alt")),
                             menuItem("Tabular",tabName="tabular1",icon=icon("th")),
                             menuItem("Predictive",tabName="predictive1",icon=icon("forward")),
                             menuItem("Comparitive",tabName="comparitive1",icon=icon("balance-scale")),
                             menuItem("Dollar Per Unit",tabName="dollar_per_unit1",icon=icon("dollar-sign")),
                             br(),
                             selectInput('in_reg_BE',"Choose BE:",choices = BE.UNQ,selected = "Collaboration")
                           )
                         ),dashboardBody(
                           tabItems(
                             tabItem(tabName = "summary1",br(),
                                     fluidRow(
                                       valueBoxOutput('out_ar_box_B',width = 3),
                                       valueBoxOutput('out_unit_box_B',width = 3),
                                       valueBoxOutput('out_list_box_B',width = 3),
                                       valueBoxOutput('out_dol_p_unit_box_B',width = 3)
                                     ),
                                     fluidRow(
                                       valueBoxOutput('out_ar_yoy_box_B',width = 3),
                                       valueBoxOutput('out_unit_yoy_box_B',width = 3),
                                       valueBoxOutput('out_list_yoy_box_B',width = 3),
                                       valueBoxOutput('out_dol_p_unit_yoy_box_B',width = 3)
                                     ),
                                     br(),br(),
                                     fluidRow(column(6,br(),htmlOutput('out_AR_Qtr_Target_1_B'),
                                                     htmlOutput('out_AR_Qtr_YoY_1_B'),
                                                     htmlOutput('out_AR_Qtr_YoY_2.1_B'),
                                                     htmlOutput('out_AR_Qtr_YoY_2.2_B')
                                     ),
                                     column(6,br(),htmlOutput('out_Units_Shipped_B'),
                                            htmlOutput('out_Units_Attached_B'),
                                            htmlOutput('out_List_Price_B'),
                                            htmlOutput('out_Dollar_Per_Unit_B')
                                     )
                                     ),
                                     br(),br(),
                                     br(),br(),fluidRow(box(title = "Summary Table",status="success",solidHeader = TRUE,
                                                            collapsible = TRUE,width=12,collapsed = TRUE,DT::DTOutput('out_summary_table_B'))),br(),
                                     fluidRow(box(title = "Attach Rate Trends",status="primary",solidHeader = TRUE,
                                                  collapsible = TRUE,width=12,collapsed = TRUE,
                                                  fluidRow(
                                                    column(6,br(),plotOutput('out_hist_quart_ar_B')),
                                                    column(6,
                                                           column(4,selectInput('in_mth_B',"Month:",choices = MTH.UNQ,selected = MTH.UNQ[2])),
                                                           column(4,selectInput('in_stack_B',"Split by",choices = c("Overall","Sales Level","Warranty","Product Band"))),
                                                           column(4,selectInput('in_line_B',"Trend by",choices = c("Overall","Sales Level","Warranty","Product Band"))),
                                                           br(),
                                                           column(12,plotOutput('out_ar_trend_stack_B'))
                                                    )
                                                  )
                                     ))
                             ),
                             tabItem(tabName = "tabular1",br(),
                                     #h3(htmlOutput("out_metric_head")),
                                     fluidRow(
                                       #column(6,br(),DT::DTOutput('out_top_n_pf'))#,
                                       column(6,br(),htmlOutput('out_band_text_high_B'),
                                              htmlOutput('out_band_text_med_B'),
                                              htmlOutput('out_band_text_overall_B')
                                       ),
                                       column(6,br(),htmlOutput('out_pf_distn_B')
                                       )
                                     ),
                                     br(),br(),
                                     selectInput('in_top_attach_B',"AR by Top:",choices = c("Attached Quantity","Total Shipments","Default List Price","Open Opportunity"),selected = "Attached Quantity"),
                                     #h3("Top Attached Items"),
                                     fluidRow(
                                       box(title = "Account",status="success",solidHeader = TRUE,width = 6,
                                           DT::DTOutput('out_top_acct_attached_B')),
                                       box(title = "Partner",status="primary",solidHeader = TRUE,width = 6,
                                           DT::DTOutput('out_top_part_attached_B'))
                                     ),
                                     fluidRow(
                                       box(title = "Product Family",status="info",solidHeader = TRUE,width = 6,
                                           DT::DTOutput('out_top_pf_attached_B')),
                                       box(title = "Region",status="warning",solidHeader = TRUE,width = 6,
                                           DT::DTOutput('out_top_BE_attached_B'))
                                     )
                             ),
                            tabItem(tabName = "comparitive1",br(),
                                                                 fluidRow(
                                                                   column(2,br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                                                                          selectInput('in_comp_reg_BE',"BE to Compare",choices = BE.UNQ, selected = BE.UNQ[2])
                                                                   ),
                                                                   column(10,br(),plotOutput('out_comp_reg_B'))
                                                                 )
                                                       ),tabItem(tabName = "accounts_view1",br(),
                                                                 fluidRow(
                                                                   valueBoxOutput('out_target_box_B',width = 3),
                                                                   infoBoxOutput('out_to_go_box_B',width = 3)
                                                                 ),
                                                                 # box(title = "Waterfall View",status="success",solidHeader = TRUE,
                                                                 #     collapsible = TRUE,width=12,
                                                                 #     #br(),
                                                                 #     plotOutput('out_waterfall_B'),br(),
                                                                 #     textOutput('out_waterfall_text_B'),br(),
                                                                 #     htmlOutput('out_cust_distn_B')
                                                                 # ),
                                                                 br(),br(),
                                                                 box(title = paste("Waterfall View"),
                                                                     status="primary",solidHeader = TRUE,width=12,
                                                                     #h3(paste("Waterfall View - as of",min(sfc_data$Date.Pulled))),
                                                                     fluidRow(
                                                                       column(3,sliderInput(inputId = "in_non_sfc_pct_B",label = "% conversion unattached",
                                                                                            min = 0,max = 100,value = 50)),
                                                                       column(3,selectInput('in_ar_qtr_B',"Attach (+1) QTR:",choices = AR.QTR.UNQ,selected = "Q3FY22"))
                                                                     ),
                                                                     br(),plotOutput('out_waterfall_B'),br(),
                                                                     textOutput('out_waterfall_text_B'),br(),
                                                                     htmlOutput('out_cust_distn_B')
                                                                 ),
                                                                 br(),DT::DTOutput('out_waterfall_sfc_table_B'),br()
                                                       ),
                            tabItem(tabName = "dollar_per_unit1",br(),
                                    fluidRow(
                                      column(8,br(),plotOutput('out_dollar_per_unit_trend_B'))
                                    ),
                                    br(),DT::DTOutput('out_dollar_per_unit_table_B'),
                                    br()#,imageOutput("out_dollar.p.unit_AR_scatter")
                            ),tabItem(tabName = "predictive1",br(),
                                      valueBoxOutput('out_ar_box_2_B'),br(),
                                      sliderInput(inputId = "in_cust_attach_B",label = "Attach Threshold - Customer",
                                                  min = 0,max = 100,value = 30),
                                      fluidRow(
                                        box(title = "High vs Low Attach - By Customer For BE",status="success",solidHeader = TRUE,
                                            collapsible = TRUE,collapsed = TRUE,
                                            DT::DTOutput ('out_cust_high_low_B')),
                                        box(title = "Actual vs Predicted - Attach Rate For BE",status="info",solidHeader = TRUE,
                                            collapsible = TRUE,collapsed = TRUE,
                                            DT::DTOutput ('out_act_pred_ar_B'))
                                      ),
                                      fluidRow(
                                        box(title = "Actual vs Predicted - Contribution to AR For BE",status="warning",solidHeader = TRUE,
                                            collapsible = TRUE,
                                            DT::DTOutput ('out_act_pred_ar_contr_B')),
                                        box(title = "Actual vs Predicted - Potential AR (On 100% conversion) For BE",status="primary",solidHeader = TRUE,
                                            collapsible = TRUE,
                                            DT::DTOutput ('out_act_pred_ptnl_impact_B'))
                                      ),
                                      br(),
                                      fluidRow(
                                        column(4),
                                        column(8,h3("High Attach Probability Customers with Low Current AR % For BE"))
                                      ),
                                      fluidRow(
                                        column(2,br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                                               selectInput('in_segment_B',"Segment",choices = segment.UNQ),
                                               selectInput('in_band_B',"Product Band",choices = band.UNQ),
                                               selectInput('in_theatre_B',"Theatre",choices = CX.L1.L2.UNQ.2)
                                        ),
                                        column(10,DT::DTOutput ('out_prob_to_attach_B'))
                                      )
                            )
                                                     )))))
               
               
server <- function(input, output){
  
  #---------- This section could also be outside the server----------------------------------
  #------------------------------------------------------------------------------------------
  summ_table <- summary_table_fn("APJC")
  
  min_year<-substr(unique(summ_table$min_mth),1,4)
  max_year<-substr(unique(summ_table$max_mth),1,4)
  qtr<-case_when(as.numeric(substr(unique(summ_table$max_mth),5,6))<=3 ~ 'Q1',
                 as.numeric(substr(unique(summ_table$max_mth),5,6))<=6 ~ 'Q2',
                 as.numeric(substr(unique(summ_table$max_mth),5,6))<=9 ~ 'Q3',
                 TRUE ~ 'Q4'
  )
  mth<-ifelse(mod(as.numeric(substr(unique(summ_table$max_mth),5,6)),3),mod(as.numeric(substr(unique(summ_table$max_mth),5,6)),3),3)
  
  #------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------
  
  #Note:another box with same function used below
  
  #BE Code Summary
  
  output$out_ar_box_B <- renderValueBox({valueBox(scales::percent(info_view_fn_B(input$in_reg_BE)$AR,accuracy = 0.1),
                                                  "Attach Rate %",icon=icon("paperclip"),color="purple"
  )
  })
  output$out_unit_box_B <- renderValueBox({valueBox(scales::number(info_view_fn_B(input$in_reg_BE)$Tot.Qty,big.mark = ","),
                                                    "Units Shipped",icon=icon("suitcase"),color="purple"
  )
  })
  output$out_list_box_B <- renderValueBox({valueBox(scales::dollar(info_view_fn_B(input$in_reg_BE)$Attached.List/1000,big.mark = ",",suffix="k",accuracy = 1L),
                                                    "Attached List Price",icon=icon("money-bill-alt"),color="purple"
  )
  })
  output$out_dol_p_unit_box_B <- renderValueBox({valueBox(scales::dollar(info_view_fn_B(input$in_reg_BE)$dollar.per.unit,big.mark = ",",accuracy=0.1),
                                                          "Dollar per Unit",icon=icon("dollar-sign"),color="purple"
  )
  })
  output$out_ar_yoy_box_B <- renderValueBox({valueBox(scales::percent(info_view_fn_B(input$in_reg_BE)$AR.YoY,accuracy = 0.1),
                                                      "Attach Rate YoY",icon=icon(ifelse(info_view_fn_B(input$in_reg_BE)$AR.YoY>=0,"thumbs-up","thumbs-down")),color=ifelse(info_view_fn_B(input$in_reg_BE)$AR.YoY>=0,"green","red")
  )
  })
  output$out_unit_yoy_box_B <- renderValueBox({valueBox(scales::percent(info_view_fn_B(input$in_reg_BE)$Tot.Qty.YoY,accuracy = 0.1),
                                                        "Units Shipped YoY",icon=icon(ifelse(info_view_fn_B(input$in_reg_BE)$Tot.Qty.YoY>=0,"thumbs-up","thumbs-down")),color=ifelse(info_view_fn_B(input$in_reg_BE)$Tot.Qty.YoY>=0,"green","red")
  )
  })
  output$out_list_yoy_box_B <- renderValueBox({valueBox(scales::percent(info_view_fn_B(input$in_reg_BE)$Attached.List.YoY,accuracy = 0.1),
                                                        "Attached List YoY",icon=icon(ifelse(info_view_fn_B(input$in_reg_BE)$Attached.List.YoY>=0,"thumbs-up","thumbs-down")),color=ifelse(info_view_fn_B(input$in_reg_BE)$Attached.List.YoY>=0,"green","red")
  )
  })
  output$out_dol_p_unit_yoy_box_B <- renderValueBox({valueBox(scales::percent(info_view_fn_B(input$in_reg_BE)$dollar.per.unit.YoY,accuracy = 0.1),
                                                              "Dollar per Unit YoY",icon=icon(ifelse(info_view_fn_B(input$in_reg_BE)$dollar.per.unit.YoY>=0,"thumbs-up","thumbs-down")),color=ifelse(info_view_fn_B(input$in_reg_BE)$dollar.per.unit.YoY>=0,"green","red")
  )
  })
  
  
  output$'out_AR_Qtr_Target_1_B'<- renderText({paste("Attach Rate for",input$in_reg_BE,"is",scales::percent(abs(info_view_fn_B(input$in_reg_BE)$AR),accuracy=0.1),",",ifelse(info_view_fn_B(input$in_reg_BE)$target.to.go=="Met Target","met",paste(round(abs(info_view_fn_B(input$in_reg_BE)$target-info_view_fn_B(input$in_reg_BE)$AR)*100,1),"%"," ","lesser than",sep = "")),"quarter target of",scales::percent(info_view_fn_B(input$in_reg_BE)$target,accuracy=0.1),sep = " ")})
  output$'out_AR_Qtr_YoY_1_B'<-renderText({paste("The YoY",ifelse(info_view_fn_B(input$in_reg_BE)$AR.YoY>=0,"growth","decline"),"of Attach Rate is",scales::percent(abs(info_view_fn_B(input$in_reg_BE)$AR.YoY),accuracy=0.1))})
  output$'out_AR_Qtr_YoY_2.1_B'<-renderText({paste("Sub Business Entity which show max YoY in Attach Rate is",ar_summ_Overall_B(input$in_reg_BE)$Sub.Business.Entity.Name[1],sep = "   ")})
  output$'out_AR_Qtr_YoY_2.2_B'<-renderText({paste("Sub Business Entity which show min YoY in Attach Rate is",rev(ar_summ_Overall_B(input$in_reg_BE)$Sub.Business.Entity.Name)[1],sep = "   ")})
  output$'out_Units_Attached_B'<-renderText({paste("The Units Attached",ifelse(info_view_fn_B(input$in_reg_BE)$Attached.Unit.YoY>=0,"increased","decreased"),"by",scales::percent(abs(info_view_fn_B(input$in_reg_BE)$Attached.Unit.YoY),accuracy=0.1),"YoY")})
  output$'out_Units_Shipped_B'<-renderText({paste("The Units Shipped",ifelse(info_view_fn_B(input$in_reg_BE)$Tot.Qty.YoY>=0,"increased","decreased"),"by",scales::percent(abs(info_view_fn_B(input$in_reg_BE)$Tot.Qty.YoY),accuracy=0.1),"YoY")})
  output$'out_List_Price_B'<-renderText({paste("The YoY Attached List price for",input$in_reg_BE,"has",ifelse(info_view_fn_B(input$in_reg_BE)$Attached.List.YoY>=0,"increased","decreased"),"by",scales::percent(abs(info_view_fn_B(input$in_reg_BE)$Attached.List.YoY),accuracy=0.1))})
  output$'out_Dollar_Per_Unit_B'<-renderText({paste("The YoY Dollar per Unit for",input$in_reg_BE,"has",ifelse(info_view_fn_B(input$in_reg_BE)$dollar.per.unit.YoY>=0,"increased","decreased"),"by",scales::percent(abs(info_view_fn_B(input$in_reg_BE)$dollar.per.unit.YoY),accuracy=0.1))})
  
  output$out_summary_table_B <- renderDT({datatable(summary_table_fn_B(input$in_reg_BE)[,c("L2_L3","AR_pre_year","AR_curr_year","AR_YoY","Att_pre_year","Tot_pre_year","Att_curr_year","Tot_curr_year","Att_YoY","Tot_YoY","Att_list_pre_year","Att_list_curr_year","Att_list_YoY")],
                                                    colnames = c("Sub BE",paste("AR % -",min_year,qtr,paste("M",mth,sep = "")),paste("AR % -",max_year,qtr,paste("M",mth,sep = "")),"AR % YoY",
                                                                 paste("Units Attached -",min_year,qtr,paste("M",mth,sep = "")),paste("Units Total -",min_year,qtr,paste("M",mth,sep = "")),paste("Units Attached -",max_year,qtr,paste("M",mth,sep = "")),paste("Units Total -",max_year,qtr,paste("M",mth,sep = "")),"Attached Units - YoY","Total Units - YoY",
                                                                 paste("Attached List -",min_year,qtr,paste("M",mth,sep = "")),paste("Attached List -",max_year,qtr,paste("M",mth,sep = "")),"Attached List YoY"
                                                    ),options = list(scrollX = TRUE)) %>%
      formatPercentage(c(2,3,4,9,10,13),1) %>%
      formatCurrency(c(5,6,7,8),currency = "", interval = 3,digits = 0) %>%
      formatCurrency(c(11,12),currency = "$", interval = 3,digits = 0) %>%
      formatStyle(c(4,9,10,13),backgroundColor = styleInterval(c(-.05,.05),c('lightpink','lightgray','lightgreen')))
  })
  
  output$out_hist_quart_ar_B <- renderPlot({hist_quart_ar_fn_B(input$in_reg_BE)})
  
  output$out_ar_trend_stack_B <- renderPlot({ar_trend_stack_fn_B(input$in_reg_BE,input$in_mth_B,input$in_stack_B,input$in_line_B)})
  
  
  #code ends
  
  output$out_ar_box_1 <- renderValueBox({valueBox(scales::percent(info_view_fn(input$in_reg_theater)$AR,accuracy = 0.1),
                                                  "Attach Rate %",icon=icon("paperclip"),color="purple"
  )
  })
  
  output$out_unit_box <- renderValueBox({valueBox(scales::number(info_view_fn(input$in_reg_theater)$Tot.Qty,big.mark = ","),
                                                  "Units Shipped",icon=icon("suitcase"),color="purple"
  )
  })
  output$out_list_box <- renderValueBox({valueBox(scales::dollar(info_view_fn(input$in_reg_theater)$Attached.List/1000,big.mark = ",",suffix="k",accuracy = 1L),
                                                  "Attached List Price",icon=icon("money-bill-alt"),color="purple"
  )
  })
  output$out_dol_p_unit_box <- renderValueBox({valueBox(scales::dollar(info_view_fn(input$in_reg_theater)$dollar.per.unit,big.mark = ",",accuracy=0.1),
                                                        "Dollar per Unit",icon=icon("dollar-sign"),color="purple"
  )
  })
  output$out_ar_yoy_box <- renderValueBox({valueBox(scales::percent(info_view_fn(input$in_reg_theater)$AR.YoY,accuracy = 0.1),
                                                    "Attach Rate YoY",icon=icon(ifelse(info_view_fn(input$in_reg_theater)$AR.YoY>=0,"thumbs-up","thumbs-down")),color=ifelse(info_view_fn(input$in_reg_theater)$AR.YoY>=0,"green","red")
  )
  })
  output$out_unit_yoy_box <- renderValueBox({valueBox(scales::percent(info_view_fn(input$in_reg_theater)$Tot.Qty.YoY,accuracy = 0.1),
                                                      "Units Shipped YoY",icon=icon(ifelse(info_view_fn(input$in_reg_theater)$Tot.Qty.YoY>=0,"thumbs-up","thumbs-down")),color=ifelse(info_view_fn(input$in_reg_theater)$Tot.Qty.YoY>=0,"green","red")
  )
  })
  output$out_list_yoy_box <- renderValueBox({valueBox(scales::percent(info_view_fn(input$in_reg_theater)$Attached.List.YoY,accuracy = 0.1),
                                                      "Attached List YoY",icon=icon(ifelse(info_view_fn(input$in_reg_theater)$Attached.List.YoY>=0,"thumbs-up","thumbs-down")),color=ifelse(info_view_fn(input$in_reg_theater)$Attached.List.YoY>=0,"green","red")
  )
  })
  output$out_dol_p_unit_yoy_box <- renderValueBox({valueBox(scales::percent(info_view_fn(input$in_reg_theater)$dollar.per.unit.YoY,accuracy = 0.1),
                                                            "Dollar per Unit YoY",icon=icon(ifelse(info_view_fn(input$in_reg_theater)$dollar.per.unit.YoY>=0,"thumbs-up","thumbs-down")),color=ifelse(info_view_fn(input$in_reg_theater)$dollar.per.unit.YoY>=0,"green","red")
  )
  })
  
  output$'out_AR_Qtr_Target_1'<- renderText({paste("Attach Rate for",input$in_reg_theater,"is",scales::percent(abs(info_view_fn(input$in_reg_theater)$AR),accuracy=0.1),",",ifelse(info_view_fn(input$in_reg_theater)$target.to.go=="Met Target","met",paste(round(abs(info_view_fn(input$in_reg_theater)$target-info_view_fn(input$in_reg_theater)$AR)*100,1),"%"," ","lesser than",sep = "")),"quarter target of",scales::percent(info_view_fn(input$in_reg_theater)$target,accuracy=0.1),sep = " ")})
  output$'out_AR_Qtr_YoY_1'<-renderText({paste("The YoY",ifelse(info_view_fn(input$in_reg_theater)$AR.YoY>=0,"growth","decline"),"of Attach Rate is",scales::percent(abs(info_view_fn(input$in_reg_theater)$AR.YoY),accuracy=0.1))})
  output$'out_AR_Qtr_YoY_2.1'<-renderText({paste("Theatres which show max YoY in Attach Rate are",ar_summ_Overall(input$in_reg_theater)$CX.Level.2[1],",",ar_summ_Overall(input$in_reg_theater)$CX.Level.2[2],"and",ar_summ_Overall(input$in_reg_theater)$CX.Level.2[3],sep = "   ")})
  output$'out_AR_Qtr_YoY_2.2'<-renderText({paste("Theatres which show min YoY in Attach Rate are",rev(ar_summ_Overall(input$in_reg_theater)$CX.Level.2)[1],",",rev(ar_summ_Overall(input$in_reg_theater)$CX.Level.2)[2],"and",rev(ar_summ_Overall(input$in_reg_theater)$CX.Level.2)[3],sep = "   ")})
  output$'out_Units_Attached'<-renderText({paste("The Units Attached",ifelse(info_view_fn(input$in_reg_theater)$Attached.Unit.YoY>=0,"increased","decreased"),"by",scales::percent(abs(info_view_fn(input$in_reg_theater)$Attached.Unit.YoY),accuracy=0.1),"YoY")})
  output$'out_Units_Shipped'<-renderText({paste("The Units Shipped",ifelse(info_view_fn(input$in_reg_theater)$Tot.Qty.YoY>=0,"increased","decreased"),"by",scales::percent(abs(info_view_fn(input$in_reg_theater)$Tot.Qty.YoY),accuracy=0.1),"YoY")})
  output$'out_List_Price'<-renderText({paste("The YoY Attached List price for",input$in_reg_theater,"has",ifelse(info_view_fn(input$in_reg_theater)$Attached.List.YoY>=0,"increased","decreased"),"by",scales::percent(abs(info_view_fn(input$in_reg_theater)$Attached.List.YoY),accuracy=0.1))})
  output$'out_Dollar_Per_Unit'<-renderText({paste("The YoY Dollar per Unit for",input$in_reg_theater,"has",ifelse(info_view_fn(input$in_reg_theater)$dollar.per.unit.YoY>=0,"increased","decreased"),"by",scales::percent(abs(info_view_fn(input$in_reg_theater)$dollar.per.unit.YoY),accuracy=0.1))})
  
  output$out_summary_table <- renderDT({datatable(summary_table_fn(input$in_reg_theater)[,c("L2_L3","AR_pre_year","AR_curr_year","AR_YoY","Att_pre_year","Tot_pre_year","Att_curr_year","Tot_curr_year","Att_YoY","Tot_YoY","Att_list_pre_year","Att_list_curr_year","Att_list_YoY")],
                                                  colnames = c("Theater/L3",paste("AR % -",min_year,qtr,paste("M",mth,sep = "")),paste("AR % -",max_year,qtr,paste("M",mth,sep = "")),"AR % YoY",
                                                               paste("Units Attached -",min_year,qtr,paste("M",mth,sep = "")),paste("Units Total -",min_year,qtr,paste("M",mth,sep = "")),paste("Units Attached -",max_year,qtr,paste("M",mth,sep = "")),paste("Units Total -",max_year,qtr,paste("M",mth,sep = "")),"Attached Units - YoY","Total Units - YoY",
                                                               paste("Attached List -",min_year,qtr,paste("M",mth,sep = "")),paste("Attached List -",max_year,qtr,paste("M",mth,sep = "")),"Attached List YoY"
                                                  ),options = list(scrollX = TRUE)) %>%
      formatPercentage(c(2,3,4,9,10,13),1) %>%
      formatCurrency(c(5,6,7,8),currency = "", interval = 3,digits = 0) %>%
      formatCurrency(c(11,12),currency = "$", interval = 3,digits = 0) %>%
      formatStyle(c(4,9,10,13),backgroundColor = styleInterval(c(-.05,.05),c('lightpink','lightgray','lightgreen')))
  })
  
  output$out_hist_quart_ar <- renderPlot({hist_quart_ar_fn(input$in_reg_theater)})
  
  output$out_ar_trend_stack <- renderPlot({ar_trend_stack_fn(input$in_reg_theater,input$in_mth,input$in_stack,input$in_line)})
  
  
  #BE Account
  output$out_target_box_B <- renderValueBox({valueBox(scales::percent(info_view_fn_B(input$in_reg_BE)$target,accuracy = 0.1),
                                                    "Target",icon=icon("bullseye"),color="purple"
  )
  })
  
  output$out_to_go_box_B <- renderInfoBox({infoBox("Target To-go",info_view_fn_B(input$in_reg_BE)$target.to.go,
                                                 icon = icon(ifelse(info_view_fn_B(input$in_reg_BE)$target.to.go=="Met Target","hourglass","hourglass-end")))})
  
  
  output$out_waterfall_B <- renderPlot({waterfall_fn_B(input$in_reg_BE,input$in_non_sfc_pct_B)})
  
  output$out_waterfall_text_B <- renderText({waterfall_text_fn_B(input$in_reg_BE,input$in_non_sfc_pct_B)})
  output$out_cust_distn_B <- renderText({cust_distn_fn_B(input$in_reg_BE)})
  # output$out_waterfall_sfc_B <- renderPlot({waterfall_sfc_fn_B(input$in_reg_BE,
  #                                                                input$in_non_sfc_pct_B,input$in_ar_qtr_B)
  # })
  output$out_waterfall_sfc_table_B <- renderDT({datatable(waterfall_sfc_table_fn_B(input$in_reg_BE,
                                                                               input$in_non_sfc_pct_B),
  colnames = c("Best.Site.HQ.Party.Name","Business.Entity","Product.Family.ID","AR","Total.Qty","Oppty.to.Attach","AR.impact")
  ) %>%
      formatPercentage(c(7),2) %>%
      formatPercentage(c(4),1) %>%
      formatCurrency(c(5,6),currency = "", interval = 3,digits = 0)
  })
  
  #End of Code 
  
  output$out_target_box <- renderValueBox({valueBox(scales::percent(info_view_fn(input$in_reg_theater)$target,accuracy = 0.1),
                                                    "Target",icon=icon("bullseye"),color="purple"
  )
  })
  output$out_to_go_box <- renderInfoBox({infoBox("Target To-go",info_view_fn(input$in_reg_theater)$target.to.go,
                                                 icon = icon(ifelse(info_view_fn(input$in_reg_theater)$target.to.go=="Met Target","hourglass","hourglass-end")))})
  output$out_waterfall <- renderPlot({waterfall_fn(input$in_reg_theater,input$in_non_sfc_pct)})
  output$out_waterfall_text <- renderText({waterfall_text_fn(input$in_reg_theater,input$in_non_sfc_pct)})
  output$out_cust_distn <- renderText({cust_distn_fn(input$in_reg_theater)})
  # output$out_waterfall_sfc <- renderPlot({waterfall_sfc_fn(input$in_reg_theater,
  #                                                          input$in_non_sfc_pct,input$in_ar_qtr)
  # })
  output$out_waterfall_sfc_table <- renderDT({datatable(waterfall_sfc_table_fn(input$in_reg_theater,
                                                                               input$in_non_sfc_pct),
  colnames = c("Best.Site.HQ.Party.Name","L1_L2","Product.Family.ID","AR","Total.Qty","Oppty.to.Attach","AR.impact")
  ) %>%
      formatPercentage(c(7),2) %>%
      formatPercentage(c(4),1) %>%
      formatCurrency(c(5,6),currency = "", interval = 3,digits = 0)
  })
  
  #--------This is currently not used------------------------
  # output$out_metric_head <-renderText({paste("<B>Insights for ",
  #                                            if(input$in_metric=="Attach Rate"){"AR (+1 Qtr)"} 
  #                                            else {input$in_metric},
  #                                            "</B>")
  #                                     })
  
  
  # output$out_top_n_pf <- DT::renderDT({datatable(top_n_pf_fn(input$in_reg_theater)) %>%
  #                                         formatPercentage(c("AR","YoY"),1) %>%
  #                                         formatCurrency("Tot.Qty",currency = "", interval = 3,digits = 0)
  #                                     }) 
  #----------------------------------------------------------
  
  output$out_band_text_high <- renderText({pf_band_text_fn(input$in_reg_theater,"High")})
  output$out_band_text_med <- renderText({pf_band_text_fn(input$in_reg_theater,"Medium")})
  output$out_band_text_overall <- renderText({pf_band_text_fn(input$in_reg_theater,"Overall")})
  output$out_pf_distn <- renderText({pf_distn_fn(input$in_reg_theater)})
  output$out_top_acct_attached <- DT::renderDT({datatable(top_attach_fn(input$in_reg_theater,"Best.Site.HQ.Party.Name",input$in_top_attach)) %>%
      formatPercentage(c(3),1) %>%
      formatCurrency(c(2),currency = ifelse(input$in_top_attach=="Default List Price","$",""),interval = 3,digits = 0) %>%
      formatStyle(c(3),color = styleInterval(c(.55,.65),c('red','black','green')))
  }) 
  output$out_top_part_attached <- DT::renderDT({datatable(top_attach_fn(input$in_reg_theater,"Partner.Name",input$in_top_attach)) %>%
      formatPercentage(c(3),1) %>%
      formatCurrency(c(2),currency = "",interval = 3,digits = 0) %>%
      formatStyle(c(3),color = styleInterval(c(.55,.65),c('red','black','green')))
  })
  output$out_top_pf_attached <- DT::renderDT({datatable(top_attach_fn(input$in_reg_theater,"Product.Family.ID",input$in_top_attach)) %>%
      formatPercentage(c(3),1) %>%
      formatCurrency(c(2),currency = "",interval = 3,digits = 0) %>%
      formatStyle(c(3),color = styleInterval(c(.55,.65),c('red','black','green')))
  })
  output$out_top_BE_attached <- DT::renderDT({datatable(top_attach_fn(input$in_reg_theater,"Business.Entity.Name",input$in_top_attach)) %>%
      formatPercentage(c(3),1) %>%
      formatCurrency(c(2),currency = "",interval = 3,digits = 0) %>%
      formatStyle(c(3),color = styleInterval(c(.55,.65),c('red','black','green')))
  })
  
  #BE server code for tabular
  output$out_band_text_high_B <- renderText({pf_band_text_fn_B(input$in_reg_BE,"High")})
  output$out_band_text_med_B <- renderText({pf_band_text_fn_B(input$in_reg_BE,"Medium")})
  output$out_band_text_overall_B <- renderText({pf_band_text_fn_B(input$in_reg_BE,"Overall")})
  output$out_pf_distn_B <- renderText({pf_distn_fn_B(input$in_reg_BE)})
  output$out_top_acct_attached_B<- DT::renderDT({datatable(top_attach_fn_B(input$in_reg_BE,"Best.Site.HQ.Party.Name",input$in_top_attach_B)) %>%
      formatPercentage(c(3),1) %>%
      formatCurrency(c(2),currency = ifelse(input$in_top_attach_B=="Default List Price","$",""),interval = 3,digits = 0) %>%
      formatStyle(c(3),color = styleInterval(c(.55,.65),c('red','black','green')))
  }) 
  output$out_top_part_attached_B<- DT::renderDT({datatable(top_attach_fn_B(input$in_reg_BE,"Partner.Name",input$in_top_attach_B)) %>%
      formatPercentage(c(3),1) %>%
      formatCurrency(c(2),currency = "",interval = 3,digits = 0) %>%
      formatStyle(c(3),color = styleInterval(c(.55,.65),c('red','black','green')))
  })
  output$out_top_pf_attached_B<- DT::renderDT({datatable(top_attach_fn_B(input$in_reg_BE,"Product.Family.ID",input$in_top_attach_B)) %>%
      formatPercentage(c(3),1) %>%
      formatCurrency(c(2),currency = "",interval = 3,digits = 0) %>%
      formatStyle(c(3),color = styleInterval(c(.55,.65),c('red','black','green')))
  })
  output$out_top_BE_attached_B <- DT::renderDT({datatable(top_attach_fn_B(input$in_reg_BE,"Level.1",input$in_top_attach_B)) %>%
      formatPercentage(c(3),1) %>%
      formatCurrency(c(2),currency = "",interval = 3,digits = 0) %>%
      formatStyle(c(3),color = styleInterval(c(.55,.65),c('red','black','green')))
  })
  
  #Code ends
  
  #Note:another box with same function used above
  output$out_ar_box_2 <- renderValueBox({valueBox(scales::percent(info_view_fn(input$in_reg_theater)$AR,accuracy = 0.1),
                                                  "Attach Rate %",icon=icon("paperclip"),color="purple"
  )
  })
  output$out_cust_high_low <- renderDT({datatable(high_low_cust_fn(input$in_reg_theater,input$in_cust_attach)[,c("Category","AR","Tot.Qty","Cust_per")],
                                                  colnames = c("Current Attach Category","AR %","Quantity Shipped","% of Customers")
  ) %>%
      formatPercentage(c(2,4),1) %>%
      formatCurrency("Tot.Qty",currency = "", interval = 3,digits = 0)
  })
  output$out_act_pred_ar <- renderDT({datatable(pred_actual_table_fn(input$in_reg_theater,input$in_cust_attach,"AR"),
                                                colnames = c("Current Attach Category" = 2)) %>%
      formatPercentage(c(2,3,4),1)
  })
  output$out_act_pred_ar_contr <- renderDT({datatable(pred_actual_table_fn(input$in_reg_theater,input$in_cust_attach,"AR Contribution"),
                                                      colnames = c("Current Attach Category" = 2)) %>%
      formatPercentage(c(2,3,4),1)
  })
  output$out_act_pred_ptnl_impact <- renderDT({datatable(pred_actual_table_fn(input$in_reg_theater,input$in_cust_attach,"Potential Impact"),
                                                         colnames = c("Current Attach Category" = 2)) %>%
      formatPercentage(c(2,3,4),1) %>%
      formatStyle(columns = c(2), valueColumns = c(1), 
                  color=styleEqual("Low Attach Customers",'white'),
                  backgroundColor = styleEqual("Low Attach Customers",'blue'))
  })
  
  output$out_prob_to_attach <- renderDT({datatable(prop_to_attach_cust_fn(input$in_reg_theater,input$in_segment,input$in_band,input$in_BE,input$in_cust_attach),
                                                   colnames = c("Best Site HQ","Best Site GU","SL 3","Segment","Sub Segment","Current AR %","Quantity Shipped","Impact on AR","Opportunity List")) %>%
      formatPercentage(c("Current.AR","ptnl.AR.impact"),1) %>%
      formatCurrency("Tot.Qty",currency = "", interval = 3,digits = 0) %>%
      formatCurrency("List.Oppty",currency = "$", interval = 3,digits = 0)
  })
  
  #BE Prediction
  output$out_ar_box_2_B <- renderValueBox({valueBox(scales::percent(info_view_fn_B(input$in_reg_BE)$AR,accuracy = 0.1),
                                                    "Attach Rate %",icon=icon("paperclip"),color="purple"
  )
  })
  output$out_cust_high_low_B <- renderDT({datatable(high_low_cust_fn_B(input$in_reg_BE,input$in_cust_attach_B)[,c("Category","AR","Tot.Qty","Cust_per")],
                                                    colnames = c("Current Attach Category","AR %","Quantity Shipped","% of Customers"), options = list(scrollX = TRUE)
  ) %>%
      formatPercentage(c(2,4),1) %>%
      formatCurrency("Tot.Qty",currency = "", interval = 3,digits = 0)
  })
  output$out_act_pred_ar_B <- renderDT({datatable(pred_actual_table_fn_B(input$in_reg_BE,input$in_cust_attach_B,"AR"),
                                                  colnames = c("Current Attach Category" = 2),options = list(scrollX = TRUE)) %>%
      formatPercentage(c(2,3,4),1)
  })
  output$out_act_pred_ar_contr_B <- renderDT({datatable(pred_actual_table_fn_B(input$in_reg_BE,input$in_cust_attach_B,"AR Contribution"),
                                                        colnames = c("Current Attach Category" = 2),options = list(scrollX = TRUE)) %>%
      formatPercentage(c(2,3,4),1)
  })
  output$out_act_pred_ptnl_impact_B <- renderDT({datatable(pred_actual_table_fn_B (input$in_reg_BE,input$in_cust_attach_B,"Potential Impact"),
                                                           colnames = c("Current Attach Category" = 2), options = list(scrollX = TRUE)) %>%
      formatPercentage(c(2,3,4),1) %>%
      formatStyle(columns = c(2), valueColumns = c(1), 
                  color=styleEqual("Low Attach Customers",'white'),
                  backgroundColor = styleEqual("Low Attach Customers",'blue'))
  })
  
  output$out_prob_to_attach_B <- renderDT({datatable(prop_to_attach_cust_fn_B(input$in_reg_BE,input$in_segment_B,input$in_band_B,input$in_theatre_B,input$in_cust_attach_B),
                                                     colnames = c("Best Site HQ","Best Site GU","SL 3","Segment","Sub Segment","Current AR %","Quantity Shipped","Impact on AR","Opportunity List"), options = list(scrollX = TRUE)) %>%
      formatPercentage(c("Current.AR","ptnl.AR.impact"),1) %>%
      formatCurrency("Tot.Qty",currency = "", interval = 3,digits = 0) %>%
      formatCurrency("List.Oppty",currency = "$", interval = 3,digits = 0)
  })
  
  
  #End of Code
  
  #BE Comparative
  
  output$out_comp_reg_B <- renderPlot({compare_theater_fn_B(input$in_reg_BE,input$in_comp_reg_BE)})
  
  #End of code
  
  #BE Dollar per unit
  output$out_dollar_per_unit_trend_B <- renderPlot({dollar_per_unit_trend_fn_B(input$in_reg_BE)})
  output$out_dollar_per_unit_table_B <- renderDT({datatable(dollar_per_unit_table_fn_B(input$in_reg_BE)[,c("L2_L3","qty_pre_year","AR_pre_year","doll_p_unit_pre_year","Ann_Net_pre_year",
                                                                                                       "qty_curr_year","AR_curr_year","doll_p_unit_curr_year","Ann_Net_curr_year",
                                                                                                       "doll_p_unit_YoY")],
                                                          colnames = c("Sub-BE",paste("Qty -",min_year,qtr,paste("M",mth,sep = "")),paste("AR % -",min_year,qtr,paste("M",mth,sep = "")),
                                                                       paste("$/Unit -",min_year,qtr,paste("M",mth,sep = "")),paste("Ann Net -",min_year,qtr,paste("M",mth,sep = "")),
                                                                       paste("Qty -",max_year,qtr,paste("M",mth,sep = "")),paste("AR % -",max_year,qtr,paste("M",mth,sep = "")),
                                                                       paste("$/Unit -",max_year,qtr,paste("M",mth,sep = "")),paste("Ann Net -",max_year,qtr,paste("M",mth,sep = "")),
                                                                       "$/Unit - YoY")) %>%
      formatPercentage(c(3,7,10),1) %>%
      formatCurrency(c(2,6),currency = "", interval = 3,digits = 0) %>%
      formatCurrency(c(4,5,8,9),currency = "$", interval = 3,digits = 0) %>%
      formatStyle(c(6:10),c(10),backgroundColor = styleInterval(c(-.05,.05),c('lightpink','lightgray','lightgreen')))
  })
  
  #End of code
  
  output$out_comp_theater_reg <- renderPlot({compare_theater_fn(input$in_reg_theater,input$in_comp_reg_theater)})
  
  output$out_dollar_per_unit_trend <- renderPlot({dollar_per_unit_trend_fn(input$in_reg_theater)})
  output$out_dollar_per_unit_table <- renderDT({datatable(dollar_per_unit_table_fn(input$in_reg_theater)[,c("L2_L3","qty_pre_year","AR_pre_year","doll_p_unit_pre_year","Ann_Net_pre_year",
                                                                                                            "qty_curr_year","AR_curr_year","doll_p_unit_curr_year","Ann_Net_curr_year",
                                                                                                            "doll_p_unit_YoY")],
                                                          colnames = c("Theater/L3",paste("Qty -",min_year,qtr,paste("M",mth,sep = "")),paste("AR % -",min_year,qtr,paste("M",mth,sep = "")),
                                                                       paste("$/Unit -",min_year,qtr,paste("M",mth,sep = "")),paste("Ann Net -",min_year,qtr,paste("M",mth,sep = "")),
                                                                       paste("Qty -",max_year,qtr,paste("M",mth,sep = "")),paste("AR % -",max_year,qtr,paste("M",mth,sep = "")),
                                                                       paste("$/Unit -",max_year,qtr,paste("M",mth,sep = "")),paste("Ann Net -",max_year,qtr,paste("M",mth,sep = "")),
                                                                       "$/Unit - YoY")) %>%
      formatPercentage(c(3,7,10),1) %>%
      formatCurrency(c(2,6),currency = "", interval = 3,digits = 0) %>%
      formatCurrency(c(4,5,8,9),currency = "$", interval = 3,digits = 0) %>%
      formatStyle(c(6:10),c(10),backgroundColor = styleInterval(c(-.05,.05),c('lightpink','lightgray','lightgreen')))
  })
  output$out_dollar.p.unit_AR_scatter <- renderImage({
    # A temp file to save the output.
    # This file will be removed later by renderImage
    outfile <- tempfile(fileext='.gif')
    
    dollar.p.unit_AR_scatter_fn(input$in_reg_theater)
    
    # Return a list containing the filename
    list(src = "outfile.gif",
         contentType = 'image/gif'
    )
  }, deleteFile = TRUE
  )     
  
}

shinyApp(ui, server)


