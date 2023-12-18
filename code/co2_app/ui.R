library(dplyr)
library(stringr)
library(tidyverse)
library(glue)
library(readxl)
library(ggplot2)
library(rlang)
library(DT)
library(broom)
library(ggfortify)
library(kableExtra)
library(corrplot)
library(car)
library(openxlsx)
library(lme4)
library(broom.mixed)
library(glmmLasso)
library(Rwave)
library(igraph)
library(visNetwork)
library(network)
library(shiny)
library(shinythemes)
library(kableExtra)
library(wavethresh)
library(mathjaxr)
library(bslib)
library(bsicons)
library(rsconnect)
source("./co2_function.R")

test_df = read_csv("./data/kaplan_mierer/kaplan_mierer.csv")
frequency_table = read_csv("./data/wavelet/frequency_table.csv")
#mix_df = read_csv("./data/wavelet/mix_df.csv")
k_means_df = read_csv("./data/clustering/k_means_df.csv")
patients = unique(k_means_df$subject)
#patients = c("HYP01", "HYP02", "HYP04", "HYP06", "HYP07", "HYP08", "HYP09", 
#             "HYP10", "HYP11", "HYP12", "HYP13", "UPENN01", "UPENN04")

patient_list = vector(mode = "list", length = length(patients))
names(patient_list) = patients
for (p in patients){
  patient_list[p] = list(k_means_df %>% filter(subject == p) %>% pull(day) %>% as.character()) 
}


#patient_list = list(
#  "HYP01" = c("2022-05-26", "2022-05-27", "2022-05-28", "2022-09-08", "2022-09-09", 
#              "2022-09-10", "2023-01-05", "2023-01-07", "2023-04-13", "2023-04-15"),
#  "HYP02" = c("2022-10-28", "2023-02-09", "2023-02-12"),
#  "HYP04" = c("2022-07-21", "2022-07-22", "2023-03-03", "2023-03-04", "2023-03-05"),
#  "HYP06" = c("2022-08-02","2022-08-03", "2022-08-04"),
#  "HYP07" = c("2022-08-11", "2022-08-12", "2022-08-13", "2022-12-07", "2022-12-08"),
#  "HYP08" = c("2022-08-11", "2022-08-12", "2022-08-13", "2022-12-08", "2022-12-09", "2022-12-10",
#              "2023-03-30", "2023-03-31", "2023-04-01"),
#  "HYP09" = c("2022-08-17", "2022-08-18", "2022-08-19", "2022-12-19", "2022-12-21"),
#  "HYP10" = c("2022-08-25", "2022-08-27", "2022-12-15"),
#  "HYP11" = c("2022-09-21", "2022-09-22"),
#  "HYP12" = c("2022-10-18", "2022-10-19", "2022-10-20"),
#  "HYP13" = c("2023-03-02", "2023-03-03", "2023-03-04"),
#  "UPENN01" = c("2022-09-19", "2022-09-20", "2022-12-16", "2022-12-18", "2023-03-18", 
#                "2023-03-19", "2023-03-20"),
#  "UPENN04" = c("2023-02-23", "2023-02-24", "2023-02-25")
#)


#patient_v = c("HYP01", "HYP02", "HYP04", "HYP06", "HYP07", "HYP08", "HYP09", "HYP10", "HYP11",  
#"HYP12", "HYP13", "UPENN01", "UPENN04")

event_summary = read_csv("./data/abnormal_summary/event_summary.csv") 
event_summary = event_summary %>% dplyr::select(event, abnormal, normal) %>% mutate(Notes = case_when(
  event == "event_45" ~ "PCO2 > 45 mmHg for > 5 mins",
  event == "event_50" ~ "PCO2 > 50 mmHg for > 5 mins",
  event == "event_45.10" ~ "PCO2 > 45 mmHg for > 10 mins",
  event == "event_50.10" ~ "PCO2 > 50 mmHg for > 10 mins",
  event == "event_spo2" ~ "SpO2 < 88% for > 5 mins",
  event == "delta_pco2_event" ~ "PCO2 > 10 mmHg increase from baseline for > 10 mins (cumulatively)",
  event == "delta_pco2_50_event" ~ "PCO2 > 10 mmHg increase from baseline to above 50 mmHg for > 10 mins (cumulatively)"
))
time_summary = read_csv("./data/abnormal_summary/time_summary.csv") 
time_summary = time_summary %>% dplyr::select(period, abnormal, normal) %>% mutate(Notes = case_when(
  period == "PCO2.45" ~ "PCO2 > 45 mmHg for > 5 mins",
  period == "PCO2.50" ~ "PCO2 > 50 mmHg for > 5 mins",
  period == "PCO2.45.10" ~ "PCO2 > 45 mmHg for > 10 mins",
  period == "PCO2.50.10" ~ "PCO2 > 50 mmHg for > 10 mins",
  period == "SpO2.88" ~ "SpO2 < 88% for > 5 mins",
  period == "delta_pco2_time" ~ "PCO2 > 10 mmHg increase from baseline for > 10 mins (cumulatively)",
  period == "delta_pco2_50_time" ~ "PCO2 > 10 mmHg increase from baseline to above 50 mmHg for > 10 mins (cumulatively)"
)) %>% rename("time" = "period", "abnormal" = "abnormal", "normal" = "normal") 


event_n = event_summary$Notes




navbarPage(
  title = "Overnight PCO2",
  theme = bs_theme(bootswatch = "slate"),
  tabPanel("Overnight Data Stream",
           sidebarLayout(
             sidebarPanel(
               selectInput("patient", "Select a patient of interest", choices = patients, selected = patients[1]),
               uiOutput("night_detail")
             ),
             mainPanel(
               fluidRow(
                 shinydashboard::box(width = 12,
                                     title = "PCO2 Distribution",
                                     plotOutput("miss_plot"))),
               fluidRow(
                 column(width = 6, 
                        shinydashboard::box(
                          width = NULL,
                          title = "Raw Feature Statistics",
                          DTOutput("raw_features"))),
                 column(width = 6, 
                        shinydashboard::box(
                          width = NULL,
                          title = "Standard Measurement",
                          DTOutput("sm")),
                        shinydashboard::box(
                          width = NULL,
                          title = "Cluster Labels",
                          DTOutput("patient_group")))
               )
             )
           )
  ),
  
  tabPanel("Discrete Wavelet Transform",
           sidebarLayout(
             sidebarPanel(
               selectInput("patient_wv", "Select a patient of interest", choices = patients, selected = patients[1]),
               uiOutput("night_detail_wv"),
               selectInput("level", "Select a level", choices = seq(13,1,-1), selected = 13),
               textOutput("HZ")
             ),
             mainPanel(
               fluidRow(
                 shinydashboard::box(width = 12,
                                     title = "DWT Coefficients",
                                     plotOutput("wave_c"))),
               fluidRow(
                 column(width = 6, 
                        shinydashboard::box(
                          width = NULL,
                          title = "Approximation Coefficient Features",
                          DTOutput("a_feature"))),
                 column(width = 6, 
                        shinydashboard::box(
                          width = NULL,
                          title = "Detail Coefficient Features",
                          DTOutput("d_feature")))
               )
             )
           )
  ),
  
  tabPanel("Linear Mixed Effect Model",
           sidebarLayout(
             sidebarPanel(
               radioButtons("feature_type", "Select the type of features", choices = c("raw features", "wavelet features"), selected = "raw features")
             ),
             mainPanel(
               fluidRow(
                 shinydashboard::box(
                   width = 12,
                   title = "Mixed Effect Result Table",
                   DTOutput("formula"))
               ),
               fluidRow(
                 column(width = 6,
                 shinydashboard::box(
                   width = NULL,
                   uiOutput("frequency_dis_1"))
                 ),
                 column(width = 6,
                        shinydashboard::box(
                          width = NULL,
                          uiOutput("frequency_dis_2"))
                 )
               ),
               fluidRow(
                 column(width = 6,
                        shinydashboard::box(
                          width = NULL,
                          uiOutput("frequency_text_box_1"))
                 ),
                 column(width = 6,
                        shinydashboard::box(
                          width = NULL,
                          uiOutput("frequency_text_box_2"))
                 )
               )
             )
           )
  ),
  
  tabPanel("Hierarchical Clustering",
           sidebarLayout(
             sidebarPanel(
               fluidRow(
                 shinydashboard::box(
                   width = NULL,
                   title = "Cluster Level Selection",
                   radioButtons("cluster_level", "Select the clustering level", choices = c("Subject level", "Visit level", "Night level"), selected = "Subject level")
                 )
               ),
               fluidRow(
                 shinydashboard::box(
                   width = NULL,
                   title = "Multiple Comparison Adjustment",
                   radioButtons("mc", "Select whether to adjust for multiple comparison issue", choices = c("True", "False"), selected = "False")
                 )
               ),
               fluidRow(
                 shinydashboard::box(
                   width = NULL,
                   uiOutput("measure_control")
                 )
               )
             ),
             mainPanel(
               fluidRow(
                 column(width = 4,
                        shinydashboard::box(
                          width = NULL,
                          uiOutput("info_type"))),
                 column(width = 8,
                        shinydashboard::box(
                          width = NULL,
                          uiOutput("v_type")))
               ),
               fluidRow(
                 column(width = 4,
                        shinydashboard::box(
                          width = NULL,
                          DTOutput("sig_table"))),
                 column(width = 4,
                        shinydashboard::box(
                          width = NULL,
                          uiOutput("ui_1"))),
                 column(width = 4,
                        shinydashboard::box(
                          width = NULL,
                          uiOutput("ui_2")))
               )
             )
           )
  ),
  
 # tabPanel("Clustering Evaluation",
 #          sidebarLayout(
 #            sidebarPanel(
 #              fluidRow(
 #                shinydashboard::box(
 #                  width = NULL,
 #                  title = "Selection of Standard",
 #                 radioButtons("standard", "Select the standard measurement based method", choices = c("MIP-based", "MIP&ALSFRS-based"), selected = "MIP-based")
 #                 ))
 #           ),
 #            mainPanel(
 #              fluidRow(
 #                column(width = 4,
 #                       shinydashboard::box(
 #                         width = NULL,
 #                         selectInput("eval_patient", "Select Patient of interest", choices = c("All Patients", "HYP01", "HYP02", "HYP04", "HYP07", "HYP08", 
 #                                                                                               "HYP09", "HYP10", "UPENN01"), selected = "All patients"),
 #                         radioButtons("std_m", "Select the type of standard measurement", choices = c("mip", "fvc", "alsfrs_total"), selected = "mip")
 #                         )),
 #                column(width = 4,
 #                       shinydashboard::box(
 #                         width = NULL,
 #                         title = "Proposed Method",
 #                         plotOutput("eval_4"))),
 #                column(width = 4,
 #                       shinydashboard::box(
 #                         width = NULL,
 #                         title = "Standard Method",
 #                         plotOutput("eval_5")))
 #              ),
 #              fluidRow(
 #                column(width = 4,
 #                       shinydashboard::box(
 #                         width = NULL,
 #                         title = "Model Agreement",
 #                         DTOutput("agreement_table"))),
 #                column(width = 4,
 #                       shinydashboard::box(
 #                         width = NULL,
 #                         title = "Std Measurement Dist.",
 #                         plotOutput("eval_1"))),
 #                column(width = 4,
 #                       shinydashboard::box(
 #                         width = NULL,
 #                         title = "Abnormal Test Survival Curve",
 #                         plotOutput("mip_survive"),
 #                         textOutput("mip_survive_p")))
 #              )
 #            )
 #         )
 # ),
  
  #tabPanel("Cluster Result vs. PCO2 Features",
  #         sidebarLayout(
  #           sidebarPanel(
  #             fluidRow(
  #               shinydashboard::box(
  #                 width = NULL,
  #                 title = "Selection of PCO2 Features",
  #                 selectInput("event_name", "Select the PCO2 related event feature", choices = c("All", event_n), selected = "All"),
  #                 selectInput("time_name", "Select the PCO2 related time feature", choices = c("All", time_n), selected = "All")
  #               ))
  #           ),
  #           mainPanel(
  #             fluidRow(
  #               column(width = 6,
  #                      shinydashboard::box(
  #                        width = NULL,
  #                        DTOutput("event_table"),
  #                        textOutput("event_p")
  #                      )),
  #               column(width = 6,
  #                      shinydashboard::box(
  #                        width = NULL,
  #                        DTOutput("time_table"),
  #                        textOutput("time_p")
  #                      ))
  #             ),
  #             fluidRow(
  #               column(width = 6,
  #                      shinydashboard::box(
  #                        width = NULL,
  #                        title = "Event Distribution",
  #                        plotOutput("event_dis"))),
  #               column(width = 6,
  #                      shinydashboard::box(
  #                        width = NULL,
  #                        title = "Time Distribution",
  #                        plotOutput("time_dis")))
  #             )
  #           )
  #         )
  #),
  
  tabPanel("Kaplan-Meier Analysis",
           sidebarLayout(
             sidebarPanel(
               radioButtons("sensitive", "Select nocturnal CO2 related abnormal test (Sensitivity Analysis)", choices = c("Nocturnal CO2 >45mmHg for >5mins", "Nocturnal CO2 >50mmHg for >10mins", ">10mmHg Increase from baseline CO2 for >10mins", ">10mmHg Increase from baseline CO2 to >50mmHg for >10mins", "Nocturnal PCO2 Test"), selected = "Nocturnal CO2 >45mmHg for >5mins"),
               radioButtons("CI", "Select whether to include confidence interval in the kaplan-meier curve", choices = c("No", "Yes"), selected = "No"),
               sliderInput("pred_time", "Predition Time Period", min = 0, max = (max(test_df$time) - 8), value = 365)
             ),
             mainPanel(
               fluidRow(
                 shinydashboard::box(width = 12,
                                     title = "Kaplan-Meier Curve",
                                     plotOutput("kmc"))
               ),
               fluidRow(
                 shinydashboard::box(width = 12,
                                     title = "Abnormal Respiratory Event Prediction",
                                     DTOutput("km_pred"))
               ),
               fluidRow(
                 column(width = 6,
                 shinydashboard::box(width = NULL,
                                     title = "fvc v.s. mip",
                                     textOutput("p_result1"))),
                 column(width = 6,
                        shinydashboard::box(width = NULL,
                                            title = "PCO2 v.s. SpO2",
                                            textOutput("p_result2")))
               ) 
             )
           )
           )
  
)