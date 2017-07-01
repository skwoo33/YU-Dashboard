tabItem_keyIndicators <-
tabItem("keyIndicators",
        fluidRow(
          column(6,
                 # valueBox(width = 6, "80%", "장학금수혜율", icon = icon("thumbs-up"))
                 valueBoxOutput("scalAmtBnftRateAmtValueBoxOut", width = 6),
                 valueBoxOutput("scalAmtBnftRateRcntValueBoxOut", width = 6)
                 # infoBoxOutput("infoBox")
          ),
          column(6,
                 fluidRow(
                   uiOutput("now"),
                   uiOutput("subTitle")
                 )
          )
        ),
        fluidRow(
          box(title = "입학생 현황",
              solidHeader = TRUE,
              status = "success",
              collapsible = TRUE,
              plotOutput("entrPreconPlotOut")
          ),
          box(title = "재학생 현황",
              solidHeader = TRUE,
              status = "info",
              collapsible = TRUE,
              plotOutput("hoshPreconPlotOut")
          )
        ),
        fluidRow(
          box(title = "등록금/장학금 현황",
              solidHeader = TRUE,
              status = "warning",
              collapsible = TRUE,
              plotOutput("regAmtScalAmtPreconPlotOut")
          ),
          box(title = "장학금 수혜율",
              solidHeader = TRUE,
              status = "danger",
              collapsible = TRUE,
              plotOutput("scalAmtBnftRatePlotOut")
          )
        )
)
