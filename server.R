library(shiny)
library(shinydashboard)
library(RJDBC)
library(data.table)
library(dplyr)
library(ggplot2)

# DB 연결정보
dbInfoDF <- read.table("db_con.conf", sep = "=", strip.white = TRUE, stringsAsFactors = FALSE, col.names = c("name", "value"))
driverClass <- as.character(subset(dbInfoDF, name == "driverClass", "value"))
classPath <- as.character(subset(dbInfoDF, name == "classPath", "value"))
jdbcUrl <- as.character(subset(dbInfoDF, name == "jdbcUrl", "value"))
drv <- JDBC(driverClass=driverClass, classPath=classPath, "`")
con <- dbConnect(drv, jdbcUrl, as.character(subset(dbInfoDF, name == "username", "value")), as.character(subset(dbInfoDF, name == "password", "value")))

# 대학, 학부(과)
query <- "select * from V_R_SREG_DEPT_UNI"
cat("대학, 학부(과) 조회 중...\n")
tm_measure <- system.time(
  univSustRst <- dbGetQuery(con, query)
)
cat(tm_measure, "\n")
cat("object.size(univSustRst) : ", object.size(univSustRst), "\n")
univSustRst <- as.data.table(univSustRst)
# NA => "" 로 변환(selectInput 에서 이용)
univSustRst[is.na(univSustRst)] <- "%"
# 대학
univRst <- univSustRst[LVL == 1]                # data.table
# univRst <- univSustRst[univSustRst$LVL == 1,]   # data.frame
univChoices <- univRst$DEPT_CD
names(univChoices) <- univRst$DEPT_NM
# 학부(과) 전체 표기
allUnivSustRst <- univSustRst[LVL == 1 & DEPT_NM == "전체"]                           # data.table
# allUnivSustRst <- univSustRst[univSustRst$LVL == 1 & univSustRst$DEPT_NM == "전체",]  # data.frame

# 입학생 현황
query <- "select * from V_R_SREG_ENTR_PRECON"
cat("입학생 현황 조회 중...\n")
tm_measure <- system.time(
  entrPreconRst <- dbGetQuery(con, query)
)
cat(tm_measure, "\n")
cat("object.size(data.frame of entrPreconRst) : ", object.size(entrPreconRst), "\n")
entrPreconRst <- as.data.table(entrPreconRst)
cat("object.size(data.table of entrPreconRst) : ", object.size(entrPreconRst), "\n")

# 재학생 현황
query <- "select * from V_R_SREG_HOSH_PRECON"
cat("재학생 현황 조회 중...\n")
tm_measure <- system.time(
  hoshPreconRst <- dbGetQuery(con, query)
)
cat(tm_measure, "\n")
cat("object.size(data.frame of hoshPreconRst) : ", object.size(hoshPreconRst), "\n")
hoshPreconRst <- as.data.table(hoshPreconRst)
cat("object.size(data.table of hoshPreconRst) : ", object.size(hoshPreconRst), "\n")

# 등록금,장학금 현황
query <- "select * from V_R_ENRO_REGAMT_SCALAMT_PRECON"
cat("등록금,장학금 현황 조회 중...\n")
tm_measure <- system.time(
  regAmtScalAmtPreconRst <- dbGetQuery(con, query)
)
cat(tm_measure, "\n")
cat("object.size(data.frame of regAmtScalAmtPreconRst) : ", object.size(regAmtScalAmtPreconRst), "\n")
regAmtScalAmtPreconRst <- as.data.table(regAmtScalAmtPreconRst)
cat("object.size(data.table of regAmtScalAmtPreconRst) : ", object.size(regAmtScalAmtPreconRst), "\n")

function(input, output, session) {
  # output$infoBox <- renderInfoBox({
  #   invalidateLater(1000)
  #   infoBox(paste0("현재 시간 : ", Sys.time()))
  # })
  # 장학금수혜율(금액)
  output$scalAmtBnftRateAmtValueBoxOut <- renderValueBox({
    if(is.null(input$univCd)) {
      cat("is.null(input$univCd)", "\n")
      return(valueBox("X%", "장학금수혜율(금액)"))
    }
    if(is.null(input$sustCd)) {
      cat("is.null(input$sustCd)", "\n")
      return(valueBox("X%", "장학금수혜율(금액)"))
    }
    
    # if(is.null(input$univCd)) return()
    # if(is.null(input$sustCd)) return()

    cat("system.time in 장학금 수혜율(금액)\n")
    tm_measure <- system.time({
      filter_query <- paste0("filter(regAmtScalAmtPreconRst, YY >= '", input$fromYy, "' & YY <= '", input$toYy, "'",
                             if(input$univCd != "%") paste0(" & UNIV_CD == '", input$univCd, "'"),
                             if(input$sustCd != "%") paste0(" & SUST_CD == '", input$sustCd, "'"),
                             ")"
      )
      # cat("filter_query : ", filter_query, "\n")
      scalAmtBnftRateDF <- eval(parse(text = filter_query))
      scalAmtBnftRateDF <-
        scalAmtBnftRateDF %>%
        summarise(scal_amt_bnft_rate = round(sum(SCAL_AMT) / sum(REG_AMT),3) * 100, scal_amt_bnft_rate2 = round(sum(SCAL_CNT) / sum(REG_CNT),3) * 100)
    })
    cat(tm_measure, "\n")

    names(scalAmtBnftRateDF) <- c("수혜율_금액","수혜율_인원")
    
    # 조회 자료 없을 경우
    if(is.na(scalAmtBnftRateDF$수혜율_금액)) {
      cat("is.na(scalAmtBnftRateDF$수혜율_금액)", "\n")
      return(valueBox("X%", "장학금수혜율(금액)"))
    }
    
    valueBox(width = NULL, paste0(scalAmtBnftRateDF$수혜율_금액, "%"), "장학금수혜율(금액)",
      color = (
        if(scalAmtBnftRateDF$수혜율_금액 >= 50) {
          "aqua"
        } else {
          "maroon"
        }
      ),
      icon = icon(
        if(scalAmtBnftRateDF$수혜율_금액 >= 50) {
          "thumbs-up"
        } else {
          "thumbs-down"
        }
      )
    )
  })
  
  # 장학금수혜율(인원)
  output$scalAmtBnftRateRcntValueBoxOut <- renderValueBox({
    if(is.null(input$univCd)) {
      cat("is.null(input$univCd)", "\n")
      return(valueBox("X%", "장학금수혜율(인원)"))
    }
    if(is.null(input$sustCd)) {
      cat("is.null(input$sustCd)", "\n")
      return(valueBox("X%", "장학금수혜율(인원)"))
    }
    
    # if(is.null(input$univCd)) return()
    # if(is.null(input$sustCd)) return()

    cat("system.time in 장학금 수혜율(인원)\n")
    tm_measure <- system.time({
      filter_query <- paste0("filter(regAmtScalAmtPreconRst, YY >= '", input$fromYy, "' & YY <= '", input$toYy, "'",
                             if(input$univCd != "%") paste0(" & UNIV_CD == '", input$univCd, "'"),
                             if(input$sustCd != "%") paste0(" & SUST_CD == '", input$sustCd, "'"),
                             ")"
      )
      # cat("filter_query : ", filter_query, "\n")
      scalAmtBnftRateDF <- eval(parse(text = filter_query))
      scalAmtBnftRateDF <-
        scalAmtBnftRateDF %>%
        summarise(scal_amt_bnft_rate = round(sum(SCAL_AMT) / sum(REG_AMT),3) * 100, scal_amt_bnft_rate2 = round(sum(SCAL_CNT) / sum(REG_CNT),3) * 100)
    })
    cat(tm_measure, "\n")

    names(scalAmtBnftRateDF) <- c("수혜율_금액","수혜율_인원")

    # 조회 자료 없을 경우
    if(is.na(scalAmtBnftRateDF$수혜율_인원)) {
      cat("is.na(scalAmtBnftRateDF$수혜율_인원)", "\n")
      return(valueBox("X%", "장학금수혜율(인원)"))
    }
    
    valueBox(width = NULL, paste0(scalAmtBnftRateDF$수혜율_인원, "%"), "장학금수혜율(인원)",
      color = (
        if(scalAmtBnftRateDF$수혜율_인원 >= 50) {
          "aqua"
        } else {
          "maroon"
        }
      ),
      icon = icon(
        if(scalAmtBnftRateDF$수혜율_인원 >= 50) {
          "thumbs-up"
        } else {
          "thumbs-down"
        }
      )
    )
  })
  
  # 현재 시간
  output$now <- renderUI({
    invalidateLater(1000)
    h4(paste0("현재 시간 : ", Sys.time()), style = "text-align:right;padding-right:20px")
  })
  
  # 주요지표 조회 대학, 학과명
  output$subTitle <- renderUI({
    if(is.null(input$univCd)) return()
    if(is.null(input$sustCd)) return()
    
    # browser()
    univNm <- 
      if(input$univCd == "%") {
        "전체"
      } else {
        univSustRst[univSustRst$DEPT_CD == input$univCd,]$DEPT_NM
      }
    sustNm <- 
      if(input$sustCd == "%") {
        "전체"
      } else {
        univSustRst[univSustRst$DEPT_CD == input$sustCd & univSustRst$LVL == 3,]$DEPT_NM
      }
    cat("output$subTitle : ", input$univCd, univNm, input$sustCd, sustNm, "\n")
    h3(paste0("대학 : ", univNm, ", 학과 : ", sustNm), style = "text-align:right;padding-right:20px")
  })
  
  # 대학
  output$univSelectOut <- renderUI({
    selectInput("univCd", "대학", univChoices)
  })
  
  # 학부(과)
  output$sustSelectOut <- renderUI({
    if(is.null(input$univCd)) return()
    
    if(input$univCd == "%") {
      sustRst <- allUnivSustRst
    } else {
      sustRst <- univSustRst[UNIV_CD == input$univCd & LVL == 3]
      sustRst <- rbind(allUnivSustRst, sustRst)
    }
    sustChoices <- sustRst$DEPT_CD
    names(sustChoices) <- sustRst$DEPT_NM
    selectInput("sustCd", "학과", sustChoices)
  })
  
  # 대학, 학부(과) 값 추출
  univCd <- reactive({
    input$univCd
  })

  sustCd <- reactive({
    input$sustCd
  })
  
  # 주요지표 대학 순환 표기
  observe({
    x <- input$dashboardRotate

    if(x) {
      invalidateLater(10000)
      next_univ_cd <- isolate({
        idx <- which(univChoices == input$univCd)
        if(idx == length(univChoices)) {
          univChoices[1]
        } else {
          univChoices[idx + 1]
        }
      })
      cat("next_univ_cd : ", next_univ_cd, "\n")
      updateSelectInput(session, "univCd", selected = next_univ_cd)
    }
  })
  
  # 입학생 현황
  output$entrPreconPlotOut <- renderPlot({
    if(is.null(univCd())) return()
    if(is.null(sustCd())) return()
    
    cat("univCd in 입학생 현황", univCd(), "\n")
    cat("sustCd in 입학생 현황", sustCd(), "\n")
    
    cat("system.time in 입학생 현황\n")
    tm_measure <- system.time({
      filter_query <- paste0("filter(entrPreconRst, YY >= '", input$fromYy, "' & YY <= '", input$toYy, "'",
                             if(univCd() != "%") paste0(" & UNIV_CD == '", univCd(), "'"),
                             if(sustCd() != "%") paste0(" & SUST_CD == '", sustCd(), "'"),
                             ")"
                            )
      cat("filter_query : ", filter_query, "\n")
      entrPreconDF <- eval(parse(text = filter_query))
      entrPreconDF <-
        entrPreconDF %>%
        group_by(YY) %>%
        summarise(cnt = sum(CNT))
    })
    cat(tm_measure, "\n")
    
    names(entrPreconDF) <- c("연도","인원")

    # y축으로 나타낼 값 범위를 min, max 값의 10% 로 보정
    revised_y_range <- round(range(entrPreconDF$인원) * c(0.9, 1))
    # cat("range(entrPreconDF$인원) : ", range(entrPreconDF$인원), "\n")
    # cat("revised_y_range : ", revised_y_range, "\n")

    # x축으로 나타낼 연도 범위 재설정(조회 연도 중 누락된 연도 추가)
    if(length(setdiff(seq(input$fromYy, input$toYy), entrPreconDF$연도)) > 0) {
      entrPreconDF <- rbind(entrPreconDF, data.frame(연도 = setdiff(seq(input$fromYy, input$toYy), entrPreconDF$연도), 인원 = 0))
    }
    
    # print(head(entrPreconDF))
    ggplot(entrPreconDF, aes(연도, 인원)) + geom_col(aes(fill = 연도)) + scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE), limits = revised_y_range, oob = scales::rescale_none)
  })
  
  # 재학생 현황
  output$hoshPreconPlotOut <- renderPlot({
    if(is.null(input$univCd)) return()
    if(is.null(input$sustCd)) return()
    
    cat("system.time in 재학생 현황\n")
    tm_measure <- system.time({
      filter_query <- paste0("filter(hoshPreconRst, YY >= '", input$fromYy, "' & YY <= '", input$toYy, "'",
                             if(input$univCd != "%") paste0(" & UNIV_CD == '", input$univCd, "'"),
                             if(input$sustCd != "%") paste0(" & SUST_CD == '", input$sustCd, "'"),
                             ")"
      )
      # cat("filter_query : ", filter_query, "\n")
      hoshPreconDF <- eval(parse(text = filter_query))
      hoshPreconDF <-
        hoshPreconDF %>%
        group_by(YY) %>%
        summarise(cnt = sum(CNT))
    })
    cat(tm_measure, "\n")
    
    names(hoshPreconDF) <- c("연도","인원")

    # y축으로 나타낼 값 범위를 min, max 값의 10% 로 보정
    revised_y_range <- round(range(hoshPreconDF$인원) * c(0.9, 1))
    
    # x축으로 나타낼 연도 범위 재설정(조회 연도 중 누락된 연도 추가)
    if(length(setdiff(seq(input$fromYy, input$toYy), hoshPreconDF$연도)) > 0) {
      hoshPreconDF <- rbind(hoshPreconDF, data.frame(연도 = setdiff(seq(input$fromYy, input$toYy), hoshPreconDF$연도), 인원 = 0))
    }
    
    ggplot(hoshPreconDF, aes(연도, 인원)) + geom_col(aes(fill = 연도)) + scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE), limits = revised_y_range, oob = scales::rescale_none)
  })
  
  # 등록금/장학금 현황
  output$regAmtScalAmtPreconPlotOut <- renderPlot({
    if(is.null(input$univCd)) return()
    if(is.null(input$sustCd)) return()
    
    cat("system.time in 등록금/장학금 현황\n")
    tm_measure <- system.time({
      filter_query <- paste0("filter(regAmtScalAmtPreconRst, YY >= '", input$fromYy, "' & YY <= '", input$toYy, "'",
                             if(input$univCd != "%") paste0(" & UNIV_CD == '", input$univCd, "'"),
                             if(input$sustCd != "%") paste0(" & SUST_CD == '", input$sustCd, "'"),
                             ")"
      )
      # cat("filter_query : ", filter_query, "\n")
      regAmtScalAmtPreconDF <- eval(parse(text = filter_query))
      regAmtScalAmtPreconDF <-
        regAmtScalAmtPreconDF %>%
        group_by(YY) %>%
        summarise(reg_amt = sum(REG_AMT), scal_amt = sum(SCAL_AMT))
    })
    cat(tm_measure, "\n")
    
    names(regAmtScalAmtPreconDF) <- c("연도","등록금","장학금")

    # x축으로 나타낼 연도 범위 재설정(조회 연도 중 누락된 연도 추가)
    if(length(setdiff(seq(input$fromYy, input$toYy), regAmtScalAmtPreconDF$연도)) > 0) {
      regAmtScalAmtPreconDF <- rbind(regAmtScalAmtPreconDF, data.frame(연도 = setdiff(seq(input$fromYy, input$toYy), regAmtScalAmtPreconDF$연도), 등록금 = 0, 장학금 = 0))
    }
    
    regAmtScalAmtPreconDF <- melt(regAmtScalAmtPreconDF, id = "연도")
    regAmtScalAmtPreconDF[is.na(regAmtScalAmtPreconDF)] <- 0  # nvl 구문 없음에 따라 NA => 0 처리
    names(regAmtScalAmtPreconDF) <- c("연도","항목","금액")

    # y축으로 나타낼 값 범위를 min, max 값의 10% 로 보정
    # 아래 구문 실행 시 위에서 추가한 연도에 의한 0값 고려해야 함
    # revised_y_range <- round(range(regAmtScalAmtPreconDF$금액) * c(0.9, 1))

    # ggplot(regAmtScalAmtPreconDF, aes(연도, 금액)) + geom_bar(aes(fill = 항목), position = "dodge", stat = "identity")
    ggplot(regAmtScalAmtPreconDF, aes(연도, 금액)) + geom_col(aes(fill = 항목), position = "dodge") + scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE))
  })
  
  # 장학금 수혜율
  output$scalAmtBnftRatePlotOut <- renderPlot({
    if(is.null(input$univCd)) return()
    if(is.null(input$sustCd)) return()
    
    cat("system.time in 장학금 수혜율\n")
    tm_measure <- system.time({
      filter_query <- paste0("filter(regAmtScalAmtPreconRst, YY >= '", input$fromYy, "' & YY <= '", input$toYy, "'",
                             if(input$univCd != "%") paste0(" & UNIV_CD == '", input$univCd, "'"),
                             if(input$sustCd != "%") paste0(" & SUST_CD == '", input$sustCd, "'"),
                             ")"
      )
      # cat("filter_query : ", filter_query, "\n")
      scalAmtBnftRateDF <- eval(parse(text = filter_query))
      scalAmtBnftRateDF <-
        scalAmtBnftRateDF %>%
        group_by(YY) %>%
        summarise(scal_amt_bnft_rate = round(sum(SCAL_AMT) / sum(REG_AMT),3) * 100, scal_amt_bnft_rate2 = round(sum(SCAL_CNT) / sum(REG_CNT),3) * 100)
    })
    cat(tm_measure, "\n")
    
    names(scalAmtBnftRateDF) <- c("연도","수혜율_금액","수혜율_인원")

    # x축으로 나타낼 연도 범위 재설정(조회 연도 중 누락된 연도 추가)
    if(length(setdiff(seq(input$fromYy, input$toYy), scalAmtBnftRateDF$연도)) > 0) {
      scalAmtBnftRateDF <- rbind(scalAmtBnftRateDF, data.frame(연도 = setdiff(seq(input$fromYy, input$toYy), scalAmtBnftRateDF$연도), 수혜율_금액 = 0, 수혜율_인원 = 0))
    }
    
    scalAmtBnftRateDF <- melt(scalAmtBnftRateDF, id = "연도")
    # scalAmtBnftRateDF[is.na(scalAmtBnftRateDF)] <- 0  # nvl 구문 없음에 따라 NA => 0 처리
    names(scalAmtBnftRateDF) <- c("연도","항목","비율")
    
    ggplot(scalAmtBnftRateDF, aes(x = 연도, y = 비율, colour = 항목, group = 항목)) + geom_line() + geom_point() + ylim(0, 100)
  })
}
