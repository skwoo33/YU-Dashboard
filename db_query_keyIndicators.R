# 주요지표 용 뷰 실행(data frame 설정)

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
