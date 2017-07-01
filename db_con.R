# library(RJDBC)
library(ROracle)
library(pool)

# DB Connection
dbInfoDF <- read.table("/R/shiny/lesson/YU Dashboard/db_con.conf", sep = "=", strip.white = TRUE, stringsAsFactors = FALSE, col.names = c("name", "value"))
username <- dbInfoDF[dbInfoDF$name == "username", "value"]
password <- dbInfoDF[dbInfoDF$name == "password", "value"]

# for RJDBC
# driverClass <- dbInfoDF[dbInfoDF$name == "driverClass", "value"]
# classPath <- dbInfoDF[dbInfoDF$name == "classPath", "value"]
# jdbcUrl <- dbInfoDF[dbInfoDF$name == "jdbcUrl", "value"]
# drv <- JDBC(driverClass=driverClass, classPath=classPath, "`")
# 
# con <- dbConnect(drv,
#                  jdbcUrl,
#                  username,
#                  password)

# for ROracle
drv <- ROracle::Oracle()
dbname <- dbInfoDF[dbInfoDF$name == "neweasy1", "value"]

# for pool
minSize <- as.numeric(dbInfoDF[dbInfoDF$name == "minSize", "value"])
maxSize <- as.numeric(dbInfoDF[dbInfoDF$name == "maxSize", "value"])
idleTimeout <- as.numeric(dbInfoDF[dbInfoDF$name == "idleTimeout", "value"])

con <- dbPool(drv = drv,
               dbname = dbname,
               username = username,
               password = password,
               minSize = minSize,
               maxSize = maxSize,
               idleTimeout = idleTimeout)
