## ----setup, include=FALSE------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------------------------------------------------------------------------------
setwd("H:/Private/ABC_data")



## ----message=FALSE, warning=FALSE, , warning=FALSE, results='hide'-------------------------------------------------------
library(tidyverse)
library(openxlsx)
library(reshape2)
library(janitor)
library(lubridate)
library(panelr)


## ----pressure, echo=FALSE------------------------------------------------------------------------------------------------

data <- read.xlsx("TVT_2021.xlsx", sheet = 'ABC SR 2021')
data <- data[-c(1:12),]
colnames(data) <- c(1:28)


## ------------------------------------------------------------------------------------------------------------------------
remove<- data[, grep(c("Tlačený|Predaný|Mesiac|celoštátné"), data)]
colnames(remove)[2] <- "ac"
prog1 <- remove[!grepl("PP|MP", remove$ac),]


## ------------------------------------------------------------------------------------------------------------------------
prog1[1] <- NULL
ind <- apply(prog1, 1, function(x) all(is.na(x)))
prog1 <- prog1[!ind,]
prog2 <- prog1[!grepl("Mesiac|Titul|novým", prog1$ac),]


## ------------------------------------------------------------------------------------------------------------------------
colnames(prog1)[2] <- "Month"
Mesiac <- c(NA, "Január", "Január", "Február", "Február", "Marec", "Marec", "Apríl", "Apríl", "Máj", "Máj", "Jún", "Jún", "Júl", "Júl", "August", "August", "September", "September", "Október", "Október", "November", "November", "December", "December")



## ------------------------------------------------------------------------------------------------------------------------
prog4 <- t(prog2) %>% as.data.frame()
prog4 <- cbind(Mesiac, prog4)
names(prog4) <- tolower(names(prog4))
prog4 <- prog4 %>% row_to_names(row_number = 1)
colnames(prog4)[1] <- "Mesiac"


## ------------------------------------------------------------------------------------------------------------------------
prog5 <- prog4 %>% melt(id = "Mesiac")


## ------------------------------------------------------------------------------------------------------------------------
predane <- prog5[seq(2, nrow(prog5), 2),]      
tlac <- prog5[seq(1, nrow(prog5), 2),]     

final <- merge(tlac, predane,
                               by.x = c("Mesiac", "variable"), 
                               by.y = c("Mesiac", "variable"), 
                               all.x = TRUE, all.y = TRUE) 


## ------------------------------------------------------------------------------------------------------------------------
final$Mesiac <- paste( final$Mesiac,"2.2020" ,sep=".")
final$Mesiac <- as.Date(final$Mesiac, "%B.%d.%Y")


## ------------------------------------------------------------------------------------------------------------------------
final$value.x <- final$value.x %>% as.numeric() %>% round()
final$value.y <- final$value.y %>% as.numeric() %>% round()


## ------------------------------------------------------------------------------------------------------------------------
final$variable <- str_remove_all(final$variable, " -SP")
final$variable<- str_remove(final$variable, " - SP")
final$variable <- sub("\\**", "", final$variable)
final$variable <- sub("\\*", "", final$variable)
final$variable <- sub("\\s\\*", "", final$variable)
final$variable <- sub("-", "", final$variable)
final$variable<- str_remove(final$variable, "\\s\\/\\*\\*")



## ------------------------------------------------------------------------------------------------------------------------
final[is.na(final)] <- 0

final <- final %>%  arrange(variable, Mesiac)
Rok <- "2021"
final <- cbind(final, Rok)
final$Mesiac <- months(final$Mesiac)
final <- cbind("AS" = NA, final)

colnames(final) <- c("Print Type", "Month", "Print Tittle", "Circulation printed",
                    "Copies Sold", "Year" )

final$Year <- final$Year %>% as.numeric()

final <- final[, c(1, 3, 2, 4, 5,6)]





## ------------------------------------------------------------------------------------------------------------------------

write.xlsx(final, file = "Print_Radio2.xlsx",overwrite = TRUE, append = TRUE, rowNames = FALSE, showNA = FALSE)


