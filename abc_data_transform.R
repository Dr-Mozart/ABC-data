

setwd("H:/Private/ABC_data")

library(tidyverse)
library(openxlsx)
library(reshape2)
library(janitor)
library(lubridate)
library(panelr)


data <- read.xlsx("TVT_2021.xlsx", sheet = 'ABC SR 2021')
data <- data[-c(1:12),]
colnames(data) <- c(1:28)

remove<- data[, grep(c("Tlačený|Predaný|Mesiac|celoštátné"), data)]
colnames(remove)[2] <- "ac"
skuska <- remove[!grepl("PP|MP", remove$ac),]


skuska[1] <- NULL
ind <- apply(skuska, 1, function(x) all(is.na(x)))
skuska <- skuska[!ind,]
skuska2 <- skuska[!grepl("Mesiac|Titul|novým", skuska$ac),]


colnames(skuska)[2] <- "Month"
Mesiac <- c(NA, "Január", "Január", "Február", "Február", "Marec", "Marec", "Apríl", "Apríl", "Máj", "Máj", "Jún", "Jún", "Júl", "Júl", "August", "August", "September", "September", "Október", "Október", "November", "November", "December", "December")



skuska4 <- t(skuska2) %>% as.data.frame()
skuska4 <- cbind(Mesiac, skuska4)
names(skuska4) <- tolower(names(skuska4))
skuska4 <- skuska4 %>% row_to_names(row_number = 1)
colnames(skuska4)[1] <- "Mesiac"
head(skuska4)
skuska5 <- skuska4 %>% melt(id = "Mesiac")

predane <- skuska5[seq(2, nrow(skuska5), 2),]      
tlac <- skuska5[seq(1, nrow(skuska5), 2),]     

final <- merge(tlac, predane,
               by.x = c("Mesiac", "variable"), 
               by.y = c("Mesiac", "variable"), 
               all.x = TRUE, all.y = TRUE) 




final$Mesiac <- paste( final$Mesiac,"2.2020" ,sep=".")
final$Mesiac <- as.Date(final$Mesiac, "%B.%d.%Y")

final$value.x <- final$value.x %>% as.numeric() %>% round()
final$value.y <- final$value.y %>% as.numeric() %>% round()

final$variable <- str_remove_all(final$variable, " -SP")
final$variable<- str_remove(final$variable, " - SP")

final$variable <- sub("\\**", "", final$variable)
final$variable <- sub("\\*", "", final$variable)
final$variable <- sub("\\s\\*", "", final$variable)
final$variable <- sub("-", "", final$variable)
final$variable<- str_remove(final$variable, "\\s\\/\\*\\*")


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


write.xlsx(final, file = "Print_Radio2.xlsx",overwrite = TRUE, append = TRUE, rowNames = FALSE, showNA = FALSE)
