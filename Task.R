# 1. Установка необходимых библиотек и загрузка данных в формате .csv
install.packages("plyr")
install.packages("plotly")
install.packages("webshot")
webshot::install_phantomjs()
library(plyr)
library(plotly)
library(webshot)
primaryIncome <- read.csv("Primary.Income.csv")
secondaryIncome <- read.csv("Secondary.Income.csv")
serviceExport <- read.csv("Service.Export.csv")
serviceImport <- read.csv("Service.Import.csv")
tradeExport <- read.csv("Trade.Export.csv")
tradeImport <- read.csv("Trade.Import.csv")
# 2. Создание таблиц
# 2.1 balanceOfPayments - таблица с данными платежного баланса
balanceOfPayments <- join(primaryIncome,secondaryIncome, by=c("EoP","Period","Trend"))
balanceOfPayments <- join(balanceOfPayments,serviceExport, by=c("EoP","Period","Trend"))
balanceOfPayments <- join(balanceOfPayments,serviceImport, by=c("EoP","Period","Trend"))
balanceOfPayments <- join(balanceOfPayments,tradeExport, by=c("EoP","Period","Trend"))
balanceOfPayments <- join(balanceOfPayments,tradeImport, by=c("EoP","Period","Trend"))
balanceOfPayments$Avg..Urals....bbl <- balanceOfPayments$Crude.oil.export.volumes..mln.ton <- balanceOfPayments$KZT.REER <- balanceOfPayments$Government...bn <- balanceOfPayments$Individuals...bn <- balanceOfPayments$Other...bn <- balanceOfPayments$Transport...bn <- balanceOfPayments$Tourism...bn <- balanceOfPayments$Telecom...bn <-balanceOfPayments$Construction...bn <-balanceOfPayments$Consulting...bn <- NULL
balanceOfPayments$periodQuarter <- as.yearqtr(balanceOfPayments$EoP, format = "%m/%d/%y")
# 2.2 oilData - таблица с данными по нефти и курсу
oilData <- join(tradeExport, tradeImport, by=c("EoP","Period","Trend"))
oilData$Trade.export...bn <- oilData$Trade.import...bn <- NULL
# 3. Визуализация данных платежного баланса
dates <- as.Date(balanceOfPayments$EoP, "%m/%d/%y")
primaryIncome$periodQuarter <- as.yearqtr(primaryIncome$EoP, format = "%m/%d/%y")
p <- plot_ly(
  x = c(primaryIncome$periodQuarter),
  y = c(primaryIncome$Primary.income...bn),
  name = "Primary Income",
  type = "bar"
)%>%
  layout(title = 'Kazakhstan Primary Income 2005-2017 quarterly',
      yaxis = list(title = 'USD bn'), barmode = 'group') %>%
  layout(xaxis = list(autotick = FALSE, showticklabels = TRUE, title = '',barmode = 'group'))
p
s <- plot_ly(balanceOfPayments, x = balanceOfPayments$periodQuarter, y = balanceOfPayments$Services.export...bn, type = 'bar', name = 'Services Exports, USD bn',
             marker = list(color = 'rgb(55, 83, 109)')) %>%
  add_trace(y = ~balanceOfPayments$Service.import...bn, name = 'Services Imports, USD bn', marker = list(color = 'rgb(26, 118, 255)')) %>%
  layout(title = 'Kazakhstan Services Exports and Imports 2005-2017, quarterly',
         xaxis = list(
           autotick = FALSE, 
           showticklabels = TRUE,
           title = "",
           tickfont = list(
             size = 14,
             color = 'rgb(107, 107, 107)')),
         yaxis = list(
           title = 'USD bn',
           titlefont = list(
             size = 16,
             color = 'rgb(107, 107, 107)'),
           tickfont = list(
             size = 14,
             color = 'rgb(107, 107, 107)')),
         legend = list(x = 0, y = 1, bgcolor = 'rgba(255, 255, 255, 0)', bordercolor = 'rgba(255, 255, 255, 0)'),
         barmode = 'group', bargap = 0.15, bargroupgap = 0.1)
s
t <- plot_ly(balanceOfPayments, x = balanceOfPayments$periodQuarter, y = balanceOfPayments$Trade.export...bn, type = 'bar', name = 'Trade Exports, USD bn',
             marker = list(color = 'rgb(55, 83, 109)')) %>%
  add_trace(y = ~balanceOfPayments$Trade.import...bn, name = 'Trade Imports, USD bn', marker = list(color = 'rgb(26, 118, 255)')) %>%
  layout(title = 'Kazakhstan Trade Exports and Imports 2005-2017, quarterly',
         xaxis = list(
           autotick = FALSE, 
           showticklabels = TRUE,
           title = "",
           tickfont = list(
             size = 14,
             color = 'rgb(107, 107, 107)')),
         yaxis = list(
           title = 'USD bn',
           titlefont = list(
             size = 16,
             color = 'rgb(107, 107, 107)'),
           tickfont = list(
             size = 14,
             color = 'rgb(107, 107, 107)')),
         legend = list(x = 0, y = 1, bgcolor = 'rgba(255, 255, 255, 0)', bordercolor = 'rgba(255, 255, 255, 0)'),
         barmode = 'group', bargap = 0.15, bargroupgap = 0.1)
t
tmpFileIncome <- tempfile(pattern = "primaryIncome", fileext = ".png")
export(p, file = tmpFileIncome)
browseURL(tmpFileIncome)
tmpFileServices <- tempfile(pattern = "services", fileext = ".png")
export(s, file = tmpFileServices)
browseURL(tmpFileServices)
tmpFileTrade <- tempfile(pattern = "trade", fileext = ".png")
export(t, file = tmpFileTrade)
browseURL(tmpFileTrade)
# 4. Построение регрессии ('товарный экспорт' = x0 + x1*'цена на нефть' + x2*'объем экспорта нефти' + x3*'курс')
tradeExport$KZT.REER <- tradeImport$KZT.REER
regTradeExport <- lm(tradeExport$Trade.export...bn ~ tradeExport$Avg..Urals....bbl + tradeExport$Crude.oil.export.volumes..mln.ton + tradeExport$KZT.REER, tradeExport)
summary(regTradeExport)
sink("regressionSummary.txt")
print(summary(regTradeExport))
sink()