install.packages("rsconnect")

rsconnect::setAccountInfo(name='danswell',
                          token='token',
                          secret='secret')


getwd()
setwd()
rsconnect::deployApp("E:/R git/shiny_rainfall/App", appName = "Open_data_rainfall")
Y
