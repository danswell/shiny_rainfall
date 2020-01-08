install.packages("rsconnect")

rsconnect::setAccountInfo(name='danswell',
                          token='5B3CF18CC546144A594CD708B26E4F33',
                          secret='NHXA1lnUSkpDbAETqTTfCkwlLta4DF11K3tdB/+h')


getwd()
rsconnect::deployApp('E:/R git/shiny_rainfall/App', appName = "Open_data_rainfall")
Y