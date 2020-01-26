client <- IntrinioSDK::ApiClient$new()

# Configure API key authorization: ApiKeyAuth
client$configuration$apiKey <- "Ojk5NzY4MDk3MDkxZWNmMTBhYmRmMWE1ODM0NTBhOTlk"

# Setup API with client
SecurityApi <- IntrinioSDK::SecurityApi$new(client)

#Stock Prices

## Required params
identifier <- "AAPL" # Character | A Security identifier (Ticker, FIGI, ISIN, CUSIP, Intrinio ID)

## Optional params
opts <- list(
  start_date = as.Date("2018-01-01"), # Date | Return prices on or after the date
  end_date = as.Date("2019-01-01"), # Date | Return prices on or before the date
  frequency = "daily", # Character | Return stock prices in the given frequency
  page_size = 100, # Integer | The number of results to return
  next_page = NULL # Character | Gets the next page of data from a previous API call
)

response <- SecurityApi$get_security_stock_prices(identifier, opts)

##View the API environment content's data frame
View(response[["content"]][["stock_prices_data_frame"]])

##Plot stock price vs. time
response[["content"]][["stock_prices_data_frame"]] %>% ggplot(aes(x=date,y=close))+geom_line()


#Market Caps

## Required params
identifier <- "AAPL" # Character | A Security identifier (Ticker, FIGI, ISIN, CUSIP, Intrinio ID)
tag <- "marketcap" # Character | An Intrinio data tag ID or code reference [see - https://data.intrinio.com/data-tags]

## Optional params
opts <- list(
  frequency = "daily", # Character | Return historical data in the given frequency
  type = NULL, # Character | Filter by type, when applicable
  start_date = as.Date("2018-01-01"), # Date | Get historical data on or after this date
  end_date = NULL, # Date | Get historical date on or before this date
  sort_order = "desc", # Character | Sort by date `asc` or `desc`
  page_size = 200, # Integer | The number of results to return
  next_page = NULL # Character | Gets the next page of data from a previous API call
)

AAPL <- SecurityApi$get_security_historical_data(identifier, tag, opts)
AAPL[["content"]][["historical_data_data_frame"]]$stock="AAPL"

identifier <- "MSFT" # Character | A Security identifier (Ticker, FIGI, ISIN, CUSIP, Intrinio ID)

MSFT <- SecurityApi$get_security_historical_data(identifier, tag, opts)
MSFT[["content"]][["historical_data_data_frame"]]$stock="MSFT"

##Merge the Environment data frames
MCap=rbind(AAPL[["content"]][["historical_data_data_frame"]],MSFT[["content"]][["historical_data_data_frame"]])

##Plot market cap vs. time
MCap %>% ggplot(aes(x=date,y=value,color=stock))+geom_line() + ggtitle("The Race to $1T Market Cap and Beyond") + 
   ylab("Market Capitalization $") + xlab("April 2019 - Jan 2020")
