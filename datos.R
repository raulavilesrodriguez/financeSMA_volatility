datos <- function(symbols, from, to, interval){
  aap <- Ticker$new(symbols)
  aap$get_history(start = from, end = to, interval = interval)
}