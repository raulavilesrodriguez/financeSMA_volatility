library(devtools)
library(blotter)
library(quantstrat)
library(quantmod)
library(FinancialInstrument)
library(PerformanceAnalytics)
library(foreach)
library(tidyverse) 
library(dplyr) 
library(TTR)
library("IRdisplay")
library(highcharter) #Interactive Plot
library(devtools)
library(yahoofinancer)
library(here)

source(here::here('datos.R'))

# ___Inputs___
from <- Sys.Date() - 4
to <- Sys.Date()
symbols <- "AMZN"
interval <- '1m'

# get data
datos_1m <- datos(symbols, from, to, interval)
datos_1m <- as.xts(datos_1m)

chartSeries(datos_1m)
#Interactive Plot
highchart(type="stock") |> 
  hc_add_series(datos_1m) |> 
  hc_add_series(SMA(datos_1m$close,n=12),name="SMA(12)") |> 
  hc_add_series(SMA(datos_1m$close,n=26),name="SMA(26)") |>
  hc_title(text=paste0("<b>Prices company: ", symbols, "</b>")) |>
  hc_add_theme(hc_theme_economist())




