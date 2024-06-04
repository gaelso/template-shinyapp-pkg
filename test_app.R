## !!! TO BE REMOVED IN PACKAGE !!!
## !!! All packages call with library should be moved to DESCRIPTION
## For Shiny app
library(htmltools)
library(shiny)
library(bslib)
library(bsicons)
library(shinyWidgets)
library(shinyjs)
library(shiny.i18n)
# library(leaflet)

## For analysis
library(dplyr)
library(purrr)
library(ggplot2)
library(readr)
library(readxl)

## For spatial analysis
library(sf)
library(terra)
## !!! END TO BE REMOVED

## Source scripts from folder R/ 
purrr::walk(list.files("R", full.names = T), source)

shiny_run_app()
