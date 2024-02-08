library(dplyr)

source("Functions/read_multiple_sheets.R") # load function

path = "/Users/corinaramont/Library/CloudStorage/OneDrive-Personal/stat 692/FoodAccessResearchAtlasData2019.xlsx"
data = ((multiplesheets(path))$`Food Access Research Atlas`) 