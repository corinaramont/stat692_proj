
# Much of the datasets are stored in a single Excel file so this allows easier
# reading for R. This function is called multiplesheets() and can 
# read multiple existing sheets/tabs in an Excel file.
# This uses the package readxl.

#last uploaded to Drive 2/1/2023

##############################################################################

#loads package
library(readxl)

multiplesheets <- function(fname) {
  
  # getting info about all excel sheets
  sheets <- readxl::excel_sheets(fname)
  tibble <- lapply(sheets, function(x) readxl::read_excel(fname, sheet = x))
  data_frame <- lapply(tibble, as.data.frame)
  
  # takes the sheet/tab names and assigns them to their respective data frames
  names(data_frame) <- sheets
  
  # returns data frame
  return(data_frame)
}

# This is an example of how you would use this function:

# specifying the path name
# path <- "/Users/mallikagupta/Desktop/Gfg.xlsx"

# call the function
# multiplesheets(path)