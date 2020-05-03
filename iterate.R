
#load the tidyverse set of packages to allow us to use associated functions in our script
library(tidyverse)

#read in a csv file called "names_master_ex.csv" from the "raw-data" subfolder  
names_data <- read.csv("./raw-data/names_master_ex.csv", stringsAsFactors = FALSE)

#the CSV file we read in has many rows of data. we use the "select" function to select
#columns "farmer" and "region", and the "distinct" function to return only unique combinations of "farmer"
# and "region."  This means we now have 1 line in our data table per farmer. 
input.list <- names_data %>% 
  select(farmer, region) %>% 
  distinct(farmer, region)

#this is a user-defined function.  It needs the inputs "farmer" and "region", which we defined 
#in our object called "input.list" above.  Within the render_report function below, another
#function called "render" from the rmarkdown package accesses our Rmd template, feeds it our list of farmers and regions created above,
#and specifies to save the rendered output (a pdf) with a unique filename {farmer-name}-2019-report.pdf (note file path
#also indicates that these reports will be saved specifically in an "outputs" subfolder in your directory)

render_report = function(farmer, region){
  rmarkdown::render(
    "./farmer-report.Rmd", params = list(
      farmer = farmer,
      region = region
    ),
    output_file = paste0("./outputs/", farmer, "-2019-report.pdf")
  )
}

#we use the pwalk function from the purrr package (part of the tidyverse) to go line by line through our input.list.  The pwalk
#function applies our render_report() function to each line in the list and creates a report for each farmer, saving 
#the output according to the file path and file name given above under "output_file =" 

pwalk(input.list, render_report)