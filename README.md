# Example R Markdown Report

On May 6, 2020, I gave a webinar hosted by Soil Health Nexus to discuss how I created reproducible (and automated!) reports using R and R Markdown. This repo contains supporting materials and examples for anyone interested in producing their own reports using R Markdown and the very handy parameters feature!

You can watch a recording of the webinar [here.](https://soilhealthnexus.org/sharing-soil-health-data-with-reproducible-reports/)

Creating a parameterized report template in R Markdown allowed me to produce individualized reports with soil data for 27 farmers participating in my research project.  A similar model could be applied by anyone who wants to create versions of the same general report with different input values.

 In this repo you can find: 

 - slides from the webinar 'slides-shdata-reports-06may2020.pdf'
 - my R Markdown template  'report-template.Rmd'
 - rendered versions of the template in PDF and HTML respectively 'report-template.pdf' and 'report-template.html'
 - sub-directories that contain images, raw-data, and scripts called by the R Markdown template & scripts when rendering the reports.  You have everything you need here to play around with the reports and see how they work

 ## Getting Started 

 Prerequisites:  You need to have R and RStudio installed.  These are both free and there is extensive documentation online about how to do this.  A good starting point:  ["Getting Started with R" by Garrett Grolemund (RStudio Support)](https://support.rstudio.com/hc/en-us/articles/201141096-Getting-Started-with-R)  

 ! Note !  Parameterized reports require recent versions of the knitr package (v1.11) and rmarkdown (v0.8) pacakges.  Make sure you have these installed before attempting to run the report.

## Next Steps

If you would like to play around with this code, I recommend that you either: 

1. Fork this repo (if you are familiar with git and GitHub) OR 
2. Download the ZIP file of this repo (green button "Clone or download").  
    - Once you have the ZIP file, unzip (extract) it to a new folder in a sensible place on your computer (make sure you know where it is!)
    - Open R Studio, click File > New Project > Existing Directory.  Then browse (navigate) to wherever your unzipped  folder is and select it as your working directory.  Check the box that says "Open in new session" and then click "Create Project" 
    - Congrats!  You now have your own R Project with all the report example files.  This helps you out because your working directory is already set (via the R Project) and the template and script files will know how to find one another.  It also means that any files you create will be saved withing this project directory.   
    - Start by opening up report-template.Rmd from within your R Project session.  From there you can also check out the R scripts under the scripts/ subdirectory and get a sense of how everything fits together.

### Other Resources 

Read more about parameterized reports [here](https://rmarkdown.rstudio.com/developer_parameterized_reports.html%23parameter_types%2F )   

Take 15 minutes and watch [this presentation](https://rstudio.com/resources/rstudioconf-2018/parameterized-r-markdown-reports-with-rstudio-connect/) by Aron Atkins from RStudio::Conf 2018 about parameterized reports for some nice examples that build in complexity.  





