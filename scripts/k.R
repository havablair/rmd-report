#External script for K graphs in farmer reports

#Part 1 sets up K levels for color-coding graph

#Reads a CSV containing fertility recommendations for potassium (K) derived from UMN fertilizer guidelines
#Min and Max functions defined below return a row index (integer) that is used to subset the full "bands"
#dataframe within make_k_graph function. This subsetted dataframe is used to define
# color bands to illustrate relative nutrient levels (Low/Medium/High) in the ggplot.


## @knitr

k.bands <- read.csv("./raw-data/k_bands.csv", stringsAsFactors = FALSE)

colnames(k.bands)[1] <- "level"

k.min <- function(low){
  if (low > 0 & low < 40) {
    return(1) #VL
  } else if (low > 41 & low < 80){
    return(2) #L
  } else if (low > 81 & low <120){
    return(3) #M
  } else if (low > 121 & low > 160){
    return(4) #H
  } else {
    return(5) #VH
  }}


k.max <- function(high){
  if (high > 0 & high < 40) {
    return(1) #VL
  } else if (high > 41 & high < 80){
    return(2) #L
  } else if (high > 81 & high <120){
    return(3) #M
  } else if (high > 121 & high > 160){
    return(4) #H
  } else {
    return(5) #VH
  }}

#Part 2 make K error bar graphs

make_k_graph <- function(reg, farmer.name){
  
  ###DATA######
  
  data <- read.csv("./raw-data/soil_test_data_ex.csv", stringsAsFactors = FALSE)
  
  #subset the target region data  
  reg.data <- subset(data, region == reg)
  indiv.data <- subset(data, farmer == farmer.name)
  
  #for farmer data, add group column and label "Your Farm" and "Your Natural Area" samples
  indiv.data$group <- ifelse(indiv.data$treatment != "UD", "Your Farm", "NA")
  indiv.data$group <- ifelse(indiv.data$treatment == "UD", "Your Natural Area", indiv.data$group)
  
  #for region data, add group column and label "Regional Farms" and "Regional Natural Areas" 
  reg.data$group <- ifelse(reg.data$treatment != "UD", "Regional Farms", "NA")
  reg.data$group <- ifelse(reg.data$treatment == "UD", "Regional Natural Areas", reg.data$group)
  
  #wrap the text in the group columns so it displays nicely on x-axis
  indiv.data$group <- str_wrap(indiv.data$group, width = 15)
  reg.data$group <- str_wrap(reg.data$group, width = 15)
  
  #Note!  The target farmer's data appears twice in the "all.points" dataframe.  
  #Once under group "Your Farm" and once as part of the "Regional Farms". 
  #Same for natural area samples.  This is not ideal.  
  #But it is one of the only ways I've been able to figure out how to get all the 
  #factors together so I can control the order  they appear on the x-axis (and include
  #target farmer's data as part of regional average).
  
  all.points <- bind_rows(indiv.data, reg.data)
  
  all.points$group <- factor(all.points$group, levels = c("Your Farm", "Regional Farms", "Your Natural\nArea", "Regional\nNatural Areas"))
  
  #get min and max readings to define K levels that should be color-coded on the graph
  high <- max(all.points$K)
  low <- min(all.points$K)

  #subset the generic K levels df to include only levels relevant to current dataset
  k.bands.df <- k.bands[k.min(low):k.max(high),]
  k.bands.df[1,2] <- low-5
  k.bands.df[nrow(k.bands.df),3] <- high+5
  
  ####CALCS#####
  
  #farm data - calculate mean, sd
  farm.means <- indiv.data %>% 
    group_by(group, position) %>% 
    summarise(mean = mean(K, na.rm = T), sd = sd(K, na.rm = T))
  
  #region data- calculate mean, sd 
  reg.means <- reg.data %>% 
    group_by(group, position) %>% 
    summarise(mean = mean(K, na.rm = T), sd = sd(K, na.rm = T))
  
  all.means <- bind_rows(reg.means, farm.means)
  
  all.means$group <- factor(all.means$group, levels = c("Your Farm", "Regional Farms", "Your Natural\nArea", "Regional\nNatural Areas"))
  
  #####GRAPH#####
  
  g <- ggplot() + 
    
    geom_rect(data = k.bands.df, aes(ymin = start, ymax = end, NULL, NULL, fill = level), xmin = 0, xmax = 5, colour = "white", size = 0.5, alpha = 0.3) + 
    
    scale_fill_manual(values=c("VL" = "#1a9641", "L" = "#a6d96a", "M" = "#ffffbf", "H" = "#fdae61", "VH" = "#d7191c")) +
    
    geom_errorbar(data = all.means, aes(x = group, ymax = mean + sd, ymin = mean - sd, color = group), width = 0.5, size = 1, position = "dodge", show.legend = FALSE) + 
    
    geom_point(data = all.means, aes(x = group, y = mean), size = 3, show.legend = FALSE) + 
    
    geom_jitter(data = all.points, aes(x = group, y = K, color = group), height = 0, width = 0.15, alpha = 0.5, show.legend = FALSE) +
    
    guides(x = guide_axis(angle = -40)) + 
    
    labs(title = glue("{farmer.name} Potassium (K)"), x = element_blank(), y = "Potassium (K) ppm ", caption = str_wrap("Error bars represent 1 standard deviation from the mean. Black points and text labels are the mean of measurements in that category. Smaller colored points represent individual sample measurements.", width = 100)) + 
    
    geom_text_repel(data = all.means, aes(x = group, y = mean, label = round(mean, digits = 1)), nudge_x = 0.4) + 
    
    theme_bw()
  
  pos.labels <- c( A = "A: Upper", B = "B: Lower")
  
  final <- g + facet_wrap(~position, label = labeller(.default = label_both, position = pos.labels, .multi_line = FALSE), scales = "free")
  
  return(final)
}