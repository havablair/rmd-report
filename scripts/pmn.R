#External script for potentially mineralizable nitrogen (PMN) graphs in farmer reports

## @knitr pmn

make_pmn_graph <- function(reg, farmer.name){
  
  ###DATA######
  
  data <- read.csv("./raw-data/pmn-data.csv", stringsAsFactors = FALSE)
  
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
  
  ####CALCS#####
  
  #farm data - calculate mean, sd
  farm.means <- indiv.data %>% 
    group_by(group, position) %>% 
    summarise(mean = mean(pmn_mg_kg, na.rm = T), sd = sd(pmn_mg_kg, na.rm = T))
  
  #region data- calculate mean, sd 
  reg.means <- reg.data %>% 
    group_by(group, position) %>% 
    summarise(mean = mean(pmn_mg_kg, na.rm = T), sd = sd(pmn_mg_kg, na.rm = T))
  
  all.means <- bind_rows(reg.means, farm.means)
  
  all.means$group <- factor(all.means$group, levels = c("Your Farm", "Regional Farms", "Your Natural\nArea", "Regional\nNatural Areas"))
  
  #####GRAPH#####
  
  g <- ggplot() +
    
    geom_errorbar(data = all.means, aes(x = group, ymax = mean + sd, ymin = mean - sd, color = group), width = 0.5, size = 1, position = "dodge", show.legend = FALSE) + 
    
    geom_point(data = all.means, aes(x = group, y = mean), size = 3, show.legend = FALSE) + 
    
    geom_jitter(data = all.points, aes(x = group, y = pmn_mg_kg, color = group), height = 0, width = 0.15, alpha = 0.5, show.legend = FALSE) +
    
    guides(x = guide_axis(angle = -40)) + 
    
    labs(title = glue("{farmer.name} Potentially Mineralizable Nitrogen"), x = element_blank(), y = "Potentially Mineralizable N (mg N / kg)", caption = str_wrap("Error bars represent 1 standard deviation from the mean. Black points and text labels are the mean of measurements in that category. Smaller colored points represent individual sample measurements.", width = 100)) + 
    
    geom_text_repel(data = all.means, aes(x = group, y = mean, label = round(mean, digits = 1)), nudge_x = 0.4)
  
  pos.labels <- c( A = "A: Upper", B = "B: Lower")
  
  final <- g + facet_wrap(~position, label = labeller(.default = label_both, position = pos.labels, .multi_line = FALSE), scales = "free")
  
  return(final)
}
