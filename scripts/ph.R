#External script for ph bar in farmer reports.  
#There is 1 function and 1 generic graphic here: 
#make_ph_graph (error bar plot), and ph_bar (makes generic colored ph bar with acid/neutral/alkaline labels)

## @knitr pmn

##Create inputs for pH bar graphic
ph_value <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
ph <- c("ph", "ph", "ph", "ph", "ph", "ph", "ph", "ph", "ph", "ph", "ph","ph","ph","ph","ph")
ph_order <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14)

ph.bar <- data.frame(ph_value, ph, ph_order)

##Graph the ph bar
ph_bar <- ggplot(ph.bar)+
  geom_col(aes(x = ph_value, y = ph, fill = ph_order), position = "stack", width = 0.5) + 
  scale_fill_gradient2(
    low = "red",
    mid = "white",
    high = "blue", 
    midpoint = 6.5, 
    aesthetics = "fill") +
  coord_cartesian(xlim = c(3, 11))+
  ylab(NULL) +
  xlab("Soil pH")+ 
  theme(axis.text.y = element_blank(), axis.ticks = element_blank(), legend.position = "none") +
  scale_x_continuous(name = "Soil pH", breaks = ph_order) +
  geom_text(aes(x = 4, y = 1, label = "Acid")) + 
  geom_text(aes(x = 7, y = 1, label = "Neutral")) + 
  geom_text(aes(x = 10, y = 1, label = "Alkaline"))


make_ph_graph <- function(reg, farmer.name){
  
  ##### DATA CLEANING #######
  
  #Read in data
  data <- read.csv("./raw-data/soil_test_data_ex.csv", stringsAsFactors = FALSE)
  
  #####DATA SUBSET######
  #subset the relevant regional & farmer data  
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
  
  #Note!  The target farmer's data appears twice in the "all.points" dataframe.  This is not ideal.  But it is one of the only ways I've been able to figure out how to get all the factors together so I can control the order  they appear on the x-axis 
  all.points <- bind_rows(indiv.data, reg.data)
  
  all.points$group <- factor(all.points$group, levels = c("Your Farm", "Regional Farms", "Your Natural\nArea", "Regional\nNatural Areas"))
  
  ####CALCS#####
  
  #farm data - calculate mean, sd
  farm.means <- indiv.data %>% 
    group_by(group, position) %>% 
    summarise(mean = mean(pH, na.rm = T), sd = sd(pH, na.rm = T))
  
  #region data- calculate mean, sd 
  reg.means <- reg.data %>% 
    group_by(group, position) %>% 
    summarise(mean = mean(pH, na.rm = T), sd = sd(pH, na.rm = T))
  
  all.means <- bind_rows(reg.means, farm.means)
  
  all.means$group <- factor(all.means$group, levels = c("Your Farm", "Regional Farms", "Your Natural\nArea", "Regional\nNatural Areas"))
  
  #####GRAPH#####
  
  g <- ggplot() +
    
    geom_errorbar(data = all.means, aes(x = group, ymax = mean + sd, ymin = mean - sd, color = group), width = 0.5, size = 1, position = "dodge", show.legend = FALSE) + 
    
    geom_point(data = all.means, aes(x = group, y = mean), size = 3, show.legend = FALSE) + 
    
    geom_jitter(data = all.points, aes(x = group, y = pH, color = group), height = 0, width = 0.15, alpha = 0.5, show.legend = FALSE) +
    
    scale_fill_viridis_d(aesthetics = c("colour", "fill")) +
    
    guides(x = guide_axis(angle = -40)) + 
    
    labs(title = glue("{farmer.name} pH"), x = element_blank(), y = "pH", caption = str_wrap("Error bars represent 1 standard deviation from the mean. Black points and text labels are the mean of measurements in that category. Smaller colored points represent individual sample measurements.", width = 100)) + 
    
    geom_text_repel(data = all.means, aes(x = group, y = mean, label = round(mean, digits = 1)), nudge_x = 0.4)
  
  pos.labels <- c( A = "A: Upper", B = "B: Lower")
  
  final <- g + facet_wrap(~position, label = labeller(.default = label_both, position = pos.labels, .multi_line = FALSE), scales = "free")
  
  return(final)
}
