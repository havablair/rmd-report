make_p_grid <- function(reg, farmer.name){
  
  ##### DATA #######
  
  #Read in data
  data <- read.csv("./raw-data/soil_test_data_ex.csv", stringsAsFactors = FALSE)
  
  #Goal is to get both P tests (Bray and Olsen) in same column, add a column to specify test type, then make a graph we can facet on test type. (So Olsen and Bray are displayed in a 2 x 2 grid and we can save space on the x-axis labels by only including them on the bottom plot)
  
  #Separate the metadata 
  meta <- data %>% select(Sample.ID, site, treatment, farmer, position, replicate, region)
  
  #Select just the P Bray
  p.bray <- data %>% 
    select(Sample.ID, P_Bray)
  
  #create column to specify test type Bray
  p.bray$type = "Bray"
  p.bray <- p.bray %>% rename(P = P_Bray)
  
  #Select just the P Olsen-Mehlich III
  p.olsen <- data %>% 
    select(Sample.ID, P_Olsen)
  
  #create column to specify test type Olsen-Mehlich3
  p.olsen$type = "Olsen-Mehlich3"
  p.olsen <- p.olsen %>% rename(P = P_Olsen)
  
  #combine P Bray and P Olsen data in long format (in same col)
  p.all <- rbind(p.bray, p.olsen)
  
  #bring back the metadata
  p.all <- full_join(p.all, meta, by = "Sample.ID")
  
  p.all$type <- as.factor(p.all$type)
  
  p.all$label <- paste(p.all$position, p.all$type, sep = " : ")
  
  #####DATA SUBSET######
  
  #subset the relevant regional data  
  reg.data <- subset(p.all, region == reg)
  indiv.data <- subset(p.all, farmer == farmer.name)
  
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
    group_by(group, position, type) %>% 
    summarise(mean = mean(P, na.rm = T), sd = sd(P, na.rm = T))
  
  #region data- calculate mean, sd 
  reg.means <- reg.data %>% 
    group_by(group, position, type) %>% 
    summarise(mean = mean(P, na.rm = T), sd = sd(P, na.rm = T))
  
  all.means <- bind_rows(reg.means, farm.means)
  
  all.means$group <- factor(all.means$group, levels = c("Your Farm", "Regional Farms", "Your Natural\nArea", "Regional\nNatural Areas"))
  
  
  #####GRAPH#####
  
  g <- ggplot()+
    
    geom_errorbar(data = all.means, aes(x = group, ymax = mean + sd, ymin = mean - sd, color = group), width = 0.5, size = 1, position = "dodge", show.legend = FALSE) + 
    
    geom_point(data = all.means, aes(x = group, y = mean), size = 3, show.legend = FALSE) +
    
    geom_jitter(data = all.points, aes(x = group, y = P, color = group), height = 0, width = 0.15, alpha = 0.5, show.legend = FALSE)+
    
    scale_fill_viridis_d(option = "plasma", aesthetics = c("colour", "fill"))+
    
    guides(x = guide_axis(angle = -40)) + 
    
    labs(title = glue("{farmer.name} Phosphorus (P)"), x = element_blank(), y = "Phosphorus (ppm)", caption = str_wrap("Error bars represent 1 standard deviation from the mean. Black points and text labels are the mean of measurements in that category. Smaller colored points represent individual sample measurements.", width = 100)) + 
    
    geom_text_repel(data = all.means, aes(x = group, y = mean, label = round(mean, digits = 1)), nudge_x = 0.4)
  
  pos.labels <- c(A = "A: Upper", B = "B: Lower")
  
  final <- g + facet_wrap(~position + type, label = labeller(.default = label_value, position = pos.labels, .multi_line = FALSE), scales = "free")
  
  return(final)
}
