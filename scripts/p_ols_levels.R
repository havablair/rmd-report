#Reads a CSV containing fertility recommendations for phosphorus (P) Olsen derived from UMN fertilizer guidelines
#for agronomic crops. Min and Max functions defined below return a row index (integer) that is used to subset the full "bands"
#dataframe within the associated graphing functions (external) for the farmer report.

## @knitr

po.bands <- read.csv("./raw-data/p_ols_bands.csv", stringsAsFactors = FALSE)

colnames(po.bands)[1] <- "level"

#takes as input low = minimum p measurement in a given vector.  
#returns the row index that includes this measurement
po.min <- function(low){
  if (low > 0 & low < 3) {
    return(1) #VL
  } else if (low > 3 & low < 7){
    return(2) #L
  } else if (low > 7 & low <11){
    return(3) #M
  } else if (low > 11 & low > 15){
    return(4) #H
  } else {
    return(5) #VH
  }}

#takes as input high = max p measurement in a given vector. 
#returns the row index from po.bands that includes this measurement
po.max <- function(high){
  if (high > 0 & high < 3) {
    return(1) #VL
  } else if (high > 3 & high < 7){
    return(2) #L
  } else if (high > 7 & high <11){
    return(3) #M
  } else if (high > 11 & high > 15){
    return(4) #H
  } else {
    return(5) #VH
  }}

#Takes as input strings with region code (RR, ST, SW, or MW) and farmer last name. 
#Returns graph of P Olsen results color coded with UMN fertility recommendations
make_p_olsen <- function(reg, farmer.name){

##### DATA CLEANING #######

#Read in data
mvtl.full <- read.csv("./raw-data/soil_test_data_ex.csv", stringsAsFactors = FALSE)

#Separate the metadata 
meta <- mvtl.full %>% select(Sample.ID, site, treatment, farmer, position, replicate, region)

#Select just the P Olsen
p.olsen <- mvtl.full %>% 
  select(Sample.ID, P_Olsen)

#create column to specify test type
p.olsen$type = "Olsen"
p.olsen <- p.olsen %>% rename(P = P_Olsen)

#bring back the metadata
p.olsen <- full_join(p.olsen, meta, by = "Sample.ID")

p.olsen$type <- as.factor(p.olsen$type)

p.olsen$label <- paste(p.olsen$position, p.olsen$type, sep = " : ")

#####DATA SUBSET######

#subset the relevant regional data  
reg.data <- subset(p.olsen, region == reg)
indiv.data <- subset(p.olsen, farmer == farmer.name)

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

#get min and max readings to define P levels that should be color coded on graph
high <- max(all.points$P)
low <- min(all.points$P)

#subset the generic P Olsen levels df to include only levels relevant to current dataset
po.bands.df <- po.bands[po.min(low):po.max(high),]
po.bands.df[1,2] <- low - 5
po.bands.df[nrow(po.bands.df),3] <- high + 5

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
  
  geom_rect(data = po.bands.df, aes(ymin = start, ymax = end, NULL, NULL, fill = level), xmin = 0, xmax = 5, alpha = 0.3) +
  
  scale_fill_manual(values=c("VL" = "#1a9641", "L" = "#a6d96a", "M" = "#ffffbf", "H" = "#fdae61", "VH" = "#d7191c"), breaks = c("VH", "H", "M", "L", "VL")) +
  
  geom_errorbar(data = all.means, aes(x = group, ymax = mean + sd, ymin = mean - sd, color = group), width = 0.5, size = 1, position = "dodge", show.legend = FALSE) + 
  
  geom_point(data = all.means, aes(x = group, y = mean), size = 3, show.legend = FALSE) +
  
  geom_jitter(data = all.points, aes(x = group, y = P, color = group), height = 0, width = 0.15, alpha = 0.75, show.legend = FALSE)+
  
  guides(x = guide_axis(angle = -40)) + 
  
  labs(title = glue("{farmer.name} Phosphorus (Olsen)"), x = element_blank(), y = "Phosphorus (ppm)", caption = str_wrap("Error bars represent 1 standard deviation from the mean. Black points and text labels are the mean of measurements in that category. Smaller colored points represent individual sample measurements.", width = 100)) + 
  
  geom_text_repel(data = all.means, aes(x = group, y = mean, label = round(mean, digits = 1)), nudge_x = 0.4) + 
  
  theme_bw()

pos.labels <- c(A = "A: Upper", B = "B:Lower")

final <- g + facet_grid(type~position, label = labeller(.default = label_value, position = pos.labels, .multi_line = FALSE), scales = "free", drop = T) 
  
return(final)
}

