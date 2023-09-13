#################### read off the cell densities manually gated on the attune
library(openxlsx)
library(ggplot2)
library("scales") # for plotting time series with ggplot
data <- read.xlsx("/Projects1/Fien/BiofilmRings/parallelExp/celldensities_gated_attune.xlsx",sheet=2,colNames=TRUE)

data$Time <- as.factor(data$Time)
levels(data$Time) <- c("-0.5","0","2","4","6","8")
data$Time <- as.numeric(as.character(data$Time))

# Average cell density and sd for technical triplates
data.sum <- aggregate(x=data$Density,
                      by=list(data$Place,data$Condition, data$Time),
                      FUN= function(x) c(avg = mean(x), sdev = sd(x)))
data.sum <- data.frame(cbind(data.sum[,1:3],data.sum$x[,1:2]))
colnames(data.sum) <- c("Place", "Condition", "Time","Avg.dens","Sd.dens")

# Plot
plot <- ggplot(data=data.sum)+
  geom_point(aes(x=Time,y=Avg.dens, colour = Condition, fill = Condition), shape = 21, size=5, alpha = 1)+
  geom_line(aes(x=Time,y=Avg.dens, group = Condition, colour = Condition), size = 2, show.legend = FALSE)+
  facet_grid(rows = vars(Place), scales = "free_y")+
  geom_errorbar(aes(ymin=Avg.dens-Sd.dens,ymax=Avg.dens+Sd.dens, x = Time),width=0.2)+
  scale_y_continuous(trans = log10_trans(),labels=scientific,limits = c(10000,1000000)) +
  #scale_fill_manual(values = colours)+
  #scale_colour_manual(values = colours)+
  labs(x="Time (hours)", y = "Average density (cells/mL)")+
  theme_bw(base_size = 30, base_family = "")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(strip.background = element_rect(colour = "black", fill = "#EFEFEE"))
plot

# Export
dpi=300
png("/Projects1/Fien/BiofilmRings/parallelExp/gated_attune_plot.png",width=12*dpi,height=8*dpi,res=dpi)
plot
dev.off()

# Making a grid per condition
Blanc <- data.sum[data.sum$Condition == "BLC",]
Blank <- data.sum[data.sum$Condition == "BLK",]
pHdec <- data.sum[data.sum$Condition == "phD",]
pHinc <- data.sum[data.sum$Condition == "Phi",]
Cladd <- data.sum[data.sum$Condition == "ClA",]

Chloor <- rbind(Blank,Cladd)
for(i in 1: length(Chloor$Condition)){
  if (Chloor$Condition[i] == "BLK"){
    Chloor$Condition[i] <- "Blank"
  }
  else if (Chloor$Condition[i] == "ClA"){
    Chloor$Condition[i] <- "Condition"
  } 
  Chloor$Treatment[i] <- "Cl addition"
}

pHdec <- rbind(Blank,pHdec)
for(i in 1: length(pHdec$Condition)){
  if (pHdec$Condition[i] == "BLK"){
    pHdec$Condition[i] <- "Blank"
  }
  else if (pHdec$Condition[i] == "phD"){
   pHdec$Condition[i] <- "Condition"
  } 
  pHdec$Treatment[i] <- "pH decrease"
}

pHinc <- rbind(Blanc, pHinc)
for(i in 1: length(pHinc$Condition)){
  if (pHinc$Condition[i] == "BLC"){
    pHinc$Condition[i] <- "Blank"
  }
  else if (pHinc$Condition[i] == "Phi"){
    pHinc$Condition[i] <- "Condition"
  } 
  pHinc$Treatment[i] <- "pH increase"
}

Data.sum.new <- rbind(Chloor, pHdec, pHinc)

# Adapt levels
Data.sum.new$Time <- as.character(as.factor(Data.sum.new$Time))
Data.sum.new <- Data.sum.new[Data.sum.new$Time != "-0.5",]

# Adapt levels
Data.sum.new$Time <- as.numeric((Data.sum.new$Time))
Data.sum.new$Place <- as.factor(Data.sum.new$Place)
levels(Data.sum.new$Place) <- c("Treated groundwater", "Treated surface water")

colours <- c("#A7D5E7","#DAF7A6")
fancy_scientific <- function(l) {
  # turn in to character string in scientific notation
  l <- format(l, scientific = TRUE)
  # quote the part before the exponent to keep all the digits
  l <- gsub("^(.*)e", "'\\1'e", l)
  # turn the 'e+' into plotmath format
  l <- gsub("e", "%*%10^", l)
  # return this as an expression
  parse(text=l)
}
plot <- ggplot(data=Data.sum.new)+
  geom_point(aes(x=Time,y=Avg.dens, fill = Condition), shape = 21, size=4, alpha = 1)+
  geom_line(aes(x=Time,y=Avg.dens, group = Condition, colour = Condition), size = 1.5, show.legend = FALSE)+
  facet_grid(cols = vars(Treatment), rows = vars(Place), scales = "free_y")+
  geom_errorbar(aes(ymin=Avg.dens-Sd.dens,ymax=Avg.dens+Sd.dens, x = Time, group = Condition),width=0.4)+
  scale_y_continuous(labels=fancy_scientific, breaks = c(10000, 500000, 1000000), limits = c(10000,1000000)) +
  scale_fill_manual(values = colours)+
  scale_colour_manual(values = colours)+
  labs(x="Time (hours)", y = "Average density (cells/mL)")+
  theme_bw(base_size = 30, base_family = "")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(strip.background = element_rect(colour = "black", fill = "#EFEFEE"))
plot

# Export
dpi=300
png("/Projects1/Fien/BiofilmRings/parallelExp/figures/conditions.png",width=14*dpi,height=10*dpi,res=dpi)
plot
dev.off()



# Calculating deltas compared with d0
Blanc <- data[data$Condition == "BLC",]
Blank <- data[data$Condition == "BLK",]
pHdec <- data[data$Condition == "phD",]
pHinc <- data[data$Condition == "Phi",]
Cladd <- data[data$Condition == "ClA",]

for (i in 1:length(Blanc$Place)){
  if ((Blanc$Place[i] == "SK") & (Blanc$Time[i] == "0")){
    D0_SK <- mean(Blanc$Density[i])     
  } else if ((Blanc$Place[i] == "OT") & (Blanc$Time[i] == "0")){
    D0_OT <- mean(Blanc$Density[i])     
  }
  if (Blanc$Place[i] == "SK"){
    Blanc$Reldens[i] <- Blanc$Density[i] - D0_SK
  } else
    Blanc$Reldens[i] <- Blanc$Density[i] - D0_OT
}

for (i in 1:length(Blank$Place)){
  if ((Blank$Place[i] == "SK") & (Blank$Time[i] == "0")){
    D0_SK <- mean(Blank$Density[i])     
  } else if ((Blank$Place[i] == "OT") & (Blank$Time[i] == "0")){
    D0_OT <- mean(Blank$Density[i])     
  }
  if (Blank$Place[i] == "SK"){
    Blank$Reldens[i] <- Blank$Density[i] - D0_SK
  } else
    Blank$Reldens[i] <- Blank$Density[i] - D0_OT
}

for (i in 1:length(pHdec$Place)){
  if ((pHdec$Place[i] == "SK") & (pHdec$Time[i] == "0")){
    D0_SK <- mean(pHdec$Density[i])     
  } else if ((pHdec$Place[i] == "OT") & (pHdec$Time[i] == "0")){
    D0_OT <- mean(pHdec$Density[i])     
  }
  if (pHdec$Place[i] == "SK"){
    pHdec$Reldens[i] <- pHdec$Density[i] - D0_SK
  } else
    pHdec$Reldens[i] <- pHdec$Density[i] - D0_OT
}

for (i in 1:length(pHinc$Place)){
  if ((pHinc$Place[i] == "SK") & (pHinc$Time[i] == "0")){
    D0_SK <- mean(pHinc$Density[i])     
  } else if ((pHinc$Place[i] == "OT") & (pHinc$Time[i] == "0")){
    D0_OT <- mean(pHinc$Density[i])     
  }
  if (pHinc$Place[i] == "SK"){
    pHinc$Reldens[i] <- pHinc$Density[i] - D0_SK
  } else
    pHinc$Reldens[i] <- pHinc$Density[i] - D0_OT
}

for (i in 1:length(Cladd$Place)){
  if ((Cladd$Place[i] == "SK") & (Cladd$Time[i] == "0")){
    D0_SK <- mean(Cladd$Density[i])     
  } else if ((Cladd$Place[i] == "OT") & (Cladd$Time[i] == "0")){
    D0_OT <- mean(Cladd$Density[i])     
  }
  if (Cladd$Place[i] == "SK"){
    Cladd$Reldens[i] <- Cladd$Density[i] - D0_SK
  } else
    Cladd$Reldens[i] <- Cladd$Density[i] - D0_OT
}


# DELTA's 
Delta <- rbind(Blanc,Blank,Cladd,pHdec,pHinc)

# Average delta's and sd for technical triplates
Delta.sum <- aggregate(x=Delta$Reldens,by=list(Delta$Place,Delta$Condition, Delta$Time),
                       FUN= function(x) c(avg = mean(x), sdev = sd(x)))
Delta.sum <- data.frame(cbind(Delta.sum[,1:3],Delta.sum$x[,1:2]))
colnames(Delta.sum) <- c("Place", "Condition", "Time","Avg.delta","Sd.delta")


# Adapt levels
Delta.sum$Time <- as.factor(Delta.sum$Time)
levels(Delta.sum$Time) <- c("-0.5","0","2","4","6","8")
Delta.sum$Time <- as.numeric(as.character(Delta.sum$Time))

Delta.sum.Cl <- Delta.sum[Delta.sum$Condition== "ClA" | Delta.sum$Condition == "BLK",]
for(i in 1: length(Delta.sum.Cl$Condition)){
  if (Delta.sum.Cl$Condition[i] == "BLK"){
    Delta.sum.Cl$Condition[i] <- "Blank"
  }
  else if (Delta.sum.Cl$Condition[i] == "ClA"){
    Delta.sum.Cl$Condition[i] <- "Condition"
  } 
  Delta.sum.Cl$Treatment[i] <- "HOCl addition"
}

Delta.sum.pHdec <- Delta.sum[Delta.sum$Condition == "phD" | Delta.sum$Condition == "BLK",]
for(i in 1: length(Delta.sum.pHdec$Condition)){
  if (Delta.sum.pHdec$Condition[i] == "BLK"){
    Delta.sum.pHdec$Condition[i] <- "Blank"
  }
  else if (Delta.sum.pHdec$Condition[i] == "phD"){
    Delta.sum.pHdec$Condition[i] <- "Condition"
  } 
  Delta.sum.pHdec$Treatment[i] <- "LSI = -0.50"
}

Delta.sum.pHin <- Delta.sum[Delta.sum$Condition == "Phi" | Delta.sum$Condition == "BLC",]
for(i in 1: length(Delta.sum.pHin$Condition)){
  if (Delta.sum.pHin$Condition[i] == "BLC"){
    Delta.sum.pHin$Condition[i] <- "Blank"
  }
  else if (Delta.sum.pHin$Condition[i] == "Phi"){
    Delta.sum.pHin$Condition[i] <- "Condition"
  } 
  Delta.sum.pHin$Treatment[i] <- "LSI = 0.30"
}


Delta.sum.new <- rbind(Delta.sum.Cl, Delta.sum.pHdec, Delta.sum.pHin)
# Adapt levels
Delta.sum.new$Time <- as.character(as.factor(Delta.sum.new$Time))
Delta.sum.new <- Delta.sum.new[Delta.sum.new$Time != "-0.5",]

# Adapt levels
Delta.sum.new$Time <- as.numeric((Delta.sum.new$Time))
Delta.sum.new$Place <- as.factor(Delta.sum.new$Place)
levels(Delta.sum.new$Place) <- c("Treated groundwater", "Treated surface water")

library(ggpubr)

# Split the data into subsets based on Treatment and Place
subset_list <- split(Delta.sum.new, list(Delta.sum.new$Treatment, Delta.sum.new$Place, Delta.sum.new$Condition))

# Calculate the regression coefficients and slope
lm_results <- lapply(subset_list, function(subset) {
  lm_model <- lm(Avg.delta ~ Time, data = subset)
  coefficients <- coef(lm_model)
  slope <- coefficients[2]
  r_squared <- summary(lm_model)$r.squared
  residuals <- residuals(lm_model)
  Condition <- unique(subset$Condition)
  Treatment <- unique(subset$Treatment)
  Place <- unique(subset$Place) # Get the unique Condition value
  return(data.frame(slope = slope, r_squared, residuals, Condition = Condition, Treatment = Treatment, Place = Place))
})

# Perform the comparison (e.g., hypothesis test, visualization)

# Combine the slope values into a single data frame
slope_df <- do.call(rbind, lm_results)
row.names(slope_df) <- NULL

# Calculate the y-coordinate for text labels based on condition
slope_df$y_pos <- with(slope_df, ave(slope, Condition, FUN = max) + cumsum(c(0, diff(slope))))
for (i in 1:length(slope_df$slope)){
  if (slope_df$Condition[i] == "Condition"){
    slope_df$y_pos[i] <- 400000
    }else{
      slope_df$y_pos[i] <- -200000
  }
}

plot <- ggplot(data=Delta.sum.new, aes(x=Time,y=Avg.delta))+
  geom_point(aes(fill = Condition), shape = 21, size=2, alpha = 1)+
  #geom_line(aes(group = Condition, colour = Condition), size = 1.5, show.legend = FALSE)+
  facet_grid(cols = vars(Treatment), rows = vars(Place), scales = "free_y")+
  geom_errorbar(aes(ymin=Avg.delta-Sd.delta,ymax=Avg.delta+Sd.delta, x = Time, group = Condition),width=0.2)+
  geom_smooth(aes(x=Time,y=Avg.delta, fill = Condition, colour = Condition), method=lm)+ #add linear trend line
  geom_label(data=slope_df, inherit.aes=TRUE, aes(x = 3.2, y = y_pos, color = Condition,
                                                           label=paste("slope =",round(slope, 0),",","R² =",round(r_squared, 2))), color = "black")+
  scale_y_continuous(labels=fancy_scientific, limits = c(-300000,500000)) +
  scale_fill_manual(values = colours)+
  scale_colour_manual(values = colours)+
  labs(x="Time (hours)", y = "Average relative bulk density (cells/mL)")+
  theme_bw(base_size = 25, base_family = "")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(strip.background = element_rect(colour = "black", fill = "#EFEFEE"))
plot

# Export
dpi=300
png("/Projects1/Fien/BiofilmRings/parallelExp/figures/relative trendline.png",width=14*dpi,height=10*dpi,res=dpi)
plot
dev.off()

# Calculate similarity between blank - condition
# Extract the subsets for the specific treatment and place
treatment <- "HOCl addition"
place <- "Treated groundwater"
condition_blank <- "Blank"
condition_other <- "Condition"

# Subset for blank condition
subset_blank <- subset(slope_df, Treatment == treatment & Place == place & Condition == condition_blank)

# Subset for other condition
subset_other <- subset(slope_df, Treatment == treatment & Place == place & Condition == condition_other)

# Extract the residuals for blank condition and other condition
residuals_blank <- subset_blank$residuals
residuals_other <- subset_other$residuals
slope_blank <- subset_blank$slope
slope_other <- subset_other$slope

# Compare the residuals using different metrics and do statistics
mean_abs_diff <- mean(abs(residuals_blank - residuals_other))
mean_squared_diff <- mean((residuals_blank - residuals_other)^2)
correlation <- cor(residuals_blank, residuals_other)
   
residuals_comparison <- t.test(residuals_other^2, residuals_blank^2)
residuals_comparison

###
# Obtain the data for each condition
data_condition <- subset(Delta.sum.new, Treatment == treatment & Place == place & Condition == "Condition")
data_blank <- subset(Delta.sum.new, Treatment == treatment & Place == place & Condition == "Blank")

# Obtain the response variable for each condition
response_condition <- data_condition$Avg.delta
response_blank <- data_blank$Avg.delta

# Fit the linear regression model for the "condition" condition
model_condition <- lm(Avg.delta ~ Time, data = data_condition)
model_blank <- lm(Avg.delta ~ Time, data = data_blank)

# Calculate the observed difference in slopes
observed_diff <- coef(model_condition)[[2]] - coef(model_blank)[[2]]

# Set the number of bootstrap iterations
num_iterations <- 1000

# Create an empty vector to store the bootstrapped differences
bootstrapped_diffs <- numeric(num_iterations)

# Perform bootstrapping
for (i in 1:num_iterations) {
  # Resample the data points with replacement
  resampled_data_condition <- data_condition[sample(nrow(data_condition), replace = TRUE), ]
  resampled_data_blank <- data_blank[sample(nrow(data_blank), replace = TRUE), ]
  
  # Fit the linear regression models with the resampled data
  model_resampled_condition <- lm(Avg.delta ~ Time, data = resampled_data_condition)
  model_resampled_blank <- lm(Avg.delta ~ Time, data = resampled_data_blank)
  
  # Calculate the bootstrapped difference in slopes
  bootstrapped_diff <- coef(model_resampled_condition)[[2]] - coef(model_resampled_blank)[[2]]
  
  # Store the bootstrapped difference
  bootstrapped_diffs[i] <- bootstrapped_diff
}

# Remove NAs from the bootstrapped differences vector
bootstrapped_diffs <- bootstrapped_diffs[!is.na(bootstrapped_diffs)]

# Calculate the p-value
p_value <- mean(abs(bootstrapped_diffs) >= abs(observed_diff))
p_value






# Split the data into subsets based on Treatment and Place
subset_list <- split(Delta.sum.new, list(Delta.sum.new$Treatment, Delta.sum.new$Place, Delta.sum.new$Condition))

# Apply lm() function to each subset and extract coefficients and R-squared values
lm_models <- lapply(subset_list, function(subset) {
  lm_model <- lm(Avg.delta ~ Time, data = subset)
  coefficients <- coef(lm_model)
  r_squared <- summary(lm_model)$r.squared
  return(data.frame(coefficients = coefficients, r_squared = r_squared))
})

# Combine the coefficients and R-squared values into a single data frame
result_df <- do.call(rbind, lm_models)

# Print or view the data frame
print(result_df)

###





###### CELL RELEASE RATE #####
for (i in 1:length(Blanc$Place)){
  if ((Blanc$Place[i] == "SK") & (Blanc$Time[i] == "0")){
    D0_SK <- mean(Blanc$Density[i])     
  } else if ((Blanc$Place[i] == "OT") & (Blanc$Time[i] == "0")){
    D0_OT <- mean(Blanc$Density[i])     
  }
  if (Blanc$Place[i] == "SK"){
    Blanc$RicoDens[i] <- (Blanc$Density[i] - D0_SK)/Blanc$Time[i]
  } else
    Blanc$RicoDens[i] <- (Blanc$Density[i] - D0_OT)/Blanc$Time[i]
}

for (i in 1:length(Blank$Place)){
  if ((Blank$Place[i] == "SK") & (Blank$Time[i] == "0")){
    D0_SK <- mean(Blank$Density[i])     
  } else if ((Blank$Place[i] == "OT") & (Blank$Time[i] == "0")){
    D0_OT <- mean(Blank$Density[i])     
  }
  if (Blank$Place[i] == "SK"){
    Blank$RicoDens[i] <- (Blank$Density[i] - D0_SK)/Blank$Time[i]
  } else
    Blank$RicoDens[i] <- (Blank$Density[i] - D0_OT)/Blank$Time[i]
}

for (i in 1:length(pHdec$Place)){
  if ((pHdec$Place[i] == "SK") & (pHdec$Time[i] == "0")){
    D0_SK <- mean(pHdec$Density[i])     
  } else if ((pHdec$Place[i] == "OT") & (pHdec$Time[i] == "0")){
    D0_OT <- mean(pHdec$Density[i])     
  }
  if (pHdec$Place[i] == "SK"){
    pHdec$RicoDens[i] <- (pHdec$Density[i] - D0_SK)/pHdec$Time[i]
  } else
    pHdec$RicoDens[i] <- (pHdec$Density[i] - D0_OT)/pHdec$Time[i]
}

for (i in 1:length(pHinc$Place)){
  if ((pHinc$Place[i] == "SK") & (pHinc$Time[i] == "0")){
    D0_SK <- mean(pHinc$Density[i])     
  } else if ((pHinc$Place[i] == "OT") & (pHinc$Time[i] == "0")){
    D0_OT <- mean(pHinc$Density[i])     
  }
  if (pHinc$Place[i] == "SK"){
    pHinc$RicoDens[i] <- (pHinc$Density[i] - D0_SK)/pHinc$Time[i]
  } else
    pHinc$RicoDens[i] <- (pHinc$Density[i] - D0_OT)/pHinc$Time[i]
}

for (i in 1:length(Cladd$Place)){
  if ((Cladd$Place[i] == "SK") & (Cladd$Time[i] == "0")){
    D0_SK <- mean(Cladd$Density[i])     
  } else if ((Cladd$Place[i] == "OT") & (Cladd$Time[i] == "0")){
    D0_OT <- mean(Cladd$Density[i])     
  }
  if (Cladd$Place[i] == "SK"){
    Cladd$RicoDens[i] <- (Cladd$Density[i] - D0_SK)/Cladd$Time[i]
  } else
    Cladd$RicoDens[i] <- (Cladd$Density[i] - D0_OT)/Cladd$Time[i]
}


# DELTA's 
Rico <- rbind(Blanc,Blank,Cladd,pHdec,pHinc)
for (i in 1:length(Rico$RicoDens)){
  if (Rico$RicoDens[i] == "NaN"){
    Rico$RicoDens[i] <- 0
  }
}

# Average delta's and sd for technical triplates
Rico.sum <- aggregate(x=Rico$RicoDens,by=list(Rico$Place,Rico$Condition, Rico$Time),
                       FUN= function(x) c(avg = mean(x), sdev = sd(x)))
Rico.sum <- data.frame(cbind(Rico.sum[,1:3],Rico.sum$x[,1:2]))
colnames(Rico.sum) <- c("Place", "Condition", "Time","Avg.rico","Sd.rico")

# Adapt levels
Rico.sum$Time <- as.factor(Rico.sum$Time)
levels(Rico.sum$Time) <- c("-0.5","0","2","4","6","8")
Rico.sum$Time <- as.numeric(as.character(Rico.sum$Time))

Rico.sum.Cl <- Rico.sum[Rico.sum$Condition== "ClA" | Rico.sum$Condition == "BLK",]
for(i in 1: length(Rico.sum.Cl$Condition)){
  if (Rico.sum.Cl$Condition[i] == "BLK"){
    Rico.sum.Cl$Condition[i] <- "Blank"
  }
  else if (Rico.sum.Cl$Condition[i] == "ClA"){
    Rico.sum.Cl$Condition[i] <- "Condition"
  } 
  Rico.sum.Cl$Treatment[i] <- "Cl addition"
}

Rico.sum.pHdec <- Rico.sum[Rico.sum$Condition == "phD" | Rico.sum$Condition == "BLK",]
for(i in 1: length(Rico.sum.pHdec$Condition)){
  if (Rico.sum.pHdec$Condition[i] == "BLK"){
    Rico.sum.pHdec$Condition[i] <- "Blank"
  }
  else if (Rico.sum.pHdec$Condition[i] == "phD"){
    Rico.sum.pHdec$Condition[i] <- "Condition"
  } 
  Rico.sum.pHdec$Treatment[i] <- "pH decrease"
}

Rico.sum.pHin <- Rico.sum[Rico.sum$Condition == "Phi" | Rico.sum$Condition == "BLC",]
for(i in 1: length(Rico.sum.pHin$Condition)){
  if (Rico.sum.pHin$Condition[i] == "BLC"){
    Rico.sum.pHin$Condition[i] <- "Blank"
  }
  else if (Rico.sum.pHin$Condition[i] == "Phi"){
    Rico.sum.pHin$Condition[i] <- "Condition"
  } 
  Rico.sum.pHin$Treatment[i] <- "pH increase"
}


Rico.sum.new <- rbind(Rico.sum.Cl, Rico.sum.pHdec, Rico.sum.pHin)
# Adapt levels
Rico.sum.new$Time <- as.character(as.factor(Rico.sum.new$Time))
Rico.sum.new <- Rico.sum.new[Rico.sum.new$Time != "-0.5",]

# Adapt levels
Rico.sum.new$Time <- as.numeric((Rico.sum.new$Time))
Rico.sum.new$Place <- as.factor(Rico.sum.new$Place)
levels(Rico.sum.new$Place) <- c("Treated groundwater", "Treated surface water")

#Only last point (8)
Rico.sum.new.8 <- Rico.sum.new[Rico.sum.new$Time == 8 | Rico.sum.new$Time == 0, ]

plot <- ggplot(data=Rico.sum.new)+
  geom_point(aes(x=Time,y=Avg.rico, fill = Condition), shape = 21, size=4, alpha = 1)+
  geom_line(aes(x=Time,y=Avg.rico, group = Condition, colour = Condition), size = 1.5, show.legend = FALSE)+
  facet_grid(cols = vars(Treatment), rows = vars(Place), scales = "free_y")+
  geom_errorbar(aes(ymin=Avg.rico-Sd.rico,ymax=Avg.rico+Sd.rico, x = Time, group = Condition),width=0.2)+
  #scale_y_continuous(trans = log10_trans(), labels=scientific) +
  scale_fill_manual(values = colours)+
  scale_colour_manual(values = colours)+
  labs(x="Time (hours)", y = "Average relative density (cells/mL)")+
  theme_bw(base_size = 30, base_family = "")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(strip.background = element_rect(colour = "black", fill = "#EFEFEE"))
plot

# Export
dpi=300
png("/Projects1/Fien/BiofilmRings/parallelExp/figures/relative.png",width=14*dpi,height=10*dpi,res=dpi)
plot
dev.off()




#### STATISTICS
Delta_Chloor <- rbind(Cladd, Blank)
Delta_pH_dec <- rbind(pHdec, Blank)
Delta_pH_inc <- rbind(pHinc, Blanc)

SK <- Delta_pH_dec[Delta_pH_dec$Place == "SK",]
OT <- Delta_Chloor[Delta_Chloor$Place == "OT",]
SKt <- SK[SK$Time == 8.0,]
OTt <- OT[OT$Time == 8.0,]

# Check normality
library(ggpubr)
ggqqplot((OTt$Reldens))
shapiro.test((OTt$Reldens))
shapiro.test((SKt$Reldens))

# Residual analysis: residuals are normal: data is normal (yes, but with low sample size its better that they both are normal then)
# fit a model and check for normality of residuals 
model <- lm(Reldens ~ Condition, data =OTt)
shapiro.test(resid(model))

# perform Levene's test for homogeneity of variances: p < 0.05: non homogenic variances (FCM;ATP_SK); homogenic (ATP_OT)
library(car)
leveneTest(Reldens ~ Condition, data = SKt)
leveneTest(Reldens ~ Condition, data = OTt)

t.test(OTt[OTt$Condition == "ClA",]$Reldens, OTt[OTt$Condition == "BLK",]$Reldens, var.equal = FALSE)
t.test(SKt[SKt$Condition == "phD",]$Reldens, SKt[SKt$Condition == "BLK",]$Reldens, var.equal = FALSE)

wilcox.test(OTt[OTt$Condition == "ClA",]$Reldens , OTt[OTt$Condition == "BLK",]$Reldens, var.equal = FALSE)
wilcox.test(SKt[SKt$Condition == "phD",]$Reldens , SKt[SKt$Condition == "BLK",]$Reldens, var.equal = FALSE)



### What is the blanc and blanks are pulled together?
Blank_2 <- rbind(Blank,Blanc)
# Average of the blanks
for (i in 1:length(Blank_2$Place)){
  if (Blank_2$Condition[i] == "BLC"){
    Blank_2$Condition[i] <- "BLK"
  } 
}


# DELTA's 
Delta <- rbind(Blank_2,Cladd,pHdec,pHinc)

# Average delta's and sd for technical triplates
Delta.sum <- aggregate(x=Delta$Reldens,by=list(Delta$Place,Delta$Condition, Delta$Time),
                       FUN= function(x) c(avg = mean(x), sdev = sd(x)))
Delta.sum <- data.frame(cbind(Delta.sum[,1:3],Delta.sum$x[,1:2]))
colnames(Delta.sum) <- c("Place", "Condition", "Time","Avg.delta","Sd.delta")


# Adapt levels
Delta.sum <- Delta.sum[Delta.sum$Time != "-0.5",]
Delta.sum$Time <- as.numeric(as.character(Delta.sum$Time))
Delta.sum$Condition <- as.factor(Delta.sum$Condition)
levels(Delta.sum$Condition) <- c("Blank", "Chlorine", "pH decrease", "pH increase")
Delta.sum$Place <- as.factor(Delta.sum$Place)
levels(Delta.sum$Place) <- c("Treated groundwater", "Treated surface water")
colnames(Delta.sum) <- c("Place", "Applied stressor", "Time","Avg.delta","Sd.delta")
colours <-c("#A7D5E7", "#D9A7E7", "#E7B9A7", "#B5E7A7")

plot <- ggplot(data=Delta.sum)+
  geom_point(aes(x=Time,y=Avg.delta, fill = `Applied stressor`), shape = 21, size=2.5, alpha = 1)+
  geom_line(aes(x=Time,y=Avg.delta, group = `Applied stressor`, colour = `Applied stressor`), size = 1.2, show.legend = FALSE)+
  facet_grid(cols = vars(Place), scales = "free_y")+
  geom_errorbar(aes(ymin=Avg.delta-Sd.delta,ymax=Avg.delta+Sd.delta, x = Time, group = `Applied stressor`, colour = `Applied stressor`),width=0.2)+
  #scale_y_continuous(trans = log10_trans(),labels=scientific,limits = c(10000,1000000)) +
  scale_fill_manual(values = colours)+
  scale_colour_manual(values = colours)+
  labs(x="Time (hours)", y = "Relative average density (cells/mL)")+
  theme_bw(base_size = 30, base_family = "")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(strip.background = element_rect(colour = "black", fill = "#EFEFEE"))
plot

# Export
dpi=300
png("/Projects1/Fien/BiofilmRings/parallelExp/delta_linear_blanks_together.png",width=14*dpi,height=8*dpi,res=dpi)
plot
dev.off()

###### Statistics: blancos samen nemen 
# Change BLC BLK to same name in data
for (i in 1:length(data$Place)){
  if (data$Condition[i] == "BLC"){
    data$Condition[i] <- "BLK"
  }
}

Delta <- Delta[Delta$Time != "-0.5",]

SK <- Delta[Delta$Place == "SK",]
OT <- Delta[Delta$Place == "OT",]

# Check normality
library(ggpubr)
ggqqplot(log10(SK$Density))
shapiro.test(log10(SK$Density))

# Residual analysis: residuals are normal: data is normal
# fit a model and check for normality of residuals --> DATA IS NOT NORMAL!!!
model <- lm(Density ~ Condition, data = SK)
shapiro.test(resid(model))

# Perform the Kruskal-Wallis test
kruskal.test(Density ~ Condition, data = SK)

# Perform post-hoc pairwise comparisons using the Dunn test
library(dunn.test)
dunn.test(SK$Density, SK$Condition, method = "bonferroni")


#### Statistics on all datapoints per condition per place ####
data.Cl <- data[data$Condition== "ClA" | data$Condition == "BLK",]
data.Cl <- data.Cl[data.Cl$Time != "-0.5",]
data.pHdec <- data[data$Condition == "phD" | data$Condition == "BLK",]
data.pHin <- data[data$Condition == "Phi" | data$Condition == "BLC",]

data.Cl.SK <- data.Cl[data.Cl$Place == "SK",]
data.Cl.OT <- data.Cl[data.Cl$Place == "OT",]
data.pHdec.SK <- data.pHdec[data.pHdec$Place == "SK",]
data.pHdec.OT <- data.pHdec[data.pHdec$Place == "OT",]
data.pHinc.SK <- data.pHin[data.pHin$Place == "SK",]
data.pHinc.OT <- data.pHin[data.pHin$Place == "OT",]

# Check normality
group1 <- data.Cl.SK[data.Cl.SK$Condition == "ClA",]
group2 <- data.Cl.SK[data.Cl.SK$Condition == "BLK",]

ggqqplot(group1$Density, fill = "lightgray")
shapiro.test((group1$Density))

# Check equality of variances: 2 groups: F test, p>0.05: equal variances
# F-test, data.Cl.SK < 0.05 --> log: OK but with welch correction!
res.ftest <- var.test(log(Density) ~ Condition, data = data.Cl.SK)
res.ftest

#two sample t-test
t.test(group1$Density, group2$Density, var.equal = FALSE)

# Create boxplots for each group at different timepoints
ggplot(data.pHdec.OT, aes(x = Condition, y = Density)) +
  geom_boxplot() 

# Keer alle groepen van data in het algemeen behandelen, maar dan sowieso wordt normaliteit op elke groep apart gezien
data <- data[data$Time != "-0.5",]
SK <- data[data$Place == "SK",]
OT <- data[data$Place == "OT",]

shapiro.test((log10(SK$Density))) #not normal

# Compute the analysis of variance
res.aov <- aov(Density ~ Condition, data = OT)
# Summary of the analysis
summary(res.aov)

pairwise.t.test(SK$Density, SK$Condition,
                p.adjust.method = "BH")

kruskal.test(Density ~Condition, data = SK)


##### Keer statistiek op de deltas #####
Delta.Cl <- Delta[Delta$Condition== "ClA" | Delta$Condition == "BLK",]
Delta.Cl <- Delta.Cl[Delta.Cl$Time != "-0.5",]
Delta.pHdec <- Delta[Delta$Condition == "phD" | Delta$Condition == "BLK",]
Delta.pHin <- Delta[Delta$Condition == "Phi" | Delta$Condition == "BLC",]

Delta.Cl.SK <- Delta.Cl[Delta.Cl$Place == "SK",]
Delta.Cl.OT <- Delta.Cl[Delta.Cl$Place == "OT",]
Delta.pHdec.SK <- Delta.pHdec[Delta.pHdec$Place == "SK",]
Delta.pHdec.OT <- Delta.pHdec[Delta.pHdec$Place == "OT",]
Delta.pHinc.SK <- Delta.pHin[Delta.pHin$Place == "SK",]
Delta.pHinc.OT <- Delta.pHin[Delta.pHin$Place == "OT",]

###
group1 <- Delta.Cl.SK[Delta.Cl.SK$Condition == "ClA",]
group2 <- Delta.Cl.SK[Delta.Cl.SK$Condition == "BLK",]

ggqqplot(group1$Density, fill = "lightgray")
shapiro.test((group1$Density))

# Check equality of variances: 2 groups: F test, p>0.05: equal variances
# F-test, data.Cl.SK < 0.05 --> log: OK but with welch correction!
res.ftest <- var.test(Density ~ Condition, data = data.Cl.SK)
res.ftest

#two sample t-test
t.test(group1$Density, group2$Density, var.equal = FALSE)
