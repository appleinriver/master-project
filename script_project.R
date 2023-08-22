## MASTER PROJECT ANALYSIS##

##INSTALL PACKAGES 
install.packages("Momocs")
install.packages("tidyverse")
library(Momocs)
library(tidyverse)

##shark analysis -- side view 
#import data 
shark_side <- list.files('/Users/jiaqiwang/Desktop/project /shark photo/masks', full.names = TRUE)
#as some photo cann't be automatically identified, turn auto.notcentered = FALSE
ssd <- import_jpg(shark_side,auto.notcentered = FALSE) 
coo_listpanel(ssd)
#import list
shark_list <- read.csv("shark_list.csv", header = TRUE)
#turn each character as factor ##important 
shark_list[sapply(shark_list, is.character)] <- lapply(shark_list[sapply(shark_list, is.character)], 
                                                       as.factor)
#output
ssd_m <- Out(ssd, fac = shark_list)

## modification  
ssd_mm2 <- ssd_m %>% 
           coo_sample(200) %>% #sample n coordinates among points 
           fgProcrustes()
stack(ssd_mm2, title = "Shark")
# EF analysis
ef_ss2 <- efourier(ssd_mm2,nb.h= 15, norm = FALSE) 
# PCA analysis 
pca_ss2 <- Momocs::PCA(ef_ss2)
pca_ss2 %>% Momocs::plot_PCA(~family, center_origin=T, zoom=1,
                            labelpoints = F, chull=F, palette=pal_seq_magma, legend = T, points = T)
pca_ss2 %>% Momocs::plot_PCA(~order, center_origin=T, zoom=1,
                            labelpoints = F, chull=F, palette=pal_seq_magma, legend = T, points = T)
pca_ss2 %>% Momocs::plot_PCA(~vertical_position, center_origin=T, zoom=1,
                            labelpoints = F, chull=F, palette=pal_seq_magma, legend = T, points = T)
pca_ss2 %>% Momocs::plot_PCA(~IUCN_list, center_origin=T, zoom=1,
                             labelpoints = F, chull=F, palette=pal_seq_magma, legend = T, points = T)
pca_ss2 %>% Momocs::plot_PCA(~body_shape, center_origin=T, zoom=0.8,
                             labelpoints = F, chull=F, palette=pal_seq_magma, legend = T, points = T)



## ray and skate analysis -- front view with tail 
#import data 
ray_skate_side <- list.files('/Users/jiaqiwang/Desktop/project /ray and skate photo/masks', full.names = TRUE)
rsf <- import_jpg(ray_skate_side,auto.notcentered = FALSE) 
# view shape 
coo_listpanel(rsf)
#import list
ray_skate_list <- read.csv("ray_skate_list.csv", header = TRUE)
#turn each character as factor ##important 
ray_skate_list[sapply(ray_skate_list, is.character)] <- lapply(ray_skate_list[sapply(ray_skate_list, is.character)], 
                                                       as.factor)
#output
rsf_m <- Out(rsf, fac = ray_skate_list)
## modification  
rsf_mm <- rsf_m %>% 
          coo_sample(200) %>% #sample n coordinates among points 
          fgProcrustes()
stack(rsf_mm, title = "Ray and Skate")
# EF analysis
ef_rsf <- efourier(rsf_mm, nb.h = 9,norm = FALSE) 
# PCA analysis 
pca_rsf <- Momocs::PCA(ef_rsf)
pca_rsf %>% Momocs::plot_PCA(~family, center_origin=T, zoom=1, 
                            labelpoints = F, chull=F, palette=pal_seq_magma, legend = T, points = T)
pca_rsf %>% Momocs::plot_PCA(~order, center_origin=T, zoom=1, 
                             labelpoints = F, chull=F, palette=pal_seq_magma, legend = T, points = T)
pca_rsf %>% Momocs::plot_PCA(~type, center_origin=T, zoom=1, 
                             labelpoints = F, chull=F, palette=pal_seq_magma, legend = T, points = T)
pca_rsf %>% Momocs::plot_PCA(center_origin=T, zoom=0.8, 
                             labelpoints = F, chull=F, palette=pal_seq_magma, legend = T, points = T)


##whale analysis -- side view 
#import data 
whale_side <- list.files('/Users/jiaqiwang/Desktop/project /cetacean photo/whale/whale mask/side', full.names = TRUE)
wsd <- import_jpg(whale_side,auto.notcentered = TRUE) 
# view shape 
coo_listpanel(wsd)
#import list
whale_list <- read.csv("whale_list.csv", header = TRUE)
#turn each character as factor ##important 
whale_list[sapply(whale_list, is.character)] <- lapply(whale_list[sapply(whale_list, is.character)], 
                                                       as.factor)
#output
wsd_m <- Out(wsd, fac = whale_list)
## modification  
wsd_mm <- wsd_m %>% 
  coo_sample(200) %>% #sample n coordinates among points 
  fgProcrustes()
stack(wsd_mm, title = "Whale")
# EF analysis
ef_ws <- efourier(wsd_mm,nb.h = 12, norm = FALSE) 
# PCA analysis 
pca_ws <- Momocs::PCA(ef_ws)
pca_ws %>% Momocs::plot_PCA(~family, center_origin=T, zoom=1, 
                             labelpoints = F, chull=F, palette=pal_seq_magma, legend = T, points = T)


## dolphin analysis -- side view 
#import data 
dolphin_side <- list.files('/Users/jiaqiwang/Desktop/project /cetacean photo/dolphin/dolphin mask/side', full.names = TRUE)
dsd <- import_jpg(dolphin_side,auto.notcentered = TRUE) 
# view shape 
coo_listpanel(dsd)
#import list
dolphin_list <- read.csv("dolphin_list.csv", header = TRUE)
#turn each character as factor ##important 
dolphin_list[sapply(dolphin_list, is.character)] <- lapply(dolphin_list[sapply(dolphin_list, is.character)], 
                                                       as.factor)
#output
dsd_m <- Out(dsd, fac = dolphin_list)
## modification  
dsd_mm <- dsd_m %>% 
  coo_sample(200) %>% #sample n coordinates among points 
  fgProcrustes()
stack(dsd_mm, title = "Dolphin")
# EF analysis
ef_ds <- efourier(dsd_mm,nb.h = 12,norm = FALSE) 
# PCA analysis 
pca_ds <- Momocs::PCA(ef_ds)
pca_ds %>% Momocs::plot_PCA(~family, center_origin=T, zoom=1, 
                            labelpoints = F, chull=F, palette=pal_seq_magma, legend = T, points = T)


## porpoise analysis -- side 
#import data 
porpoise_side <- list.files('/Users/jiaqiwang/Desktop/project /cetacean photo/porpoise/porpoise mask/side', full.names = TRUE)
psd <- import_jpg(porpoise_side,auto.notcentered = TRUE) 
# view shape 
coo_listpanel(psd)
#output
psd_m <- Out(psd)
## modification  
psd_mm <- psd_m %>% 
  coo_sample(200) %>% #sample n coordinates among points 
  fgProcrustes()
stack(psd_mm, title = "Porpoise")
# EF analysis
ef_ps <- efourier(psd_mm,nb.h = 13,norm = FALSE) 
# PCA analysis 
pca_ps <- Momocs::PCA(ef_ps)
pca_ps %>% Momocs::plot_PCA(center_origin=T, zoom=0.8, 
                            labelpoints = F, chull=F, palette=pal_seq_magma, points = T)


## combine whale, dolphin, and porpoise together (cetacean) -- side 
#import data 
total_side <- list.files('/Users/jiaqiwang/Desktop/project /cetacean photo/total/side', full.names = TRUE)
tsd <- import_jpg(total_side,auto.notcentered = TRUE) 
# view shape 
coo_listpanel(tsd)
#import list
total_list <- read.csv("cetacean_list.csv", header = TRUE)
#turn each character as factor ##important 
total_list[sapply(total_list, is.character)] <- lapply(total_list[sapply(total_list, is.character)], 
                                                           as.factor)
#output
tsd_m <- Out(tsd, fac = total_list)
## modification  
tsd_mm <- tsd_m %>% 
  coo_sample(200) %>% #sample n coordinates among points 
  fgProcrustes()
stack(tsd_mm, title = "Cetacean")
# EF analysis
ef_ts <- efourier(tsd_mm,nb.h = 12,norm = FALSE) 
# PCA analysis 
pca_ts <- Momocs::PCA(ef_ts)
pca_ts %>% Momocs::plot_PCA(~type, center_origin=T, zoom=1, 
                            labelpoints = F, chull=F, palette=pal_seq_magma, points = T)

##barchat showing the percentage of variation explained by principle components 
par(mfrow = c(2, 3))
var_explained_shark <- pca_ss2$sdev^2  / sum(pca_ss2$sdev^2) * 100
var_explained_shark_first_three <- var_explained_shark[1:3]
data_dfs <- data.frame(
  Principal_Component = paste0("PC", 1:3),
  Percentage_Explained = var_explained_shark_first_three
)
ggplot(data_dfs, aes(x = Principal_Component, y = Percentage_Explained)) +
  geom_bar(stat = "identity") +
  labs(title = "Shark",
       x = "Principal Component", y = "Percentage")+
  theme_classic()

var_explained_ray <- pca_rsf$sdev^2  / sum(pca_rsf$sdev^2) * 100
var_explained_ray_first_three <- var_explained_ray[1:3]
data_dfr <- data.frame(
  Principal_Component = paste0("PC", 1:3),
  Percentage_Explained = var_explained_ray_first_three
)
ggplot(data_dfr, aes(x = Principal_Component, y = Percentage_Explained)) +
  geom_bar(stat = "identity") +
  labs(title = "Ray and Skate",
       x = "Principal Component", y = "Percentage")+
  theme_classic()

var_explained_whale <- pca_ws$sdev^2  / sum(pca_ws$sdev^2) * 100
var_explained_whale_first_three <- var_explained_whale[1:3]
data_dfw <- data.frame(
  Principal_Component = paste0("PC", 1:3),
  Percentage_Explained = var_explained_whale_first_three
)
ggplot(data_dfw, aes(x = Principal_Component, y = Percentage_Explained)) +
  geom_bar(stat = "identity") +
  labs(title = "Whale",
       x = "Principal Component", y = "Percentage")+
  theme_classic()

var_explained_dolphin <- pca_ds$sdev^2  / sum(pca_ds$sdev^2) * 100
var_explained_dolphin_first_three <- var_explained_dolphin[1:3]
data_dfd <- data.frame(
  Principal_Component = paste0("PC", 1:3),
  Percentage_Explained = var_explained_dolphin_first_three
)
ggplot(data_dfd, aes(x = Principal_Component, y = Percentage_Explained)) +
  geom_bar(stat = "identity") +
  labs(title = "Dolphin",
       x = "Principal Component", y = "Percentage")+
  theme_classic()

var_explained_porpoise <- pca_ps$sdev^2  / sum(pca_ps$sdev^2) * 100
var_explained_porpoise_first_three <- var_explained_porpoise[1:3]
data_df <- data.frame(
  Principal_Component = paste0("PC", 1:3),
  Percentage_Explained = var_explained_porpoise
)
ggplot(data_df, aes(x = Principal_Component, y = Percentage_Explained)) +
  geom_bar(stat = "identity") +
  labs(title = "Porpoise",
       x = "Principal Component", y = "Percentage")+
  theme_classic()

var_explained_cetacean <- pca_ts$sdev^2 / sum(pca_ts$sdev^2) * 100
var_explained_cetacean_first_three <- var_explained_cetacean[1:3]
data_dfc <- data.frame(
  Principal_Component = paste0("PC", 1:3),
  Percentage_Explained = var_explained_cetacean_first_three
)
ggplot(data_dfc, aes(x = Principal_Component, y = Percentage_Explained)) +
  geom_bar(stat = "identity") +
  labs(title = "Cetacean",
       x = "Principal Component", y = "Percentage")+
  theme_classic()
# Load required libraries
library(ggplot2)
library(gridExtra)

# Combine the individual ggplot plots using grid.arrange()
combined_plot <- grid.arrange(
  ggplot(data_dfs, aes(x = Principal_Component, y = Percentage_Explained)) +
    geom_bar(stat = "identity") +
    labs(title = "Shark",
         x = "Principal Component", y = "Percentage") +
    ylim(0,70)+
    theme_classic(),
  
  ggplot(data_dfr, aes(x = Principal_Component, y = Percentage_Explained)) +
    geom_bar(stat = "identity") +
    labs(title = "Ray and Skate",
         x = "Principal Component", y = "Percentage") +
    ylim(0,70)+
    theme_classic(),
  
  ggplot(data_dfw, aes(x = Principal_Component, y = Percentage_Explained)) +
    geom_bar(stat = "identity") +
    labs(title = "Whale",
         x = "Principal Component", y = "Percentage") +
    ylim(0,70)+
    theme_classic(),
  
  ggplot(data_dfd, aes(x = Principal_Component, y = Percentage_Explained)) +
    geom_bar(stat = "identity") +
    labs(title = "Dolphin",
         x = "Principal Component", y = "Percentage") +
    ylim(0,70)+
    theme_classic(),
  
  ggplot(data_df, aes(x = Principal_Component, y = Percentage_Explained)) +
    geom_bar(stat = "identity") +
    labs(title = "Porpoise",
         x = "Principal Component", y = "Percentage") +
    ylim(0,70)+
    theme_classic(),
  
  ggplot(data_dfc, aes(x = Principal_Component, y = Percentage_Explained)) +
    geom_bar(stat = "identity") +
    labs(title = "Cetacean",
         x = "Principal Component", y = "Percentage") +
    ylim(0,70)+
    theme_classic(),
  
  ncol = 2  # Number of columns in the grid layout
)

# Print the combined plot
print(combined_plot)

## extra pca value 
#install packages 
install.packages("devtools")
library(devtools)
install_github("kassambara/factoextra")
library(factoextra)
#extra value for shark side pca 
pca_sd <- as.data.frame.matrix(pca_ss2$x)
write.csv(pca_sd, "PCA_shark.csv")
#extra value for ray and skate side pca 
pca_rssd <- as.data.frame.matrix(pca_rsf$x)
write.csv(pca_rssd, "PCA_ray_skate.csv")
pca_ps <- as.data.frame.matrix(pca_ps)
write.csv(pca_ps,"PCA_porpoise.csv")
porpoise_pca <- read.csv("PCA_porpoise.csv", header = TRUE)
#other species follow the similar method 

##extinction scenario_shark
#import the file and calcualte PCA volume 
require(geometry)
shark_pca <- read.csv("PCA_shark.csv",header = TRUE)
require(geometry)
shark_pca_matrix <- subset(shark_pca, select = c(PC1, PC2, PC3) )
PCA_vol_shark <- convhulln(shark_pca_matrix, options = "FA")$vol 
# keep the species remaining in the loop 
# Create an empty vector to store the results
PCA_volume_shark <- vector("numeric", 100)
PCA_ex_shark <- vector("numeric", 100)
remaining_species_shark <- list()

for (i in 1:100) {
  extinction_shark <- subset(shark_pca, select = c(species, redlistCategory))
  extinction_shark$prob.ext <- ifelse(
    extinction_shark$redlistCategory == "Data Deficient",
    sample(c(0.97, 0.97/2, 0.97/4, 0.97/8, 0.97/16), nrow(extinction_shark), replace = TRUE),
    ifelse(
      extinction_shark$redlistCategory == "Critically Endangered",
      0.97,
      ifelse(
        extinction_shark$redlistCategory == "Endangered",
        0.97/2,
        ifelse(
          extinction_shark$redlistCategory == "Vulnerable",
          0.97/4,
          ifelse(
            extinction_shark$redlistCategory == "Near Threatened",
            0.97/8,
            0.97/16
          )
        )
      )
    )
  )
  
  extinction_shark$exp.prob.ext <- runif(nrow(extinction_shark))
  extinction_shark$if.ext <- extinction_shark$exp.prob.ext < extinction_shark$prob.ext
  
  remaining_species_shark[[i]] <- extinction_shark$species[extinction_shark$if.ext == FALSE]
  
  remaining_shark_1 <- extinction_shark %>% filter(if.ext == FALSE)
  shark_pca_1 <- shark_pca %>% filter(species %in% remaining_shark_1$species)
  
  shark_pca_matrix_1 <- subset(shark_pca_1, select = c(PC1, PC2, PC3))
  
  if (nrow(shark_pca_matrix_1) >= 4) {
    PCA_volume_shark[i] <- with(convhulln(shark_pca_matrix_1, options = "FA"), vol)
    PCA_ex_shark[i] <- sum(extinction_shark$if.ext ==TRUE)
  } else {
    PCA_volume_shark[i] <- NA
  }
}

print(PCA_SR_shark)
PCA_SR_pc_shark <- 100-(PCA_SR_shark/41*100)
print(PCA_SR_pc_shark)
PCA_volume_p_shark <- (PCA_volume_shark/PCA_vol_shark)*100
print(PCA_volume_p_shark)
shark_remain_prob <- as.data.frame(((41-PCA_ex_shark)/41)*100)
#polygon plot 
# Calculate the upper quartile value
upper_quartile_shark <- quantile(PCA_volume_shark, 0.75, na.rm = TRUE)
# Rank the values in PCA_volume_shark
ranked_values_shark <- rank(PCA_volume_shark)
# Find the index of the upper quartile value
upper_index_shark <- which(ranked_values == floor(0.75 * length(PCA_volume_shark)))
# Get the corresponding remaining species for the upper quartile volume
upper_species_shark <- remaining_species_shark[upper_index]
# Print the species at the upper quartile volume
cat("Species at the upper quartile volume:", unlist(upper_species_shark), "\n")

# Calculate the lower quartile value
lower_quartile_shark <- quantile(PCA_volume_shark, 0.25, na.rm = TRUE)
# Rank the values in PCA_volume_shark
ranked_values_shark <- rank(PCA_volume_shark)
# Find the index of the lower quartile value
lower_index_shark <- which(ranked_values == floor(0.25 * length(PCA_volume_shark)))
# Get the corresponding remaining species for the lower quartile volume
lower_species_shark <- remaining_species_shark[lower_index]
# Print the species at the lower quartile volume
cat("Species at the lower quartile volume:", unlist(lower_species_shark), "\n")

upper_quartile_df1 <- shark_pca %>%
  filter(species %in% unlist(upper_species_shark))

lower_quartile_df1 <- shark_pca %>%
  filter(species %in% unlist(lower_species_shark))

whole_df1 <- shark_pca


upper_quartile_df1$range <- "Upper Quartile"
lower_quartile_df1$range <- "Lower Quartile"
whole_df1$range <- "Whole"

# Adjust the order of levels in the "range" variable to change the layer sequence
PCA_data1 <- rbind(whole_df1, upper_quartile_df1,lower_quartile_df1 )
PCA_data1$range <- factor(PCA_data1$range, levels = c("Whole", "Upper Quartile", "Lower Quartile"))

# Function to find convex hull for each group
find_hull <- function(df) df[chull(df$PC1, df$PC2), ]

hulls <- PCA_data1 %>%
  group_by(range) %>%
  do(find_hull(.))

col_list <- c("#999999","#00CDCD","#FFD700") # Colors for each range

# Plot the points and convex hulls with different colors for each range
plot1 <- ggplot(PCA_data1, aes(x = PC1, y = PC2)) +
  geom_point(aes(colour = range, shape = range)) +
  geom_polygon(data = hulls, aes(alpha = 0.5, fill = range)) +
  theme_classic() +
  scale_color_manual(values = col_list) +
  scale_fill_manual(values = col_list)+
  ggtitle("Shark")+
  guides(alpha = "none")+
  xlim(-0.035,0.075)+
  ylim(-0.035,0.055)


##extinction scenario_ray_and_skate
#import the file and calculate PCA volume 
ray_skate_pca <- read.csv("PCA_ray_skate.csv", header = TRUE)
require(geometry)
ray_skate_pca_matrix <- subset(ray_skate_pca, select = c(PC1, PC2, PC3) )
PCA_vol_ray_skate <- convhulln(ray_skate_pca_matrix, options = "FA")$vol 
# keep the species remaining in the loop with smallest the largest value 
# Create an empty vector to store the results
PCA_volume_ray_skate <- vector("numeric", 100)
PCA_ex_ray_skate <- vector("numeric", 100)
remaining_species_ray_skate <- list()
for (i in 1:100) {
  extinction_ray_skate <- subset(ray_skate_pca, select = c(species, redlistCategory))
  extinction_ray_skate$prob.ext <- ifelse(
    extinction_ray_skate$redlistCategory == "Data Deficient",
    sample(c(0.97, 0.97/2, 0.97/4, 0.97/8, 0.97/16), nrow(extinction_ray_skate), replace = TRUE),
    ifelse(
      extinction_ray_skate$redlistCategory == "Critically Endangered",
      0.97,
      ifelse(
        extinction_ray_skate$redlistCategory == "Endangered",
        0.97/2,
        ifelse(
          extinction_ray_skate$redlistCategory == "Vulnerable",
          0.97/4,
          ifelse(
            extinction_ray_skate$redlistCategory == "Near Threatened",
            0.97/8,
            0.97/16
          )
        )
      )
    )
  )
  
  extinction_ray_skate$exp.prob.ext <- runif(nrow(extinction_ray_skate))
  extinction_ray_skate$if.ext <- extinction_ray_skate$exp.prob.ext < extinction_ray_skate$prob.ext
  
  remaining_species_ray_skate[[i]] <- extinction_ray_skate$species[extinction_ray_skate$if.ext == FALSE]
  
  remaining_ray_skate_1 <- extinction_ray_skate[extinction_ray_skate$if.ext == FALSE, ]
  ray_skate_pca_1 <- ray_skate_pca[ray_skate_pca$species %in% remaining_ray_skate_1$species, ]
  
  ray_skate_pca_matrix_1 <- subset(ray_skate_pca_1, select = c(PC1, PC2, PC3))
  
  if (nrow(ray_skate_pca_matrix_1) >= 4) {
    PCA_volume_ray_skate[i] <- with(convhulln(ray_skate_pca_matrix_1, options = "FA"), vol)
    PCA_ex_ray_skate[i] <- sum(extinction_ray_skate$if.ext == TRUE)
  }
}

ray_remain_prob <- as.data.frame(((28-PCA_ex_ray_skate)/28)*100)

#polygon plot 
#Calculate the upper quartile value
upper_quartile_ray_skate <- quantile(PCA_volume_ray_skate, 0.75, na.rm = TRUE)
# Rank the values in PCA_volume_shark
ranked_values_ray_skate <- rank(PCA_volume_ray_skate)
# Find the index of the upper quartile value
upper_index_ray_skate <- which(ranked_values == floor(0.75 * length(PCA_volume_ray_skate)))
# Get the corresponding remaining species for the upper quartile volume
upper_species_ray_skate <- remaining_species_ray_skate[[upper_index]]
# Print the species at the upper quartile volume
cat("Species at the upper quartile volume:", unlist(upper_species_ray_skate), "\n")

# Calculate the lower quartile value
lower_quartile_ray_skate <- quantile(PCA_volume_ray_skate, 0.25, na.rm = TRUE)
# Rank the values in PCA_volume_shark
ranked_values_ray_skate <- rank(PCA_volume_ray_skate)
# Find the index of the lower quartile value
lower_index_ray_skate <- which(ranked_values == floor(0.25 * length(PCA_volume_ray_skate)))
# Get the corresponding remaining species for the lower quartile volume
lower_species_ray_skate <- remaining_species_ray_skate[[lower_index]]
# Print the species at the lower quartile volume
cat("Species at the lower quartile volume:", unlist(lower_species_ray_skate), "\n")

upper_quartile_df2 <- ray_skate_pca %>%
  filter(species %in% unlist(upper_species_ray_skate))

lower_quartile_df2 <- ray_skate_pca  %>%
  filter(species %in% unlist(lower_species_ray_skate))

whole_df2 <- ray_skate_pca


upper_quartile_df2$range <- "Upper Quartile"
lower_quartile_df2$range <- "Lower Quartile"
whole_df2$range <- "Whole"

PCA_data2 <- rbind(whole_df2, upper_quartile_df2,lower_quartile_df2 )
PCA_data2$range <- factor(PCA_data2$range, levels = c("Whole", "Upper Quartile", "Lower Quartile"))
# Function to find convex hull for each group
find_hull <- function(df) df[chull(df$PC1, df$PC2), ]

hulls <- PCA_data2 %>%
  group_by(range) %>%
  do(find_hull(.))


col_list <- c("#999999","#00CDCD", "#FFD700")

# Plot the points and convex hulls with different colors for each range
plot2 <- ggplot(PCA_data2, aes(x = PC1, y = PC2)) +
  geom_point(aes(colour = range, shape = range)) +
  geom_polygon(data = hulls, aes(alpha = 0.5, fill = range)) +
  theme_classic() +
  scale_color_manual(values = col_list) +
  scale_fill_manual(values = col_list)+
  ggtitle("Ray and skate")+
  guides(alpha = "none")+
  xlim(-0.035,0.075)+
  ylim(-0.035,0.055)


##extinction scenario for whales 
#extra value for whale side pca 
pca_ws <- as.data.frame.matrix(pca_ws$x)
write.csv(pca_ws, "PCA_whale.csv")
whale_pca <- read.csv("PCA_whale.csv",header = TRUE)
##extinction scenario_whale
#calculate trait diversity from PCA
require(geometry)
whale_pca_matrix <- subset(whale_pca, select = c(PC1, PC2, PC3) )
PCA_vol_whale <- convhulln(whale_pca_matrix, options = "FA")$vol 
# Create an empty vector to store the results
PCA_volume_whale <- vector("numeric", 100)
PCA_ex_whale <- vector("numeric", 100)
remaining_species_whale <- list()

for (i in 1:100) {
  extinction_whale <- subset(whale_pca, select = c(species, redlistCategory))
  extinction_whale$prob.ext <- ifelse(
    extinction_whale$redlistCategory == "Data Deficient",
    sample(c(0.97, 0.97/2, 0.97/4, 0.97/8, 0.97/16), nrow(extinction_whale), replace = TRUE),
    ifelse(
      extinction_whale$redlistCategory == "Critically Endangered",
      0.97,
      ifelse(
        extinction_whale$redlistCategory == "Endangered",
        0.97/2,
        ifelse(
          extinction_whale$redlistCategory == "Vulnerable",
          0.97/4,
          ifelse(
            extinction_whale$redlistCategory == "Near Threatened",
            0.97/8,
            0.97/16
          )
        )
      )
    )
  )
  
  extinction_whale$exp.prob.ext <- runif(nrow(extinction_whale))
  extinction_whale$if.ext <- extinction_whale$exp.prob.ext < extinction_whale$prob.ext
  
  remaining_species_whale[[i]] <- extinction_whale$species[extinction_whale$if.ext == FALSE]
  
  remaining_whale_1 <- extinction_whale[extinction_whale$if.ext == FALSE, ]
  whale_pca_1 <- whale_pca[whale_pca$species %in% remaining_whale_1$species, ]
  
  whale_pca_matrix_1 <- subset(whale_pca_1, select = c(PC1, PC2, PC3))
  
  if (nrow(whale_pca_matrix_1) >= 4) {
    PCA_volume_whale[i] <- with(convhulln(whale_pca_matrix_1, options = "FA"), vol)
    PCA_ex_whale[i] <- sum(extinction_whale$if.ext == TRUE)
  }
}

PCA_volume_whale
PCA_ex_whale
print(PCA_SR_whale)
PCA_SR_pc_whale <- 100-(PCA_SR_whale/45*100)
print(PCA_SR_pc_whale)
PCA_volume_p_whale <- (PCA_volume_whale/PCA_vol_whale)*100
print(PCA_volume_p_whale)
whale_remain_prob <- as.data.frame(((45-PCA_ex_whale)/45)*100)

#polygon plot 
# Calculate the upper quartile value
upper_quartile_whale <- quantile(PCA_volume_whale, 0.75, na.rm = TRUE)
# Rank the values in PCA_volume_whale
ranked_values_whale <- rank(PCA_volume_whale)
# Find the index of the upper quartile value
upper_index_whale <- which(ranked_values == floor(0.75 * length(PCA_volume_whale)))
# Get the corresponding remaining species for the upper quartile volume
upper_species_whale <- remaining_species_whale[[upper_index]]
# Print the species at the upper quartile volume
cat("Species at the upper quartile volume:", unlist(upper_species_whale), "\n")


# Calculate the lower quartile value
lower_quartile_whale <- quantile(PCA_volume_whale, 0.25, na.rm = TRUE)
# Rank the values in PCA_volume_ray_skate
ranked_values_whale <- rank(PCA_volume_whale)
# Find the index of the lower quartile value
lower_index_whale <- which(ranked_values == floor(0.25 * length(PCA_volume_whale)))
# Get the corresponding remaining species for the lower quartile volume
lower_species_whale <- remaining_species_whale[[lower_index]]
# Print the species at the lower quartile volume
cat("Species at the lower quartile volume:", unlist(lower_species_whale), "\n")


upper_quartile_df3 <- whale_pca %>%
  filter(species %in% unlist(upper_species_whale))
lower_quartile_df3 <- whale_pca %>%
  filter(species %in% unlist(lower_species_whale))
whole_df3 <- whale_pca

upper_quartile_df3$range <- "Upper Quartile"
lower_quartile_df3$range <- "Lower Quartile"
whole_df3$range <- "whole"

PCA_data3 <- rbind(whole_df3, upper_quartile_df3, lower_quartile_df3)
PCA_data3$range <- factor(PCA_data3$range, levels = c("whole", "Upper Quartile", "Lower Quartile"))

find_hull <- function(df) df[chull(df$PC1, df$PC2), ]

hulls <- PCA_data3 %>%
  group_by(range) %>%
  do(find_hull(.))

col_list <- c("#999999","#00CDCD", "#FFD700")

# Plot the points and convex hulls with different colors for each range
plot3 <- ggplot(PCA_data3, aes(x = PC1, y = PC2)) +
  geom_point(aes(colour = range, shape = range)) +
  geom_polygon(data = hulls, aes(alpha = 0.5, fill = range)) +
  theme_classic() +
  scale_color_manual(values = col_list) +
  scale_fill_manual(values = col_list) +
  ggtitle("whale")+
  guides(alpha = "none")+
  xlim(-0.035,0.075)+
  ylim(-0.035,0.055)


##extinction scenario for dolphin 
#extra value for dolphin side pca 
pca_ds <- as.data.frame.matrix(pca_ds$x)
write.csv(pca_ds, "PCA_dolphin.csv")
dolphin_pca <- read.csv("PCA_dolphin.csv",header = TRUE)
##extinction scenario_dolphin
#calculate trait diversity from PCA
require(geometry)
dolphin_pca_matrix <- subset(dolphin_pca, select = c(PC1, PC2, PC3) )
PCA_vol_dolphin <- convhulln(dolphin_pca_matrix, options = "FA")$vol 
# keep the species remaining in the loop with smallest the largest value 
# Create an empty vector to store the results
PCA_volume_dolphin <- vector("numeric", 100)
PCA_ex_dolphin <- vector("numeric", 100)
remaining_species_dolphin <- list()

for (i in 1:100) {
  extinction_dolphin <- subset(dolphin_pca, select = c(species, redlistCategory))
  extinction_dolphin$prob.ext <- ifelse(
    extinction_dolphin$redlistCategory == "Data Deficient",
    sample(c(0.97, 0.97/2, 0.97/4, 0.97/8, 0.97/16), nrow(extinction_dolphin), replace = TRUE),
    ifelse(
      extinction_dolphin$redlistCategory == "Critically Endangered",
      0.97,
      ifelse(
        extinction_dolphin$redlistCategory == "Endangered",
        0.97/2,
        ifelse(
          extinction_dolphin$redlistCategory == "Vulnerable",
          0.97/4,
          ifelse(
            extinction_dolphin$redlistCategory == "Near Threatened",
            0.97/8,
            0.97/16
          )
        )
      )
    )
  )
  
  extinction_dolphin$exp.prob.ext <- runif(nrow(extinction_dolphin))
  extinction_dolphin$if.ext <- extinction_dolphin$exp.prob.ext < extinction_dolphin$prob.ext
  
  remaining_species_dolphin[[i]] <- extinction_dolphin$species[extinction_dolphin$if.ext == FALSE]
  
  remaining_dolphin_1 <- extinction_dolphin[extinction_dolphin$if.ext == FALSE, ]
  dolphin_pca_1 <- dolphin_pca[dolphin_pca$species %in% remaining_dolphin_1$species, ]
  
  dolphin_pca_matrix_1 <- subset(dolphin_pca_1, select = c(PC1, PC2, PC3))
  
  if (nrow(dolphin_pca_matrix_1) >= 4) {
    PCA_volume_dolphin[i] <- with(convhulln(dolphin_pca_matrix_1, options = "FA"), vol)
    PCA_ex_dolphin[i] <- sum(extinction_dolphin$if.ext == TRUE)
  }
}

PCA_volume_p_dolphin <- (PCA_volume_dolphin/PCA_vol_dolphin)*100
dolphin_remain_prob <- as.data.frame(((29-PCA_ex_dolphin)/29)*100)

#polygon plot 
# Calculate the upper quartile value
upper_quartile_dolphin <- quantile(PCA_volume_dolphin, 0.75, na.rm = TRUE)
# Rank the values in PCA_volume_dolphin
ranked_values_dolphin <- rank(PCA_volume_dolphin)
# Find the index of the upper quartile value
upper_index_dolphin <- which(ranked_values == floor(0.75 * length(PCA_volume_dolphin)))
# Get the corresponding remaining species for the upper quartile volume
upper_species_dolphin <- remaining_species_dolphin[[upper_index]]
# Print the species at the upper quartile volume
cat("Species at the upper quartile volume:", unlist(upper_species_dolphin), "\n")


# Calculate the lower quartile value
lower_quartile_dolphin <- quantile(PCA_volume_dolphin, 0.25, na.rm = TRUE)
# Rank the values in PCA_volume_ray_skate
ranked_values_dolphin <- rank(PCA_volume_dolphin)
# Find the index of the lower quartile value
lower_index_dolphin <- which(ranked_values == floor(0.25 * length(PCA_volume_dolphin)))
# Get the corresponding remaining species for the lower quartile volume
lower_species_dolphin <- remaining_species_dolphin[[lower_index]]
# Print the species at the lower quartile volume
cat("Species at the lower quartile volume:", unlist(lower_species_dolphin), "\n")


whole_df4 <- dolphin_pca

whole_df4$range <- "whole"

upper_quartile_df4 <- dolphin_pca %>%
  filter(species %in% unlist(upper_species_dolphin))
lower_quartile_df4 <- dolphin_pca %>%
  filter(species %in% unlist(lower_species_dolphin))

upper_quartile_df4$range <- "Upper Quartile"
lower_quartile_df4$range <- "Lower Quartile"

PCA_data4 <- rbind(whole_df4, upper_quartile_df4, lower_quartile_df4)
PCA_data4$range <- factor(PCA_data4$range, levels = c("whole", "Upper Quartile", "Lower Quartile"))

find_hull <- function(df) df[chull(df$PC1, df$PC2), ]

hulls <- PCA_data4 %>%
  group_by(range) %>%
  do(find_hull(.))

col_list <- c("#999999","#00CDCD", "#FFD700")

# Plot the points and convex hulls with different colors for each range
plot4 <- ggplot(PCA_data4, aes(x = PC1, y = PC2)) +
  geom_point(aes(colour = range, shape = range)) +
  geom_polygon(data = hulls, aes(alpha = 0.5, fill = range)) +
  theme_classic() +
  scale_color_manual(values = col_list) +
  scale_fill_manual(values = col_list)+
  ggtitle("dolphin")+
  guides(alpha = "none")+
  xlim(-0.035,0.075)+
  ylim(-0.035,0.055)
 
##porpoise 
#extra value for porpoise side pca 
pca_ps <- as.data.frame.matrix(pca_ps$x)
write.csv(pca_ps, "PCA_porpoise.csv")
porpoise_pca <- read.csv("PCA_porpoise.csv",header = TRUE)


##extinction scenario for cetacean
#extra value for cetacean side pca 
pca_ts <- as.data.frame.matrix(pca_ts$x)
write.csv(pca_ts, "PCA_cetacean.csv")
cetacean_pca <- read.csv("PCA_cetacean.csv",header = TRUE)
##extinction scenario_cetacean
#calculate trait diversity from PCA
require(geometry)
PCA_volume_cetacean <- vector("numeric", 100)
PCA_ex_cetacean <- vector("numeric", 100)
remaining_species_cetacean <- list()  # Create a vector to store the results
for (i in 1:100) {
  extinction_cetacean <- subset(cetacean_pca, select = c(species, redlistCategory))
  extinction_cetacean$prob.ext <- ifelse(
    extinction_cetacean$redlistCategory == "Data Deficient",
    sample(c(0.97, 0.97/2, 0.97/4, 0.97/8, 0.97/16), nrow(extinction_cetacean), replace = TRUE),
    ifelse(
      extinction_cetacean$redlistCategory == "Critically Endangered",
      0.97,
      ifelse(
        extinction_cetacean$redlistCategory == "Endangered",
        0.97/2,
        ifelse(
          extinction_cetacean$redlistCategory == "Vulnerable",
          0.97/4,
          ifelse(
            extinction_cetacean$redlistCategory == "Near Threatened",
            0.97/8,
            0.97/16
          )
        )
      )
    )
  )
  
  extinction_cetacean$exp.prob.ext <- runif(nrow(extinction_cetacean))
  extinction_cetacean$if.ext <- extinction_cetacean$exp.prob.ext < extinction_cetacean$prob.ext
  
  remaining_species_cetacean[[i]] <- extinction_cetacean$species[extinction_cetacean$if.ext == FALSE]
  
  remaining_cetacean_1 <- extinction_cetacean[extinction_cetacean$if.ext == FALSE, ]
  cetacean_pca_1 <- cetacean_pca[cetacean_pca$species %in% remaining_cetacean_1$species, ]
  
  cetacean_pca_matrix_1 <- subset(cetacean_pca_1, select = c(PC1, PC2, PC3))
  
  if (nrow(cetacean_pca_matrix_1) >= 4) {
    PCA_volume_cetacean[i] <- with(convhulln(cetacean_pca_matrix_1, options = "FA"), vol)
    PCA_ex_cetacean[i] <- sum(extinction_cetacean$if.ext == TRUE)
  }
}

PCA_volume_p_cetacean <- (PCA_volume_cetacean/PCA_vol_cetacean)*100
cetacean_remain_prob <- as.data.frame(((80-PCA_ex_cetacean)/80)*100)
#polygon plot 
# Calculate the upper quartile value
upper_quartile_cetacean <- quantile(PCA_volume_cetacean, 0.75, na.rm = TRUE)
# Rank the values in PCA_volume_cetacean
ranked_values_cetacean <- rank(PCA_volume_cetacean)
# Find the index of the upper quartile value
upper_index_cetacean <- which(ranked_values == floor(0.75 * length(PCA_volume_cetacean)))
# Get the corresponding remaining species for the upper quartile volume
upper_species_cetacean <- remaining_species_cetacean[[upper_index]]
# Print the species at the upper quartile volume
cat("Species at the upper quartile volume:", unlist(upper_species_cetacean), "\n")


# Calculate the lower quartile value
lower_quartile_cetacean <- quantile(PCA_volume_cetacean, 0.25, na.rm = TRUE)
# Rank the values in PCA_volume_cetacean
ranked_values_cetacean <- rank(PCA_volume_cetacean)
# Find the index of the lower quartile value
lower_index_cetacean <- which(ranked_values == floor(0.25 * length(PCA_volume_cetacean)))
# Get the corresponding remaining species for the lower quartile volume
lower_species_cetacean <- remaining_species_cetacean[[lower_index]]
# Print the species at the lower quartile volume
cat("Species at the lower quartile volume:", unlist(lower_species_cetacean), "\n")

upper_quartile_df5 <- cetacean_pca %>%
  filter(species %in% unlist(upper_species_cetacean))
lower_quartile_df5 <- cetacean_pca %>%
  filter(species %in% unlist(lower_species_cetacean))

upper_quartile_df5$range <- "Upper Quartile"
lower_quartile_df5$range <- "Lower Quartile"

whole_df5 <- cetacean_pca

whole_df5$range <- "whole"

PCA_data5 <- rbind(whole_df5, upper_quartile_df5, lower_quartile_df5)
PCA_data5$range <- factor(PCA_data5$range, levels = c("whole", "Upper Quartile", "Lower Quartile"))

find_hull <- function(df) df[chull(df$PC1, df$PC2), ]

hulls <- PCA_data5 %>%
  group_by(range) %>%
  do(find_hull(.))

col_list <- c("#999999","#00CDCD", "#FFD700")

# Plot the points and convex hulls with different colors for each range
plot5 <- ggplot(PCA_data5, aes(x = PC1, y = PC2)) +
  geom_point(aes(colour = range, shape = range)) +
  geom_polygon(data = hulls, aes(alpha = 0.5, fill = range)) +
  theme_classic() +
  scale_color_manual(values = col_list) +
  scale_fill_manual(values = col_list)+
  ggtitle("cetacean")+
  guides(alpha = "none")+
  xlim(-0.035,0.075)+
  ylim(-0.035,0.055)
  
#combine the plot together 
library(ggpubr)
ggarrange(plot1, plot2, plot3, plot4, plot5, ncol=2, nrow=3,common.legend = TRUE, legend="right")

shark_extinct_half_matrix <- subset(shark_extinct_half_df, select = c(PC1, PC2, PC3) )
PCA_vol_shark_half_extinct <- convhulln(shark_extinct_half_matrix, options = "FA")$vol 
(PCA_vol_shark_half_extinct/PCA_vol_shark)*100
shark_remian_half_matrix <- subset(shark_remain_half, select = c(PC1,PC2,PC3))
PCA_vol_shark_half_remain <- convhulln(shark_remian_half_matrix, options = "FA")$vol
(PCA_vol_shark_half_remain/PCA_vol_shark)*100


## 50% extinction scenario 
shark_extinct_half <- sample(shark_pca$species, 21, replace = FALSE, prob = shark_pca$ext.prob)
shark_extinct_half_df <- shark_pca %>% filter(species %in% shark_extinct_half)
shark_remain_half <- shark_pca %>% filter(!species %in% shark_extinct_half)
shark_remian_half_matrix <- subset(shark_remain_half, select = c(PC1,PC2,PC3))
PCA_vol_shark_half_remain <- convhulln(shark_remian_half_matrix, options = "FA")$vol
PCA_vol_shark_prob <- (PCA_vol_shark_half_remain/PCA_vol_shark)*100

whole_df1 <- shark_pca
whole_df1$range <- "Whole"
shark_remain_half$range <- "50% remain"
shark_extinct_half_df$range <- "50% extinct"

PCA_half_data1 <- rbind(whole_df1, shark_remain_half, shark_extinct_half_df)
PCA_half_data1$range <- factor(PCA_half_data1$range, levels = c("Whole", "50% remain", "50% extinct"))
find_hull <- function(df) df[chull(df$PC1, df$PC2), ]

hulls <- PCA_half_data1 %>%
  group_by(range) %>%
  do(find_hull(.))


col_list <- c("#999999","#00CDCD","#FFD700")

# Plot the points and convex hulls with different colors for each range
plot_sp <- ggplot(PCA_half_data1, aes(x = PC1, y = PC2)) +
  geom_point(aes(colour = range, shape = range)) +
  geom_polygon(data = hulls, aes(alpha = 0.5, fill = range)) +
  theme_classic() +
  scale_color_manual(values = col_list) +
  scale_fill_manual(values = col_list)+
  ggtitle("Shark probability")+
  guides(alpha = "none")+
  xlim(-0.035,0.07)+
  ylim(-0.035,0.055)




shark_extinct_half_random <- sample(shark_pca$species,21, replace = FALSE)
shark_extinct_half_r_df <- shark_pca %>% filter(species %in% shark_extinct_half_random)
shark_remain_half_r <- shark_pca %>% filter(!species %in% shark_extinct_half_random)
shark_remian_half_r_matrix <- subset(shark_remain_half_r, select = c(PC1,PC2,PC3))
(PCA_vol_shark_half_r_remain/PCA_vol_shark)*100

PCA_half_r_data1 <- rbind(whole_df1, shark_remain_half_r, shark_extinct_half_r_df)
PCA_half_r_data1$range <- factor(PCA_half_r_data1$range, levels = c("Whole", "50% remain", "50% extinct"))
find_hull <- function(df) df[chull(df$PC1, df$PC2), ]

hulls <- PCA_half_r_data1 %>%
  group_by(range) %>%
  do(find_hull(.))


col_list <- c("#999999","#00CDCD","#FFD700")

# Plot the points and convex hulls with different colors for each range
plot_sr <-ggplot(PCA_half_r_data1, aes(x = PC1, y = PC2)) +
  geom_point(aes(colour = range, shape = range)) +
  geom_polygon(data = hulls, aes(alpha = 0.5, fill = range)) +
  theme_classic() +
  scale_color_manual(values = col_list) +
  scale_fill_manual(values = col_list)+
  ggtitle("Ray and Skate random")+
  guides(alpha = "none")+
  xlim(-0.035,0.07)+
  ylim(-0.035,0.055)

require(gridExtra)
plot_shark <- grid.arrange(plot1, plot_sp, plot_sr, ncol = 2)

ray_skate_remian_half_matrix <- subset(ray_skate_remain_half, select = c(PC1,PC2,PC3))
PCA_vol_ray_skate_half_remain <- convhulln(ray_skate_remian_half_matrix, options = "FA")$vol
(PCA_vol_ray_skate_half_remain/PCA_vol_ray_skate)*100

ray_skate_pca$prob.ext<- ifelse(ray_skate_pca$redlistCategory == "Data Deficient", sample(c(0.97, 0.97/2, 0.97/4, 0.97/8, 0.97/16), 1), ray_skate_pca$prob.ext)
ray_skate_extinct_half <- sample(ray_skate_pca$species, 14, replace = FALSE, prob = ray_skate_pca$prob.ext)
ray_skate_extinct_half_df <- ray_skate_pca %>% filter(species %in% ray_skate_extinct_half)
ray_skate_remain_half <- ray_skate_pca %>% filter(!species %in% ray_skate_extinct_half)
whole_df2 <- ray_skate_pca
whole_df2$range <- "Whole"
ray_skate_remain_half$range <- "50% remain"
ray_skate_extinct_half_df$range <- "50% extinct"

PCA_half_data2 <- rbind(whole_df2, ray_skate_remain_half, ray_skate_extinct_half_df)
PCA_half_data2$range <- factor(PCA_half_data2$range, levels = c("Whole", "50% remain", "50% extinct"))
find_hull <- function(df) df[chull(df$PC1, df$PC2), ]

hulls <- PCA_half_data2 %>%
  group_by(range) %>%
  do(find_hull(.))


col_list <- c("#999999","#00CDCD","#FFD700")

# Plot the points and convex hulls with different colors for each range
plot_rsp <- ggplot(PCA_half_data2, aes(x = PC1, y = PC2)) +
  geom_point(aes(colour = range, shape = range)) +
  geom_polygon(data = hulls, aes(alpha = 0.5, fill = range)) +
  theme_classic() +
  scale_color_manual(values = col_list) +
  scale_fill_manual(values = col_list)+
  ggtitle("Ray and Skate probability")+
  guides(alpha = "none")+
  xlim(-0.035,0.07)+
  ylim(-0.035,0.055)

ray_skate_remian_half_r_matrix <- subset(ray_skate_remain_half_r, select = c(PC1,PC2,PC3))
PCA_vol_ray_skate_half_r_remain <- convhulln(ray_skate_remian_half_r_matrix, options = "FA")$vol
(PCA_vol_ray_skate_half_r_remain/PCA_vol_ray_skate)*100

ray_skate_extinct_half_random <- sample(ray_skate_pca$species,14, replace = FALSE)
ray_skate_extinct_half_r_df <- ray_skate_pca %>% filter(species %in% ray_skate_extinct_half_random)
ray_skate_remain_half_r <- ray_skate_pca %>% filter(!species %in% ray_skate_extinct_half_random)
whole_df2 <- ray_skate_pca
whole_df2$range <- "Whole"
ray_skate_remain_half_r$range <- "50% remain"
ray_skate_extinct_half_r_df$range <- "50% extinct"

PCA_half_r_data2 <- rbind(whole_df2, ray_skate_remain_half_r, ray_skate_extinct_half_r_df)
PCA_half_r_data2$range <- factor(PCA_half_r_data2$range, levels = c("Whole", "50% remain", "50% extinct"))
find_hull <- function(df) df[chull(df$PC1, df$PC2), ]

hulls <- PCA_half_r_data2 %>%
  group_by(range) %>%
  do(find_hull(.))


col_list <- c("#999999","#00CDCD","#FFD700")

# Plot the points and convex hulls with different colors for each range
plot_rsr <-ggplot(PCA_half_r_data2, aes(x = PC1, y = PC2)) +
  geom_point(aes(colour = range, shape = range)) +
  geom_polygon(data = hulls, aes(alpha = 0.5, fill = range)) +
  theme_classic() +
  scale_color_manual(values = col_list) +
  scale_fill_manual(values = col_list)+
  ggtitle("Ray and Skate random")+
  guides(alpha = "none")+
  xlim(-0.035,0.07)+
  ylim(-0.035,0.055)

require(gridExtra)
plot_ray_skate <-grid.arrange(plot2, plot_rsp, plot_rsr, ncol = 2)

whale_remian_half_matrix <- subset(whale_remain_half, select = c(PC1,PC2,PC3))
PCA_vol_whale_half_remain <- convhulln(whale_remian_half_matrix, options = "FA")$vol
(PCA_vol_whale_half_remain/PCA_vol_whale)*100

whale_pca$ext.prob<- ifelse(whale_pca$redlistCategory == "Data Deficient", sample(c(0.97, 0.97/2, 0.97/4, 0.97/8, 0.97/16), 1), whale_pca$ext.prob)
whale_extinct_half <- sample(whale_pca$species, 23, replace = FALSE, prob = whale_pca$ext.prob)
whale_extinct_half_df <- whale_pca %>% filter(species %in% whale_extinct_half)
whale_remain_half <- whale_pca %>% filter(!species %in% whale_extinct_half)
whole_df3 <- whale_pca
whole_df3$range <- "Whole"
whale_remain_half$range <- "50% remain"
whale_extinct_half_df$range <- "50% extinct"

PCA_half_data3 <- rbind(whole_df3, whale_remain_half, whale_extinct_half_df)
PCA_half_data3$range <- factor(PCA_half_data3$range, levels = c("Whole", "50% remain", "50% extinct"))
find_hull <- function(df) df[chull(df$PC1, df$PC2), ]

hulls <- PCA_half_data3 %>%
  group_by(range) %>%
  do(find_hull(.))


col_list <- c("#999999","#00CDCD","#FFD700")

# Plot the points and convex hulls with different colors for each range
plot_wp <- ggplot(PCA_half_data3, aes(x = PC1, y = PC2)) +
  geom_point(aes(colour = range, shape = range)) +
  geom_polygon(data = hulls, aes(alpha = 0.5, fill = range)) +
  theme_classic() +
  scale_color_manual(values = col_list) +
  scale_fill_manual(values = col_list)+
  ggtitle("Whale probability")+
  guides(alpha = "none")+
  xlim(-0.035,0.07)+
  ylim(-0.035,0.055)

whale_remian_half_r_matrix <- subset(whale_remain_half_r, select = c(PC1,PC2,PC3))
PCA_vol_whale_half_r_remain <- convhulln(whale_remian_half_r_matrix, options = "FA")$vol
(PCA_vol_whale_half_r_remain/PCA_vol_whale)*100

whale_extinct_half_random <- sample(whale_pca$species,23, replace = FALSE)
whale_extinct_half_r_df <- whale_pca %>% filter(species %in% whale_extinct_half_random)
whale_remain_half_r <- whale_pca %>% filter(!species %in% whale_extinct_half_random)
whole_df3 <- whale_pca
whole_df3$range <- "Whole"
whale_remain_half_r$range <- "50% remain"
whale_extinct_half_r_df$range <- "50% extinct"

PCA_half_r_data3 <- rbind(whole_df3, whale_remain_half_r, whale_extinct_half_r_df)
PCA_half_r_data3$range <- factor(PCA_half_r_data3$range, levels = c("Whole", "50% remain", "50% extinct"))
find_hull <- function(df) df[chull(df$PC1, df$PC2), ]

hulls <- PCA_half_r_data3 %>%
  group_by(range) %>%
  do(find_hull(.))


col_list <- c("#999999","#00CDCD","#FFD700")

plot_wr <-ggplot(PCA_half_r_data3, aes(x = PC1, y = PC2)) +
  geom_point(aes(colour = range, shape = range)) +
  geom_polygon(data = hulls, aes(alpha = 0.5, fill = range)) +
  theme_classic() +
  scale_color_manual(values = col_list) +
  scale_fill_manual(values = col_list)+
  ggtitle("Whale random")+
  guides(alpha = "none")+
  xlim(-0.035,0.07)+
  ylim(-0.035,0.055)

require(gridExtra)
plot_whale <- grid.arrange(plot3, plot_wp, plot_wr, ncol = 2)

dolphin_remian_half_matrix <- subset(dolphin_remain_half, select = c(PC1,PC2,PC3))
PCA_vol_dolphin_half_remain <- convhulln(dolphin_remian_half_matrix, options = "FA")$vol
(PCA_vol_dolphin_half_remain/PCA_vol_dolphin)*100


dolphin_pca$ext.prob<- ifelse(dolphin_pca$redlistCategory == "Data Deficient", sample(c(0.97, 0.97/2, 0.97/4, 0.97/8, 0.97/16), 1), dolphin_pca$ext.prob)
dolphin_extinct_half <- sample(dolphin_pca$species, 15, replace = FALSE, prob = dolphin_pca$ext.prob)
dolphin_extinct_half_df <- dolphin_pca %>% filter(species %in% dolphin_extinct_half)
dolphin_remain_half <- dolphin_pca %>% filter(!species %in% dolphin_extinct_half)
whole_df4 <- dolphin_pca
whole_df4$range <- "Whole"
dolphin_remain_half$range <- "50% remain"
dolphin_extinct_half_df$range <- "50% extinct"

PCA_half_data4 <- rbind(whole_df4, dolphin_remain_half, dolphin_extinct_half_df)
PCA_half_data4$range <- factor(PCA_half_data4$range, levels = c("Whole", "50% remain", "50% extinct"))
find_hull <- function(df) df[chull(df$PC1, df$PC2), ]

hulls <- PCA_half_data4 %>%
  group_by(range) %>%
  do(find_hull(.))


col_list <- c("#999999","#00CDCD","#FFD700")

# Plot the points and convex hulls with different colors for each range
plot_dp <- ggplot(PCA_half_data4, aes(x = PC1, y = PC2)) +
  geom_point(aes(colour = range, shape = range)) +
  geom_polygon(data = hulls, aes(alpha = 0.5, fill = range)) +
  theme_classic() +
  scale_color_manual(values = col_list) +
  scale_fill_manual(values = col_list)+
  ggtitle("Dolphin probability")+
  guides(alpha = "none")+
  xlim(-0.035,0.07)+
  ylim(-0.035,0.055)


dolphin_remian_half_r_matrix <- subset(dolphin_remain_half_r, select = c(PC1,PC2,PC3))
PCA_vol_dolphin_half_r_remain <- convhulln(dolphin_remian_half_r_matrix, options = "FA")$vol
(PCA_vol_dolphin_half_r_remain/PCA_vol_dolphin)*100

dolphin_extinct_half_random <- sample(dolphin_pca$species,15, replace = FALSE)
dolphin_extinct_half_r_df <- dolphin_pca %>% filter(species %in% dolphin_extinct_half_random)
dolphin_remain_half_r <- dolphin_pca %>% filter(!species %in% dolphin_extinct_half_random)
whole_df4 <- dolphin_pca
whole_df4$range <- "Whole"
dolphin_remain_half_r$range <- "50% remain"
dolphin_extinct_half_r_df$range <- "50% extinct"

PCA_half_r_data4 <- rbind(whole_df4, dolphin_remain_half_r, dolphin_extinct_half_r_df)
PCA_half_r_data4$range <- factor(PCA_half_r_data4$range, levels = c("Whole", "50% remain", "50% extinct"))
find_hull <- function(df) df[chull(df$PC1, df$PC2), ]

hulls <- PCA_half_r_data4 %>%
  group_by(range) %>%
  do(find_hull(.))


col_list <- c("#999999","#00CDCD","#FFD700")

plot_dr <-ggplot(PCA_half_r_data4, aes(x = PC1, y = PC2)) +
  geom_point(aes(colour = range, shape = range)) +
  geom_polygon(data = hulls, aes(alpha = 0.5, fill = range)) +
  theme_classic() +
  scale_color_manual(values = col_list) +
  scale_fill_manual(values = col_list)+
  ggtitle("Dolphin random")+
  guides(alpha = "none")+
  xlim(-0.035,0.07)+
  ylim(-0.035,0.055)

plot_dolphin <- grid.arrange(plot4, plot_dp, plot_dr, ncol = 2)

cetacean_remian_half_matrix <- subset(cetacean_remain_half, select = c(PC1,PC2,PC3))
PCA_vol_cetacean_half_remain <- convhulln(cetacean_remian_half_matrix, options = "FA")$vol
(PCA_vol_cetacean_half_remain/PCA_vol_cetacean)*100

cetacean_pca$ext.prob<- ifelse(cetacean_pca$redlistCategory == "Data Deficient", sample(c(0.97, 0.97/2, 0.97/4, 0.97/8, 0.97/16), 1), cetacean_pca$ext.prob)
cetacean_extinct_half <- sample(cetacean_pca$species, 40, replace = FALSE, prob = cetacean_pca$ext.prob)
cetacean_extinct_half_df <- cetacean_pca %>% filter(species %in% cetacean_extinct_half)
cetacean_remain_half <- cetacean_pca %>% filter(!species %in% cetacean_extinct_half)
whole_df5 <- cetacean_pca
whole_df5$range <- "Whole"
cetacean_remain_half$range <- "50% remain"
cetacean_extinct_half_df$range <- "50% extinct"

PCA_half_data5 <- rbind(whole_df5, cetacean_remain_half, cetacean_extinct_half_df)
PCA_half_data5$range <- factor(PCA_half_data5$range, levels = c("Whole", "50% remain", "50% extinct"))
find_hull <- function(df) df[chull(df$PC1, df$PC2), ]

hulls <- PCA_half_data5 %>%
  group_by(range) %>%
  do(find_hull(.))


col_list <- c("#999999","#00CDCD","#FFD700")

# Plot the points and convex hulls with different colors for each range
plot_cp <- ggplot(PCA_half_data5, aes(x = PC1, y = PC2)) +
  geom_point(aes(colour = range, shape = range)) +
  geom_polygon(data = hulls, aes(alpha = 0.5, fill = range)) +
  theme_classic() +
  scale_color_manual(values = col_list) +
  scale_fill_manual(values = col_list)+
  ggtitle("Cetacean probability")+
  guides(alpha = "none")+
  xlim(-0.035,0.07)+
  ylim(-0.035,0.055)

cetacean_remian_half_r_matrix <- subset(cetacean_remain_half_r, select = c(PC1,PC2,PC3))
PCA_vol_cetacean_half_r_remain <- convhulln(cetacean_remian_half_r_matrix, options = "FA")$vol
(PCA_vol_cetacean_half_r_remain/PCA_vol_cetacean)*100

cetacean_extinct_half_random <- sample(cetacean_pca$species,40, replace = FALSE)
cetacean_extinct_half_r_df <- cetacean_pca %>% filter(species %in% cetacean_extinct_half_random)
cetacean_remain_half_r <- cetacean_pca %>% filter(!species %in% cetacean_extinct_half_random)
whole_df5 <- cetacean_pca
whole_df5$range <- "Whole"
cetacean_remain_half_r$range <- "50% remain"
cetacean_extinct_half_r_df$range <- "50% extinct"

PCA_half_r_data5 <- rbind(whole_df5, cetacean_remain_half_r, cetacean_extinct_half_r_df)
PCA_half_r_data5$range <- factor(PCA_half_r_data5$range, levels = c("Whole", "50% remain", "50% extinct"))
find_hull <- function(df) df[chull(df$PC1, df$PC2), ]

hulls <- PCA_half_r_data5 %>%
  group_by(range) %>%
  do(find_hull(.))


col_list <- c("#999999","#00CDCD","#FFD700")

plot_cr <-ggplot(PCA_half_r_data5, aes(x = PC1, y = PC2)) +
  geom_point(aes(colour = range, shape = range)) +
  geom_polygon(data = hulls, aes(alpha = 0.5, fill = range)) +
  theme_classic() +
  scale_color_manual(values = col_list) +
  scale_fill_manual(values = col_list)+
  ggtitle("Cetacean random")+
  guides(alpha = "none")+
  xlim(-0.035,0.07)+
  ylim(-0.035,0.055)

plot_cetacean <-grid.arrange(plot5, plot_cp, plot_cr, ncol = 2)

grid.arrange(plot_shark, plot_ray_skate,plot_whale,plot_dolphin,plot_cetacean,ncol = 2 , common.legend = TRUE)

par(mfrow=c(3,2))

## remove 50% of the species scenario for sharks
# Create an empty vector to store the results
results_shark <- numeric(100)
# Repeat the process 100 times based on probability of extinction 
for (i in 1:100) {
  # Sampling half of the species based on the extinction probability
  shark_extinct_half <- sample(shark_pca$species, 21, replace = FALSE, prob = shark_pca$ext.prob)
  
  # Create a data frame with the species selected for extinction
  shark_extinct_half_df <- shark_pca %>% filter(species %in% shark_extinct_half)
  
  # Create a data frame with the remaining species
  shark_remain_half <- shark_pca %>% filter(!species %in% shark_extinct_half)
  
  # Create a matrix with the remaining species' PCA coordinates
  shark_remain_half_matrix <- subset(shark_remain_half, select = c(PC1, PC2, PC3))
  
  # Calculate the volume of the convex hull for the remaining species
  PCA_vol_shark_half_remain <- convhulln(shark_remain_half_matrix, options = "FA")$vol
  
  # Calculate the probability of extinction
  PCA_vol_shark_prob <- (PCA_vol_shark_half_remain / PCA_vol_shark) * 100
  
  # Store the result in the results vector
  results_shark[i] <- PCA_vol_shark_prob
}



# Create an empty vector to store the results
results_random_shark <- numeric(100)
# Repeat the process 100 times without probability of extinction 
for (i in 1:100) {
  # Sampling half of the species randomly (without considering extinction probabilities)
  shark_extinct_half_random <- sample(shark_pca$species, 21, replace = FALSE)
  
  # Create a data frame with the species selected for extinction randomly
  shark_extinct_half_r_df <- shark_pca %>% filter(species %in% shark_extinct_half_random)
  
  # Create a data frame with the remaining species
  shark_remain_half_r <- shark_pca %>% filter(!species %in% shark_extinct_half_random)
  
  # Create a matrix with the remaining species' PCA coordinates
  shark_remain_half_r_matrix <- subset(shark_remain_half_r, select = c(PC1, PC2, PC3))
  
  # Calculate the volume of the convex hull for the remaining species
  PCA_vol_shark_half_r_remain <- convhulln(shark_remain_half_r_matrix, options = "FA")$vol
  
  # Calculate the probability of extinction
  PCA_vol_shark_prob_random <- (PCA_vol_shark_half_r_remain / PCA_vol_shark) * 100
  
  # Store the result in the results vector
  results_random_shark[i] <- PCA_vol_shark_prob_random
}
#combine data 
shark_prob_df <- data.frame("ProbabilityBased" = results_shark,"Random" = results_random_shark)
#box plot 
boxplot(shark_prob_df, ylab="Percentage of remaining PCA volume", main = "Shark")
#t test 
t_test_shark <- t.test(results_shark, results_random_shark, alternative = "greater")

## remove 50% of the species scenario for rays and skates 
results_ray_skate <- numeric(100)
# Repeat the process 100 times with probabiltiy of extinction 
for (i in 1:100) {
  # Sampling half of the species based on the extinction probability
  ray_skate_extinct_half <- sample(ray_skate_pca$species, 14, replace = FALSE, prob = ray_skate_pca$prob.ext)
  
  # Create a data frame with the species selected for extinction
  ray_skate_extinct_half_df <- ray_skate_pca %>% filter(species %in% ray_skate_extinct_half)
  
  # Create a data frame with the remaining species
  ray_skate_remain_half <- ray_skate_pca %>% filter(!species %in% ray_skate_extinct_half)
  
  # Create a matrix with the remaining species' PCA coordinates
  ray_skate_remain_half_matrix <- subset(ray_skate_remain_half, select = c(PC1, PC2, PC3))
  
  # Calculate the volume of the convex hull for the remaining species
  PCA_vol_ray_skate_half_remain <- convhulln(ray_skate_remain_half_matrix, options = "FA")$vol
  
  # Calculate the probability of extinction
  PCA_vol_ray_skate_prob <- (PCA_vol_ray_skate_half_remain / PCA_vol_ray_skate) * 100
  
  # Store the result in the results vector
  results_ray_skate[i] <- PCA_vol_ray_skate_prob
}


# Create an empty vector to store the results
results_random_ray_skate <- numeric(100)
# Repeat the process 100 times without probability of extinction 
for (i in 1:100) {
  # Sampling half of the species randomly (without considering extinction probabilities)
  ray_skate_extinct_half_random <- sample(ray_skate_pca$species, 14, replace = FALSE)
  
  # Create a data frame with the species selected for extinction randomly
  ray_skate_extinct_half_r_df <- ray_skate_pca %>% filter(species %in% ray_skate_extinct_half_random)
  
  # Create a data frame with the remaining species
  ray_skate_remain_half_r <- ray_skate_pca %>% filter(!species %in% ray_skate_extinct_half_random)
  
  # Create a matrix with the remaining species' PCA coordinates
  ray_skate_remain_half_r_matrix <- subset(ray_skate_remain_half_r, select = c(PC1, PC2, PC3))
  
  # Calculate the volume of the convex hull for the remaining species
  PCA_vol_ray_skate_half_r_remain <- convhulln(ray_skate_remain_half_r_matrix, options = "FA")$vol
  
  # Calculate the probability of extinction
  PCA_vol_ray_skate_prob_random <- (PCA_vol_ray_skate_half_r_remain / PCA_vol_ray_skate) * 100
  
  # Store the result in the results vector
  results_random_ray_skate[i] <- PCA_vol_ray_skate_prob_random
}
#combine data 
ray_skate_prob_df <- data.frame("ProbabilityBased" = results_ray_skate,"Random" = results_random_ray_skate)
#box plot 
boxplot(ray_skate_prob_df, ylab="Percentage of remaining PCA volume", main = "Ray and Skate")
#t test 
t_test_ray_skate<- t.test(results_ray_skate, results_random_ray_skate, alternative = "greater")

## remove 50% of the species scenario for whales 
# Create an empty vector to store the results
results_whale <- numeric(100)
# Repeat the process 100 times with probabiltiy of extinction 
for (i in 1:100) {
  # Sampling half of the species based on the extinction probability
  whale_extinct_half <- sample(whale_pca$species, 23, replace = FALSE, prob = whale_pca$ext.prob)
  
  # Create a data frame with the species selected for extinction
  whale_extinct_half_df <- whale_pca %>% filter(species %in% whale_extinct_half)
  
  # Create a data frame with the remaining species
  whale_remain_half <- whale_pca %>% filter(!species %in% whale_extinct_half)
  
  # Create a matrix with the remaining species' PCA coordinates
  whale_remain_half_matrix <- subset(whale_remain_half, select = c(PC1, PC2, PC3))
  
  # Calculate the volume of the convex hull for the remaining species
  PCA_vol_whale_half_remain <- convhulln(whale_remain_half_matrix, options = "FA")$vol
  
  # Calculate the probability of extinction
  PCA_vol_whale_prob <- (PCA_vol_whale_half_remain / PCA_vol_whale) * 100
  
  # Store the result in the results vector
  results_whale[i] <- PCA_vol_whale_prob
}

# Create an empty vector to store the results
results_random_whale <- numeric(100)
# Repeat the process 100 times without probabiltiy of extinction 
for (i in 1:100) {
  # Sampling half of the species randomly (without considering extinction probabilities)
  whale_extinct_half_random <- sample(whale_pca$species, 23, replace = FALSE)
  
  # Create a data frame with the species selected for extinction randomly
  whale_extinct_half_r_df <- whale_pca %>% filter(species %in% whale_extinct_half_random)
  
  # Create a data frame with the remaining species
  whale_remain_half_r <- whale_pca %>% filter(!species %in% whale_extinct_half_random)
  
  # Create a matrix with the remaining species' PCA coordinates
  whale_remain_half_r_matrix <- subset(whale_remain_half_r, select = c(PC1, PC2, PC3))
  
  # Calculate the volume of the convex hull for the remaining species
  PCA_vol_whale_half_r_remain <- convhulln(whale_remain_half_r_matrix, options = "FA")$vol
  
  # Calculate the probability of extinction
  PCA_vol_whale_prob_random <- (PCA_vol_whale_half_r_remain / PCA_vol_whale) * 100
  
  # Store the result in the results vector
  results_random_whale[i] <- PCA_vol_whale_prob_random
}
whale_prob_df <- data.frame("ProbabilityBased" = results_whale,"Random" = results_random_whale)
boxplot(whale_prob_df, ylab="Percentage of remaining PCA volume", main = "Whale")
t_test_whale <- t.test(results_whale, results_random_whale, alternative = "greater")

## remove 50% of the species scenario for dolphins
results_dolphin <- numeric(100)
# Repeat the process 100 times with probability of extinction 
for (i in 1:100) {
  # Sampling half of the species based on the extinction probability
  dolphin_extinct_half <- sample(dolphin_pca$species, 15, replace = FALSE, prob = dolphin_pca$ext.prob)
  
  # Create a data frame with the species selected for extinction
  dolphin_extinct_half_df <- dolphin_pca %>% filter(species %in% dolphin_extinct_half)
  
  # Create a data frame with the remaining species
  dolphin_remain_half <- dolphin_pca %>% filter(!species %in% dolphin_extinct_half)
  
  # Create a matrix with the remaining species' PCA coordinates
  dolphin_remain_half_matrix <- subset(dolphin_remain_half, select = c(PC1, PC2, PC3))
  
  # Calculate the volume of the convex hull for the remaining species
  PCA_vol_dolphin_half_remain <- convhulln(dolphin_remain_half_matrix, options = "FA")$vol
  
  # Calculate the probability of extinction
  PCA_vol_dolphin_prob <- (PCA_vol_dolphin_half_remain / PCA_vol_dolphin) * 100
  
  # Store the result in the results vector
  results_dolphin[i] <- PCA_vol_dolphin_prob
}

# Create an empty vector to store the results
results_random_dolphin <- numeric(100)
# Repeat the process 100 times without probability of extinction 
for (i in 1:100) {
  # Sampling half of the species randomly (without considering extinction probabilities)
  dolphin_extinct_half_random <- sample(dolphin_pca$species, 15, replace = FALSE)
  
  # Create a data frame with the species selected for extinction randomly
  dolphin_extinct_half_r_df <- dolphin_pca %>% filter(species %in% dolphin_extinct_half_random)
  
  # Create a data frame with the remaining species
  dolphin_remain_half_r <- dolphin_pca %>% filter(!species %in% dolphin_extinct_half_random)
  
  # Create a matrix with the remaining species' PCA coordinates
  dolphin_remain_half_r_matrix <- subset(dolphin_remain_half_r, select = c(PC1, PC2, PC3))
  
  # Calculate the volume of the convex hull for the remaining species
  PCA_vol_dolphin_half_r_remain <- convhulln(dolphin_remain_half_r_matrix, options = "FA")$vol
  
  # Calculate the probability of extinction
  PCA_vol_dolphin_prob_random <- (PCA_vol_dolphin_half_r_remain / PCA_vol_dolphin) * 100
  
  # Store the result in the results vector
  results_random_dolphin[i] <- PCA_vol_dolphin_prob_random
}
dolphin_prob_df <- data.frame("ProbabilityBased" = results_dolphin,"Random" = results_random_dolphin)
boxplot(dolphin_prob_df, ylab="Percentage of remaining PCA volume", main = "Dolphin")
t_test_dolphin <- t.test(results_dolphin, results_random_dolphin, alternative = "greater")

## remove 50% of the species scenario for cetacean
results_cetacean <- numeric(100)
# Repeat the process 100 times with probability of extinction 
for (i in 1:100) {
  # Sampling half of the species based on the extinction probability
  cetacean_extinct_half <- sample(cetacean_pca$species, 40, replace = FALSE, prob = cetacean_pca$ext.prob)
  
  # Create a data frame with the species selected for extinction
  cetacean_extinct_half_df <- cetacean_pca %>% filter(species %in% cetacean_extinct_half)
  
  # Create a data frame with the remaining species
  cetacean_remain_half <- cetacean_pca %>% filter(!species %in% cetacean_extinct_half)
  
  # Create a matrix with the remaining species' PCA coordinates
  cetacean_remain_half_matrix <- subset(cetacean_remain_half, select = c(PC1, PC2, PC3))
  
  # Calculate the volume of the convex hull for the remaining species
  PCA_vol_cetacean_half_remain <- convhulln(cetacean_remain_half_matrix, options = "FA")$vol
  
  # Calculate the probability of extinction
  PCA_vol_cetacean_prob <- (PCA_vol_cetacean_half_remain / PCA_vol_cetacean) * 100
  
  # Store the result in the results vector
  results_cetacean[i] <- PCA_vol_cetacean_prob
}

# Create an empty vector to store the results
results_random_cetacean <- numeric(100)
# Repeat the process 100 times without probability of extinction 
for (i in 1:100) {
  # Sampling half of the species randomly (without considering extinction probabilities)
  cetacean_extinct_half_random <- sample(cetacean_pca$species, 40, replace = FALSE)
  
  # Create a data frame with the species selected for extinction randomly
  cetacean_extinct_half_r_df <- cetacean_pca %>% filter(species %in% cetacean_extinct_half_random)
  
  # Create a data frame with the remaining species
  cetacean_remain_half_r <- cetacean_pca %>% filter(!species %in% cetacean_extinct_half_random)
  
  # Create a matrix with the remaining species' PCA coordinates
  cetacean_remain_half_r_matrix <- subset(cetacean_remain_half_r, select = c(PC1, PC2, PC3))
  
  # Calculate the volume of the convex hull for the remaining species
  PCA_vol_cetacean_half_r_remain <- convhulln(cetacean_remain_half_r_matrix, options = "FA")$vol
  
  # Calculate the probability of extinction
  PCA_vol_cetacean_prob_random <- (PCA_vol_cetacean_half_r_remain / PCA_vol_cetacean) * 100
  
  # Store the result in the results vector
  results_random_cetacean[i] <- PCA_vol_cetacean_prob_random
}
cetacean_prob_df <- data.frame("ProbabilityBased" = results_cetacean,"Random" = results_random_cetacean)
boxplot(cetacean_prob_df, ylab="Percentage of remaining PCA volume", main = "Cetacean")
t_test_cetacean <- t.test(results_cetacean, results_random_cetacean, alternative = "greater")

#redo lost 10% of the species for sharks  
par(mfrow=c(3,2))
# Create an empty vector to store the results
results_shark_10 <- numeric(100)
# Repeat the process 100 times
for (i in 1:100) {
  # Sampling half of the species based on the extinction probability
  shark_extinct_10 <- sample(shark_pca$species, 4, replace = FALSE, prob = shark_pca$ext.prob)
  
  # Create a data frame with the species selected for extinction
  shark_extinct_10_df <- shark_pca %>% filter(species %in% shark_extinct_10)
  
  # Create a data frame with the remaining species
  shark_remain_10 <- shark_pca %>% filter(!species %in% shark_extinct_10)
  
  # Create a matrix with the remaining species' PCA coordinates
  shark_remain_10_matrix <- subset(shark_remain_10, select = c(PC1, PC2, PC3))
  
  # Calculate the volume of the convex hull for the remaining species
  PCA_vol_shark_10_remain <- convhulln(shark_remain_10_matrix, options = "FA")$vol
  
  # Calculate the probability of extinction
  PCA_vol_shark_prob_10 <- (PCA_vol_shark_10_remain / PCA_vol_shark) * 100
  
  # Store the result in the results vector
  results_shark_10[i] <- PCA_vol_shark_prob_10
}

# Create an empty vector to store the results
results_random_shark_10 <- numeric(100)
# Repeat the process 100 times without 
for (i in 1:100) {
  # Sampling half of the species randomly (without considering extinction probabilities)
  shark_extinct_10_random <- sample(shark_pca$species, 4, replace = FALSE)
  
  # Create a data frame with the species selected for extinction randomly
  shark_extinct_10_r_df <- shark_pca %>% filter(species %in% shark_extinct_10_random)
  
  # Create a data frame with the remaining species
  shark_remain_10_r <- shark_pca %>% filter(!species %in% shark_extinct_10_random)
  
  # Create a matrix with the remaining species' PCA coordinates
  shark_remain_10_r_matrix <- subset(shark_remain_10_r, select = c(PC1, PC2, PC3))
  
  # Calculate the volume of the convex hull for the remaining species
  PCA_vol_shark_10_r_remain <- convhulln(shark_remain_10_r_matrix, options = "FA")$vol
  
  # Calculate the probability of extinction
  PCA_vol_shark_prob_random_10 <- (PCA_vol_shark_10_r_remain / PCA_vol_shark) * 100
  
  # Store the result in the results vector
  results_random_shark_10[i] <- PCA_vol_shark_prob_random_10
}
shark_prob_df_10 <- data.frame("ProbabilityBased" = results_shark_10,"Random" = results_random_shark_10)
boxplot(shark_prob_df_10, ylab="Percentage of remaining PCA volume", main = "Shark")
t_test_shark_10 <- t.test(results_shark_10, results_random_shark_10, alternative = "greater")

## remove 10% for rays and skates 
# Create an empty vector to store the results
results_ray_skate_10 <- numeric(100)
# Repeat the process 100 times
for (i in 1:100) {
  # Sampling half of the species based on the extinction probability
  ray_skate_extinct_10 <- sample(ray_skate_pca$species, 3, replace = FALSE, prob = ray_skate_pca$prob.ext)
  
  # Create a data frame with the species selected for extinction
  ray_skate_extinct_10_df <- ray_skate_pca %>% filter(species %in% ray_skate_extinct_10)
  
  # Create a data frame with the remaining species
  ray_skate_remain_10 <- ray_skate_pca %>% filter(!species %in% ray_skate_extinct_10)
  
  # Create a matrix with the remaining species' PCA coordinates
  ray_skate_remain_10_matrix <- subset(ray_skate_remain_10, select = c(PC1, PC2, PC3))
  
  # Calculate the volume of the convex hull for the remaining species
  PCA_vol_ray_skate_10_remain <- convhulln(ray_skate_remain_10_matrix, options = "FA")$vol
  
  # Calculate the probability of extinction
  PCA_vol_ray_skate_prob_10 <- (PCA_vol_ray_skate_10_remain / PCA_vol_ray_skate) * 100
  
  # Store the result in the results vector
  results_ray_skate_10[i] <- PCA_vol_ray_skate_prob_10
}

# Create an empty vector to store the results
results_random_ray_skate_10 <- numeric(100)
# Repeat the process 100 times without 
for (i in 1:100) {
  # Sampling half of the species randomly (without considering extinction probabilities)
  ray_skate_extinct_10_random <- sample(ray_skate_pca$species, 3, replace = FALSE)
  
  # Create a data frame with the species selected for extinction randomly
  ray_skate_extinct_10_r_df <- ray_skate_pca %>% filter(species %in% ray_skate_extinct_10_random)
  
  # Create a data frame with the remaining species
  ray_skate_remain_10_r <- ray_skate_pca %>% filter(!species %in% ray_skate_extinct_10_random)
  
  # Create a matrix with the remaining species' PCA coordinates
  ray_skate_remain_10_r_matrix <- subset(ray_skate_remain_10_r, select = c(PC1, PC2, PC3))
  
  # Calculate the volume of the convex hull for the remaining species
  PCA_vol_ray_skate_10_r_remain <- convhulln(ray_skate_remain_10_r_matrix, options = "FA")$vol
  
  # Calculate the probability of extinction
  PCA_vol_ray_skate_prob_random_10 <- (PCA_vol_ray_skate_10_r_remain / PCA_vol_ray_skate) * 100
  
  # Store the result in the results vector
  results_random_ray_skate_10[i] <- PCA_vol_ray_skate_prob_random_10
}
ray_skate_prob_df_10 <- data.frame("ProbabilityBased" = results_ray_skate_10,"Random" = results_random_ray_skate_10)
boxplot(ray_skate_prob_df_10, ylab="Percentage of remaining PCA volume", main = "Ray and Skate")
t_test_ray_skate_10 <- t.test(results_ray_skate_10, results_random_ray_skate_10, alternative = "greater")

## for whales 
# Create an empty vector to store the results
results_whale_10 <- numeric(100)
# Repeat the process 100 times
for (i in 1:100) {
  # Sampling half of the species based on the extinction probability
  whale_extinct_10 <- sample(whale_pca$species, 5, replace = FALSE, prob = whale_pca$ext.prob)
  
  # Create a data frame with the species selected for extinction
  whale_extinct_10_df <- whale_pca %>% filter(species %in% whale_extinct_10)
  
  # Create a data frame with the remaining species
  whale_remain_10 <- whale_pca %>% filter(!species %in% whale_extinct_10)
  
  # Create a matrix with the remaining species' PCA coordinates
  whale_remain_10_matrix <- subset(whale_remain_10, select = c(PC1, PC2, PC3))
  
  # Calculate the volume of the convex hull for the remaining species
  PCA_vol_whale_10_remain <- convhulln(whale_remain_10_matrix, options = "FA")$vol
  
  # Calculate the probability of extinction
  PCA_vol_whale_prob_10 <- (PCA_vol_whale_10_remain / PCA_vol_whale) * 100
  
  # Store the result in the results vector
  results_whale_10[i] <- PCA_vol_whale_prob_10
}

# Create an empty vector to store the results
results_random_whale_10 <- numeric(100)
# Repeat the process 100 times without 
for (i in 1:100) {
  # Sampling half of the species randomly (without considering extinction probabilities)
  whale_extinct_10_random <- sample(whale_pca$species, 5, replace = FALSE)
  
  # Create a data frame with the species selected for extinction randomly
  whale_extinct_10_r_df <- whale_pca %>% filter(species %in% whale_extinct_10_random)
  
  # Create a data frame with the remaining species
  whale_remain_10_r <- whale_pca %>% filter(!species %in% whale_extinct_10_random)
  
  # Create a matrix with the remaining species' PCA coordinates
  whale_remain_10_r_matrix <- subset(whale_remain_10_r, select = c(PC1, PC2, PC3))
  
  # Calculate the volume of the convex hull for the remaining species
  PCA_vol_whale_10_r_remain <- convhulln(whale_remain_10_r_matrix, options = "FA")$vol
  
  # Calculate the probability of extinction
  PCA_vol_whale_prob_random_10 <- (PCA_vol_whale_10_r_remain / PCA_vol_whale) * 100
  
  # Store the result in the results vector
  results_random_whale_10[i] <- PCA_vol_whale_prob_random_10
}
whale_prob_df_10 <- data.frame("ProbabilityBased" = results_whale_10,"Random" = results_random_whale_10)
boxplot(whale_prob_df_10, ylab="Percentage of remaining PCA volume", main = "Whale")
t_test_whale_10 <- t.test(results_whale_10, results_random_whale_10, alternative = "greater")

##for dolphins 
# Create an empty vector to store the results
results_dolphin_10 <- numeric(100)
# Repeat the process 100 times
for (i in 1:100) {
  # Sampling half of the species based on the extinction probability
  dolphin_extinct_10 <- sample(dolphin_pca$species, 3, replace = FALSE, prob = dolphin_pca$ext.prob)
  
  # Create a data frame with the species selected for extinction
  dolphin_extinct_10_df <- dolphin_pca %>% filter(species %in% dolphin_extinct_10)
  
  # Create a data frame with the remaining species
  dolphin_remain_10 <- dolphin_pca %>% filter(!species %in% dolphin_extinct_10)
  
  # Create a matrix with the remaining species' PCA coordinates
  dolphin_remain_10_matrix <- subset(dolphin_remain_10, select = c(PC1, PC2, PC3))
  
  # Calculate the volume of the convex hull for the remaining species
  PCA_vol_dolphin_10_remain <- convhulln(dolphin_remain_10_matrix, options = "FA")$vol
  
  # Calculate the probability of extinction
  PCA_vol_dolphin_prob_10 <- (PCA_vol_dolphin_10_remain / PCA_vol_dolphin) * 100
  
  # Store the result in the results vector
  results_dolphin_10[i] <- PCA_vol_dolphin_prob_10
}

# Create an empty vector to store the results
results_random_dolphin_10 <- numeric(100)
# Repeat the process 100 times without 
for (i in 1:100) {
  # Sampling half of the species randomly (without considering extinction probabilities)
  dolphin_extinct_10_random <- sample(dolphin_pca$species, 3, replace = FALSE)
  
  # Create a data frame with the species selected for extinction randomly
  dolphin_extinct_10_r_df <- dolphin_pca %>% filter(species %in% dolphin_extinct_10_random)
  
  # Create a data frame with the remaining species
  dolphin_remain_10_r <- dolphin_pca %>% filter(!species %in% dolphin_extinct_10_random)
  
  # Create a matrix with the remaining species' PCA coordinates
  dolphin_remain_10_r_matrix <- subset(dolphin_remain_10_r, select = c(PC1, PC2, PC3))
  
  # Calculate the volume of the convex hull for the remaining species
  PCA_vol_dolphin_10_r_remain <- convhulln(dolphin_remain_10_r_matrix, options = "FA")$vol
  
  # Calculate the probability of extinction
  PCA_vol_dolphin_prob_random_10 <- (PCA_vol_dolphin_10_r_remain / PCA_vol_dolphin) * 100
  
  # Store the result in the results vector
  results_random_dolphin_10[i] <- PCA_vol_dolphin_prob_random_10
}
dolphin_prob_df_10 <- data.frame("ProbabilityBased" = results_dolphin_10,"Random" = results_random_dolphin_10)
boxplot(dolphin_prob_df_10, ylab="Percentage of remaining PCA volume", main = "Dolphin")
t_test_dolphin_10 <- t.test(results_dolphin_10, results_random_dolphin_10, alternative = "greater")

##for cetacean
# Create an empty vector to store the results
results_cetacean_10 <- numeric(100)
# Repeat the process 100 times
for (i in 1:100) {
  # Sampling half of the species based on the extinction probability
  cetacean_extinct_10 <- sample(cetacean_pca$species, 8, replace = FALSE, prob = cetacean_pca$ext.prob)
  
  # Create a data frame with the species selected for extinction
  cetacean_extinct_10_df <- cetacean_pca %>% filter(species %in% cetacean_extinct_10)
  
  # Create a data frame with the remaining species
  cetacean_remain_10 <- cetacean_pca %>% filter(!species %in% cetacean_extinct_10)
  
  # Create a matrix with the remaining species' PCA coordinates
  cetacean_remain_10_matrix <- subset(cetacean_remain_10, select = c(PC1, PC2, PC3))
  
  # Calculate the volume of the convex hull for the remaining species
  PCA_vol_cetacean_10_remain <- convhulln(cetacean_remain_10_matrix, options = "FA")$vol
  
  # Calculate the probability of extinction
  PCA_vol_cetacean_prob_10 <- (PCA_vol_cetacean_10_remain / PCA_vol_cetacean) * 100
  
  # Store the result in the results vector
  results_cetacean_10[i] <- PCA_vol_cetacean_prob_10
}

# Create an empty vector to store the results
results_random_cetacean_10 <- numeric(100)
# Repeat the process 100 times without 
for (i in 1:100) {
  # Sampling half of the species randomly (without considering extinction probabilities)
  cetacean_extinct_10_random <- sample(cetacean_pca$species, 8, replace = FALSE)
  
  # Create a data frame with the species selected for extinction randomly
  cetacean_extinct_10_r_df <- cetacean_pca %>% filter(species %in% cetacean_extinct_10_random)
  
  # Create a data frame with the remaining species
  cetacean_remain_10_r <- cetacean_pca %>% filter(!species %in% cetacean_extinct_10_random)
  
  # Create a matrix with the remaining species' PCA coordinates
  cetacean_remain_10_r_matrix <- subset(cetacean_remain_10_r, select = c(PC1, PC2, PC3))
  
  # Calculate the volume of the convex hull for the remaining species
  PCA_vol_cetacean_10_r_remain <- convhulln(cetacean_remain_10_r_matrix, options = "FA")$vol
  
  # Calculate the probability of extinction
  PCA_vol_cetacean_prob_random_10 <- (PCA_vol_cetacean_10_r_remain / PCA_vol_cetacean) * 100
  
  # Store the result in the results vector
  results_random_cetacean_10[i] <- PCA_vol_cetacean_prob_random_10
}
cetacean_prob_df_10 <- data.frame("ProbabilityBased" = results_cetacean_10,"Random" = results_random_cetacean_10)
boxplot(cetacean_prob_df_10, ylab="Percentage of remaining PCA volume", main = "Cetacean")
t_test_cetacean_10 <- t.test(results_cetacean_10, results_random_cetacean_10, alternative = "greater")


#redo lose 50% but random probabilities for sharks 
ext_prob_values <- c(0.97, 0.97/2, 0.97/4, 0.97/8, 0.97/16)
shark_pca_r <- shark_pca
shark_pca_r$ext.prob <- sample(ext_prob_values, nrow(shark_pca_r), replace = TRUE)

# Create an empty vector to store the results
results_shark_50 <- numeric(100)

# Repeat the process 100 times
for (i in 1:100) {
  # Sampling half of the species based on the extinction probability
  shark_extinct_half <- sample(shark_pca_r$species, 21, replace = FALSE, prob = shark_pca_r$ext.prob)
  
  # Create a data frame with the species selected for extinction
  shark_extinct_half_df <- shark_pca_r %>% filter(species %in% shark_extinct_half)
  
  # Create a data frame with the remaining species
  shark_remain_half <- shark_pca_r %>% filter(!species %in% shark_extinct_half)
  
  # Create a matrix with the remaining species' PCA coordinates
  shark_remain_half_matrix <- subset(shark_remain_half, select = c(PC1, PC2, PC3))
  
  # Calculate the volume of the convex hull for the remaining species
  PCA_vol_shark_half_remain <- convhulln(shark_remain_half_matrix, options = "FA")$vol
  
  # Calculate the probability of extinction
  PCA_vol_shark_prob <- (PCA_vol_shark_half_remain / PCA_vol_shark) * 100
  
  # Store the result in the results vector
  results_shark_50[i] <- PCA_vol_shark_prob
}



# Create an empty vector to store the results
results_random_shark_50 <- numeric(100)

# Repeat the process 100 times
for (i in 1:100) {
  # Sampling half of the species randomly (without considering extinction probabilities)
  shark_extinct_half_random <- sample(shark_pca_r$species, 21, replace = FALSE)
  
  # Create a data frame with the species selected for extinction randomly
  shark_extinct_half_r_df <- shark_pca_r %>% filter(species %in% shark_extinct_half_random)
  
  # Create a data frame with the remaining species
  shark_remain_half_r <- shark_pca_r %>% filter(!species %in% shark_extinct_half_random)
  
  # Create a matrix with the remaining species' PCA coordinates
  shark_remain_half_r_matrix <- subset(shark_remain_half_r, select = c(PC1, PC2, PC3))
  
  # Calculate the volume of the convex hull for the remaining species
  PCA_vol_shark_half_r_remain <- convhulln(shark_remain_half_r_matrix, options = "FA")$vol
  
  # Calculate the probability of extinction
  PCA_vol_shark_prob_random <- (PCA_vol_shark_half_r_remain / PCA_vol_shark) * 100
  
  # Store the result in the results vector
  results_random_shark_50[i] <- PCA_vol_shark_prob_random
}
shark_prob_df_50 <- data.frame("ProbabilityBased" = results_shark_50,"Random" = results_random_shark_50)
boxplot(shark_prob_df_50, ylab="Percentage of remaining PCA volume", main = "Shark")
t_test_shark <- t.test(results_shark_50, results_random_shark_50, alternative = "greater")


#redo lose 50% but random probabilities for rays and skates 
ext_prob_values <- c(0.97, 0.97/2, 0.97/4, 0.97/8, 0.97/16)
ray_skate_pca_r <- ray_skate_pca
ray_skate_pca_r$ext.prob <- sample(ext_prob_values, nrow(ray_skate_pca_r), replace = TRUE)

# Create an empty vector to store the results
results_ray_skate_50 <- numeric(100)

# Repeat the process 100 times
for (i in 1:100) {
  # Sampling half of the species based on the extinction probability
  ray_skate_extinct_half <- sample(ray_skate_pca_r$species, 14, replace = FALSE, prob = ray_skate_pca_r$ext.prob)
  
  # Create a data frame with the species selected for extinction
  ray_skate_extinct_half_df <- ray_skate_pca_r %>% filter(species %in% ray_skate_extinct_half)
  
  # Create a data frame with the remaining species
  ray_skate_remain_half <- ray_skate_pca_r %>% filter(!species %in% ray_skate_extinct_half)
  
  # Create a matrix with the remaining species' PCA coordinates
  ray_skate_remain_half_matrix <- subset(ray_skate_remain_half, select = c(PC1, PC2, PC3))
  
  # Calculate the volume of the convex hull for the remaining species
  PCA_vol_ray_skate_half_remain <- convhulln(ray_skate_remain_half_matrix, options = "FA")$vol
  
  # Calculate the probability of extinction
  PCA_vol_ray_skate_prob <- (PCA_vol_ray_skate_half_remain / PCA_vol_ray_skate) * 100
  
  # Store the result in the results vector
  results_ray_skate_50[i] <- PCA_vol_ray_skate_prob
}



# Create an empty vector to store the results
results_random_ray_skate_50 <- numeric(100)

# Repeat the process 100 times
for (i in 1:100) {
  # Sampling half of the species randomly (without considering extinction probabilities)
  ray_skate_extinct_half_random <- sample(ray_skate_pca_r$species, 14, replace = FALSE)
  
  # Create a data frame with the species selected for extinction randomly
  ray_skate_extinct_half_r_df <- ray_skate_pca_r %>% filter(species %in% ray_skate_extinct_half_random)
  
  # Create a data frame with the remaining species
  ray_skate_remain_half_r <- ray_skate_pca_r %>% filter(!species %in% ray_skate_extinct_half_random)
  
  # Create a matrix with the remaining species' PCA coordinates
  ray_skate_remain_half_r_matrix <- subset(ray_skate_remain_half_r, select = c(PC1, PC2, PC3))
  
  # Calculate the volume of the convex hull for the remaining species
  PCA_vol_ray_skate_half_r_remain <- convhulln(ray_skate_remain_half_r_matrix, options = "FA")$vol
  
  # Calculate the probability of extinction
  PCA_vol_ray_skate_prob_random <- (PCA_vol_ray_skate_half_r_remain / PCA_vol_ray_skate) * 100
  
  # Store the result in the results vector
  results_random_ray_skate_50[i] <- PCA_vol_ray_skate_prob_random
}
ray_skate_prob_df_50 <- data.frame("ProbabilityBased" = results_ray_skate_50,"Random" = results_random_ray_skate_50)
boxplot(ray_skate_prob_df_50, ylab="Percentage of remaining PCA volume", main = "Ray and Skate")
t_test_ray_skate <- t.test(results_ray_skate, results_random_ray_skate, alternative = "greater")


#redo lose 50% but random probabilities 
ext_prob_values <- c(0.97, 0.97/2, 0.97/4, 0.97/8, 0.97/16)
whale_pca_r <- whale_pca
whale_pca_r$ext.prob <- sample(ext_prob_values, nrow(whale_pca_r), replace = TRUE)

# Create an empty vector to store the results
results_whale_50 <- numeric(100)

# Repeat the process 100 times
for (i in 1:100) {
  # Sampling half of the species based on the extinction probability
  whale_extinct_half <- sample(whale_pca_r$species, 23, replace = FALSE, prob = whale_pca_r$ext.prob)
  
  # Create a data frame with the species selected for extinction
  whale_extinct_half_df <- whale_pca_r %>% filter(species %in% whale_extinct_half)
  
  # Create a data frame with the remaining species
  whale_remain_half <- whale_pca_r %>% filter(!species %in% whale_extinct_half)
  
  # Create a matrix with the remaining species' PCA coordinates
  whale_remain_half_matrix <- subset(whale_remain_half, select = c(PC1, PC2, PC3))
  
  # Calculate the volume of the convex hull for the remaining species
  PCA_vol_whale_half_remain <- convhulln(whale_remain_half_matrix, options = "FA")$vol
  
  # Calculate the probability of extinction
  PCA_vol_whale_prob <- (PCA_vol_whale_half_remain / PCA_vol_whale) * 100
  
  # Store the result in the results vector
  results_whale_50[i] <- PCA_vol_whale_prob
}



# Create an empty vector to store the results
results_random_whale_50 <- numeric(100)

# Repeat the process 100 times
for (i in 1:100) {
  # Sampling half of the species randomly (without considering extinction probabilities)
  whale_extinct_half_random <- sample(whale_pca_r$species, 23, replace = FALSE)
  
  # Create a data frame with the species selected for extinction randomly
  whale_extinct_half_r_df <- whale_pca_r %>% filter(species %in% whale_extinct_half_random)
  
  # Create a data frame with the remaining species
  whale_remain_half_r <- whale_pca_r %>% filter(!species %in% whale_extinct_half_random)
  
  # Create a matrix with the remaining species' PCA coordinates
  whale_remain_half_r_matrix <- subset(whale_remain_half_r, select = c(PC1, PC2, PC3))
  
  # Calculate the volume of the convex hull for the remaining species
  PCA_vol_whale_half_r_remain <- convhulln(whale_remain_half_r_matrix, options = "FA")$vol
  
  # Calculate the probability of extinction
  PCA_vol_whale_prob_random <- (PCA_vol_whale_half_r_remain / PCA_vol_whale) * 100
  
  # Store the result in the results vector
  results_random_whale_50[i] <- PCA_vol_whale_prob_random
}
whale_prob_df_50 <- data.frame("ProbabilityBased" = results_whale_50,"Random" = results_random_whale_50)
boxplot(whale_prob_df_50, ylab="Percentage of remaining PCA volume", main = "Whale")
t_test_whale <- t.test(results_whale, results_random_whale, alternative = "greater")


#redo lose 50% but random probabilities 
ext_prob_values <- c(0.97, 0.97/2, 0.97/4, 0.97/8, 0.97/16)
dolphin_pca_r <- dolphin_pca
dolphin_pca_r$ext.prob <- sample(ext_prob_values, nrow(dolphin_pca_r), replace = TRUE)

# Create an empty vector to store the results
results_dolphin_50 <- numeric(100)

# Repeat the process 100 times
for (i in 1:100) {
  # Sampling half of the species based on the extinction probability
  dolphin_extinct_half <- sample(dolphin_pca_r$species, 15, replace = FALSE, prob = dolphin_pca_r$ext.prob)
  
  # Create a data frame with the species selected for extinction
  dolphin_extinct_half_df <- dolphin_pca_r %>% filter(species %in% dolphin_extinct_half)
  
  # Create a data frame with the remaining species
  dolphin_remain_half <- dolphin_pca_r %>% filter(!species %in% dolphin_extinct_half)
  
  # Create a matrix with the remaining species' PCA coordinates
  dolphin_remain_half_matrix <- subset(dolphin_remain_half, select = c(PC1, PC2, PC3))
  
  # Calculate the volume of the convex hull for the remaining species
  PCA_vol_dolphin_half_remain <- convhulln(dolphin_remain_half_matrix, options = "FA")$vol
  
  # Calculate the probability of extinction
  PCA_vol_dolphin_prob <- (PCA_vol_dolphin_half_remain / PCA_vol_dolphin) * 100
  
  # Store the result in the results vector
  results_dolphin_50[i] <- PCA_vol_dolphin_prob
}



# Create an empty vector to store the results
results_random_dolphin_50 <- numeric(100)

# Repeat the process 100 times
for (i in 1:100) {
  # Sampling half of the species randomly (without considering extinction probabilities)
  dolphin_extinct_half_random <- sample(dolphin_pca_r$species, 15, replace = FALSE)
  
  # Create a data frame with the species selected for extinction randomly
  dolphin_extinct_half_r_df <- dolphin_pca_r %>% filter(species %in% dolphin_extinct_half_random)
  
  # Create a data frame with the remaining species
  dolphin_remain_half_r <- dolphin_pca_r %>% filter(!species %in% dolphin_extinct_half_random)
  
  # Create a matrix with the remaining species' PCA coordinates
  dolphin_remain_half_r_matrix <- subset(dolphin_remain_half_r, select = c(PC1, PC2, PC3))
  
  # Calculate the volume of the convex hull for the remaining species
  PCA_vol_dolphin_half_r_remain <- convhulln(dolphin_remain_half_r_matrix, options = "FA")$vol
  
  # Calculate the probability of extinction
  PCA_vol_dolphin_prob_random <- (PCA_vol_dolphin_half_r_remain / PCA_vol_dolphin) * 100
  
  # Store the result in the results vector
  results_random_dolphin_50[i] <- PCA_vol_dolphin_prob_random
}
dolphin_prob_df_50 <- data.frame("ProbabilityBased" = results_dolphin_50,"Random" = results_random_dolphin_50)
boxplot(dolphin_prob_df_50, ylab="Percentage of remaining PCA volume", main = "Dolphin")
t_test_dolphin <- t.test(results_dolphin, results_random_dolphin, alternative = "greater")

#redo lose 50% but random probabilities 
ext_prob_values <- c(0.97, 0.97/2, 0.97/4, 0.97/8, 0.97/16)
cetacean_pca_r <- cetacean_pca
cetacean_pca_r$ext.prob <- sample(ext_prob_values, nrow(cetacean_pca_r), replace = TRUE)

# Create an empty vector to store the results
results_cetacean_50 <- numeric(100)

# Repeat the process 100 times
for (i in 1:100) {
  # Sampling half of the species based on the extinction probability
  cetacean_extinct_half <- sample(cetacean_pca_r$species, 40, replace = FALSE, prob = cetacean_pca_r$ext.prob)
  
  # Create a data frame with the species selected for extinction
  cetacean_extinct_half_df <- cetacean_pca_r %>% filter(species %in% cetacean_extinct_half)
  
  # Create a data frame with the remaining species
  cetacean_remain_half <- cetacean_pca_r %>% filter(!species %in% cetacean_extinct_half)
  
  # Create a matrix with the remaining species' PCA coordinates
  cetacean_remain_half_matrix <- subset(cetacean_remain_half, select = c(PC1, PC2, PC3))
  
  # Calculate the volume of the convex hull for the remaining species
  PCA_vol_cetacean_half_remain <- convhulln(cetacean_remain_half_matrix, options = "FA")$vol
  
  # Calculate the probability of extinction
  PCA_vol_cetacean_prob <- (PCA_vol_cetacean_half_remain / PCA_vol_cetacean) * 100
  
  # Store the result in the results vector
  results_cetacean_50[i] <- PCA_vol_cetacean_prob
}



# Create an empty vector to store the results
results_random_cetacean_50 <- numeric(100)

# Repeat the process 100 times
for (i in 1:100) {
  # Sampling half of the species randomly (without considering extinction probabilities)
  cetacean_extinct_half_random <- sample(cetacean_pca_r$species, 40, replace = FALSE)
  
  # Create a data frame with the species selected for extinction randomly
  cetacean_extinct_half_r_df <- cetacean_pca_r %>% filter(species %in% cetacean_extinct_half_random)
  
  # Create a data frame with the remaining species
  cetacean_remain_half_r <- cetacean_pca_r %>% filter(!species %in% cetacean_extinct_half_random)
  
  # Create a matrix with the remaining species' PCA coordinates
  cetacean_remain_half_r_matrix <- subset(cetacean_remain_half_r, select = c(PC1, PC2, PC3))
  
  # Calculate the volume of the convex hull for the remaining species
  PCA_vol_cetacean_half_r_remain <- convhulln(cetacean_remain_half_r_matrix, options = "FA")$vol
  
  # Calculate the probability of extinction
  PCA_vol_cetacean_prob_random <- (PCA_vol_cetacean_half_r_remain / PCA_vol_cetacean) * 100
  
  # Store the result in the results vector
  results_random_cetacean_50[i] <- PCA_vol_cetacean_prob_random
}
cetacean_prob_df_50 <- data.frame("ProbabilityBased" = results_cetacean_50,"Random" = results_random_cetacean_50)
boxplot(cetacean_prob_df_50, ylab="Percentage of remaining PCA volume", main = "Cetacean")
t_test_cetacean <- t.test(results_cetacean, results_random_cetacean, alternative = "greater")

#combine the above into one plot 
par(mfrow=c(3,2))
shark_prob_combine <- data.frame("ProbabilityBased50" = results_shark,"Random50" = results_random_shark, "ProbabilityBased10" = results_shark_10, "Random10"= results_random_shark_10)
boxplot(shark_prob_combine, ylab="Percentage of remaining PCA volume", main = "Shark")
ray_skate_prob_combine <- data.frame("ProbabilityBased50" = results_ray_skate,"Random50" = results_random_ray_skate, "ProbabilityBased10" = results_ray_skate_10, "Random10"= results_random_ray_skate_10)
boxplot(ray_skate_prob_combine, ylab="Percentage of remaining PCA volume", main = "Ray and Skate")
whale_prob_combine <- data.frame("ProbabilityBased50" = results_whale, "Random50" = results_random_whale, "ProbabilityBased10" = results_whale_10, "Random10"= results_random_whale_10)
boxplot(whale_prob_combine, ylab="Percentage of remaining PCA volume", main = "Whale")
dolphin_prob_combine <- data.frame("ProbabilityBased50" = results_dolphin,"Random50" = results_random_dolphin, "ProbabilityBased10" = results_dolphin_10, "Random10"= results_random_dolphin_10)
boxplot(dolphin_prob_combine, ylab="Percentage of remaining PCA volume", main = "Dolphin")
cetacean_prob_combine <- data.frame("ProbabilityBased50" = results_cetacean,"Random50" = results_random_cetacean, "ProbabilityBased10" = results_cetacean_10, "Random10"= results_random_cetacean_10)
boxplot(cetacean_prob_combine, ylab="Percentage of remaining PCA volume", main = "Cetacean")

write.csv(shark_prob_combine, "shark_prob.csv")
shark_prob_s <- read.csv("shark_prob.csv", header = TRUE)
shark_scatter <- ggplot(shark_prob_s, aes(x = X.of_species_remove, y = X.of_remaining_PCA_volume, color = type)) +
  geom_point()+
  labs(title = "Shark",             
       x = "percentage of species removed",                  
       y = "percentage of remaining PCA volume") +
  theme_classic()

write.csv(ray_skate_prob_combine, "ray_skate_prob.csv")
ray_skate_prob_s <- read.csv("ray_skate_prob.csv", header = TRUE)
ray_skate_scatter <- ggplot(ray_skate_prob_s, aes(x = X.of_species_remove, y = X.of_remaining_PCA_volume, color = type)) +
  geom_point()+
  labs(title = "Ray and Skate",             
       x = "percentage of species removed",                  
       y = "percentage of remaining PCA volume") +
  theme_classic()

write.csv(whale_prob_combine, "whale_prob.csv")
whale_prob_s <- read.csv("whale_prob.csv", header = TRUE)
whale_scatter <- ggplot(whale_prob_s, aes(x = X.of_species_remove, y = X.of_remaining_PCA_volume, color = type)) +
  geom_point()+
  labs(title = "Whale",             
       x = "percentage of species removed",                  
       y = "percentage of remaining PCA volume") +
  theme_classic()

write.csv(dolphin_prob_combine, "dolphin_prob.csv")
dolphin_prob_s <- read.csv("dolphin_prob.csv", header = TRUE)
dolphin_scatter <- ggplot(dolphin_prob_s, aes(x = X.of_species_remove, y = X.of_remaining_PCA_volume, color = type)) +
  geom_point()+
  labs(title = "Dolphin",             
       x = "percentage of species removed",                  
       y = "percentage of remaining PCA volume") +
  theme_classic()

write.csv(shark_prob_combine, "shark_prob.csv")
shark_prob_s <- read.csv("shark_prob.csv", header = TRUE)
shark_scatter <- ggplot(shark_prob_s, aes(x = X.of_species_remove, y = X.of_remaining_PCA_volume, color = type)) +
  geom_point()+
  labs(title = "Shark",             
       x = "percentage of species removed",                  
       y = "percentage of remaining PCA volume") +
  theme_classic()

write.csv(cetacean_prob_combine, "cetacean_prob.csv")
cetacean_prob_s <- read.csv("cetacean_prob.csv", header = TRUE)
cetacean_scatter <- ggplot(cetacean_prob_s, aes(x = X.of_species_remove, y = X.of_remaining_PCA_volume, color = type)) +
  geom_point()+
  labs(title = "Cetacean",             
       x = "percentage of species removed",                  
       y = "percentage of remaining PCA volume") +
  theme_classic()

ggarrange(shark_scatter, ray_skate_scatter,whale_scatter,dolphin_scatter,cetacean_scatter,common.legend = TRUE, legend = "right")

prob_50 <- read.csv("50.csv", header = TRUE)
prob_10 <- read.csv("10.csv", header = TRUE)
prob_50p <- ggplot(prob_50, aes(y=X.of_remaining_PCA_volume, x=group, colour = type))+
  geom_boxplot()+
  labs(title = "remove 50% of species", x = " ", y ="percentage of remaining PCA volume") +
  theme_classic()
prob_10p <- ggplot(prob_10, aes(y=X.of_remaining_PCA_volume, x=group, colour = type))+
  geom_boxplot()+
  labs(title = "remove 10% of species", x = " ", y ="percentage of remaining PCA volume") +
  theme_classic()

PCA_volume_p_shark <- (PCA_volume_shark/PCA_vol_shark)*100
data_Shark <- data.frame(PCA_volume_p_shark)
PCA_volume_p_ray_skate <- (PCA_volume_ray_skate/PCA_vol_ray_skate)*100
data_Ray_Skate <- data.frame(PCA_volume_p_ray_skate)
PCA_volume_p_whale <- (PCA_volume_whale/PCA_vol_whale)*100
data_whale <- data.frame(PCA_volume_p_whale)
PCA_volume_p_dolphin <- (PCA_volume_dolphin/PCA_vol_dolphin)*100
data_dolphin <- data.frame(PCA_volume_p_dolphin)
PCA_volume_p_cetacean <- (PCA_volume_cetacean/PCA_vol_cetacean)*100
data_cetacean <- data.frame(PCA_volume_p_cetacean)

random_exchange <- function(values) {
      n <- length(values)
      sample(values, size = n, replace = FALSE)
   }

# Initialize vectors and lists
PCA_volume_shark_random <- vector("numeric", 100)
PCA_ex_shark_random <- vector("numeric", 100)
remaining_species_shark_random <- list()

# Iterate through 100 iterations
for (i in 1:100) {
  # Subset the data frame and create the modified 'prob.ext' column
  extinction_shark_random <- shark_pca
  extinction_shark_random$ext.prob <- extinction_shark_random %>% mutate(ext.prob = random_exchange(ext.prob))
  
  # Create additional columns
  extinction_shark_random$exp.prob.ext <- runif(nrow(extinction_shark_random))
  extinction_shark_random$if.ext <- extinction_shark_random$exp.prob.ext < extinction_shark_random$prob.ext
  
  # Populate the 'remaining_species_shark_random' list
  remaining_species_shark_random[[i]] <- extinction_shark_random$species[extinction_shark_random$if.ext == FALSE]
  
  # Filter data frames based on conditions
  remaining_shark_1_random <- extinction_shark_random %>% filter(if.ext == FALSE)
  shark_pca_1_random <- shark_pca %>% filter(species %in% remaining_shark_1_random$species)
  
  # Subset the data for PCA
  shark_pca_matrix_1_random <- subset(shark_pca_1_random, select = c(PC1, PC2, PC3))
  
  # Perform calculations if enough data points are available
  if (nrow(shark_pca_matrix_1_random) >= 4) {
    PCA_volume_shark_random[i] <- with(convhulln(shark_pca_matrix_1_random, options = "FA"), vol)
    PCA_ex_shark_random[i] <- sum(extinction_shark_random$if.ext == TRUE)
  }
}

PCA_volume_shark_random_p <- (PCA_volume_shark_random/PCA_vol_shark)*100
data_shark_random <- data.frame(PCA_volume_shark_random_p)

shark_shu_pro <- as.data.frame((lengths(remaining_species_shark_random)/41)*100)

PCA_volume_ray_skate_random <- vector("numeric", 100)
PCA_ex_ray_skate_random <- vector("numeric", 100)
remaining_species_ray_skate_random <- list()

# Iterate through 100 iterations
for (k in 1:100) {
  
  extinction_ray_skate_random <- ray_skate_pca
  extinction_ray_skate_random$ext.prob <- random_exchange(extinction_ray_skate_random$prob.ext)
  
  extinction_ray_skate_random$ext.prob.ext <- runif(nrow(extinction_ray_skate_random))
  extinction_ray_skate_random$if.ext <- extinction_ray_skate_random$ext.prob.ext < extinction_ray_skate_random$ext.prob
  
  remaining_species_ray_skate_random[[k]] <- extinction_ray_skate_random$species[extinction_ray_skate_random$if.ext == FALSE]
  
  # Filter data frames based on conditions
  remaining_ray_skate_1_random <- extinction_ray_skate_random %>% filter(if.ext == FALSE)
  ray_skate_pca_1_random <- ray_skate_pca %>% filter(species %in% remaining_ray_skate_1_random$species)
  
  # Subset the data for PCA
  ray_skate_pca_matrix_1_random <- subset(ray_skate_pca_1_random, select = c(PC1, PC2, PC3))
  
  # Perform calculations if enough data points are available
  if (nrow(ray_skate_pca_matrix_1_random) >= 4) {
    PCA_volume_ray_skate_random[k] <- with(convhulln(ray_skate_pca_matrix_1_random, options = "FA"), vol)
    PCA_ex_ray_skate_random[k] <- sum(extinction_ray_skate_random$if.ext == TRUE)
  }
}

PCA_volume_ray_skate_random_p <- (PCA_volume_ray_skate_random/PCA_vol_ray_skate)*100
data_ray_skate_random <- data.frame(PCA_volume_ray_skate_random_p)
ray_shu_pro <- as.data.frame((lengths(remaining_species_ray_skate_random)/28)*100)

# Initialize vectors and lists
PCA_volume_whale_random <- vector("numeric", 100)
PCA_ex_whale_random <- vector("numeric", 100)
remaining_species_whale_random <- list()

# Iterate through 100 iterations
for (a in 1:100) {
  # Subset the data frame and create the modified 'prob.ext' column
  extinction_whale_random <- whale_pca
  extinction_whale_random <- extinction_whale_random %>%
    mutate(ext.prob = random_exchange(ext.prob))
  
  
  # Create additional columns
  extinction_whale_random$exp.prob.ext <- runif(nrow(extinction_whale_random))
  extinction_whale_random$if.ext <- extinction_whale_random$exp.prob.ext < extinction_whale_random$ext.prob
  
  # Populate the 'remaining_species_shark_random' list
  remaining_species_whale_random[[a]] <- extinction_whale_random$species[extinction_whale_random$if.ext == FALSE]
  
  # Filter data frames based on conditions
  remaining_whale_1_random <- extinction_whale_random %>% filter(if.ext == FALSE)
  whale_pca_1_random <- whale_pca %>% filter(species %in% remaining_whale_1_random$species)
  
  # Subset the data for PCA
  whale_pca_matrix_1_random <- subset(whale_pca_1_random, select = c(PC1, PC2, PC3))
  
  # Perform calculations if enough data points are available
  if (nrow(whale_pca_matrix_1_random) >= 4) {
    PCA_volume_whale_random[a] <- with(convhulln(whale_pca_matrix_1_random, options = "FA"), vol)
    PCA_SR_whale_random[a] <- sum(extinction_whale_random$if.ext == TRUE)
  }
}
PCA_volume_whale_random_p <- (PCA_volume_whale_random/PCA_vol_whale)*100
data_whale_random <- data.frame(PCA_volume_whale_random_p)
whale_shu_pro <- as.data.frame((lengths(remaining_species_whale_random)/45)*100)


# Initialize vectors and lists
PCA_volume_dolphin_random <- vector("numeric", 100)
PCA_ex_dolphin_random <- vector("numeric", 100)
remaining_species_dolphin_random <- list()

# Counter for successful iterations
successful_iterations <- 0

# Iterate until you have 100 valid values
for (i in 1:100) {
  while (TRUE) {
    # Subset the data frame and create the modified 'prob.ext' column
    extinction_dolphin_random <- dolphin_pca
    extinction_dolphin_random$ext.prob <- random_exchange(extinction_dolphin_random$ext.prob)
    
    extinction_dolphin_random$exp.prob.ext <- runif(nrow(extinction_dolphin_random))
    extinction_dolphin_random$if.ext <- extinction_dolphin_random$exp.prob.ext < extinction_dolphin_random$ext.prob
    
    remaining_species_dolphin_random[[i]] <- extinction_dolphin_random$species[extinction_dolphin_random$if.ext == FALSE]
    
    remaining_dolphin_1_random <- extinction_dolphin_random %>% filter(if.ext == FALSE)
    dolphin_pca_1_random <- dolphin_pca %>% filter(species %in% remaining_dolphin_1_random$species)
    
    # Subset the data for PCA
    dolphin_pca_matrix_1_random <- subset(dolphin_pca_1_random, select = c(PC1, PC2, PC3))
    
    # Perform calculations if enough data points are available
    if (nrow(dolphin_pca_matrix_1_random) >= 4) {
      PCA_volume_dolphin_random[successful_iterations + 1] <- with(convhulln(dolphin_pca_matrix_1_random, options = "FA"), vol)
      PCA_SR_dolphin_random[successful_iterations + 1] <- sum(remaining_dolphin_1_random$if.ext == TRUE)
      
      # Increment the successful_iterations counter
      successful_iterations <- successful_iterations + 1
      
      # Exit the inner loop
      break
    }
  }
  
  # Check if you have reached 100 successful iterations
  if (successful_iterations >= 100) {
    break
  }
}


PCA_volume_dolphin_random_p <- (PCA_volume_dolphin_random/PCA_vol_dolphin)*100
data_dolphin_random <- data.frame(PCA_volume_dolphin_random_p)
dolphin_shu_pro <- as.data.frame((lengths(remaining_species_dolphin_random)/29)*100)

# Initialize vectors and lists
PCA_volume_cetacean_random <- vector("numeric", 100)
PCA_ex_cetacean_random <- vector("numeric", 100)
remaining_species_cetacean_random <- list()

# Counter for successful iterations
successful_iterations <- 0

# Iterate until you have 100 valid values
for (i in 1:100) {
  while (TRUE) {
    # Subset the data frame and create the modified 'prob.ext' column
    extinction_cetacean_random <- cetacean_pca
    extinction_cetacean_random$ext.prob <- random_exchange(extinction_cetacean_random$ext.prob)
    
    extinction_cetacean_random$exp.prob.ext <- runif(nrow(extinction_cetacean_random))
    extinction_cetacean_random$if.ext <- extinction_cetacean_random$exp.prob.ext < extinction_cetacean_random$ext.prob
    
    remaining_species_cetacean_random[[i]] <- extinction_cetacean_random$species[extinction_cetacean_random$if.ext == FALSE]
    
    remaining_cetacean_1_random <- extinction_cetacean_random %>% filter(if.ext == FALSE)
    cetacean_pca_1_random <- cetacean_pca %>% filter(species %in% remaining_cetacean_1_random$species)
    
    # Subset the data for PCA
    cetacean_pca_matrix_1_random <- subset(cetacean_pca_1_random, select = c(PC1, PC2, PC3))
    
    # Perform calculations if enough data points are available
    if (nrow(cetacean_pca_matrix_1_random) >= 4) {
      PCA_volume_cetacean_random[successful_iterations + 1] <- with(convhulln(cetacean_pca_matrix_1_random, options = "FA"), vol)
      PCA_SR_cetacean_random[successful_iterations + 1] <- sum(remaining_cetacean_1_random$if.ext == TRUE)
      
      # Increment the successful_iterations counter
      successful_iterations <- successful_iterations + 1
      
      # Exit the inner loop
      break
    }
  }
  
  # Check if you have reached 100 successful iterations
  if (successful_iterations >= 100) {
    break
  }
}


PCA_volume_cetacean_random_p <- (PCA_volume_cetacean_random/PCA_vol_cetacean)*100
data_cetacean_random <- data.frame(PCA_volume_cetacean_random_p)
cetacean_shu_pro <- as.data.frame((lengths(remaining_species_cetacean_random)/80)*100)

data_all <- read.csv("data_all.csv", header = TRUE)
allp <- ggplot(data_all, aes(y=X.of_remaining_PCA_volume, x=group, colour = type))+
  geom_boxplot()+
  labs(title = "extinction scenario", x = " ", y ="percentage of remaining PCA volume") +
  theme_classic()
allp<- allp+scale_color_manual(values=c("#999999", "#E69F00"))
data_sr <- read.csv("data_sr.csv", header = TRUE)
srp <- ggplot(data_sr, aes(y=remaining_p, x=species, colour = type))+
  geom_boxplot()+
  labs(title="extinction scenario", x ="", y ="percentage of remaining species richness")+
  theme_classic()
srp<- srp+scale_color_manual(values=c("#999999", "#E69F00"))

ggarrange(prob_50p, prob_10p, allp, srp, common.legend = TRUE, legend = "right")

t.test(PCA_volume_p_shark, PCA_volume_shark_random_p, alternative = "greater")
t.test(PCA_volume_p_ray_skate, PCA_volume_ray_skate_random_p, alternative = "greater")
t.test(PCA_volume_p_whale, PCA_volume_whale_random_p, alternative = "greater")
t.test(PCA_volume_p_dolphin, PCA_volume_dolphin_random_p, alternative = "greater")
t.test(PCA_volume_p_cetacean, PCA_volume_cetacean_random_p, alternative = "greater")



shark_scatter <- ggplot()+
  geom_point(mapping=aes(x=PC1, y=PC2, colour = redlistCategory), data=shark_pca)+
  labs(title = "shark", x="PC1", y="PC2")+
  theme_classic()
ray_skate_scatter <-  ggplot()+
  geom_point(mapping=aes(x=PC1, y=PC2, colour = redlistCategory), data=ray_skate_pca)+
  labs(title = "ray and skate", x="PC1", y="PC2")+
  theme_classic()
whale_scatter <- ggplot()+
  geom_point(mapping=aes(x=PC1, y=PC2, colour = redlistCategory), data=whale_pca)+
  labs(title = "whale", x="PC1", y="PC2")+
  theme_classic()
dolphin_scatter <- ggplot()+
  geom_point(mapping=aes(x=PC1, y=PC2, colour = redlistCategory), data=dolphin_pca)+
  labs(title = "dolphin", x="PC1", y="PC2")+
  theme_classic()
porpoise_scatter <- ggplot()+
  geom_point(mapping=aes(x=PC1, y=PC2, colour = redlistCategory), data=porpoise_pca)+
  labs(title = "porpoise", x="PC1", y="PC2")+
  theme_classic()
cetacean_scatter <- ggplot()+
  geom_point(mapping=aes(x=PC1, y=PC2, colour = redlistCategory), data=cetacean_pca)+
  labs(title = "cetacean", x="PC1", y="PC2")+
  theme_classic()
ggarrange(shark_scatter,ray_skate_scatter,whale_scatter,dolphin_scatter,porpoise_scatter,cetacean_scatter,common.legend = TRUE, legend = "right")

model_shark_pc <- lm(ext.prob~PC1+PC2, data = shark_pca)
summary(model_shark_pc)
ggplot()+
  geom_point(mapping=aes(x=PC1, y= ext.prob), data=shark_pca)+
  theme_classic()
ggplot()+
  geom_point(mapping=aes(x=PC2, y= ext.prob), data=shark_pca)+
  theme_classic()
model_ray_skate_pc <- lm(prob.ext~PC1+PC2, data = ray_skate_pca)
summary(model_ray_skate_pc)
ggplot()+
  geom_point(mapping=aes(x=PC1, y= prob.ext), data=ray_skate_pca)+
  theme_classic()
ggplot()+
  geom_point(mapping=aes(x=PC2, y= prob.ext), data=ray_skate_pca)+
  theme_classic()
model_whale_pc <- lm(ext.prob~PC1+PC2, data = whale_pca)
summary(model_whale_pc)
ggplot()+
  geom_point(mapping=aes(x=PC1, y= ext.prob), data=whale_pca)+
  theme_classic()
ggplot()+
  geom_point(mapping=aes(x=PC2, y= ext.prob), data=whale_pca)+
  theme_classic()
model_dolphin_pc <- lm(ext.prob~PC1+PC2, data = dolphin_pca)
summary(model_dolphin_pc)
ggplot()+
  geom_point(mapping=aes(x=PC1, y= ext.prob), data=dolphin_pca)+
  theme_classic()
ggplot()+
  geom_point(mapping=aes(x=PC2, y= ext.prob), data=dolphin_pca)+
  theme_classic()
model_porpoise_pc <- lm(ext.prob~PC1+PC2, data = porpoise_pca)
summary(model_porpoise_pc)
ggplot()+
  geom_point(mapping=aes(x=PC1, y= ext.prob), data=porpoise_pca)+
  theme_classic()
ggplot()+
  geom_point(mapping=aes(x=PC2, y= ext.prob), data=porpoise_pca)+
  theme_classic()
model_cetacean_pc <- lm(ext.prob~PC1+PC2, data = cetacean_pca)
summary(model_cetacean_pc)
ggplot()+
  geom_point(mapping=aes(x=PC1, y= ext.prob), data=cetacean_pca)+
  theme_classic()
ggplot()+
  geom_point(mapping=aes(x=PC2, y= ext.prob), data=cetacean_pca)+
  theme_classic()
