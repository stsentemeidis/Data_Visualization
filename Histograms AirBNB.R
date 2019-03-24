# INSTALL AND LOAD PACKAGES ----

packages_list <- c('ggplot2',
                   'ggalt',
                   'gridExtra',
                   'scales',
                   'grid',
                   'lattice',
                   'ggthemes',
                   'extrafont',
                   'plotly',
                   'plyr',
                   'leaflet',
                   'maps'
)

for (i in packages_list){
  if(!i%in%installed.packages()){
    install.packages(i, dependencies = TRUE, repos = "http://cran.us.r-project.org")
    library(i, character.only = TRUE)
    print(paste0(i, ' has been installed'))
  } else {
    print(paste0(i, ' is already installed'))
    library(i, character.only = TRUE)
  }
}


# READ DATASET -
data <- read.csv('/Users/Ashley/Clouds/Google Drive IE/01. MBD/02. Term 2/00. O-1-7 - Term 2/04. Data Visualization/Final Assignment - Part 2/Possible Datasets/Airbnb Amsterdam/airbnb-amsterdam/listings_clean.csv')

# VARIABLE TYPE CHECKING
str(data)
data$host_name <- as.character(data$host_name)

# COLOR PALETTE AND FONTS ----
# We used the colors of the AirBNB logo for our charts.

# Palette 1
color1 = rgb(255/255, 90/255, 96/255, 1)
color2 = 'white'
color3 = 'black'
color4 = rgb(90/255, 101/255, 255/255, 1)
font1 = 'Impact'
font2 = 'Trebuchet MS'

# DISTRIBUTIONS
################################################################
ggplot(data=data, aes(data$accommodates)) + 
  geom_histogram(col=color1, breaks=seq(0, 9, by=1),
                 aes(fill=color1), fill = color1) +
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color3, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color2))+ scale_y_continuous(labels = comma)
grid.text(unit(0.7, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Accomodates",
          gp=gpar(col=color1, fontsize=16, fontfamily = font2))
################################################################
ggplot(data=data, aes(data$bathrooms)) + 
  geom_histogram(col=color1, breaks=seq(0, 4, by=1),
                 aes(fill=color1), fill = color1) +
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color3, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color2))+ scale_y_continuous(labels = comma)
grid.text(unit(0.7, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Bathrooms",
          gp=gpar(col=color1, fontsize=16, fontfamily = font2))
################################################################
ggplot(data=data, aes(data$beds)) + 
  geom_histogram(col= color1, breaks=seq(0, 4, by=1),
                 aes(fill=color1), fill = color1) +
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color3, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color2))+ scale_y_continuous(labels = comma)
grid.text(unit(0.7, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Beds",
          gp=gpar(col=color1, fontsize=16, fontfamily = font2))
################################################################
ggplot(data=data, aes(data$host_total_listings_count)) + 
  geom_histogram(col= color1, breaks=seq(0, 6, by=1),
                 aes(fill=color1), fill = color1) +
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color3, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color2))+ scale_y_continuous(labels = comma)
grid.text(unit(0.6, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Host's Total Listings",
          gp=gpar(col=color1, fontsize=16, fontfamily = font2))
################################################################
ggplot(data=data, aes(data$price)) + 
  geom_histogram(col= color1,
                 aes(fill=color1),binwidth = 10, fill = color1) +
  xlim(0,600)+
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color3, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color2))+ scale_y_continuous(labels = comma)
grid.text(unit(0.7, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Price",
          gp=gpar(col=color1, fontsize=16, fontfamily = font2))
################################################################
ggplot(data=data, aes(data$weekly_price)) + 
  geom_histogram(col= color1,
                 aes(fill=color1),binwidth = 10) +
  xlim(0,400)+ ylim(0,300)+
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color3, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color2))
grid.text(unit(0.2, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Weekly Price",
          gp=gpar(col=color1, fontsize=16, fontfamily = font2))
################################################################
ggplot(data=data, aes(data$monthly_price)) + 
  geom_histogram(col= color1,
                 aes(fill=color1),binwidth = 10, fill = color1) +
  xlim(0,380)+ ylim(0,140)+
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color3, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color2))
grid.text(unit(0.6, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Monthly Price",
          gp=gpar(col=color1, fontsize=16, fontfamily = font2))
################################################################
ggplot(data=data, aes(data$security_deposit)) + 
  geom_histogram(col= color1,
                 aes(fill=color1),binwidth = 3, fill = color1) +
  xlim(0,180)+ ylim(0,140)+
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color3, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color2))
grid.text(unit(0.2, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Security Deposit",
          gp=gpar(col=color1, fontsize=16, fontfamily = font2))
################################################################
ggplot(data=data, aes(data$cleaning_fee)) + 
  geom_histogram(col= color1,
                 aes(fill=color1),binwidth = 10,fill=color1) +
  xlim(0,380)+ ylim(0,150)+
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color3, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color2))
grid.text(unit(0.6, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Cleaning Fee",
          gp=gpar(col=color1, fontsize=16, fontfamily = font2))
################################################################
ggplot(data=data, aes(data$price_two_nights_two_people)) + 
  geom_histogram(col= color1,
                 aes(fill=color1),binwidth = 5, fill = color1) +
  xlim(25,140)+ ylim(0,150)+
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color3, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color2))
grid.text(unit(0.2, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Price for 2 nights for 2 guest",
          gp=gpar(col=color1, fontsize=16, fontfamily = font2))
################################################################
ggplot(data=data, aes(data$minimum_nights)) + 
  geom_histogram(col= color1,
                 aes(fill=color1),binwidth = 5, fill = color1) +
  xlim(0,100)+ ylim(0,150)+
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color3, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color2))
grid.text(unit(0.6, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Minimum Nights",
          gp=gpar(col=color1, fontsize=16, fontfamily = font2))
################################################################
ggplot(data=data, aes(data$maximum_nights)) + 
  geom_histogram(col= color1,
                 aes(fill=color1),binwidth = 5, fill = color1) +
  xlim(26,200)+ ylim(0,130)+
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color3, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color2))
grid.text(unit(0.6, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Maximum Nights",
          gp=gpar(col=color1, fontsize=16, fontfamily = font2))
################################################################
ggplot(data=data, aes(data$availability_30)) + 
  geom_histogram(col= color1,
                 aes(fill=color1),binwidth = 2, fill = color1) +
  xlim(0,35)+ scale_y_continuous(labels = comma,limits = c(0,1500))+
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color3, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color2))
grid.text(unit(0.6, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Availability 30",
          gp=gpar(col=color1, fontsize=16, fontfamily = font2))
################################################################
ggplot(data=data, aes(data$availability_60)) + 
  geom_histogram(col= color1,
                 aes(fill=color1),binwidth = 2, fill = color1) +
  xlim(0,70)+ ylim(0,800)+
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color3, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color2))
grid.text(unit(0.6, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Availability 60",
          gp=gpar(col=color1, fontsize=16, fontfamily = font2))
################################################################
ggplot(data=data, aes(data$availability_90)) + 
  geom_histogram(col= color1,
                 aes(fill=color1),binwidth = 2, fill = color1) +
  xlim(0,100)+ ylim(0,660)+
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color3, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color2))
grid.text(unit(0.6, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Availability 90",
          gp=gpar(col=color1, fontsize=16, fontfamily = font2))
################################################################
ggplot(data=data, aes(data$availability_365)) + 
  geom_histogram(col= color1,
                 aes(fill=color1),binwidth = 2, fill = color1) +
  xlim(0,200)+ ylim(0,550)+
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color3, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color2))
grid.text(unit(0.6, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Availability 365",
          gp=gpar(col=color1, fontsize=16, fontfamily = font2))
################################################################
ggplot(data=data, aes(data$number_of_reviews)) + 
  geom_histogram(col= color1,
                 aes(fill=color1),binwidth = 5, fill = color1) +
  xlim(0,300)+ ylim(0,500)+
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color3, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color2))
grid.text(unit(0.6, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Number of Reviews",
          gp=gpar(col=color1, fontsize=16, fontfamily = font2))
################################################################
ggplot(data=data, aes(data$reviews_per_month)) + 
  geom_histogram(col= color1,
                 aes(fill=color1),binwidth = 1, fill = color1) +
  xlim(7,15)+ ylim(0,50)+
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color3, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color2))
grid.text(unit(0.6, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Reviews per Month",
          gp=gpar(col=color1, fontsize=16, fontfamily = font2))
################################################################
ggplot(data=data, aes(data$review_scores_rating)) + 
  geom_histogram(col= color1,
                 aes(fill=color1),binwidth = 2, fill = color1) +
  xlim(40,100)+ scale_y_continuous(labels = comma, limits = c(0,3000))+
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color3, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color2))
grid.text(unit(0.2, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Review Scores rating",
          gp=gpar(col=color1, fontsize=16, fontfamily = font2))
################################################################
ggplot(data=data, aes(data$review_scores_accuracy)) + 
  geom_histogram(col= color1,
                 aes(fill=color1),binwidth = 1, fill = color1) +
  xlim(0,8)+ ylim(0,100)+
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color3, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color2))
grid.text(unit(0.2, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Review Scores Accuracy",
          gp=gpar(col=color1, fontsize=16, fontfamily = font2))
################################################################
ggplot(data=data, aes(data$review_scores_cleanliness)) + 
  geom_histogram(col= color1,
                 aes(fill=color1),binwidth = 1, fill = color1) +
  xlim(0,6)+ ylim(0,40)+
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color3, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color2))
grid.text(unit(0.1, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Review Scores Cleanliness",
          gp=gpar(col=color1, fontsize=16, fontfamily = font2))
################################################################
ggplot(data=data, aes(data$review_scores_checkin)) + 
  geom_histogram(col= color1,
                 aes(fill=color1),binwidth = 1, fill = color1) +
  xlim(0,6.5)+ ylim(0,21)+
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color3, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color2))
grid.text(unit(0.1, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Review Scores Check-In",
          gp=gpar(col=color1, fontsize=16, fontfamily = font2))
################################################################
ggplot(data=data, aes(data$review_scores_communication)) + 
  geom_histogram(col= color1,
                 aes(fill=color1),binwidth = 1, fill = color1) +
  xlim(0,6.5)+ ylim(0,21)+
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color3, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color2))
grid.text(unit(0.45, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Review Scores Communication",
          gp=gpar(col=color1, fontsize=16))
################################################################
ggplot(data=data, aes(data$review_scores_location)) + 
  geom_histogram(col= color1,
                 aes(fill=color1),binwidth = 1, fill = color1) +
  xlim(0,6.5)+ ylim(0,10)+
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color3, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color2))
grid.text(unit(0.1, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Review Scores Location",
          gp=gpar(col=color1, fontsize=16, fontfamily = font2))

