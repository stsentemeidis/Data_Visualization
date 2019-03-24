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
data <- read.csv('listings_clean.csv')

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

# SCATTER PLOTS
################################################################
ggplot(data, aes(data$price, data$number_of_reviews, color = color1)) +
  geom_point(shape = 16, size = 1, show.legend = FALSE) +
  scale_x_continuous(labels = comma, limits = c(0,600))+ scale_y_continuous(labels = comma, limits = c(0,300))+
  theme_tufte()+ labs(x = "Price", y='Number of Reviews')+
  theme(axis.ticks = element_blank(),
        axis.text.y.left = element_text(hjust = 1.5, family = font2),
        axis.text.x.bottom = element_text(vjust = 5, family = font2))
grid.text(unit(0.5, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Price & No of Reviews",
          gp=gpar(col=color1, fontsize=16, fontfamily = font2))
################################################################
ggplot(data, aes(data$price, data$availability_365, color = color1)) +
  geom_point(shape = 16, size = 1, show.legend = FALSE) +
 ylim(0,400) +scale_x_continuous(labels = comma, limits = c(0,1000))+
  theme_tufte()+ labs(x = "Price", y='Number of Reviews')+
  theme(axis.ticks = element_blank(),
        axis.text.y.left = element_text(hjust = 1.5, family = font2),
        axis.text.x.bottom = element_text(vjust = 5, family = font2))
grid.text(unit(0.6, 'npc'), unit(0.95,"npc"), check.overlap = T,just = "left",
          label="Price & No of Reviews",
          gp=gpar(col=color1, fontsize=16, fontfamily = font2))
################################################################
ggplot(data, aes(data$price, data$review_scores_rating, color = color1)) +
  geom_point(shape = 16, size = 1, show.legend = FALSE) +
   ylim(40,100) +scale_x_continuous(labels = comma, limits = c(0,1000))+
  theme_tufte()+ labs(x = "Price", y='Number of Reviews')+
  theme(axis.ticks = element_blank(),
        axis.text.y.left = element_text(hjust = 1.5, family = font2),
        axis.text.x.bottom = element_text(vjust = 5, family = font2))
grid.text(unit(0.55, 'npc'), unit(0.2,"npc"), check.overlap = T,just = "left",
          label="Price & Reviews Scores Rating",
          gp=gpar(col=color1, fontsize=14, fontfamily = font2))
################################################################
ggplot(data, aes(data$weekly_price, data$price_two_nights_two_people, color = color1)) +
  geom_point(shape = 16, size = 1, show.legend = FALSE) +
  xlim(0,500)+ scale_y_continuous(labels = comma, limits = c(0,2500))+
  theme_tufte()+ labs(x = "Price", y='Number of Reviews')+
  theme(axis.ticks = element_blank(),
        axis.text.y.left = element_text(hjust = 1.5, family = font2),
        axis.text.x.bottom = element_text(vjust = 5, family = font2))
grid.text(unit(0.45, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Price & Reviews Scores Rating",
          gp=gpar(col=color1, fontsize=14, fontfamily = font2))

