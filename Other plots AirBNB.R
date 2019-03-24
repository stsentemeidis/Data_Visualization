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
spacing <-15

# OTHER PLOTS
################################################################
ggplot(data,aes(x = data$host_response_time[data$host_response_time!=c('NA')], fill = data$host_is_superhost)) +
  geom_bar()+ scale_fill_manual(values = alpha(c(color1, color4)))+
  theme_tufte(ticks=FALSE, base_size = 8)+ scale_y_continuous(labels=comma)+
  theme(legend.position = 'None',
        axis.title = element_blank(),
        axis.text.x  = element_text(size = 8, family = font2),
        axis.text.y  = element_text(size = 8, family = font2))

grid.text(0.95, unit(1,"npc") - unit(0.9,"line"), check.overlap = T,just = "right",vjust = 2,
          label=paste("Superhost",paste(rep(" ",spacing), collapse='')),
          gp=gpar(col=color4, fontsize=16,fontface="bold"))
grid.text(0.95, unit(1,"npc") - unit(1,"line"), check.overlap = T,just = "right",
          label=paste("Host",paste(rep(" ",spacing), collapse='')),
          gp=gpar(col=color1, fontsize=16,fontface="bold"))
################################################################
ggplot(data,aes(x = data$room_type, fill = data$host_is_superhost)) +
  geom_bar()+ scale_y_continuous(labels=comma)+scale_fill_manual(values = alpha(c(color1, color4)))+
  theme_tufte(ticks=FALSE, base_size = 8)+
  theme(legend.position = 'None',
        axis.title = element_blank(),
        axis.text.x  = element_text(size = 8, family = font2),
        axis.text.y  = element_text(size = 8, family = font2))

grid.text(0.95, unit(1,"npc") - unit(0.9,"line"), check.overlap = T,just = "right",vjust = 2,
          label=paste("Superhost",paste(rep(" ",spacing), collapse='')),
          gp=gpar(col=color4, fontsize=16,fontface="bold"))
grid.text(0.95, unit(1,"npc") - unit(1,"line"), check.overlap = T,just = "right",
          label=paste("Host",paste(rep(" ",spacing), collapse='')),
          gp=gpar(col=color1, fontsize=16,fontface="bold"))
################################################################
ggplot(data,aes(x = data$neighbourhood, fill = data$host_is_superhost)) +
  geom_bar()+
  theme_tufte(ticks=FALSE, base_size = 8)+ scale_y_continuous(labels=comma) + scale_fill_manual(values = alpha(c(color1, color4)))+
  theme(legend.position = 'None',
        axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
        axis.title = element_blank(),
        axis.text.y  = element_text(size = 8, family = font2))

grid.text(0.95, unit(1,"npc") - unit(0.9,"line"), check.overlap = T,just = "right",vjust = 2,
          label=paste("Superhost",paste(rep(" ",spacing), collapse='')),
          gp=gpar(col=color4, fontsize=16,fontface="bold"))
grid.text(0.95, unit(1,"npc") - unit(1,"line"), check.overlap = T,just = "right",
          label=paste("Host",paste(rep(" ",spacing), collapse='')),
          gp=gpar(col=color1, fontsize=16,fontface="bold"))
################################################################
ggplot(data,aes(x = data$neighbourhood, fill = data$instant_bookable)) +
  geom_bar()+
  theme_tufte(ticks=FALSE, base_size = 8)+ scale_y_continuous(labels=comma)+scale_fill_manual(values = alpha(c(color1, color4)))+
  theme(legend.position = 'None',
        axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
        axis.title = element_blank(),
        axis.text.y  = element_text(size = 8, family = font2))
grid.text(0.95, unit(1,"npc") - unit(0.9,"line"), check.overlap = T,just = "right",vjust = 2,
          label=paste("Instant Book",paste(rep(" ",spacing), collapse='')),
          gp=gpar(col=color4, fontsize=16,fontface="bold"))
grid.text(0.95, unit(1,"npc") - unit(1,"line"), check.overlap = T,just = "right",
          label=paste("Pending Approval",paste(rep(" ",spacing), collapse='')),
          gp=gpar(col=color1, fontsize=16,fontface="bold"))
################################################################
ggplot(data,aes(x = data$neighbourhood, fill = data$is_location_exact)) +
  geom_bar()+
  theme_tufte(ticks=FALSE, base_size = 8)+ scale_y_continuous(labels=comma)+scale_fill_manual(values = alpha(c(color1, color4)))+
  theme(legend.position = 'None',
        axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
        axis.title = element_blank(),
        axis.text.y  = element_text(size = 8, family = font2))

grid.text(1.1, unit(1,"npc") - unit(0.9,"line"), check.overlap = T,just = "right",vjust = 2,
          label=paste("Location Accurate",paste(rep(" ",spacing), collapse='')),
          gp=gpar(col=color4, fontsize=16,fontface="bold"))
grid.text(1.1, unit(1,"npc") - unit(1,"line"), check.overlap = T,just = "right",
          label=paste("Location Not Accurate",paste(rep(" ",spacing), collapse='')),
          gp=gpar(col=color1, fontsize=16,fontface="bold"))
################################################################
ggplot(data,aes(x = data$cancellation_policy, fill = data$host_is_superhost)) +
  geom_bar()+
  theme_tufte(ticks=FALSE, base_size = 8)+ scale_y_continuous(labels=comma)+scale_fill_manual(values = alpha(c(color1, color4)))+
  scale_x_discrete()+
  theme(legend.position = 'None',
        axis.text.x = element_text(angle = 35,hjust = 1, size = 7),
        axis.title = element_blank(),
        axis.text.y  = element_text(size = 8, family = font2))

grid.text(1.1, unit(1,"npc") - unit(0.9,"line"), check.overlap = T,just = "right",vjust = 2,
          label=paste("Superhost",paste(rep(" ",spacing), collapse='')),
          gp=gpar(col=color4, fontsize=16,fontface="bold"))
grid.text(1.1, unit(1,"npc") - unit(1,"line"), check.overlap = T,just = "right",
          label=paste("Host",paste(rep(" ",spacing), collapse='')),
          gp=gpar(col=color1, fontsize=16,fontface="bold"))
################################################################
ggplot(data,aes(x = data$cancellation_policy, fill = data$room_type)) +
  geom_bar()+
  theme_tufte(ticks=FALSE, base_size = 8)+ scale_y_continuous(labels=comma)+scale_fill_manual(values = alpha(c(color1, color4,'green3')))+
  theme(legend.position = 'None',
        axis.text.x = element_text(angle = 35, hjust = 1, size = 10),
        axis.title = element_blank(),
        axis.text.y  = element_text(size = 8, family = font2))

grid.text(1.1, unit(1,"npc") - unit(0.9,"line"), check.overlap = T,just = "right",vjust = 2,
          label=paste("Private Room",paste(rep(" ",spacing), collapse='')),
          gp=gpar(col=color4, fontsize=14,fontface="bold"))
grid.text(1.1, unit(1,"npc") - unit(1,"line"), check.overlap = T,just = "right",
          label=paste("Entire Home/Apt",paste(rep(" ",spacing), collapse='')),
          gp=gpar(col=color1, fontsize=14,fontface="bold"))


