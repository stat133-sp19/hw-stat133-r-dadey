#Load packages
library("jpeg")
library("grid")
library("ggplot2")

#Set wd for exercise
setwd("C:/Users/RD/Documents/R/workout1/images")

#Load court image
court <- "./nba-court.jpg"

court.img <- rasterGrob(readJPEG(court),
                        width = unit(1, "npc"),
                        height = unit(1, "npc"))

#Create shot charts

theme_set(theme_minimal())

klay.shot.chart <- ggplot(data = klay.thompson) +
  annotation_custom(court.img, xmin = -250, xmax = 250, ymin = -50, ymax = 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Short Chart:  Klay Thompson (2016 season)') +
  theme_minimal()
  
draymond.shot.chart <- ggplot(data = draymond.green) +
  annotation_custom(court.img, xmin = -250, xmax = 250, ymin = -50, ymax = 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Short Chart:  Draymond Green (2016 season)') +
  theme_minimal()

stephen.shot.chart <- ggplot(data = stephen.curry) +
  annotation_custom(court.img, xmin = -250, xmax = 250, ymin = -50, ymax = 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Short Chart:  Stephen Curry (2016 season)') +
  theme_minimal()

andre.shot.chart <- ggplot(data = andre.iguodala) +
  annotation_custom(court.img, xmin = -250, xmax = 250, ymin = -50, ymax = 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Short Chart:  Andre Iguodala (2016 season)') +
  theme_minimal()

kevin.shot.chart <- ggplot(data = kevin.durant) +
  annotation_custom(court.img, xmin = -250, xmax = 250, ymin = -50, ymax = 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Short Chart:  Keven Durant (2016 season)') +
  theme_minimal()

all.shot.chart <- ggplot(data = shots.data, aes(x = x, y = y, color = shot_made_flag)) +
  annotation_custom(court.img, xmin = -250, xmax = 250, ymin = -50, ymax = 420) +
  geom_point() +
  ylim(-50, 420) +
  facet_wrap(~ name, nrow = 2) +
  theme_minimal() +
  theme(legend.position = "top", strip.background = NULL, plot.background = NULL) +
  ggtitle('Short Charts:  GSW (2016 season)')

#Print as PDFs
pdf(file = "./andre.iguodala.shot.chart.pdf", 6.5, 5)
print(andre.shot.chart)
dev.off()

pdf(file = "./stephen.curry.shot.chart.pdf", 6.5, 5)
print(stephen.shot.chart)
dev.off()

pdf(file = "./draymond.green.shot.chart.pdf", 6.5, 5)
print(draymond.shot.chart)
dev.off()

pdf(file = "./kevin.durant.shot.chart.pdf", 6.5, 5)
print(kevin.shot.chart)
dev.off()

pdf(file = "./klay.thompson.shot.chart.pdf", 6.5, 5)
print(klay.shot.chart)
dev.off()

pdf(file = "./gsw.shot.charts.pdf", 8, 7)
print(all.shot.chart)
dev.off()

png(filename = "./gsw.shot.charts.png", 8, 7, units = "in", res = 120)
print(all.shot.chart)
dev.off()
#END

