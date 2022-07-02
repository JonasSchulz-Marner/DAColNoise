library("ggplot2")
library(gridExtra)

a <- 500
b <- 100

set.seed(11)
X_1 <- rnorm(a, 2, 0.7)
Y_1 <- rnorm(a, 1, 1.2)
X_2 <- rnorm(b, 0, 1)
Y_2 <- rnorm(b, 0, 1)
class <- as.factor(c(rep(2, a), rep(1,b)))
class_1 <- data.frame(X = c(X_1,X_2), Y = c(Y_1,Y_2), class = class)
ggplot(class_1, aes(x = X, y = Y, color = class, shape = class)) + geom_point(size = 3) + theme_grey(base_size = 22)


smote_data <- smotefamily::SMOTE(X = class_1[,c(1,2)], target = class, K = 5, dup_size = 4)$data
ggplot(smote_data) + geom_point(aes(x = X, y = Y, color = class, shape = class), size = 3) +
  geom_point(data = subset(smote_data, class == 1), aes(x = X, y = Y, color = class, shape = class), size = 3)+ theme_grey(base_size = 22)


adasyn_data <- smotefamily::ADAS(X = class_1[-3], target = class_1[3], K = 5)$data
ggplot(adasyn_data) + geom_point(aes(x = X, y = Y, color = class, shape = class), size = 3) +
  geom_point(data = subset(adasyn_data, class == 1), aes(x = X, y = Y, color = class, shape = class), size = 3)+ theme_grey(base_size = 22)


set.seed(10)
white_noise = data.frame(index = seq(1,200), noise = noise("power", alpha = 0)@left[1:200])
ggplot(white_noise) + geom_line(aes(x = index, y = noise)) + theme_grey(base_size = 15)


set.seed(10)
noise = noise("power", alpha = 0)@left
s = 1
augmented_x = c()
for (i in 1:4){
  augmented_x = c(augmented_x,class_1[which(class_1$class == 1),1]+noise[((100*(i-1))+1):((100*(i-1))+100)]*s)
}

augmented_y = c()
for (i in 1:4){
  augmented_y = c(augmented_y,class_1[which(class_1$class == 1),2]+noise[((100*(i-1))+401):((100*(i-1))+500)]*s)
}

augmented_data = data.frame(X = augmented_x, Y = augmented_y, class = 1)
augmented_data = rbind(class_1, augmented_data)

ggplot(augmented_data) + geom_point(aes(x = X, y = Y, color = class, shape = class), size = 3) +
  geom_point(data = subset(augmented_data, class == 1), aes(x = X, y = Y, color = class, shape = class), size = 3)+ theme_grey(base_size = 22)




# spectrum of the noise
length = 100
# violet
set.seed(11)
noise <- tuneR::noise(kind = "power", alpha = -2)@left[1:3000]
pow_noise = powspec(noise)
psd_noise = data.frame(Y = log(pow_noise[,5]), 
                       X = log(seq(1,128)), 
                       X_line = seq(0,5, length.out = 128),
                       line = seq(min(log(pow_noise[,5])),min(log(pow_noise[,5]))+10, length.out = 128))
plot_violet1 = ggplot(psd_noise) + geom_line(aes(x=X, y=Y)) +
  geom_line(aes(x=X_line, y=line), color = "orange") + 
  xlab("log frequency") + ylab("log power") + ggtitle("power density spectrum") + 
  theme(plot.title=element_text(size = 12, hjust = 0.5)) + 
  ylim(-10,5.81) + 
  theme(axis.title = element_text(size=8))

signal_noise = data.frame(Y = noise[1:length],
                          X = seq(1,length))
plot_violet2 = ggplot(signal_noise) + geom_line(aes(x=X, y=Y), color = "#a31fe0") +
  ylim(-0.75,0.75) + 
  xlab("") + ylab("") + ggtitle(expression(paste("violet noise with ", beta, " = -2"))) + 
  theme(plot.title=element_text(size = 12, face = "bold", hjust = 0.5))


# blue
set.seed(15)
noise <- tuneR::noise(kind = "power", alpha = -1)@left[1:3000]
pow_noise = powspec(noise)
psd_noise = data.frame(Y = log(pow_noise[,5]), 
                       X = log(seq(1,128)), 
                       X_line = seq(0,5, length.out = 128),
                       line = seq(min(log(pow_noise[,5]))+1,min(log(pow_noise[,5]))+6, length.out = 128))
plot_blue1 = ggplot(psd_noise) + geom_line(aes(x=X, y=Y)) +
  geom_line(aes(x=X_line, y=line), color = "orange") + 
  xlab("log frequency") + ylab("log power") + 
  theme(plot.title=element_text(size = 12, hjust = 0.5)) + 
  ylim(-10,5.81) + 
  theme(axis.title = element_text(size=8))

signal_noise = data.frame(Y = noise[1:length],
                          X = seq(1,length))
plot_blue2 = ggplot(signal_noise) + geom_line(aes(x=X, y=Y), color = "blue") +
  ylim(-0.75,0.75) + 
  xlab("") + ylab("") + ggtitle(expression(paste("blue noise with ", beta, " = -1"))) + 
  theme(plot.title=element_text(size = 12, face = "bold", hjust = 0.5))


# white
set.seed(7)
noise <- tuneR::noise(kind = "power", alpha = 0)@left[1:3000]
pow_noise = powspec(noise)
psd_noise = data.frame(Y = log(pow_noise[,5]), 
                       X = log(seq(1,128)), 
                       X_line = seq(0,5, length.out = 128),
                       line = seq(mean(log(pow_noise[,5])),mean(log(pow_noise[,5])), length.out = 128))
plot_white1 = ggplot(psd_noise) + geom_line(aes(x=X, y=Y)) +
  geom_line(aes(x=X_line, y=line), color = "orange") + 
  xlab("log frequency") + ylab("log power") + 
  theme(plot.title=element_text(size = 12, hjust = 0.5)) + 
  ylim(-10,5.81) + 
  theme(axis.title = element_text(size=8))

signal_noise = data.frame(Y = noise[1:length],
                          X = seq(1,length))
plot_white2 = ggplot(signal_noise) + geom_line(aes(x=X, y=Y)) +
  ylim(-0.75,0.75) + 
  xlab("") + ylab("") + ggtitle(expression(paste("white noise with ", beta, " = 0 "))) + 
  theme(plot.title=element_text(size = 12, face = "bold", hjust = 0.5))


# pink
set.seed(7)
noise <- tuneR::noise(kind = "power", alpha = 1)@left[1:3000]
pow_noise = powspec(noise)
psd_noise = data.frame(Y = log(pow_noise[,5]), 
                       X = log(seq(1,128)), 
                       X_line = seq(0,5, length.out = 128),
                       line = seq(max(log(pow_noise[,5])),max(log(pow_noise[,5]))-5, length.out = 128))
plot_pink1 = ggplot(psd_noise) + geom_line(aes(x=X, y=Y)) +
  geom_line(aes(x=X_line, y=line), color = "orange") + 
  xlab("log frequency") + ylab("log power") +
  theme(plot.title=element_text(size = 12, hjust = 0.5)) + 
  ylim(-10,5.81) + 
  theme(axis.title = element_text(size=8))

signal_noise = data.frame(Y = noise[1:length],
                          X = seq(1,length))
plot_pink2 = ggplot(signal_noise) + geom_line(aes(x=X, y=Y), color = "#eb36d0") +
  ylim(-0.75,0.75) + 
  xlab("") + ylab("") + ggtitle(expression(paste("pink noise with ", beta, " = 1, "))) + 
  theme(plot.title=element_text(size = 12, face = "bold", hjust = 0.5))


# red
set.seed(6)#1
noise <- tuneR::noise(kind = "power", alpha = 2)@left[1:3000]
pow_noise = powspec(noise)
psd_noise = data.frame(Y = log(pow_noise[,5]), 
                       X = log(seq(1,128)), 
                       X_line = seq(0,5, length.out = 128),
                       line = seq(max(log(pow_noise[,5]))-5,max(log(pow_noise[,5]))-15, length.out = 128))
plot_red1 = ggplot(psd_noise) + geom_line(aes(x=X, y=Y)) +
  geom_line(aes(x=X_line, y=line), color = "orange") + 
  xlab("log frequency") + ylab("log power") + 
  theme(plot.title=element_text(size = 12, hjust = 0.5)) + 
  ylim(-10,5.81) + 
  theme(axis.title = element_text(size=8))

signal_noise = data.frame(Y = noise[1:length],
                          X = seq(1,length))
plot_red2 = ggplot(signal_noise) + geom_line(aes(x=X, y=Y), color = "red") +
  ylim(-0.75,0.75) +  
  xlab("") + ylab("") + ggtitle(expression(paste("red noise with ", beta, " = 2, "))) + 
  theme(plot.title=element_text(size = 12, face = "bold", hjust = 0.5))

grid.arrange(plot_violet2, plot_violet1, 
             plot_blue2, plot_blue1, 
             plot_white2, plot_white1, 
             plot_pink2, plot_pink1,
             plot_red2, plot_red1,ncol=2)


