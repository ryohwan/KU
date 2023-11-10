library(ggplot2)
mpg

ggplot(mpg) + geom_point(mapping=aes(x=displ, y=hwy))
ggplot(mpg) + geom_point(aes(displ, hwy))

ggplot(mpg) + geom_point(aes(displ, hwy, color=class))
ggplot(mpg) + geom_point(aes(displ, hwy, color="blue"))
ggplot(mpg) + geom_point(aes(displ, hwy), color="blue")

ggplot(mpg) + geom_smooth(aes(displ, hwy))
ggplot(mpg) + geom_point(aes(displ, hwy)) + geom_smooth(aes(displ, hwy))
ggplot(mpg, aes(displ, hwy)) + geom_point() + geom_smooth()
ggplot(mpg, aes(displ, hwy)) + geom_point() + geom_smooth(se=F)
ggplot(mpg, aes(displ, hwy)) + geom_point() + geom_smooth(span=1) ### default=0.75
ggplot(mpg, aes(displ, hwy)) + geom_point() + geom_smooth(span=0.2)

ggplot(mpg, aes(displ, hwy)) + geom_point(aes(color=drv)) + geom_smooth()
ggplot(mpg, aes(displ, hwy)) + geom_point() + geom_smooth(aes(color=drv))
ggplot(mpg, aes(displ, hwy, color=drv)) + geom_point() + geom_smooth()
ggplot(mpg, aes(displ, hwy)) + geom_point() + geom_smooth(method="lm")

ggplot(mpg, aes(drv, hwy)) + geom_point() 
ggplot(mpg, aes(drv, hwy)) + geom_boxplot() 
ggplot(mpg, aes(drv, hwy)) + geom_violin() 

ggplot(mpg, aes(hwy)) + geom_histogram() 
ggplot(mpg, aes(hwy, color=drv)) + geom_histogram() 
ggplot(mpg, aes(hwy, fill=drv)) + geom_histogram() 
ggplot(mpg, aes(hwy, fill=drv)) + geom_histogram(alpha=0.5) 

ggplot(mpg, aes(hwy)) + geom_freqpoly() 
ggplot(mpg, aes(hwy, color=drv)) + geom_freqpoly() 

ggplot(mpg, aes(hwy)) + geom_histogram() + facet_wrap(~drv)
ggplot(mpg, aes(hwy)) + geom_histogram() + facet_wrap(~drv, nrow=3)
ggplot(mpg, aes(hwy)) + geom_histogram() + facet_grid(.~drv)
ggplot(mpg, aes(hwy)) + geom_histogram() + facet_grid(drv~.)



ggplot(mpg, aes(hwy)) + geom_histogram(fill="skyblue", color="black") 
ggplot(mpg, aes(hwy)) + geom_histogram(fill="skyblue", color="black", 
                                       breaks=seq(10, 45, 5)) 


ggplot(mpg, aes(displ, hwy)) + geom_point() 
ggplot(mpg, aes(displ, hwy)) + geom_jitter() 
ggplot(mpg, aes(displ, hwy)) + geom_point(alpha=0.3) 

ggplot(mpg, aes(displ, hwy)) + geom_point(alpha=0.3) + 
  xlab("Displacement") + ylab("Highway MPG")

ggplot(mpg, aes(displ, hwy)) + geom_point(alpha=0.3) + 
  xlab("") + ylab(NULL)

ggplot(mpg, aes(displ, hwy)) + geom_point(alpha=0.3) + 
  ggtitle("Scatterplot")

ggplot(mpg, aes(displ, hwy)) + geom_point(alpha=0.3) + 
  xlim(1, 7) + ylim(0, 60)

ggplot(mpg, aes(displ, hwy)) + geom_point(alpha=0.3) +
  scale_x_continuous(breaks=c(2, 4, 6))

ggplot(mpg, aes(displ, hwy)) + geom_point(alpha=0.3) +
  scale_x_continuous(breaks=c(2, 4, 6), labels=c("2.0", "4.0", "6.0"))


ggplot(mpg, aes(displ, log(hwy))) + geom_point(alpha=0.3) 

ggplot(mpg, aes(displ, hwy)) + geom_point(alpha=0.3) + 
  scale_y_continuous(trans="log")

ggplot(mpg, aes(displ, hwy)) + geom_point(alpha=0.3) + 
  scale_y_continuous(trans="log", breaks=seq(15, 40, 5))




ggplot(mpg, aes(drv, hwy)) + geom_boxplot() 

ggplot(mpg, aes(drv, hwy)) + geom_boxplot() + 
  scale_x_discrete(labels=c("f"="Front", "r"="Rear", "4"="4 wheels"))

ggplot(mpg, aes(drv, hwy)) + geom_boxplot() +
  xlim("f", "r", "4")

ggplot(mpg, aes(drv, hwy)) + geom_boxplot() +
  xlim("f", "r")

ggplot(mpg, aes(drv, hwy)) + geom_boxplot() +
  xlim("f", "r", "4") + 
  scale_x_discrete(labels=c("f"="Front", "r"="Rear", "4"="4 wheels"))



### bar graph

ggplot(mpg, aes(class)) + geom_bar()

ggplot(mpg, aes(class)) + geom_bar() + 
  scale_x_discrete(guide=guide_axis(angle=90))



### color
ggplot(mpg) + geom_point(aes(displ, hwy, color=drv))

ggplot(mpg) + geom_point(aes(displ, hwy, color=drv)) + 
  scale_color_manual(values=c("tomato", "skyblue", "limegreen"))

library(RColorBrewer)
display.brewer.all()

ggplot(mpg) + geom_point(aes(displ, hwy, color=drv)) + 
  scale_color_brewer(palette="Dark2")


ggplot(mpg, aes(class)) + geom_bar(fill="skyblue")
ggplot(mpg, aes(class, fill=class)) + geom_bar()

ggplot(mpg, aes(class, fill=class)) + geom_bar() +
  scale_fill_manual(values=c("tomato", "skyblue", "limegreen", 
                             "orange", "forestgreen", "plum","mediumpurple"))

ggplot(mpg, aes(class, fill=class)) + geom_bar() +
  scale_fill_brewer(palette="Set3")


### shape
ggplot(mpg) + geom_point(aes(displ, hwy, shape=drv))

ggplot(mpg) + geom_point(aes(displ, hwy, shape=drv)) +
  scale_shape_manual(values=c(1, 2, 3))

gg<-ggplot(mpg, aes(displ, hwy)) + geom_point(alpha=0.3) 
gg
gg + geom_smooth()
gg + xlab("Displacement") + ggtitle("Scatterplot")

setwd("C:\\Users\\user\\Dropbox\\KNOU_2023_spring\\학부\\데이터시각화")
ggsave("Scatterplot.png", gg, width = 5, height = 5, units="cm")
ggsave("Scatterplot.pdf", gg, width = 5, height = 5, units="cm")
ggsave("Scatterplot.jpeg", gg, width = 5, height = 5, units="cm")
ggsave("Scatterplot.tiff", gg, width = 5, height = 5, units="cm")




