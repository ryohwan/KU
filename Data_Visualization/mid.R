library(ggplot2)
head(msleep)

# 몸무게(bodywt)를 가로축, 뇌 무게(brainwt)를 세로축으로 하는 산점도를 그리시오.
ggplot(msleep, aes(bodywt, brainwt)) + geom_point()

# 방법1. 몸무게(bodywt), 뇬 무게(brainwt)에 로그값으로 하는 산점도를 그리시오.
ggplot(msleep, aes(log(bodywt), log(brainwt))) +
  geom_smooth(method="lm") +
  ggtitle("202135-157844") +
  geom_point()

# 방법2. 몸무게(bodywt), 뇌 무게(brainwt)에 로그값으로 하는 산점도를 그리시오.
ggplot(msleep, aes(bodywt, brainwt)) + geom_point() +
  geom_smooth(method="lm") +
  scale_y_continuous(trans="log") +
  scale_x_continuous(trans="log") +
  geom_point(alpha=0.3) +
  ggtitle("202135-157844") +
  xlab("Body Weight") + ylab("Brain Weight")

head(mpg)
ggplot(mpg, aes(displ, hwy, color=drv)) +
  scale_color_brewer(palette="Dark2") +
  geom_point(alpha=0.3) +
  geom_smooth(method="lm") +
  ggtitle("202135-157844")
ggplot(mpg, aes(drv, hwy)) +
  geom_boxplot() +
  scale_x_discrete(labels=c("f"="앞바퀴굴림", "r"="뒷바퀴굴림", "4"="네바퀴굴림"), guide=guide_axis(angle=45)) +
  ggtitle("202135-157844")
ggplot(mpg, aes(drv, hwy)) +
    geom_violin() +
    scale_x_discrete(labels=c("f"="Front", "r"="Rear", "4"="4 wheels"))
    ggtitle("202135-157844")

