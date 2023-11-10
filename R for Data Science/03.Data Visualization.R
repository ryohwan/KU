# 이 장에서는 ggplot2 를 이용하여 데이터를 시각화하는 법을 배울 것이다.
# R 에서 그래프를 만드는 시스템이 몇명 있지만 이 중 가장 우아하고 다재다능한 시스템 중 하나는 ggplot2 이다.
# ggplot2 는 그래프를 설명하고 작성하는 시스템인 그래픽 문법 으로 그래프를 구현한다.
# ggplot2 로 하나의 시스템을 배우고 이를 여러 곳에 적용할 수 있다.
# library 준비하기
library(tidyverse)

# 3.2 첫 단계
# 다음의 질문에 답하기 위해 그래프를 이용해 보자.
# 엔진이 큰 차가 작은 차보다 연료를 더 많이 소비하는가?
# 이미 답은 알고 있겠지만, 답을 정교하게 만들어보자.
# 엔진 크기와 연비의 관계는 어떠한가? 양의 관계인가? 음의 관계? 선형인가? 비선형인가?
# 3.2.1 mpg 데이터프레임
# ggplot2 에 있는 mpg 데이터프레임(다른 표현으로 ggplot2::mpg)으로 여러분의 답을 확인할 수 있다.
# 데이터프레임은 변수들(열)과 관측값들(행)의 직사각형 형태 모음이다.
# mpg 에는 미 환경보호당국이 수집한 차 모델 38 개에 대한 관측 값들이 포함되어 있다.
mpg
head(mpg)

# mpg에 대해 알고 싶으면 ?mpg 를 입력하면 된다.
?mpg

# 3.2.2 ggplot 생성하기
# 다음의 코드를 실행하여 mpg 데이터 플롯을 그려라. displ 를 x 축, hwy 을 y 축에 놓아라.
# 엔진크기(displ)와 고속도로 연비(hwy)의 관계를 보여준다.
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))

# 3.2.3 그래프 작성 템플릿
# 이제 코드를 ggplot2로 그래프를 만드는, 재사용 가능한 템플릿으로 바꿔보자.
# 그래프를 만들려면 다음의 코드에서
# 괄호 안의 <>로 둘러쌓인 부분을,
# 해당되는 데이터셋, 지옴 함수, 매핑모음으로 바꾸어라.
# ------------------------------------------------------
# ggplot(data = <DATA>) +
# <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))
# ------------------------------------------------------
# 이 장의 나머지 부분에서는 이 템플릿을 완성하고 확장하여
# 다른 유형의 그래프 들을 만드는 법을 살펴볼 것이다.
# <MAPPINGS> 부분부터 시작해보자.
#
# 3.2.4 연습문제
# ggplot(data = mpg) 을 실행하라. 어떤 것이 나오는가?
ggplot(data = mpg)

# mpg 에 행이 몇개 있는가? 열은 몇개인가?
# drv 변수는 무엇을 의미하는가?
# 이를 알아보기 위해 ?mpg 을 실행하여 도움말을 읽으라.
# hwy vs cyl 산점도를 그리라.
ggplot(data = mpg) +
  geom_point(mapping = aes(x = cyl, y = hwy))

# class vs drv 산점도를 그리면 어떻게 되는가?
# 이 플롯은 왜 쓸모가 없는가?

# 3.3 심미성 매핑
# “그래프는 전혀 예상하지 못한 것을 보여줄 때 가장 큰 가치를 가진다.” — 존 튜키
# 다음의 그래프에서 한 그룹의 점들(빨간색으로 강조)은
# 선형 추세를 벗어나는 것처럼 보인다.
# 이 차들은 예상한 것보다 연비가 높다.
# 이 차들을 어떻게 설명할 수 있을까?
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy), color = 'blue')

# 플롯의 색상들을 보면 이상값 중 다수가 2인승 차임을 보여준다.
# 이 차들은 하이브리드 차가 아닌 것 같고, 놀랍게도 스포츠카들이다!
# 스포츠카들은 SUV와 픽업트럭처럼 엔진이 크지만,
# 차체가 중형차나 소형차처럼 작아서 연비가 좋다.
# 다시 생각해보면 이 차들은 엔진 크기가 컸기 때문에 하이브리드일 가능성이 낮다.
# 앞의 예제에서 class 변수를 색상 심미성에 매핑했지만
# 이 변수를 같은 방법으로 크기(size) 심미성에 매핑할 수도 있다.
# 이 경우, 각 점의 크기는 차종을 나타낼 것이다.
# 여기서 경고가 뜨는데, 순서형이 아닌 변수(class)를 순서형 심미성(size)으로
# 매핑하는 것은 바람직하지 않기 때문이다.
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, size = class, color = class))

# class 를 점의 투명도를 제어하는 알파(alpha) 심미성이나
# 점의 모양을 제어하는 모양(shape) 심미성에 매핑할 수도 있다.
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class, color = class))

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, shape = class, color = class))


# 연습문제 3.3.1.1
# blue 색상을 사용하고 싶다면 다음과 같이 할 수 있다.
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = "blue"))

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")

# 3.4 자주 일어나는 문제들
# R 은 극도로 까다롭고 문자를 잘못 위치시키면 완전히 다르게 될 수 있다.
# ( 가 모두 ) 와 짝을 이루고, " 가 모두 다른 " 와 짝을 이루는지 확인하라.
# 때론 코드를 실행해도 아무 일도 일어나지 않을 수 있다. 콘솔의 좌측을 체크하라.
# 즉, + 라면 R 은 여러분이 표현문을 완성한 것으로 생각하지 않고 마저 끝내기를 기다린다는 의미이다.
# 보통은 이런 경우 esc 키를 눌러서 현재 명령처리를 중단하고 처음부터 다시 시작하는 게 쉽게 처리하는 방법이다.
ggplot(data = mpg)
+ geom_point(mapping = aes(x = displ, y = hwy))

# # 3.5 Facets
# 변수를 추가하는 방법으로 심미성을 이용하는 방법을 보았다.
# 또 다른 방법은 범주형 변수에 특히 유용한 방법인데,
# 플롯을 면분할(facet, 데이터 각 서브셋을 표시하는 하위 플롯)로 나누는 것이다.
# 플롯을 하나의 변수에 대해 면분할하기 위해서는, facet_wrap() 을 이용하면 된다.
# facet_wrap() 의 첫 번째 인수로는 ~ 와 뒤에 변수 이름이 따라오는 공식(formula)이어야 한다.
# (여기서 “공식”은 R 데이터 구조의 한 형태이며 “등식(equation)”과 같은 의미가 아니다.)
# facet_wrap() 에 전달하는 변수는 이산형이어야 한다.
# facet_grid() 는 두 개의 변수를 사용할 수 있다.
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ cyl)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = class)) +
  facet_grid(drv + cyl ~ .)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = class)) +
  facet_grid(. ~ drv + cyl)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = class)) +
  facet_grid(drv ~ cyl, scales = "free")

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = class)) +
  facet_grid(drv ~ cyl, scales = "free_x")

# 열이나 행으로 면분할하고 싶지 않다면 변수 이름 대신 .를 활용.
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = class)) +
  facet_grid(. ~ cyl)

# What plots does the following code make? What does . do?
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)

# Take the first faceted plot in this section:
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_wrap(~class, nrow = 2)

# Which of the following two plots makes
# it easier to compare engine size (displ)
# across cars with different drive trains?
# What does this say about when to place
# a faceting variable across rows or columns?
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ drv)

# 3.6 기하 객체
# 두 플롯은 동일한 x 변수, 동일한 y 변수를 포함하고,
# 동일한 데이터를 나타낸다. 그러나 둘은 같지 않다.
# 각 플롯은 데이터를 표현하는 시각 객체가 다르다.
# ggplot2 문법으로는 두 플롯이 다른 지옴(geom)을 사용한다고 말한다.
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

# ggplot2의 모든 지옴 함수는 mapping 인수를 가진다.
# 그러나 모든 심미성이 모든 지옴과 작동하는 것은 아니다.
# 점의 shape(모양)을 설정할 수 있지만, 선의 “shape” 을 설정할 수는 없다.
# 반면, 선의 linetype(선 유형)을 설정하는 것을 생각해볼 수 있다.
# geom_smooth() 는 linetype으로 매핑된 변수의 고유값마다.
# 다른 형태의 선을 그린다.
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))

###################################
# gganimate #######################
###################################
# gganimate 패키지는 ggplot2 패키지를 확장하여
# 애니메이션을 만들 수 있게 해준다.
# gganimate 패키지는 ggplot2 패키지의 geom_point() 함수를
# 확장하여 애니메이션을 만들 수 있게 해준다.
# gganimate extends the grammar of graphics as implemented by ggplot2 to include the description of animation. It does this by providing a range of new grammar classes that can be added to the plot object in order to customise how it should change with time.
#
# transition_*() defines how the data should be spread out and how it relates to itself across time.
# view_*() defines how the positional scales should change along the animation.
# shadow_*() defines how data from other points in time should be presented in the given point in time.
# enter_*()/exit_*() defines how new data should appear and how old data should disappear during the course of the animation.
# ease_aes() defines how different aesthetics should be eased during transitions.
install.packages(c("gifski", "av"))
library(ggplot2)
library(gganimate)

ggplot(mtcars, aes(factor(cyl), mpg)) +
  geom_boxplot() +
  # Here comes the gganimate code
  transition_states(
    gear,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() +
  exit_shrink() +
  ease_aes('sine-in-out')

# install.packages('devtools')
devtools::install_github('thomasp85/gganimate')

library(gapminder)
library(gapminder)

ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')

################################################################
# 40개가 넘는 geom 예제  : https://exts.ggplot2.tidyverse.org/gallery/
################################################################

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))

ggplot(data = mpg) +
  geom_smooth(
    mapping = aes(x = displ, y = hwy, color = drv),
    show.legend = FALSE
  )

# 같은 플롯에 여러 지옴을 표시하려면 ggplot() 에 여러 지옴 함수를 추가하라.
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = class)) +
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))

# 지옴 함수에 매핑을 넣으면 ggplot2는 해당 레이어에 대한 로컬 매핑으로 처리 한다.
# 이렇게 되면 해당 레이어에만 이 매핑이 추가되거나 전역 매핑을 덮어쓴다.
# 즉, 다른 레이어마다 다른 심미성을 표시하는 것이 가능하다.
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class)) +
  geom_smooth()

# 같은 원리로 레이어마다 다른 data 를 지정할 수 있다.
# 여기서 우리의 평활선 은 mpg 데이터셋의 서브셋인 경차만을 표시한다.
# geom_smooth() 의 로컬 데이터 인수는 해당 레이어에 한해서만
# ggplot() 의 전역 데이터 인수를 덮어쓴다.
ggplot(data = mpg, mapping = aes(x=displ, y = hwy)) +
  geom_point(mapping = aes(color = class)) +
  geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE)

# Will these two graphs look different? Why/why not?
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth()
ggplot() +
  geom_point(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_smooth(data = mpg, mapping = aes(x = displ, y = hwy))

# Recreate the R code necessary
# to generate the following graphs.
# Note that wherever a categorical variable is used
# in the plot, it’s drv.
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = drv)) +
  geom_smooth(mapping = aes(color = drv))

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = drv)) +
  geom_smooth(mapping = aes(color = drv, linetype = drv))

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = drv)) +
  geom_smooth(mapping = aes(color = drv, linetype = drv), se = FALSE)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = drv)) +
  geom_smooth(mapping = aes(color = drv, linetype = drv), se = FALSE) +
  facet_wrap(~class)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = drv)) +
  geom_smooth()

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = drv, scale = "free"))

#3.7 통계적 변환
# 다음으로, 막대 그래프(bar chart)를 보자.
# 막대 그래프는 간단할 것 같지만,
# 플롯에 대해 미묘한 것을 드러내기 때문에 흥미로운 차트이다.
# geom_bar()로 그려지는 기본 막대 그래프를 생각해보라.
# 다음의 차트는 diamonds 데이터셋에서 cut 으로 그룹한 다이아몬드의
# 총 개수를 표시한다. diamonds 데이터셋은 ggplot2에 있으며
# 약 ~54,000개 다이아몬드 각각의
# 가격(price), 캐럿(carat), 색상(color),
# 투명도(clarity), 컷(cut)과 같은 정보가 있다.
# 차트는 저품질 컷보다 고품질 컷의 다이아몬드가 더 많음을 보여준다.
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))

demo <- tribble(
  ~cut, ~freq,
  "Fair", 1610,
  "Good", 4906,
  "Very Good", 12082,
  "Premium", 13791,
  "Ideal", 21551
)
ggplot(data = demo) +
  geom_bar(mapping = aes(x = cut, y = freq), stat = "identity")

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, y = after_stat(prop), group = 1))

# 코드에서 통계적 변환에 주의를 많이 집중시키고자 할 수 있다.
# 예를 들어 계산하는 요약값에 주의를 집중시키고자 고유한 x 값
# 각각에 대해 y 값을 요약하는 stat_summary() 를 사용할 수 있다.
ggplot(data = diamonds) +
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.min = min,
    fun.max = max,
    fun = median
  )

####################################################
# ggplot2 치트 시트 : <http://rstudio.com/cheatsheets
####################################################
# 3.8 위치조정
# 막대 그래프와 연관된 마법이 하나 더 있다.
# 막대 그래프에 색상을 입힐 수 있는데,
# color 심미성을 이용하거나 좀 더 유용하게는 fill 을 이용하면 된다
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, color = cut))
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = cut))

# fill 심미성을 다른 변수(예: clarity)에 매핑하면 어떤 일이 일어나는지 잘 보자.
# 누적 막대 그래프가 생성된다.
# 각각의 색상이 입혀진 직사각형은 cut 과 clarity 의 조합을 나타낸다.
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = clarity))

#position 인수로 지정하는 위치조정에 의해 막대 누적이 자동으로 수행된다.
# 누적 막대 그래프를 원하지 않는다면 다음의 "identity", "dodge", "fill"
# 세 옵션 중 하나를 선택하면 된다.
# position = "identity" 를 하면 각 객체를 그래프 문맥에 해당되는 곳에 정확히 배치한다.
# 막대와 겹치기 때문에 막대에 대해서는 그다지 유용하지 않다.
# 겹치는 것을 구별하려면 alpha를 작은 값으로 설정하여 막대들을 약간 투명하게 하거나,
# fill = NA 로 설정하여 완전히 투명하게 해야 한다.
ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) +
  geom_bar(alpha = 2/5, position = "identity")
ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) +
  geom_bar(fill = NA, position = "identity")

# identity 위치 조정은 포인트와 같은 2차원 지옴에서 더 유용한데
# 여기에서는 identity가 기본값이다.
# position = "fill" 을 하면 누적막대처럼 동작하지만 누적막대들이
# 동일한 높이가 되도록 한다. 이렇게 하면 그룹들 사이에 비율을 비교하기 쉬워진다.
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")

# position = "dodge" 를 하면 겹치는 객체가 서로 옆에 배치된다.
# 이렇게 하면 개별 값들을 비교하기 쉬워진다.
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")

# 막대 그래프에는 유용하지 않지만 산점도에는 매우 유용한,
# 다른 형태의 조정도 있다. 우리의 첫 번째 산점도를 떠올려보라.
# 데이터셋에 234개 관측값이 있는데도 플롯에서 126개 점만 표시하고 있다는 것을 눈치챘는가?
# hwy 와 displ 의 값들이 반올림이 되어서 점들이 격자 위에 나타나 많은 점이
# 서로 겹쳤다. 이 문제를 오버플롯팅이라고 한다.
# 이러한 방식은 많은 데이터가 어디에 있는지 보기 힘들게 만든다.
# 데이터 포인트들이 그래프에 걸쳐 동일하게 퍼져있는가?
# 아니면 hwy 와 displ 의 특정 조합이 109개 값을 포함하고 있는가?
# 위치 조정을 지터(jitter, 조금씩 움직임)로 설정하여 이 격자 방법을 피할 수 있다.
# position = "jitter" 를 하면 각 점에 적은 양의 랜덤 노이즈가 추가된다.
# 이렇게 하면 어느 두 점도 같은 양의 랜덤 노이즈를 받을 가능성이 없기 때문에 포인트가 퍼지게 된다.
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy), position = "jitter")

# 3.9 좌표계
# 좌표계는 아마도 ggplot2에서 가장 복잡한 부분일 것이다.
# 기본 좌표계는 각 점의 위치를 결정할 때 x와 y 위치가
# 독립적으로 움직이는 데카르트(Cartesian) 좌표계이다.
# 이것 말고도 도움이 되는 다른 좌표계들이 많다.
# coord_flip() 은 x와 y축을 바꾼다.
# (예를 들어) 수평 박스 플롯이 필요할 때 유용하다.
# 라벨이 길어서 x축과 겹치지 않고 들어맞게 하기 힘들 경우에도 유용하다.
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot()
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot() +
  coord_flip()


# 지도 데이터 : http://blog.revolutionanalytics.com/2009/11/choropleth-challenge-result.html
# 서울시 지도 시각화 : https://givitallugot.github.io/articles/2020-03/R-visualization-1-seoulmap
# 한국 행정경계지도 : https://datadoctorblog.com/2021/01/27/R-Vis-korea-map/
# 한국 지도 시각화 : https://www.r-bloggers.com/2017/03/visualizing-korean-data-with-ggplot2/

# ggplot2 는 지도 데이터를 사용할 수 있다.
nz <- map_data("nz")
ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")
ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  coord_quickmap()

bar <- ggplot(data = diamonds) +
  geom_bar(
    mapping = aes(x = cut, fill = cut),
    show.legend = FALSE,
    width = 1
  ) +
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)
bar + coord_flip()
bar + coord_polar()

# 3.10 그래픽 레이어 문법
# ggplot(data = <DATA>) +
#   <GEOM_FUNCTION>(
#      mapping = aes(<MAPPINGS>),
#      stat = <STAT>,
#      position = <POSITION>
#   ) +
#   <COORDINATE_FUNCTION> +
#   <FACET_FUNCTION>



######################################
#### 한국 지도 시각화
######################################
library("rgdal")
map = readOGR("TL_SCCO_CTPRVN.shp")
class(map)
slotNames(map)
ldf_map_info = map@data
head(df_map_info)




