# 6 워크플로: 파이프
# 6.1 들어가기
# 파이프는 일련의 다중연산을 깔끔하게 표현할 수 있는 강력한 도구이다.
# 앞 장에서 간단하게 소개했지만 더 진행하기에 앞서 파이프가 작동하는 방법과 대략적인 역사를 조금 설명하려고 한다.

# 6.1.1 파이프를 왜 써야하나?
# 개별 dplyr 함수들은 꽤 단순하기 때문에 복잡한 문제를 풀기 위해서는 여러 동사들을 조합해야 할 경우가 많다.
# 이 장의 마지막에서는 다음과 같이 적절히 복잡한 파이프를 작성하게 될 것이다:
library(nycflights13)
library(tidyverse)

flights |>
  filter(!is.na(arr_delay), !is.na(tailnum)) |>
  group_by(tailnum) |>
  summarise(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )
#이 파이프는 네 단계로 이루어져 있지만,
# 빠르게 훑어보고 주된 의미를 파악하는 것이 쉽다.
# flights 로 시작해서, 그다음 필터하고, 그다음 그룹화하고, 그다음 요약하라.

# 파이프를 사용하지 않았다면 어떻게 될까?
# 같은 문제를 해결할 수 있겠지만, 앞 함수 내부에 함수호출을 중첩해야 할 것이다.
summarise(
  group_by(
    filter(
      flights,
      !is.na(arr_delay), !is.na(tailnum)
    ),
    tailnum
  ),
  delay = mean(arr_delay, na.rm = TRUE
  ),
  n = n()
)
# 아니면 중간 변수를 많이 만들어야 할 것이다.
flights1 <- filter(flights, !is.na(arr_delay), !is.na(tailnum))
flights2 <- group_by(flights1, tailnum)
flights3 <- summarise(flights2, delay = mean(arr_delay, na.rm = TRUE), n = n())
flights3

# 6.2 magrittr 와 %>% 파이프
# tidyverse 를 지금까지 사용해 왔다면,
# Stefan Milton Bache 의 magrittr 패키지에서 온 %>% 파이프가 더 익숙할 것이다.
# The magrittr 패키지는 핵심 tidyverse 에 포함되어 있기 때문에
# tidyverse 를 사용할 때마다 %>% 를 사용할 수 있다.
library(tidyverse)
mtcars %>%
  group_by(cyl) %>%
  summarise(n=n())
# 간단한 케이스에 대해서는 |> 와 %>% 는 동일하게 동작한다.
# 왜 베이스 파이프를 추천하는가?
# 첫번째로, 베이스 R 에 있기 때문에 --> tidyverse 를 사용하지 않을 때도 항상 사용할 수 있다.
# 두번째로는, |> 가 magrittr 파이프보다 꽤 간단하다.
# 2014년 %>% 가 만들어진 2014 년과 |> 이
# R 4.1.0 에 포함된 2021년 사이 우리는 파이프의 핵심 장점을 날카롭게 하여,
# allowing the base implementation to jettison to estoeric and relatively unimportant features.

# 6.2.1 핵심 차이점
# If you haven’t used %>% you can skip this section;
# if you have, read on to learn about the most important differences.

# %>% allows you to use.
# as a placeholder to control how the object on the left is passed to the function on the right.
# R 4.2.0 will bring a _ as a placeholder with the additional restriction that it must be named.

# The base pipe |> doesn’t support any of the more complex uses of.
# such as passing. to more than one argument, or the special behavior when used with ..

# 베이스 파이프에는 $ (그리고 유사한 함수들)을 사용하는 편리한 방법이 아직 없다. magrittr 에서는, 다음과 같이 작성할 수 있다:
mtcars %>% .$cyl
#>  [1] 6 6 4 6 8 6 8 4 4 6 6 8 8 8 8 8 8 4 4 4 4 8 8 8 8 4 4 4 8 6 8 4
# 하지만 베이스 파이프는 이상해 진다:
mtcars |> (`$`)(cyl)
#>  [1] 6 6 4 6 8 6 8 4 4 6 6 8 8 8 8 8 8 4 4 4 4 8 8 8 8 4 4 4 8 6 8 4
# 다행스럽게도 대신 dplyr::pull() 을 사용할 수 있다:
mtcars |> pull(cyl)
#>  [1] 6 6 4 6 8 6 8 4 4 6 6 8 8 8 8 8 8 4 4 4 4 8 8 8 8 4 4 4 8 6 8 4
# When calling a function with no argument,
# you could drop the parenthesis,
# and write (e.g.) x %>% ungroup.
# The parenthesis are always required with |>.

# Starting a pipe with
# .,like . %>% group_by(x) %>% summarise(x) would create a function
# rather than immediately performing the pipe.

