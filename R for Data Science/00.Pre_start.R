# 본 책에서 사용하는 패키지를 설치한다.
# R은 기본적으로 tidyverse를 사용한다.
# tidyverse는 ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats 등을 포함한다.
# tidyverse는 데이터 분석을 위한 패키지들을 모아놓은 패키지이다.
# tidyverse는 한번에 설치할 수 있다.
# tidyverse를 설치하면 tidyverse를 포함하는 패키지들이 모두 설치된다.
install.packages("tidyverse")
# tidyverse를 사용하기 위해 불러온다.
library(tidyverse)
# tidyverse package update 방법은
tidyverse_update()

# 그 외에 사용할 패키지들을 설치한다.
install.packages(c("nycflights13", "gapminder", "Lahman"))

# R에서 한글로 된 로그를 영러로 변경하기
Sys.setenv(LANGUAGE = "en")

# 예제를 재현 가능하게 만들기 위해 포함해야할 세가지 필수 요수
# 패키지, 데이터, 코드
# 1. 패키지 업데이트
tidyverse_update()
# 2. 데이터 포함하기
# R에서는 dput()를 사용하여 재현되는 R코드 생성
# 3. 코드 쉽게 읽을 수 있도록 조금의 시간 투자
# 주석, 공백, 변수의 정보성 등

devtools::session_info(c("tidyverse"))

