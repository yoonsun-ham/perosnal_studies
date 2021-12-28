
# sample size 및 반응 모수 입력
## 모수
base1 <- 10000
base2 <- 10000
# base3 <- 19999
## 반응, 큰걸 ctr 1쪽에 할당
ctr1 <- 300
ctr2 <- 275
# ctr3 <- 300


#### 순열검정
## 비복원추출 & 랜덤하게 나올 수 있는 두 그룹의 차이_ 순열검정
## 반복 횟수
r <- 1000

perm_fun <- function(x, n1, n2)
{
  n <- n1 + n2
  idx_b <- sample(1:n, n1)
  idx_a <- setdiff(1:n, idx_b)
  mean_diff <- mean(x[idx_b]) - mean(x[idx_a])
  return(mean_diff)
}

obs_pct_diff <- (ctr1/base1 - ctr2/base2)*100
conversion <- c(rep(0, (base1+base2-ctr1-ctr2)), rep(1, ctr1+ctr2))
perm_diffs <- rep(0, r)
for(i in 1:r) {
 perm_diffs[i] = perm_fun(conversion, base1, base2)*100
}
hist(perm_diffs, xlab="Conversion rate (percent)", mian ='')
abline(v = obs_pct_diff, lty=2, lwd=1.5)
text(" Observed\n difference", x=obs_pct_diff, y=par()$usr[4]-20, adj=0)

obs_pct_diff

mean(perm_diffs>obs_pct_diff)


#### p-value 계산기
#### P-value 는 틀릴 확률이 아닌, 이러한 차이보다 극단의 차이가 발생할 확률임
#### 귀무가설이 맞다고 가정할 때 얻은 결과보다 극단적인 결과가 실제로 관측될 확률

# https://abtestguide.com/calc/

group <- c("groupA", "groupB")
view <- c(base1,base2)
click <- c(ctr1,ctr2)
ctr <- c(ctr1/base1,ctr2/base2)

df <- data.frame(group,view,click,ctr)

head(df)
prop.test(click,view,conf.level = 0.95)


#### 베이지안
#https://rpubs.com/anthonybmasters/bayesian-ab-test
#https://abtest.skplanet.com/a-b-test-bayesian-statistics/

library(rjags)
library(plotly)
library(tidybayes)
library(tidyverse)


set.seed(1988)

xA <- ctr2; nA <- base2; alphaA <- 1 ; betaA <- 1
xB <- ctr1; nB <- base1; alphaB <- 1 ; betaB <- 1
number_sims <- 50000

alphaA_post <- alphaA+xA; betaA_post <- betaA + nA - xA
alphaB_post <- alphaB+xB; betaB_post <- betaB + nB - xB

pA <- rbeta(number_sims, alphaA_post, betaA_post)
pB <- rbeta(number_sims, alphaB_post, betaB_post)

pB_minus_pA <- pB - pA %>% as_tibble()
pB_minus_pA_ci <- pB_minus_pA %>%
  mean_qi(value*100)

# B가 더 클 확률
pB_minus_pA_prob <- sum(pB_minus_pA > 0) / number_sims
pB_minus_pA_BF <- pB_minus_pA_prob / (1 - pB_minus_pA_prob)

#베이지안 팩터
pB_minus_pA_BF

pB_minus_pA_prob

# https://yozm.wishket.com/magazine/detail/1034/
# https://www.dynamicyield.com/lesson/bayesian-testing/
# https://m.blog.naver.com/PostView.naver?isHttpsRedirect=true&blogId=sw4r&logNo=221409143181
# https://vwo.com/tools/ab-test-significance-calculator/?utm_source=google&utm_medium=paid&utm_campaign=mof_ee1_search_category_mof_ab-testing_calculator_exact&utm_content=536602244047&utm_term=ab%20test%20calculator&gclid=Cj0KCQiAwqCOBhCdARIsAEPyW9lBZmL0bV2KYuuYBsjxECWUxbkPxDkrSvX_XYUIgyzoNi2elenRJXwaAjg0EALw_wcB

#https://abtestguide.com/bayesian/
