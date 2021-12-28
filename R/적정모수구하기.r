
# power 검정력
# sig.level 유의수준
# effect_size 탐지하고자하는 효과의 크기
# n 표본크기
# alternative two.sided, less, greater
## 단방향 검정을 하려면 대립가설을 더 크다고 설정해야 함

# https://abtestguide.com/abtestsize/

library("pwr")
effect_size = ES.h(p1=0.1*(1.03), p2= 0.1)
pwr.2p.test(h=effect_size, sig.level=0.05, power = 0.9, alternative='greater')

# 테스트 적정 모수는 검증하고 싶은 크기의 차이에 따라 달라짐
## 검증하고 싶은 크기의 차이가 작을수록 필요한 모수는 커짐
