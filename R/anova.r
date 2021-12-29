# 그룹 내 분산이 그룹간 분산보다 작을 경우, 그룹 간 통계적으로 유의한 차이가 있다고 함 
# analysis of variance -> anova 라고 부름

# sample data - page별 체류 시간

p1 <- c(164, 172, 177, 156, 195)
p2 <- c(178, 191, 182, 185, 177)
p3 <- c(175, 193, 171, 163, 176)
p4 <- c(155, 166, 164, 170, 168)

# 재 표본 추출
## 한 상자에 모으고, 4개 그룹 추출, 평균 기록, 평균 사이의 분산 기록, 반복

p1_df <- data.frame('page1',p1)
p2_df <- data.frame('page2',p2)
p3_df <- data.frame('page3',p3)
p4_df <- data.frame('page4',p4)

colnames(p1_df)[1] <- 'pages'
colnames(p2_df)[1] <- 'pages'
colnames(p3_df)[1] <- 'pages'
colnames(p4_df)[1] <- 'pages'

colnames(p1_df)[2] <- 'time'
colnames(p2_df)[2] <- 'time'
colnames(p3_df)[2] <- 'time'
colnames(p4_df)[2] <- 'time'

four_sessions <- rbind(p1_df,p2_df,p3_df,p4_df)


# 순열검정
# pr(Prob) 우연히 발생할 수 있는 확률
# Iter 순열검정을 몇 번 했는지

library(lmPerm)
summary(aovp(time ~ pages, data = four_sessions))


# f 통계량
## 잔차오차로 인한 분산과 그룹의 평균의 분산에 대한 비율
## df 자유도, sum sq 제곱합, mean sq 평균제곱편차, f value f통계량 
## f 통계량 = ms(처리)/ms(오차)
summary(aov(time ~ pages, data = four_sessions))


# 이원분산분석을 위한 random valeus

rnd <- data.frame(c(0,1,0,1,0,0,1,0,1,0,0,1,0,1,0,0,1,0,1,0))
colnames(rnd)[1] <- 'weekday'


four_sessions <- cbind(four_sessions,rnd)

summary(aovp(time ~ ., data = four_sessions))
summary(aov(time~ ., data = four_sessions))

boxplot(four_sessions$time)

boxplot(time~pages+weekday,
 data=four_sessions,
 mian = 'Tttt',
 xlab = 'type',
 ylab = 'seconds',
 col = 'orange',
 border = 'blue')

