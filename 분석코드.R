koreanlife = read.csv('koreanlife.csv')
library(dplyr)
install.packages('ggplot2')
library(ggplot2)

head(koreanlife)

koreanlife %>% 
  group_by(age) %>% 
  summarise(m_suicide=mean(suicide),'삶의 만족도'=mean(satisfaction),'외로움'=mean(loneliness)) %>% 
  arrange(desc(m_suicide))
   # 연령별로 자살충동, 만족도, 외로움 평균 구하기

koreanlife %>% 
  group_by(sex) %>% 
  summarise(m_suicide=mean(suicide),'삶의 만족도'=mean(satisfaction),'외로움'=mean(loneliness))%>% 
  arrange(desc(m_suicide))
# 성별로 평균 구하기

koreanlife %>% 
  group_by(area) %>% 
  summarise(m_suicide=mean(suicide),'삶의 만족도'=mean(satisfaction),'외로움'=mean(loneliness))%>% 
  arrange(desc(m_suicide))
# 지역별로 평균구하기

koreanlife %>% 
  summarise(mean(suicide), mean(satisfaction), mean(loneliness)) # 전연령 자살충동, 만족도, 외로움 평균

t.test(data=koreanlife, suicide~sex)

cor.test(koreanlife$suicide, koreanlife$loneliness) # 자살과 외로움 상관관계 분석 (상관계수 r이 0.49로 다소 높은 상관관계)
cor.test(koreanlife$suicide, koreanlife$satisfaction) # 자살과 만족감 상관관계 분석 (상관계수 r이 -0.27로 약한 상관관계)
cor.test(koreanlife$satisfaction, koreanlife$loneliness) # 만족감과 외로움 상관관계 분석 (상관계수 r이 -0.27로 약한 상관관계)

RA=lm(suicide~satisfaction+loneliness, data=koreanlife) # 만족감과 외로움에 따른 자살충동 회귀분석
summary(RA)

SA=lm(satisfaction~job_value+society_safety +self_control +korean_pride+help_weak +family_talk +family_belief +personal_economy +health  , data=koreanlife) # 전체 만족감과 9개 변인 회귀분석

LA=lm(loneliness~job_value+society_safety +self_control +korean_pride+help_weak +family_talk +family_belief +personal_economy +health, data=koreanlife) # 전체 외로움과 9개 변인 회귀분석

summary(SA)
summary(LA)


# 20대 데이터 저장 후, 회귀분석
age20=koreanlife %>% 
  filter(age=='19~29세')

CC=lm(suicide~satisfaction+loneliness, data=age20) # 20대의 자살충동과 만족감,외로움 다중회귀분석
summary(CC)

mean(age20$suicide) # 20대 자살의 평균
mean(age20$satisfaction) # 20대 만족감의 평균
mean(age20$loneliness) # 20대 외로움의 평균

cor.test(age20$suicide, age20$satisfaction) # 자살과 만족감 상관관계 분석 (상관계수 r이 -0.27로 약한 상관관계)
cor.test(age20$suicide, age20$loneliness) # 자살과 외로움 상관관계 분석 (상관계수 r이 0.51로 다소 높은 상관관계)
cor.test(age20$satisfaction, age20$loneliness) # 20대 만족감과 외로움 상관관계 분석

AA = lm(satisfaction~job_value+society_safety +self_control +korean_pride+help_weak +family_talk +family_belief +personal_economy +health  , data=age20) # 20대의 만족감과 9개 변인 회귀분석

summary(AA)

BB = lm(loneliness~job_value+society_safety +self_control +korean_pride+help_weak +family_talk +family_belief +personal_economy +health  , data=age20) # 20대의 우울감과 9개 변인 회귀분석

summary(BB)




       