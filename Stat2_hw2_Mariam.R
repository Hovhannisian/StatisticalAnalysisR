knitr::opts_chunk$set(echo = TRUE)


library(readxl)
tuna <- read_excel("tuna.xlsx")
head(tuna)
model_tuna <- lm(log(sal1) ~ apr1 + apr2 + apr3 + disp + dispad, data = tuna)
summary(model_tuna)


library('linearHypothesis')
linearHypothesis(model_tuna,c('disp=0'), title = 'i')
#p is less than alpha, so reject H0 => display in a store is significant

linearHypothesis(model_tuna, c('dispad=0'), title ='ii')
#p is less than alpha, so reject H0 => display in a store and an ad are significant

linearHypothesis(model_tuna, c('disp=0', 'dispad=0'), title ='iii')
#reject H0=> display and/or ad are significant


linearHypothesis(model_tuna, c('disp=0', 'dispad=0'), title ='iv')
#reject two-tail hypothesis. Statistcally, we can state that display and ad are more significant than display only



####
pizza <- read_excel("pizza4.xlsx")
head(pizza)
model_pizza <- lm(pizza ~ female + hs + income + age, data = pizza)
summary(model_pizza)

model_pizza2 <- lm(pizza ~ female + hs + income + age + female*income, data = pizza)
summary(model_pizza2)

fem_income = median(pizza$income[pizza$female==1])


