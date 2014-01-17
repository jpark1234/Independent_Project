install.packages("QuantPsyc")
require(QuantPsyc)
#OLS series 
model1 <- lm(Texas.clean$drace.code ~ Texas.clean$age + Texas.clean$vrace + Texas.clean$vgender + Texas.clean$vnum)
summary(model1)
model1.b <- lm(Texas.clean$drace.bi ~ Texas.clean$age + Texas.clean$vrace + Texas.clean$vgender + Texas.clean$vnum)
summary(model1.b)

lm.beta(model1)
model2 <- lm(Texas.clean$drace.code ~ Texas.clean$age + Texas.clean$vrace + Texas.clean$dv.rdiff + Texas.clean$vgender + Texas.clean$vnum)
summary(model2)

lm.beta(model2)
model3 <- lm(Texas.clean$drace.code ~ Texas.clean$age + Texas.clean$vrace + Texas.clean$w.m + Texas.clean$vgender + Texas.clean$vnum)
summary(model3)

model3.b <- lm(Texas.clean$drace.bi ~ Texas.clean$age + Texas.clean$vrace + Texas.clean$w.m + Texas.clean$vgender + Texas.clean$vnum)
summary(model3.b)

model4 <- lm(Texas.clean$drace.bi ~ Texas.clean$age + Texas.clean$vrace + Texas.clean$w.m2 + Texas.clean$vgender + Texas.clean$vnum)
summary(model4)
lm.beta(model4)
model5<- lm(Texas.clean$drace.code ~ Texas.clean$age + Texas.clean$vrace + Texas.clean$w.m + Texas.clean$vgender + Texas.clean$vnum
            + Texas.clean$neg.score + Texas.clean$pos.score + Texas.clean$relig.score + Texas.clean$pain.score + Texas.clean$finish.score)

#creating binary race predictor variable 
Texas.clean$drace.bi = Texas.clean$drace.code
Texas.clean$drace.bi[Texas.clean$drace.code==1]=0
Texas.clean$drace.bi[Texas.clean$drace.code!=1]=1


model5 <- glm(Texas.clean$drace.bi ~ Texas.clean$age + Texas.clean$vrace.bi +
              Texas.clean$vgender + Texas.clean$vnum, 
              family = binomial(link = logit))
summary(model5)
exp(coef(model5))

model6 <- glm(Texas.clean$drace.bi ~ Texas.clean$age + Texas.clean$vrace.bi +
                Texas.clean$vgender + Texas.clean$vnum
              + Texas.clean$neg.score + Texas.clean$pos.score + Texas.clean$relig.score + Texas.clean$pain.score + Texas.clean$finish.score,
              family = binomial(link = logit))
summary(model6)
exp(coef(model6))

