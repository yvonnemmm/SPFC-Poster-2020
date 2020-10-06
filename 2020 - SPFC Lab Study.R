#### LOAD DATA ####

library(haven)
library(psych) 

dat <- read_sav("SPU Lab Hypocrisy Personality_and_Attitudes_Survey_2019_April 28, 2020_18.12.sav")

#### SCORING MEASURES ####

###green attitudes
dat$at <- rowMeans(data.frame(dat$ShamAtt_1, dat$ShamAtt_2, dat$ShamAtt_3,
                              dat$ShamAtt_4, dat$ShamAtt_5, dat$ShamAtt_6))

psych::alpha(data.frame(dat$ShamAtt_1, dat$ShamAtt_2, dat$ShamAtt_3,
                        dat$ShamAtt_4, dat$ShamAtt_5, dat$ShamAtt_6))

###TOSCA 
#guilt
dat$tos_gu <- rowMeans(data.frame(dat$Tos1_3, dat$Tos2_1, dat$Tos3_3,
                                  dat$Tos4_4, dat$Tos5_4, dat$Tos6_4,
                                  dat$Tos7_3, dat$Tos8_4, dat$Tos9_3, 
                                  dat$Tos10_3, dat$Tos11_2))

psych::alpha(data.frame(dat$Tos1_3, dat$Tos2_1, dat$Tos3_3,
                        dat$Tos4_4, dat$Tos5_4, dat$Tos6_4,
                        dat$Tos7_3, dat$Tos8_4, dat$Tos9_3, 
                        dat$Tos10_3, dat$Tos11_2))

#shame
dat$tos_sh <- rowMeans(data.frame(dat$Tos1_1, dat$Tos2_2, dat$Tos3_1,
                                  dat$Tos4_3, dat$Tos5_1, dat$Tos6_2, 
                                  dat$Tos7_4, dat$Tos8_2, dat$Tos9_2, 
                                  dat$Tos10_1, dat$Tos11_3))

psych::alpha(data.frame(dat$Tos1_1, dat$Tos2_2, dat$Tos3_1,
                        dat$Tos4_3, dat$Tos5_1, dat$Tos6_2, 
                        dat$Tos7_4, dat$Tos8_2, dat$Tos9_2, 
                        dat$Tos10_1, dat$Tos11_3))

#externalization
dat$tos_ext <- rowMeans(data.frame(dat$Tos1_4, dat$Tos2_3, dat$Tos3_2, 
                                   dat$Tos4_1, dat$Tos5_2, dat$Tos6_1, 
                                   dat$Tos7_2, dat$Tos8_3, dat$Tos9_1,
                                   dat$Tos10_2, dat$Tos11_4))
#det
dat$tos_det <- rowMeans(data.frame(dat$Tos1_2, dat$Tos2_4, dat$Tos3_4,
                                   dat$Tos4_2, dat$Tos5_3, dat$Tos6_3, 
                                   dat$Tos7_1, dat$Tos8_1, dat$Tos9_4,
                                   dat$Tos10_4, dat$Tos11_1))

###Gasp
#guilt-negatice-Behavior-Evaluation (nbe)
dat$nbe <- 4* rowMeans(data.frame(dat$Q63_1, dat$Q65_1, dat$Q67_2, 
                                  dat$Q67_4))
#Guilt-Repair (gr)
dat$gr <- 4* rowMeans(data.frame(dat$Q63_2, dat$Q64_1, dat$Q65_3, 
                                 dat$Q67_3))
#Shame-Negative-self-Evaluation (nse)
dat$nse <- 4* rowMeans(data.frame(dat$Q63_3, dat$Q64_2, dat$Q65_2,
                                  dat$Q67_1 ))
#Shame-Withdraw (sw)
dat$sw <- 4* rowMeans(data.frame(dat$Q63_4, dat$Q64_3, dat$Q64_4,
                                 dat$Q65_4))
#shame total 
dat$shtot <- dat$nse + dat$sw
#guilt total
dat$gutot <- dat$gr + dat$nbe

##### CONDITIONS ######
dat$condition <- NA

dat$condition[dat$FL_3_DO_ConditionOne_Control_ == 1] <- "control"
dat$condition[dat$FL_3_DO_ConditionTwo_Experimental_==1] <- "exp"
table(dat$condition)

# check for knowledge of experimental condition
structure(dat$Q60)

xtabs(~ dat$Q60 + dat$condition)
#some peopel get it wrong ... drop them

dat$drop <- FALSE
dat$drop[dat$condition=="control" & dat$Q60 == 1] <- TRUE
dat$drop[dat$condition=="exp" & dat$Q60 == 2] <- TRUE
table(dat$drop)
dat <- dat[!dat$drop,]

#### ANALYSIS ####

#checking for skew in green attitudes measure 

mean(dat$at)
#mean 4.64 

psych::skew(dat$at)
#skew -2.46 really negatively skewed 

qqnorm(dat$at)
#not normally distributed 

hist(dat$at)

###transforming negative scew of green attitudes measure 

#flip data around
dat$atT <- max(dat$at) + 1 - dat$at
#taking the squar root 
dat$atT <- sqrt(dat$atT)
#fliping it back around 
dat$atT <- max(dat$atT) +1 - dat$atT

#checking skew after transformation 
psych::skew(dat$atT)
# -1.82 which is better, but I don't think its that good still
hist(dat$atT)

### transforming skew using the natural log 

dat$atT2 <- max(dat$at) + 1 - dat$at
dat$atT2 <- log(dat$atT2)
dat$atT2 <- max(dat$atT2) +1 - dat$atT2

#checking skew after second transformation 
psych::skew(dat$atT2)
# -1.32 is much better, maybe there is a more powerful transformation? 
hist(dat$atT2)

#### TESTING MAIN EFFECT ####
#hypothesis: people who were in the control group (2 = false) will have a higher 
#score on the green attitudes variable than people who were in the experimental
#group (1=true)

structure(dat$Q60)
#If I sound green minded I will be asked to 
#return and complete a follow-up study 
#1 = True, 2 = False 

#two sample T-test 
t.test(at ~ condition, data = dat)
t.test(atT2 ~ condition, data = dat)
#looks like group one had a higher mean than group two which 
#goes against our hypothesis, but the difference is insignificant regardless
#t(53) = 1.11, p = 0.27 

wilcox.test(atT2 ~ condition, data = dat)

# skew is so bad, we might bootstrap

library(boot)
temp <- data.frame(at=dat$at, condition=dat$condition)

boot.median <- function(temp, d){
  run <- temp[d,]
  exp <- psych::interp.median(run$at[run$condition=="exp"])
  cont <- psych::interp.median(run$at[run$condition== "control"])
  return(exp - cont) 
}

# get bootstrapping contents
boot.results <- boot::boot(temp, boot.median, R=10000)
ll <- boot.ci(boot.results, type="perc")$percent[4]
ul <- boot.ci(boot.results, type="perc")$percent[5]
CI <- paste0("[", format(round(ll, 2), nsmall = 2), ", ", format(round(ul, 2), nsmall = 2), "]")
CI

boot.ci(boot.results)

#check % giving max
library(tidyverse)
dat$max <- dat$at==5
table(dat$max, dat$condition) %>% chisq.test()
# not significant

#get colunn percentages
table(dat$max, dat$condition) %>% prop.table(., margin=2) %>% "*"(100) %>% round(2)


### CORR WITH GUILT ####

lm(at ~ tos_gu*condition, data=dat) %>% summary()
cor.test(dat$at, dat$tos_gu)
cor.test(dat$atT2, dat$tos_gu)

ggplot2::ggplot(data=dat, aes(x=tos_gu, y=atT2))+
  geom_jitter()+
  geom_smooth(method=lm)+
  theme_light()


ggplot2::ggplot(data=dat, aes(x=nbe, y=atT2))+
  geom_jitter()+
  geom_smooth(method=lm)+
  theme_light()

ggplot2::ggplot(data=dat, aes(x=gr, y=atT2))+
  geom_jitter()+
  geom_smooth(method=lm)+
  theme_light()


cor.test(dat$at, dat$tos_sh)
cor.test(dat$atT2, dat$tos_sh)


ggplot2::ggplot(data=dat, aes(x=tos_sh, y=atT2))+
  geom_jitter()+
  geom_smooth(method=lm)+
  theme_light()

lm(at ~ tos_gu + tos_sh, data=dat) %>% lm.beta::lm.beta() %>% summary()

dat$Sex
