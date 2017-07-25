## check working directory
getwd()

## set working directory to today's folder
setwd("/media/mandy/Volume/transcend/life/2016kurs/session3/")

## load bird data
birds <- read.table("data/bird.dat")


## load mz data
library(Hmisc)
mz <- spss.get("../session2/data/mz2010_cf.sav")

ggplot(mz,aes(x = EF49,fill = EF46)) +
  geom_bar(position = position_fill())

ggplot(mz,aes(x = EF44,y = EF20)) +
  geom_point()

ggplot(mz,aes(x = EF44,y = EF20)) +
  geom_jitter()

ggplot(mz,aes(x = EF44,y = EF20)) +
  geom_jitter(alpha = 0.2)


## Load the ALLBUS data
allbus <- stata.get("data/ZA5240_v2-0-0.dta")

## V81 geschlecht
## V417 nettoeinkommen (offen und Liste)
## V118 arbeitsstunden

ggplot(allbus,aes(x = V81, y = V417)) +
  geom_boxplot() +
  scale_y_log10()


ggplot(allbus,aes(x = V81, y = V417)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2) +
  scale_y_log10()


df <- data.frame(value = c(rnorm(100,mean = 0,sd = 0.1),rnorm(100,mean = 1,sd = 0.1)), 
                 group = rep(c("A","B"),each = 100))

ggplot(df,aes(group,value)) +
  geom_boxplot() +
  stat_summary(geom="point",fun.y = "mean",colour = "red", size = 3)

df2 <- data.frame(value = c(rnorm(100,mean = 0,sd = 5),rnorm(100,mean = 1,sd = 5)), group = rep(c("A","B"),each = 100))
(p2 <- ggplot(df2,aes(group,value)) +
  geom_boxplot() +
  stat_summary(geom="point",fun.y = "mean",colour = "red", size = 3))


p1 <- ggplot(df,aes(group,value)) +
                geom_boxplot() +
                stat_summary(geom="point",fun.y = "mean",colour = "red", size = 3) +
                scale_y_continuous(limits = c(-10,10))

                 
