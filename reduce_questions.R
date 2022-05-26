# install.packages('ltm') # logistic distribution estimation
library(ltm)

# Goal: Reduce the 25-item IQ test to a short form 9-item IQ test
# Data: http://openpsychometrics.org/_rawdata/IQ1.zip

# load dataset
IQ1 <- read.csv('./IQ1/data.csv')
head(IQ1)

# select questions only
IQbin <- IQ1[, 1:25]
head(IQbin)

# a score of 10 means that the person answered correctly
# any score different from (less than) 10 indicates the person
# did not fully answer the question
IQbin[IQbin < 10] <- 0
IQbin[IQbin == 10] <- 1
head(IQbin)

# Fit Binary Unidimensional IRT 2PL model
IRT2pl <- ltm(IQbin ~ z1, IRT.param = TRUE)

# Step 1: Remove misfitting items

# return the goodness of fit of the items with chi-squared test
# if p-value < 0.05 then item misfits
fit <- item.fit(IRT2pl)
fit

# put p-values along with item names into a dataframe
fit_pv <- as.data.frame(fit$p.values)
fit_pv

# take a subset of items and p-values by excluding the misfitting values
fit_pv <- subset(fit_pv, `fit$p.values` >= 0.05)
fit_pv

plot(IRT2pl, type='IIC') # for all items
plot(IRT2pl, type='IIC', items=0) # sum for all items

# at each level of theta, what is the amount of information in each question 
info <- as.data.frame(list(plot(IRT2pl, type='IIC')))
info


# Step 2: Select items that provide the most information at given levels
# of theta. This would allow the short form test to provide information
# for students with low, medium, and high levels of theta (-1.5, 0, +1.5).

# take the row at theta ~ -1.5, 0, 1.5
low_abl <- t(as.data.frame(info[31, 2:26]))
med_abl <- t(as.data.frame(info[51, 2:26]))
high_abl <- t(as.data.frame(info[70, 2:26]))

# merge non-misfit items to information dataframe
fit_pv1 <- merge(fit_pv, low_abl, by=0, all.x=TRUE)
fit_pv2 <- merge(fit_pv, med_abl, by=0, all.x=TRUE)
fit_pv3 <- merge(fit_pv, high_abl, by=0, all.x=TRUE)

# col1 = item number
# col2 = how well it fits the model
# col3 = the information that question has
fit_pv1

# sort each dataframe by the highest information values. This would allow
# for the selection of the items with the most information at each level of
# ability. By doing so, we can select the top-most 3 items from each
# dataframe (which represents a level of theta) and
# create the short form test that has 9 items

fit_pv1[order(-fit_pv1$`31`),][1:5,]
fit_pv2[order(-fit_pv2$`51`),][1:5,]
fit_pv3[order(-fit_pv3$`70`),][1:5,]

# select the 9 most informative items
IQbin.9 <- IQbin[, c(16, 15, 3, 10, 19, 24, 18, 12, 23)]

# fit 2PL model
IRT2pl_9 <- ltm(IQbin.9 ~ z1, IRT.param = TRUE)

par(mfrow=c(1,2)) # set the plotting area into a 1*2 array
plot(IRT2pl, type='IIC', items=0)
plot(IRT2pl_9, type='IIC', items=0)
par(mfrow=c(1,1))
