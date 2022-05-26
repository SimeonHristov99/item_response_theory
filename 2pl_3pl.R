# install.packages('ltm') # logistic distribution estimation

library(ltm)

data(LSAT)
head(LSAT)

# run the model with logistoc distribution using a 2PL
IRTmodel <- ltm(LSAT ~ z1, IRT.param = TRUE)

# get the parameters
summary(IRTmodel)
coef(IRTmodel)

plot(IRTmodel, type='ICC') # all items at once (ICC = Item Characteristic Curve)
plot(IRTmodel, type='ICC', items=3) # one item at a time
plot(IRTmodel, type='ICC', items=c(2,3)) # multiple items

# The item information function is the reciprocal of the variance for the
# estimate of theta. It is related to how accurately theta can be estimated.
# The more information an item provides about a given level of theta,
# the more accurately that item can estimate theta at that level.

# Therefore, higher information at a given level of theta means more precision
# for estimating examinees who may truly possess that level of theta.

# Items in a test may likely differ in the amounts of information
# they provide at different levels of theta.
# Each item individually may provide little information.

plot(IRTmodel, type='IIC')

# Test information function is the sum of the information across all items.
plot(IRTmodel, type='IIC', item=0)

factor.scores(IRTmodel)
person.fit(IRTmodel)
item.fit(IRTmodel)

# run a model with 3PL
IRTmodel2 <- tpm(LSAT, type="latent.trait", IRT.param = TRUE)

summary(IRTmodel2)
coef(IRTmodel2)

plot(IRTmodel2, type='ICC') # all items at once (ICC = Item Characteristic Curve)
plot(IRTmodel2, type='IIC', item=0) # Test Information Function

factor.scores(IRTmodel2)
person.fit(IRTmodel2)
item.fit(IRTmodel2)

# use the anova function to compare to see if the addition of the guessing parameter helped
# the models are the same, so prefer the 2PL because it's simpler
anova(IRTmodel, IRTmodel2)
