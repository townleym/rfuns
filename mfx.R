require(magrittr)

pplogit.glm = function(mod) {
	
	coefs = coef(mod)
	varnames = names(coef(mod))
	
	ncoef = length(coefs)
	
	# create an identity matrix/dataframe with 1s on the diagonal
	newdata = matrix(data = rep(0, ncoef**2), nrow = ncoef, ncol = ncoef)
	diag(newdata) = rep(1, ncoef)
	newdata = data.frame(newdata)
	colnames(newdata) = varnames
	
	# Get predicted probabilties
	preds = predict(mod, newdata = newdata, type = "response")
	names(preds) = varnames
	
	preds
}

mfxlogit.glm = function(mod) {
	
	coefs = coef(mod)
	varnames = names(coef(mod))
	
	ncoef = length(coefs)
	
	# create an identity matrix/dataframe with 1s on the diagonal
	newdata = matrix(data = rep(0, ncoef**2), nrow = ncoef, ncol = ncoef)
	diag(newdata) = rep(1, ncoef)
	newdata[,1] = rep(1, ncoef)

	newdata = data.frame(newdata)
	colnames(newdata) = varnames
	
	# Get predicted probabilties
	preds = predict(mod, newdata = newdata, type = "response")
	
	mfx = preds[2:length(preds)] - preds[1]
	names(mfx) = varnames[2:length(varnames)]
	mfx
}

# Data: http://www.umass.edu/statdata/statdata/stat-logistic.html
myopia = read.csv(file = "~/Documents/reference/R/datasets/myopia.csv")
names(myopia) = tolower(names(myopia))
head(myopia)

f.old = myopic ~ age + gender + spheq + al + acd + lt + vcd + mommy + dadmy
f = myopic ~ gender + mommy + dadmy
m1 = glm(f, data = myopia, family = binomial())
summary(m1)

pplogit.glm(m1) %>% round(3)
mfxlogit.glm(m1) %>% round(3)

m.lpm = lm(f, data = myopia)
m2 = glm(f, data = myopia, family = binomial(log), start = c(-5, 0, 0, 0))
summary(m2)

m3 = glm(f, data = myopia, family = poisson())
summary(m3)

data.frame(coef(m1), coef(m2), coef(m3))
data.frame(exp(coef(m2)), exp(coef(m3))) %>% round(3)
data.frame(pplogit.glm(m1)) %>% round(3)
data.frame(mfxlogit.glm(m1)) %>% round(3)


# Make a dataframe with 1 for each variable
# matrix of zeroes

coefs = coef(m1)
ncoef = length(coefs)
varnames = names(coefs)

newdata = matrix(data = rep(0, ncoef^2), nrow = ncoef, ncol = ncoef)

# poke 1s into the diagonal
diag(newdata) = rep(1, ncoef)
# make it a data frame and give the column names the same as the coefficients
newdata = data.frame(newdata)
colnames(newdata) = varnames

# Get the predicted probabilities
preds = predict.glm(m1, newdata = newdata, type = "response")
names(preds) = varnames
cbind(coefs, preds)

# then marginal effects are each coefficient times the intercept minus the intercept
mfx = (preds[2:length(preds)] * preds[1]) - preds[1]
data.frame(mfx) %>% round(3)

newdata
newdata[,1] = rep(1, ncoef)
preds = predict(m1, newdata = newdata, type = "response")
mfx = preds[2:length(preds)] - preds[1]
names(mfx) = varnames[2:length(varnames)]
data.frame(mfx) %>% round(3)