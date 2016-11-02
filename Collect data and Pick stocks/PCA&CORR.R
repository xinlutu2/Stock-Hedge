library('corrplot')

return = read.csv('return.csv')
#plot correlation matrix based on hierarchical clustering
#get an overall sense about correlation between each company
corr = cor(return)
corrplot(corr, order = "hclust", addrect = 3)

#Pick the groups of stocks with high correlation based on the plot
#corrplot to verify
return_subset = read.csv('return_subset.csv')
corr_1 = cor(return_subset)
corrplot(corr_1, order = "hclust", addrect = 3)


#Follow the principal component analysis (PCA) to find
#high correlated stock based on reference
fit <- princomp(corr, cor=TRUE)
summary(fit)
loadings(fit)
plot(fit,type="lines")
fit$scores
#biplot(fit, choices = c(88,89))
biplot(fit, choices = c(90,91))


