library(bnlearn)
library(ggplot2)

workdf<-data.frame(supplier_data)
workdf$supplier_name<-as.factor(workdf$supplier_name)
workdf$products_sold<-as.numeric(workdf$products_sold)
workdf$num_employees<-as.numeric(workdf$num_employees)
workdf$num_transports<-as.numeric(workdf$num_transports)
workdf$income<-as.numeric(workdf$income)


# white list

wl<-data.frame(from = "supplier_name", to = c("income","num_employees","co2_emissions"))

#  blacklist
bl <- data.frame(from = c("income"), to = c("supplier_name"))

# structure learning
trainig_set<-workdf[1:200,]
net<-hc(workdf, score = 'bic-cg'
    , whitelist = wl
    ,blacklist = bl
)
plot(net)

# parameter leanrig
net_coef<-bn.fit(net, workdf)
print(net_coef)
##bn.fit.barchart(net_coef$income)

# beilif propagation

## new data observed
test_set<-workdf[201:300,]
bn_new<-predict(net_coef,node="income", data= test_set , method="bayes-lw")

#predicted values for income
print(bn_new) 

# density

plot(density(bn_new))
abline(v=mean(bn_new), col='blue')
lines(density(trainig_set$income))
abline(v=mean(trainig_set$income), col='red')
lines(density(test_set$income))
abline(v=mean(test_set$income), col='purple')

