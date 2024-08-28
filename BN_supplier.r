library(bnlearn)

workdf<-data.frame(supplier_data)
workdf$supplier_name<-as.factor(workdf$supplier_name)
workdf$products_sold<-as.numeric(workdf$products_sold)
workdf$num_employees<-as.numeric(workdf$num_employees)
workdf$num_transports<-as.numeric(workdf$num_transports)
workdf$income<-as.numeric(workdf$income)

# structure learning
net<-hc(workdf, score = 'bic-cg')
plot(net)

# parameter leanrig
net_coef<-bn.fit(net, workdf)
print(net_coef)
##bn.fit.barchart(net_coef$income)

# beilif propagation

