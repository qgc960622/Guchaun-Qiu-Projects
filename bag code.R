library(ggplot2)

#import data
bags = read.csv("/Users/liutianyao/Desktop/career tu/实战project template/section3/ssence_bags_info.csv")

###insight 1: price distribution of bags in top 10 pages ###
summary(bags$Price)
#Most expensive one: Saint Laurent - Black Large Becky Bag
bags[bags$Price==max(bags$Price),]
#Cheapest one: Carhartt Work In Progress - Brown Small Essentials Bag 
bags[bags$Price==min(bags$Price),]

#percentile
percentile = as.data.frame(quantile(as.numeric(bags$Price),seq(0,1,0.02)))
percentile$percentile = rownames(percentile)
colnames(percentile) = c('bag_price','percentile')

ggplot(data=percentile, aes(x = reorder(percentile, bag_price), y=bag_price)) +
  geom_bar(stat="identity")



###insight 2: each competitor's avg price level###
brandPrice <- aggregate(bags$Price, by = list(bags$Brand), 
                                      FUN = mean)
brandPrice = brandPrice[order(-brandPrice$x),]
