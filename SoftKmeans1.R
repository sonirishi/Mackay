rm(list=ls(all=T))

setwd('E:/Documents/Practice/Mackay')

library(dplyr)
library(ggplot2)
library(broom)

data_fin = read.csv("data_cluster.csv"); data_fin = data_fin[c('x','y')]

clust <- data_fin %>% ungroup() %>% dplyr::select(x, y) %>% kmeans(2)

clustermean = as.data.frame(clust$centers); colnames(clustermean) = c('x','y')

ggplot(augment(clust, data_fin), aes(x, y)) + geom_point(aes(color = .cluster)) +
  geom_point(aes(x, y), data = clustermean, size = 10, shape = "x") +
  labs(color = "K-means assignments")

data_fin = read.csv("data_cluster.csv"); data_fin = data_fin[c('x','y')]

k=2

responsiblity_matrix = matrix(rep(0,nrow(data_fin)*2),nrow(data_fin),2)

cluster_probability = matrix(c(0.5,0.5),1,2)

start = sample(1:nrow(data_fin),2)

centre = matrix(c(runif(1,min(data_fin$x),max(data_fin$x)),
                runif(1,min(data_fin$y),max(data_fin$y)),
                runif(1,min(data_fin$x),max(data_fin$x)),
                runif(1,min(data_fin$y),max(data_fin$y))),2,2)

sigmasq = matrix(rep(0.1,4),2,2)

total_responsibility = matrix(rep(0,2),1,2)

dist_fin = function(val,centre){
  return((val-centre)^2)
}

for(iter in 1:100){
  dist_matrix = matrix(rep(0,nrow(data_fin)*4),nrow(data_fin),4)
  likelihood = matrix(rep(0,nrow(data_fin)*2),nrow(data_fin),2)
  for(i in 1:k){
    dist_matrix[,i] = dist_fin(data_fin[,1],centre[i,1])  ## Col 1 for cluster i
    dist_matrix[,i+2] = dist_fin(data_fin[,2],centre[i,2])  ## Col 2 for cluster i
  }
  for(i in 1:k){
    likelihood[,i] = cluster_probability[i]/(2*pi*sqrt(sigmasq[i,1]*sigmasq[i,2]))*exp(dist_matrix[,i]/2*sigmasq[i,1] + dist_matrix[,i+2]/2*sigmasq[i,2])
  }
  for(i in 1:k){
    responsiblity_matrix[,i] = likelihood[,i]/(likelihood[,1] + likelihood[,2])
  }
  for(i in 1:k){
    centre[i,1] = sum(responsiblity_matrix[,i]*data_fin[,1])/sum(responsiblity_matrix[,i])
    centre[i,2] = sum(responsiblity_matrix[,i]*data_fin[,2])/sum(responsiblity_matrix[,i])
    cluster_probability[i] = sum(responsiblity_matrix[,i])/sum(responsiblity_matrix)
    sigmasq[i,1] = sum(responsiblity_matrix[,i]*dist_matrix[,i])/sum(responsiblity_matrix[,i])
    sigmasq[i,2] = sum(responsiblity_matrix[,i]*dist_matrix[,i+2])/sum(responsiblity_matrix[,i])
  }
}

data_fin$cluster = ifelse(responsiblity_matrix[,1] > responsiblity_matrix[,2],1,2)

center_df = as.data.frame(centre); colnames(center_df) = c('x','y')

ggplot(data_fin, aes(x, y)) + geom_point(aes(color = cluster)) + 
  geom_point(data=center_df,aes(x, y),colour="red", size=5)


