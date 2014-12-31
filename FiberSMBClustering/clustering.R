# dexterlau@google.com 20140731
# SMB customer profiling clusters based on DNB and TNS data
library(grt)
set.seed(1)
tns.data <- read.csv("20140806_SampleTNSData_FH.csv")

tns.data.labels <- cbind(tns.data[1:8])
tns.data.features <- tns.data[9:18]
tns.data.features.scaled <- scale(tns.data.features)

# Elbow method http://stackoverflow.com/questions/15376075/cluster-analysis-in-r-determine-the-optimal-number-of-clusters
wss <- (nrow(tns.data.features.scaled)-1)*sum(apply(tns.data.features.scaled,2,var))
  for (i in 3:30) wss[i] <- sum(kmeans(tns.data.features.scaled,centers=i,nstart=30)$withinss)
plot(3:30, wss[3:30], type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

# Run kmeans
tns.clusters <- kmeans(tns.data.features.scaled, 21, nstart=25)

# Compute weight of each cluster
tns.clusters.summary.weights <- count(data.frame(tns.clusters$cluster))$freq/nrow(data.frame(tns.clusters$cluster))
tns.clusters.summary <- cbind(count(data.frame(tns.clusters$cluster)), tns.clusters.summary.weights)

# Compute and plot the scaled and unscaled centers
attr(tns.clusters$centers,'scaled:scale') <- attr(tns.data.features.scaled , 'scaled:scale')
attr(tns.clusters$centers,'scaled:center') <- attr(tns.data.features.scaled , 'scaled:center')
tns.clusters.unscaled <- data.frame(unscale(tns.clusters$centers))
tns.clusters.centers <- data.frame(tns.clusters$centers)
tns.clusters.centers$name <- sprintf("%02d",as.numeric(rownames(tns.clusters$centers)))
tns.clusters.centers$weights <- as.factor(tns.clusters.summary.weights)
#tns.clusters.centers$weightname <- paste(tns.clusters.centers$name,sprintf("(%04.1f%%)",tns.clusters.summary.weights*100))
tns.clusters.centers$weightname <- paste(sprintf("(%04.1f%%)",tns.clusters.summary.weights*100),tns.clusters.centers$name)

tns.clusters.unscaled$name <- sprintf("%02d",as.numeric(rownames(tns.clusters$centers)))
tns.clusters.unscaled$weights <- tns.clusters.summary.weights
tns.clusters.unscaled$weightname <- paste(tns.clusters.centers$name,sprintf("(%04.1f%%)",tns.clusters.summary.weights*100))

tns.clusters.centers.m <- melt(tns.clusters.centers)
tns.clusters.centers.m <- ddply(tns.clusters.centers.m, .(variable), transform, rescale = rescale(value))
#ggplot(tns.clusters.centers.m, aes(variable, name, weights)) + geom_tile(aes(fill = rescale, height=as.numeric(as.character(weights))), colour = "white") + scale_fill_gradient(low = "white", high = "steelblue")
ggplot(tns.clusters.centers.m, aes(variable, weightname, weights)) + geom_tile(aes(fill = rescale), colour = "white") + scale_fill_gradient(low = "white", high = "steelblue", name="Scale", guide=FALSE) + xlab("Feature") + ylab("Cluster")
tns.business.clusters <- cbind(tns.clusters$cluster, tns.data.labels, tns.data.features)
write.csv(tns.clusters.unscaled,"CentersOutput.csv")
write.csv(tns.clusters$centers,"CentersScaledOutput.csv")
write.csv(tns.business.clusters,"ClusterOutput.csv")