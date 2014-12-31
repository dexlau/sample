### 20130713_FinalProject.R by dexlau@gmail.com (Dexter Lau) ###

# Load the data from libraries
library(class)
library(ggplot2)

###########################################
# DEFINE FUNCTIONS
###########################################
KNN <- function(data, labels, percent, max = 100){

	N <- nrow(data)						# total data rows
	train.pct <- percent					# percent of data to train on

	set.seed(1)							# set seed for sampling consistency
	train.index <- sample(1:N, train.pct * N)       # random sample of records (training set)
	train.data <- data[train.index, ]			# perform train/test split
	test.data <- data[-train.index, ]			# note use of neg index...different than Python!

	train.labels <- as.factor(as.matrix(labels)[train.index, ])     # extract training set labels
	test.labels <- as.factor(as.matrix(labels)[-train.index, ])     # extract test set labels

	## Apply the Model
	# initialize results object
	err.rates <- data.frame()

	max.k <- max

	# perform fit for various values of k
	for (k in 1:max.k) {
		knn.fit <- knn(
			train = train.data,         # training set
			test = test.data,           # test set
			cl = train.labels,          # true labels
			k = k                       # number of NN to poll
		  )

		# print params and confusion matrix for each value k
		cat('\n', 'k = ', k, ', train.pct = ', train.pct, '\n', sep='')
		print(table(test.labels, knn.fit))

		# store generalation error and append to total results
		this.err <- sum(test.labels != knn.fit) / length(test.labels)
		#print (err.rates)
		err.rates <- rbind(err.rates, this.err)
	}
} 

###########################################
# PREPROCESSING
###########################################
channel.data<-  read.csv('channelStats.csv',header=T)
labels <- channel.data$cluster
channel.data$cluster <- NULL
channel.data$channel <- NULL

#Age weights
weights.age <- 10/7
weights.gender <- 10/4
weights.device <- 1/9
#weights.age <- 1
#weights.gender <- 1
#weights.device <- 1

channel.data$ar_3 <- channel.data$ar_3*weights.age
channel.data$ar_4 <- channel.data$ar_4*weights.age
channel.data$ar_5 <- channel.data$ar_5*weights.age
channel.data$ar_6 <- channel.data$ar_6*weights.age
channel.data$ar_7 <- channel.data$ar_7*weights.age
channel.data$ar_blank <- channel.data$ar_blank*weights.age
channel.data$gender_m <- channel.data$gender_m*weights.gender
channel.data$gender_f <- channel.data$gender_f*weights.gender
channel.data$gender_o <- channel.data$gender_o*weights.gender
channel.data$gender_blank <- channel.data$gender_blank*weights.gender
channel.data$device_android <- channel.data$device_android*weights.device
channel.data$device_ios <- channel.data$device_ios*weights.device
channel.data$device_mweb <- channel.data$device_mweb*weights.device
channel.data$device_tvandroid <- channel.data$device_tvandroid*weights.device
channel.data$device_tvhtml5 <- channel.data$device_tvhtml5*weights.device
channel.data$device_tvlite <- channel.data$device_tvlite*weights.device
channel.data$device_web <- channel.data$device_web*weights.device
channel.data$device_xbox <- channel.data$device_xbox*weights.device
channel.data$device_blank <- channel.data$device_blank*weights.device

N <- nrow(channel.data)
train.pct <- .6

err.rates <- data.frame()

max.k <- 70

for(i in 1:1) {
  set.seed(1)				# set seed before sampling
  train.index <- sample(1:N, train.pct * N)
  train.data <- channel.data[train.index, ]
  test.data <- channel.data[-train.index, ] 
  
  train.labels <- as.factor(as.matrix(labels)[train.index, ])     
  test.labels <- as.factor(as.matrix(labels)[-train.index, ])
  
  for (k in 1:max.k) {
    knn.fit <- knn(
      train = train.data,		# training set
      test = test.data,			# test set
      cl = train.labels,		# true labels
      k = k					# number of NN to poll
    )
    
    cat('\n', 'k = ', k, ', train.pct = ', train.pct, '\n', sep='')
    print(table(test.labels, knn.fit))
    
    this.err <- sum(test.labels != knn.fit) / length(test.labels)
    err.rates <- rbind(err.rates, this.err)
  }
  results <- data.frame(1:max.k, err.rates)   # create results summary data frame
  names(results) <- c('k', 'err.rate')        # label columns of results df
  
  # create title for results plot
  title <- paste('knn results (train.pct = ', train.pct, ')', sep='')
  
  # create results plot
  results.plot <- ggplot(results, aes(x=k, y=err.rate)) + geom_point() + geom_line()
  results.plot <- results.plot + ggtitle(title)
  
  # draw results plot
  results.plot
}