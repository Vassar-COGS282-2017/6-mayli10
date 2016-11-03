# Set up...
load("data/train.Rdata")
load("data/test.Rdata")

show.digit <- function(i, which.matrix="test"){
  if(which.matrix=="test"){
    m <- testData
    mL <- testLabels
  }
  if(which.matrix=="train"){
    m <- trainData
    mL <- trainLabels
  }
  image(matrix(m[i,], nrow=28)[,28:1], 
        col=gray(12:1/12), # color scheme for visualizing
        xlab=which(mL[i,]==1)-1, # extract the label from the label matrix
        xaxt='n',yaxt='n' # remove x and y axes.
  )
}

# The above statements load four data frames total:
# testData - vector representations of handwritten digits for testing the network (60,000 examples)
# trainData - same, but for training the network (10,000 examples)
# testLabels - vector representation of the correct label for text examples
# trainData - same, but for training.

# run this line to see what a single vector looks like
testData[10,]

# the show.digit function will visualize the vector in a more interpretable way:
show.digit(10, which.matrix="test")
show.digit(10, which.matrix="train")

# the labels are given by a ten element vector, with the corresponding digit set to 1.
testLabels[10,] # correct answer is "3"
trainLabels[10,] # correct answer is "8"
