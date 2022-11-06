
#install_tensorflow()

# Load libraries
library(keras)
library(tensorflow)

rm(list=ls())

# Set categories to numbers 
iris[,5] <- as.numeric(iris[,5])

# Turn iris into a matrix
iris <- as.matrix(iris)

# Set dimnames to NULL
dimnames(iris) <- NULL

# Normalize attributes
iris[,1:4] = normalize(iris[,1:4])

summary(iris)

# Determine sample size
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.67,0.33))

# Split the iris data
iris.training <- iris[ind==1, 1:4]
iris.test <- iris[ind==2,1:4]

# Split the class attribute
iris.trainingtarget <- iris[ind==1,5]
iris.testtarget <- iris[ind==2,5]

# One hot encoding training target values
iris.trainLabels <- to_categorical(iris.trainingtarget)

# One hot encoding test target values
iris.testLabels <- to_categorical(iris.testtarget)

# Double check the result
print(iris.testLabels)

# Initialize a sequential model
model <- keras_model_sequential()

# Add layers to the model
model %>%
  layer_dense(units=5, activation='relu', input_shape=c(4)) %>%
  layer_dense(units=8, activation='relu') %>%
  layer_dense(units=4, activation='softmax')

summary(model)

# Get model configuration
get_config(model)

# Get layer configuration
get_layer(model, index=1)

# List the model's layer
model$layers

# List the input tensors
model$inputs

# List the output tensors
model$outputs

# Compile the model
model %>% compile(
  loss='categorical_crossentropy', 
  optimizer='adam',
  metrics='accuracy'
)

# Fit the model
model %>% fit(
  iris.training,
  iris.trainLabels,
  epochs=200,
  batch_size=5,
  validation_split=0.2
)

history <- model %>% fit(
  iris.training,
  iris.trainLabels,
  epochs=500,
  batch_size=5,
  validation_split=0.2
)

# Plot history
plot(history)

# Plot the model loss of the training data
plot(history$metrics$loss, 
     main="Model Loss", 
     xLab="epoch", 
     ylab="loss", 
     col="blue", 
     type="l"
)

# Plot the model loss of the test data
lines(history$metrics$val_loss,
      col="green"
)

# Add legend
legend("topright", c("train","test"), col=c("blue","green"), lty=c(1,1))

# Predict the classes for the test data
classes<-model %>% predict_classes(iris.test, batch_size=128)

# Confusion Matrix
c_matrix=table(iris.testtarget, classes)
print(c_matrix)


cat('Accuracy:', sum(diag(c_matrix))/sum(c_matrix)*100,'classes')
plot(history)

                                 