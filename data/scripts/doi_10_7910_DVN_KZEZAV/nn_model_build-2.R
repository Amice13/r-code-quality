path <- "your file directory"

FLAGS <- flags(
  flag_numeric("dropout1", 0.3),
  flag_numeric("dropout2", 0.3),
  flag_integer('neurons1', 128),
  flag_integer('neurons2', 128),
  flag_numeric('l2', 0.001),
  flag_numeric('lr', 0.001)
)

build_model <- function(){
  model <- keras_model_sequential() %>% 
    layer_dense(units = FLAGS$neurons1, activation = "relu", input_shape = dim(xTrain)[2],
                kernel_regularizer = regularizer_l2(l = FLAGS$l2)) %>% 
    layer_dropout(rate = FLAGS$dropout1) %>%
    layer_dense(units = FLAGS$neurons2, activation = "relu", 
                kernel_regularizer = regularizer_l2(l = FLAGS$l2)) %>%
    layer_dropout(rate = FLAGS$dropout2) %>%
    layer_dense(units = 4, activation = "softmax")
  
  model %>% compile(
    optimizer = optimizer_rmsprop(lr = FLAGS$lr),
    loss = "categorical_crossentropy",
    metrics = c("accuracy")
  )
  model
}

model <- build_model()

early_stop <- callback_early_stopping(monitor = "val_loss", patience = 20)

epochs <- 10

# Fit the model and store training stats
history <- model %>% fit(
  xTrain,
  yTrain,
  epochs = epochs,
  validation_data = list(xTest, yTest),
  verbose = 1,
  callbacks = list(early_stop)
)

plot(history)

score <- model %>% evaluate(
  xTest, yTest,
  verbose = 0
)

save_model_hdf5(model, paste0(path, "nn_tuning.h5"))

cat('Test loss:', score[1], '\n')
cat('Test accuracy:', score[2], '\n')

