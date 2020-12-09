###########################################################################
###########################################################################
###                                                                     ###
###         Script name:  BinaryCNNParasiteCells.R                      ###
###         Author:       Gary Hutson                                   ###
###         Date Created: 23/10/2020                                    ###
###         Description:  CNN of malaria parasite cells classification  ###
###         Copyright:    All reprints and usage should be cited        ###
###                                                                     ###
###########################################################################
###########################################################################
<<<<<<< Updated upstream

=======
  >>>>>>> Stashed changes

# -----------------------------------------------------------
#                     Load in libraries                    #
# -----------------------------------------------------------


library(tensorflow)
library(keras)
library(plyr)
library(dplyr)
library(ggplot2)
library(magrittr)
library(tidyr)
library(sys)
library(caret)
library(magick)
library(fs)
library(abind)
library(imager)
library(purrr)

# -----------------------------------------------------------
#                     Create Function                       #
# -----------------------------------------------------------

show_image <- function(path){
  
  if (!is.character(path)){
    stop("The file path must be a character string")
  }
  
  image <- imager::load.image(path)
  plot(image)
}

get_image_info <- function(img){
  image_result <- list(img_width = imager::width(img),
                       img_height = imager::height(img),
                       img_depth = imager::depth(img),
                       img_colour_channels=imager::spectrum(img))
  
  return(image_result)
  
}


# -----------------------------------------------------------
#                     Set directories of images             #
# -----------------------------------------------------------

#setwd("C:/Users/GaryHutson/Desktop/NHS R Community - Lightening Talk 2020")
dataset_dir <- "Data/cell_images/"

train_dir <- paste0(dataset_dir, "/train/")
test_dir <- paste0(dataset_dir, "/test/")

# List files with magick and create an animation of all the parasite cells in training

train_parasite <- dir_ls(path=paste0(train_dir, "parasite"),
                         glob = "*.png")
train_uninfected <- dir_ls(path=paste0(train_dir, "uninfected"),
                           glob = "*.png")

# View an indexed image from the uninfected and parasitsed bunch

parasite_image <- show_image(train_parasite[2])
uninfected_image <- show_image(train_uninfected[2])

# Check the dimensions of the images - these potentially need to be scaled in Keras before 
# learning on the images can commence

dim(parasite_image)
dim(uninfected_image)
# This shows the image width x height x depth x colour channels i.e. rgb - this will need to be
# set in Keras

# Loop through all images to get the image info


# -----------------------------------------------------------
#                     Create image animations               #
# -----------------------------------------------------------

system.time(train_parasite[1:100] %>%
              map(image_read) %>%
              image_join() %>%
              image_scale("300x300") %>% 
              image_animate(fps = .5) %>%
              image_write("Data/Parasite_Cells.gif"))

system.time(train_uninfected[1:100] %>%
              map(image_read) %>%
              image_join() %>%
              image_scale("300x300") %>% 
              image_animate(fps = .5) %>%
              image_write("Data/Uninfected_Cells.gif"))


# -----------------------------------------------------------
#                    Build Baseline Model                   #
# -----------------------------------------------------------
image_shape <- c(130,130,3)
print(image_shape)

#Build Keras Baseline Model

model <- keras_model_sequential() %>% 
  layer_conv_2d(filters=32, kernel_size=c(3,3), activation = "relu",
                input_shape = image_shape) %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  
  layer_conv_2d(filters=64, kernel_size = c(3,3),
                input_shape = image_shape, activation="relu") %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  
  
  layer_conv_2d(filters=64, kernel_size = c(3,3)) %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  
  
  layer_conv_2d(filters=32, kernel_size=c(3,3), activation = "relu",
                input_shape = image_shape) %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  
  layer_flatten() %>% 
  layer_dense(1, activation = "sigmoid") %>% 
  layer_dropout(0.5)

# -----------------------------------------------------------
#                     Compile baseline model                #
# -----------------------------------------------------------


model %>% 
  compile(
    loss='binary_crossentropy',
    optimizer=optimizer_rmsprop(),
    metrics = c("acc")
  )

print(model) # This will print the model structure


train_datagen <- image_data_generator(rescale = 1/255)
test_datagen <- image_data_generator(rescale=1/255)
batch_size <- 16

train_generator <- flow_images_from_directory(
  train_dir,
  train_datagen,
  target_size = c(image_shape[1:2]), 
  batch_size = batch_size,
  class_mode = "binary"
)

test_generator <- flow_images_from_directory(
  test_dir,
  test_datagen, 
  target_size = c(image_shape[1:2]),
  batch_size = batch_size,
  class_mode = "binary"
)

#batch <- generator_next(train_generator)
#print(batch)

history <- model %>% fit_generator(
  train_generator,
  steps_per_epoch = 150,
  epochs = 50,
  validation_data = test_generator,
  validation_steps = 75
)

model %>% save_model_hdf5("Data/parasite_cells_classification.h5")

# -----------------------------------------------------------
#           Image Augmentation to improve model             #
# -----------------------------------------------------------

image_gen <- image_data_generator(rotation_range = 40,
                                  width_shift_range = 0.1,
                                  height_shift_range = 0.1, 
                                  shear_range = 0.1,
                                  zoom_range = 0.8, #Zoom is the key addition
                                  horizontal_flip = T,
                                  fill_mode = 'nearest',
                                  rescale = 1/255)

help("image_data_generator")

test_datagen <- image_data_generator(rescale=1/255)
# This normalises the pixel scales so our DL can work with the images



# -----------------------------------------------------------
#           Create Augmented Model                         #
# -----------------------------------------------------------
# Create a new model
model <- keras_model_sequential() %>% 
  layer_conv_2d(filters=32, kernel_size=c(3,3), activation = "relu",
                input_shape = image_shape) %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  
  layer_conv_2d(filters=64, kernel_size = c(3,3),
                input_shape = image_shape, activation="relu") %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  
  
  layer_conv_2d(filters=128, kernel_size = c(3,3),
                input_shape = image_shape, activation="relu") %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_conv_2d(filters=128, kernel_size = c(3,3),
                input_shape = image_shape, activation="relu") %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  
  layer_flatten() %>% 
  layer_dense(512, activation = "relu") %>% 
  layer_dense(1, activation = "sigmoid") %>% 
  layer_dropout(0.5)

model %>%  compile(
  loss = "binary_crossentropy",
  optimizer = optimizer_rmsprop(lr=1e-4),
  metrics = c("acc")
)

# Get model summary 
summary(model)

# -----------------------------------------------------------
#           Create augmented versions of images            #
# -----------------------------------------------------------

train_gen_augment <- flow_images_from_directory(
  train_dir,
  image_gen, 
  target_size = c(image_shape[1:2]),
  batch_size = batch_size * 2,
  class_mode = "binary"
)

test_gen_augment <- flow_images_from_directory(
  test_dir,
  test_datagen,
  target_size = c(image_shape[1:2]),
  batch_size = batch_size * 2,
  class_mode = "binary"
)

# Batch size is 24,960 and batch size selected is 32 so there is 780 images per batch

history_augment <- model %>% 
  fit_generator(
    train_gen_augment,
    steps_per_epoch = 100,
    epochs = 50, 
    validation_data = test_gen_augment,
    validation_steps = as.integer(100 / 2),
    callbacks = callback_early_stopping(monitor = "val_loss", 
                                        patience=5)
  )



# -----------------------------------------------------------
#           Save best fitting model                       #
# -----------------------------------------------------------

summary(history_augment$metrics$acc)

model %>% save_model_hdf5("Data/parasite_cells_classification_augmented.h5")

# -----------------------------------------------------------
#          Load model to replace the need to train model    #
# -----------------------------------------------------------


model <- load_model_hdf5("Data/parasite_cells_classification_augmented.h5")


# -----------------------------------------------------------
#  Predicting and preprocessing test image for prediction   #
# -----------------------------------------------------------


# Make a prediction with our model

pred_img <- train_parasite[100] #Selects the index as a prediction of a new parasitic image
img_new <- image_load(pred_img, target_size = c(image_shape[1:2]))
pred_img <- image_to_array(img_new)
img_tensor <- array_reshape(pred_img, c(1, image_shape)) # Reshape the array to have a 1 and the image shape
img_tensor <- img_tensor / 255 #Scale the image between 0 and 1
plot(as.raster(img_tensor[1,,,])) #Select the prediction from the tensor array


# Predict image class from model

predval <- predict(model, img_tensor)
pred <- keras::predict_classes(model, img_tensor) %>% 
  as.data.frame() %>% 
  dplyr::mutate(Class=case_when(
    V1 == 0 ~ "Parasite Class",
    TRUE ~ "Uninfected"
  )) %>% 
  dplyr::rename(ClassPred=V1) %>% 
  cbind(predval)

print(pred)

# Due to keras from directory sorting by alphabetical order
# Parasite will be give label 0 and Uninfected 1, so we switch these

# -----------------------------------------------------------
# -----------------------------------------------------------
# -----------------------------------------------------------
#                        EOF                                #
# -----------------------------------------------------------
# -----------------------------------------------------------
# -----------------------------------------------------------



