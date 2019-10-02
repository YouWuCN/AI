# -*- coding: utf-8 -*-
"""
Created on Fri Sep 28 09:58:48 2018

@author: Mike
"""

import numpy as np
import matplotlib.pyplot as plt
from keras.datasets import cifar10
from keras.utils import np_utils
from keras import backend as K

# In *older* versions of Tensorflow/Keras you may need to adjust the image 
# dimension ordering. If you need to, uncomment the two code lines below.
# What we are doing is just making sure the image format is as desired. 
# This will make the feature (x) data - i.e. the RGB pixel values - for 
# each image have the shape 3x32x32.
# if K.backend()=='tensorflow':          # <- Uncomment this if required
#    K.set_image_dim_ordering("th")      # <- Uncomment this if required

# This is the main function. You need to write the getModel and fitModel functions to pass to this.
# Call your functions 'myGetModel' and 'myFitModel'.
# The getModel function should accept an object of the CIFAR class, and return a compiled Keras CNN model. 
# In this function you will specify the network structure (including regularization) and the optimizer to 
# be used (and its parameters like learning rate), and run compile the model (in the Keras sense of running 
# model.compile).
# The fitModel function should accect two arguments. The first is the CNN model you return from your getModel 
# function, and the second is the CIFAR classed data object. It will return a trained Keras CNN model, which 
# will then be applied to the test data. In this function you will train the model, using the Keras model.fit 
# function. You will need to specify all parameters of the training algorithm (batch size, etc), and the 
# callbacks you will use (EarlyStopping and ModelCheckpoint). You will need to make sure you save and load 
# into the model the weight values of its best performing epoch.
def runImageClassification(getModel=None,fitModel=None,seed=7):
    # Fetch data. You may need to be connected to the internet the first time this is done.
    # After the first time, it should be available in your system. On the off chance this
    # is not the case on your system and you find yourself repeatedly downloading the data, 
    # you should change this code so you can load the data once and pass it to this function. 
    print("Preparing data...")
    data=CIFAR(seed)
        
    # Create model 
    print("Creating model...")
    model=getModel(data)

    # Fit model
    print("Fitting model...")
    model=fitModel(model,data)

    # Evaluate on test data
    print("Evaluating model...")
    score = model.evaluate(data.x_test, data.y_test, verbose=0)
    print('Test accuracy:', score[1])

# This is the class that wraps the CIFAR data. You will probably need to be connected to the
# internet the first time you create an object of this class, as the data will be downloaded.
# After that, the data should be stored by Keras and no downloading will be required. 
# Important fields that you will need to use are: x_train, y_train, x_valid, y_valid, input_dim and 
# num_classes. The first four of these are the training and validation data (split into features and
# target). Note that these have been made ready for use with a Keras network - check out the code
# if you are interested. The last two are the number of input features and the number of target 
# classes. These will be needed when defining your CNN.
# The only public method is the showImages function, which you can use to see some labelled images
# from the (validation) data.
# Remember that the x_test and y_test fields will be blank when your functions are run in evaluation -
# so you cannot peek at these cases!
class CIFAR:
    def __init__(self,seed=0):
        # Get and split data
        data = self.__getData(seed)
        self.x_train_raw=data[0][0]
        self.y_train_raw=data[0][1]
        self.x_valid_raw=data[1][0]
        self.y_valid_raw=data[1][1]
        self.x_test_raw=data[2][0]
        self.y_test_raw=data[2][1]
        # Record input/output dimensions
        self.num_classes=10
        self.input_dim=self.x_train_raw.shape[1:]
         # Convert data
        self.y_train = np_utils.to_categorical(self.y_train_raw, self.num_classes)
        self.y_valid = np_utils.to_categorical(self.y_valid_raw, self.num_classes)
        self.y_test = np_utils.to_categorical(self.y_test_raw, self.num_classes)
        self.x_train = self.x_train_raw.astype('float32')
        self.x_valid = self.x_valid_raw.astype('float32')
        self.x_test = self.x_test_raw.astype('float32')
        self.x_train  /= 255
        self.x_valid  /= 255
        self.x_test /= 255
        # Class names
        self.class_names=['airplane','automobile','bird','cat','deer',
               'dog','frog','horse','ship','truck']

    def __getData (self,seed=0):
        (x_train, y_train), (x_test, y_test) = cifar10.load_data()
        return self.__shuffleData(x_train,y_train,x_test,y_test,seed)
    
    def __shuffleData (self,x_train,y_train,x_test,y_test,seed=0):
        tr_perc=.75
        va_perc=.15
        x=np.concatenate((x_train,x_test))
        y=np.concatenate((y_train,y_test))
        np.random.seed(seed)
        np.random.shuffle(x)
        np.random.seed(seed)
        np.random.shuffle(y)
        indices = np.random.permutation(len(x))
        tr=round(len(x)*tr_perc)
        va=round(len(x)*va_perc)
        self.tr_indices=indices[0:tr]
        self.va_indices=indices[tr:(tr+va)]
        self.te_indices=indices[(tr+va):len(x)]
        x_tr=x[self.tr_indices,]
        x_va=x[self.va_indices,]
        x_te=x[self.te_indices,]
        y_tr=y[self.tr_indices,]
        y_va=y[self.va_indices,]
        y_te=y[self.te_indices,]
        return ((x_tr,y_tr),(x_va,y_va),(x_te,y_te))

    # Print figure with 10 random images, one from each class
    def showImages(self):
        fig = plt.figure(figsize=(8,3))
        for i in range(self.num_classes):
            ax = fig.add_subplot(2, 5, 1 + i, xticks=[], yticks=[])
            idx = np.where(self.y_valid_raw[:]==i)[0]
            features_idx = self.x_valid_raw[idx,::]
            img_num = np.random.randint(features_idx.shape[0])
            im = np.transpose(features_idx[img_num,::],(1,2,0))
            ax.set_title(self.class_names[i])
            plt.imshow(im)
        plt.show()        

