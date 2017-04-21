# fisher-linear-classiffier
This repo implements a Fisher’s linear discriminant classifier that can recognize scanned images of the 26 lower-case characters provided in the file “Assignment 1 Dataset.zip”. 
- The zip file contains two folders
  * “Train” and “Test”.
  - The “Train” folder contains 7 images for each lower-case character. 
  - The “Test” folder contains 2 images for each lower-case character. 

* The images in the “Train” folder should be used to train a classifier for each character using the method given at the bottom of slide 9 in Lecture 2.pdf. 
* After the classifiers are trained, test each classifier using the images given in the “Test” folder.

# Training Phase

# input data
I loaded the input data in one matrix called images.matrix
- has 145 column 
  * from 1 to 144 : are features each represent pixel value
  * 145 : is class lable assumbtion all is 1
- has 182 row 
  * each repesent an image of the training data
  * each 7 are for character 
    - eg : 
    - character A from row 1 to 7 
    - character A from row 8 to 15 ..etc 
# Equation and Arguments 
