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
To train from data we need to get W and W.node
, where
- w = Sw.invers( m1 - m2 ) 
- w.node = -w.Transpose( m1 + m2 )/2
So, as we see we need to calculate mean first , 
-  mean = 1/total.points.no *(summation of all points of the class)
class 1 is always have N1 = 7 each have 144 feature
# class 2 is always have  N2 = 175 each have 144 feature 

# Helper link for mean:
 https://math.stackexchange.com/questions/24485/find-the-average-of-a-collection-of-points-in-2d-space
# Helper link for how to write a function:
 https://www.r-bloggers.com/how-to-write-and-debug-an-r-function/

Now, Lets start.. 
We will do it for 26 classiffer 
# 1st step : Get current Class 1 and Class 2

- Make for Class 1 , sart and end index using current loop index

# start_index = ([currIndex-1]*7)+1 
# End index = start_index + 6

- For class 2 we have 3 cases:
	* if (curr.indx == 1)
     - class 2 start = end.index+1
     - class 2 end = 182
    * if (curr.indx == 26)
     - class 2 start = 1
     - class 2 end = start.index-1
    * Otherwise,
     part of c2 will be before c1 and part after , so
     - sub1 = input.features[1:(start.index-1) , 1:144] # for data before class one in main matrix
     - sub2 = input.features[(end.index+1):182 , 1:144] # for data after class one in main matrix
     - class 2 = rbind(sub1 ,sub2)
# 2nd step : calculate mean of class 1 and mean of class 2
	- use the function build in R or call mean.class function
# 3rd step : Calculate Sw 
 - Equation 
  * Sw = summation((Xc1 - m1)*(Xc1 - m1)T) + summation((Xc2 - m2)*(Xc2 - m2)T)
  - Lets get S1 first
  	* get (Xc1 - m1) , dim (1 x 144)
  	* get (Xc1 - m1)T , dim (144 x 1)
  	* get (Xc1 - m1)*(Xc1 - m1)T , # dim (144 X 144)
  	* get summation((Xc1 - m1)*(Xc1 - m1)T) 
  - Do the same to class 2 
  - Sw = S1 + S2 # dim (144 x 144)
# 4th step : Calculate W 
 - Equation
  * w = Sw.invers( m1 - m2 )
 - get Sw.invers ,  dim (144 x 144)
 - get mean.diffrence ,  dim (1 x 144)
 - get w.curr =   mean.diffrence %*% Sw.invers  , dim(1 x 144)
 - Append in big matrix of W of dim (26 x 144)
# 5th step : Calculate W node
 - Equation
  * w.node = w.Transpose( m1 + m2 )/2
 - get w.Transpose ,  dim(144 x 1)
 - get mean.sum , dim(1 x 144)
 - get w.node.curr , dim(1 x 1)
 - Append in big matrix of W node of dim (26 x 1)
# -------- Training is Done --------- #
# Load Testing data 
	- do as in training data but images will be in matrix (52 x 144) each row represent an image.
# Testing steps
	- Loop on each image in testing matrix
	- pass this image on the 26 classifier
	- substute with current W and W node to get y(x)
	- keep max y(x) index
# Plotting Results
	- x-axis plot characters
	- y-axis plot number of correctly classiffied images  

























