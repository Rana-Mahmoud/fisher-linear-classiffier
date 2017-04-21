# This Script implements Fisher’s linear discriminant classifier 
# that can recognize scanned images of the 26 lower-case characters
library(jpeg)
library(MASS)
# Step 1 :
#--------- Load training data----------------------
# each character have 7 images and 144 pixel(feature)
# so lets make for each charachter matrix 7 rows and 144 column
# ----------------------------------------------------
# ================= Combine Paths ====================
mainpath = '/home/rana/Desktop/fisher-linear-classiffier/Train/A1'
endpath  = '.jpg'
#-----------------------------
characters = list('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z')
#-----------------------------
# Lets combine paths
paths = list()
countPaths = 1
for (c in (1:26)){  # Loop on each character
  for (n in (1:7)){ # loop on the 7 imges of each character
    part1  = paste( mainpath , characters[[c]], sep="") 
    part2  = paste(n , endpath, sep="") # comine teh current path
    paths[countPaths] = paste(part1 ,part2, sep="")
    countPaths = countPaths + 1 # increment list path position
  }
}
# =====================================================
# ================= Read All images ===================
# helper link for reading images data :
# https://www.rdocumentation.org/packages/readbitmap/versions/0.1-4/topics/read.bitmap
#-------------------------------------------------------
# heper link for matrix initialization:
# https://www.r-bloggers.com/making-matrices-with-zeros-and-ones/
#--------------------------------------------------------
images.matrix = matrix(0, 1, 144) # initialize zeros matrix 1 row 145 column 
#img.count = 1
for (curr in (1:182)){  # Loop on the paths list
  curr.img = readJPEG(paths[[curr]], native = FALSE)
  # sort pixels in one list of features
  # Now ---------- lets read all pixels in on list of 144 feature
  curr_pixels = list()
  count = 1   # counter of features list
  for (x in 1:12){    # loop on x axis 
    for (y in 1:12){  # loop on y axis
      curr_pixels[count] = curr.img[x,y] # append pixel value in list of features
      count = count + 1 # incremant index of list
    }
  }
  # combin as row to the main data matrix
  images.matrix = rbind(images.matrix,as.double(curr_pixels))
}
# Delete the 1st row as it was zeros for just initialization
input.features = as.matrix(images.matrix[2:183,1:144])# now we have x.par
#------------------------------------------------------------------------- 
# =================== Lets Start the Algorithm ===================
# Equation is : y(x) = (w.Transpose*x) + w.node
# , Where
# w = Sw.invers( m1 - m2 )
# w.node = w.Transpose( m1 + m2 )/2
# ----------------------------------
# ================== first lets make function calculate mean ======================
# mean = 1/total.points.no *(summation of all points of the class)
# class 1 is always have N1 = 7 each have 144 feature
# class 2 ' ' ' ' ' ' '  N2 = 175 ' ' ' ' ' ' ' ' '' 
# Helper link for mean:
# https://math.stackexchange.com/questions/24485/find-the-average-of-a-collection-of-points-in-2d-space
# Helper link for how to write a function:
# https://www.r-bloggers.com/how-to-write-and-debug-an-r-function/
# ----------------------------------------------------------------------------
# function calculate mean for single feature
mean.feature <- function(feature.list , class.no)
{
  sum.features = 0
  for (i in 1:length(feature.list))
  {
    sum.features =  sum.features + feature.list[i]
  }
  calculated.mean <- (1/class.no) * sum.features
  return(calculated.mean)
}
# Testing statment working well
# this is mean of feature 1 class 1 
#mean(input.features[1:7,1:1]) # 0.003361345
#val = mean.feature(input.features[1:7,1:1] , 7)  # 0.003361345
#-------------------------------------------------------------------------------
# function calculate mean for single class
mean.class <- function(class.matrix , class.no)
{
  class.mean.list = list()
  for(i in 1:144)
  {
    # calculate mean of current feature
    curr.feature.mean = mean.feature(class.matrix[1:class.no , i:i] , class.no) 
    # append the mean in the class mean list
    class.mean.list[i] = curr.feature.mean
  }
  return(class.mean.list)
}
# Testing statments and it is working well 
# make temp class 1 of char A
#char.A = input.features[1:7 , 1:144]
#N1 = 7
#m1 = mean.class(char.A , N1)
#char.Not.A = input.features[8:182 , 1:144]
#N2 = 175
#m2 = mean.class(char.Not.A , N2)
#-------------------------------------------------------------------------------
# ============= Build big loop to make 26 mean 1 and 2 ======================
#-------------------------------------------------------------------------------
big.m1 = matrix(0,1,144) # main mean matrix of 26 m1 for each classifier
big.m2 = matrix(0,1,144) # main mean matrix of 26 m2 for each classifier
for (curr.indx in 1:26)
{ 
  # loop 26 time to make 26 classifier for characters
  # ----------- make class one subset --------------
  # start from ([currIndex-1]*7)+1 to currIndex*7 
  start.index = ((curr.indx-1)*7)+1
  end.index  = start.index + 6
  n1 = 7 # cound of points in class 1
  class.one = input.features[start.index:end.index , 1:144]
  # ----------- make class two subset --------------
  if (curr.indx == 1)
  {
    print("I'm in if ")
    print(curr.indx)
    class.two = input.features[(end.index+1):182 , 1:144]
  }
  else if(curr.indx == 26)
  {
    print("I'm in Elseif")
    print(curr.indx)
    class.two = input.features[1:(start.index-1) , 1:144]
  }
  else
  {
    print("I'm in else")
    print(curr.indx)
    sub1 = input.features[1:(start.index-1) , 1:144] # for data before class one in main matrix
    sub2 = input.features[(end.index+1):182 , 1:144] # for data after class one in main matrix
    class.two = rbind(sub1 ,sub2)
  }
  n2 = 175 # cound of points in class 2
  #--------------- calculate m1 , m2 -------------------
  m1 = mean.class(class.one , n1)
  m2 = mean.class(class.two , n2)
  # -------------- Append means to big mean matrices -----------------
  big.m1 = rbind(big.m1 , as.double(m1))
  big.m2 = rbind(big.m2 , as.double(m2))
}
# Remove initial row in big mean matricies
big.m1 = big.m1[2:27 , 1:144 ]
big.m2 = big.m2[2:27 , 1:144 ]
# --------------------------------------------------------------------






















