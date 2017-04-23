# This Script implements Fisher’s linear discriminant classifier 
# that can recognize scanned images of the 26 lower-case characters
library(jpeg)
library(MASS)
library(plotly)
# Step 1 :
#--------- Load training & Testing data----------------------
# each character have 7 images and 144 pixel(feature)
# so lets make for each charachter matrix 7 rows and 144 column
# ----------------------------------------------------
# ================= Combine Paths ====================
mainpath.train = '/home/rana/Desktop/fisher-linear-classiffier/Train/A1'
mainpath.test = '/home/rana/Desktop/fisher-linear-classiffier/Test/A1'
endpath  = '.jpg'
#-----------------------------
characters = list('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z')
#-----------------------------
# Lets combine paths
paths.train = list()
paths.test = list()
countPaths.train  = 1
countPaths.test  = 1
for (c in (1:26)){  # Loop on each character
  for (n in (1:9)){ # loop on the 7 imges of each character
    if (n > 7)
    {
      part1.test  = paste( mainpath.test , characters[[c]], sep="") 
      part2 = paste(n , endpath, sep="") # comine teh current path
      paths.test[countPaths.test] = paste(part1.test ,part2, sep="")
      countPaths.test = countPaths.test + 1 # increment list path position
    }
    else
    {
      part1.train  = paste( mainpath.train , characters[[c]], sep="") 
      part2  = paste(n , endpath, sep="") # comine teh current path
      paths.train[countPaths.train] = paste(part1.train ,part2, sep="")
      countPaths.train = countPaths.train + 1 # increment list path position
    }
  }
}
print("Paths of training and testing images loaded successfully")
# ========== Paths of training and testing images loaded successfully ============
# =====================================================
# ================= Read All images ===================
# helper link for reading images data :
# https://www.rdocumentation.org/packages/readbitmap/versions/0.1-4/topics/read.bitmap
#-------------------------------------------------------
# heper link for matrix initialization:
# https://www.r-bloggers.com/making-matrices-with-zeros-and-ones/
#--------------------------------------------------------
images.matrix = matrix(0, 1, 144) # initialize zeros matrix 1 row 144 column 
testing.images.matrix = matrix(0, 1, 144) # initialize zeros matrix 1 row 144 column 
#  ================  Load training ===================
for (curr in (1:182)){  # Loop on the paths list
  curr.img = readJPEG(paths.train[[curr]], native = FALSE)
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
input.features = as.matrix(images.matrix[2:183,1:144])# now we have x.par dim(182 x 144)
dim(input.features)
#  ================  Load testing =================
for (curr in (1:52)){  # Loop on the paths list
  curr.img = readJPEG(paths.test[[curr]], native = FALSE)
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
  testing.images.matrix = rbind(testing.images.matrix,as.double(curr_pixels))
}
# Delete the 1st row as it was zeros for just initialization
testing.features = as.matrix(testing.images.matrix[2:53,1:144])# dim(182 x 144)
dim(testing.features)
#------------------------------------------------------------------------- 
# =================== Lets Start the Algorithm ===================
# Equation is : y(x) = (w.Transpose*x) + w.node
# , Where
# w = Sw.invers( m1 - m2 )
# w.node = -w.Transpose( m1 + m2 )/2
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
    class.mean.list[i] =as.double(curr.feature.mean)
  }
  typeof(class.mean.list)
  class.mean.mat =t(as.matrix(class.mean.list)) # dim (1 x 144)
  dim(class.mean.mat)
  return(class.mean.mat)
}
# ============= Build big loop to make 26 mean 1 and 2 ======================
#-------------------------------------------------------------------------------
w.matrix = matrix(0,1,144) # main W matrix of 26 for each classifier in row
w.node.matrix = matrix(0,1,1) # main W node column of 26 value for each classifier in row
# loop 26 time to make 26 classifier for characters
for (curr.indx in 1:26)
{ 
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
  m1 = mean.class(class.one , n1) # dim(1x144)
  dim(m1)
  m2 = mean.class(class.two , n2) # dim(1x144)
  dim(m2)
  # -------------- CALCULATE Sw FROME MEAN -----------------
  # Equation :
  # Sw = summation((Xc1 - m1)*(Xc1 - m1)T) + summation((Xc2 - m2)*(Xc2 - m2)T)
  # ----------------------------  S1   --------------------------------
  c1.sum = 0 # summation initialization
  for (c1 in 1:7) #loop on class one 
  {
    curr.row = t(as.matrix(class.one[c1:c1 , 1:144])) # dim (1 x 144)
    dim(curr.row)
    # (Xc1 - m1)
    dim(m1)
    curr.sub = curr.row - as.double(m1) # dim (1 x 144)
    dim(curr.sub)
    # (Xc1 - m1)T
    curr.sub.Transpose = t(curr.sub) # dim (144 x 1)
    dim(curr.sub.Transpose)
    # (Xc1 - m1)*(Xc1 - m1)T
    curr.Mult =  curr.sub  %*% curr.sub.Transpose # dim (1 X 1)
    dim(curr.Mult)
    # summation((Xc1 - m1)*(Xc1 - m1)T)
    c1.sum = c1.sum + curr.Mult 
    dim(c1.sum)
  }# end loop on class 1 for Sw summition
  dim(c1.sum) #  dim (1 X 1)
  # ----------------------------  S2   --------------------------------
  c2.sum = 0  # summation initialization
  for (c2 in 1:175) #loop on class one 
  {
    curr.row = t(as.matrix(class.two[c2:c2 , 1:144])) # dim (1 x 144)
    dim(curr.row)
    # (Xc2 - m2)
    dim(m2)
    curr.sub = curr.row - as.double(m2) # dim (1 x 144)
    dim(curr.sub)
    # (Xc2 - m2)T
    curr.sub.Transpose = t(curr.sub) # dim (144 x 1)
    dim(curr.sub.Transpose)
    # (Xc2 - m2)*(Xc2 - m2)T
    curr.Mult =  curr.sub  %*% curr.sub.Transpose # dim (1 X 1)
    dim(curr.Mult)
    # summation((Xc2 - m2)*(Xc2 - m2)T)
    c2.sum = c2.sum + curr.Mult 
  }# end loop on class 2 for Sw summition
  dim(c2.sum) # dim (1 X 1)
  # ========================== Calculate Sw ==========================
  Sw = 0
  Sw = c1.sum + c2.sum # dim (144 x 144)
  dim (Sw)
  # -----------------------------------------
  # ====== CALCULATE (W) FOR EACH CLASSIFIER ==========
  # w = Sw.invers( m1 - m2 )
  Sw.invers = ginv(Sw) #   dim (1 x 1)
  dim(Sw.invers)
  mean.diffrence = t(as.matrix(as.double(m1) - as.double(m2))) #   dim(1 x 144)
  dim(mean.diffrence)
  w.curr =   Sw.invers %*% mean.diffrence # dim(1 x 144)
  dim(w.curr)
  w.matrix = rbind(w.matrix , as.double(w.curr)) # append curr.w in big matrix of W
  dim(w.matrix)
  # -----------------------------------------
  # ====== CALCULATE (W Node) FOR EACH CLASSIFIER ==========
  # w.node = w.Transpose( m1 + m2 )/2
  w.Transpose = t(w.curr) #   dim(144 x 1)
  dim(w.Transpose)
  mean.sum = t(as.matrix(as.double(m1) + as.double(m2))) # dim(1 x 144)
  dim(mean.sum)
  w.mult.mean =  mean.sum %*% (- w.Transpose)   # dim(1 x 1)
  dim(w.mult.mean)
  w.node.curr = w.mult.mean*(1/2)  # dim(1 x 1)
  dim(w.node.curr)
  w.node.matrix = rbind(w.node.matrix , as.double(w.node.curr)) # append to big w.node matrix
  dim(w.node.matrix)
} # end 26 classifier loop 
# Remove initial w.matrix row and w.node
w.matrix = w.matrix[2:27,1:144]   # dim(26 x 144)
dim(w.matrix)
w.node.matrix = as.matrix(w.node.matrix[2:27,1:1]) # dim( 26 x 1 )
dim(w.node.matrix)
# -------------------- Training is DONE -------------------------------
print("Training is done ")
# ======================= Lets Start Testing ==========================






























