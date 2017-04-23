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





