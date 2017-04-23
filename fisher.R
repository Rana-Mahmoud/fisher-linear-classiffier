# This Script implements Fisher’s linear discriminant classifier 
# that can recognize scanned images of the 26 lower-case characters
library(jpeg)
library(MASS)
library(plotly)
# Step 1 :
#--------- Load training data----------------------
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
# =====================================================