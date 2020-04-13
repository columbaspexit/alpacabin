
# Transformation matrices used over & over

# Permutation/Orientation Vectors ----------------------------------------
p1 <- matrix(c(1,0,0, 0,1,0, 0,0,1), ncol=3)
p2 <- matrix(c(1,0,0, 0,0,1, 0,1,0), ncol=3)
p3 <- matrix(c(0,1,0, 1,0,0, 0,0,1), ncol=3)
p4 <- matrix(c(0,0,1, 1,0,0, 0,1,0), ncol=3)
p5 <- matrix(c(0,1,0, 0,0,1, 1,0,0), ncol=3)
p6 <- matrix(c(0,0,1, 0,1,0, 1,0,0), ncol=3)

# Matrix of all permutations/orientations ---------------------------------
perms <- array(c(p1,p2,p3,p4,p5,p6), c(3,3,6))
dimnames(perms)[[3]] <- c("p1","p2","p3","p4","p5","p6")
rm(p1,p2,p3,p4,p5,p6)


# Differences Tesseract ---------------------------------------------------
# This encodes which coordinate,for each pair of coordinates, spans bin
diff_tess <- array(# rem-x  rem-y rem-z
  c(0,0,0, 1,0,0, 0,0,0, # The rem-x prism spans its NEXT dim, binW
    0,1,0, 0,0,0, 0,0,0, # The rem-y prism spans its PREVIOUS dim, binL

    0,0,0, 0,0,0, 0,1,0, # The rem-y prism spans its NEXT dim, binH
    0,0,0, 0,0,1, 0,0,0, # The rem-z prism spans its PREVIOUS dim, binW

    0,0,1, 0,0,0, 0,0,0, # The rem-z prism spans its NEXT dim, binL
    0,0,0, 0,0,0, 1,0,0) # The rem-x prism spans its PREVIOUS dim, binH
  ,c(3,3,2,3), dimnames= list(Coords = c("x","y","z"),
                              Prism = c("remX","remY","remZ"),
                              Sweep = c("Next","Prev"),
                              Overlap = c("x/y","y/z","z/x")))


# Coordinate Sweeps -------------------------------------------------------
# The 2x2x2 combinations of which coordinate sweeps where:
twpoth <- array(0,c(3,3,2,2,2))
for (i in 1:2) {
  for (j in 1:2){
    for (k in 1:2){
      twpoth[,,i,j,k] <- diff_tess[,,i,1] + diff_tess[,,j,2] + diff_tess[,,k,3]
    }
  }
}
twpoth <- array(twpoth,c(3,3,8))
dimnames(twpoth) <- list(Coords = c("x","y","z"),
                         Prism = c("remX","remY","remZ"),
                         SweepDims = c("s000","s001","s010","s011","s100","s101","s110","s111"))
rm(i,j,k)
