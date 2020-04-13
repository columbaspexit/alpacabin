

##-The big bad-------------------------------------------


#' Pack Bin Function
#'
#' Calculate the number of items that fit in a bin.
#' @param item_dims Vector containing item dimensions
#' @param bin_dims Vector containing bin dimensions
#' @param init_qty Number to track how many items fit in bin. Defaults to 0.
#' @param itr Number to tracks how many recursions have been run. Defaults to 0.
#' @return Returns two values: how many items fit & how many iterations it took \code{pack_bin} to calculate.
#' @examples
#' pack_bin(c(7.5,5,.25),c(24,12,10))
#' pack_bin(c(14,12.5,3),c(24,12,10))

pack_bin <- function(item_dims, bin_dims, init_qty = 0, itr = 0) {

  # Does the item even fit in bin ?
  if (!test_fit(item_dims,bin_dims)) {
    return(c(init_qty, itr))
  }

  # The six item Permutations/Orientations within bin
  item_perms <- apply(perms, 3, function(x) x %*% item_dims)

  # Total items that fit along each coord for each Orientation
  it_dimsums <- bin_dims %/% item_perms

  # If item doesn't fit, it's not a valid Orientation
  it_dimsums[it_dimsums == 0] <- NA

  # Total items that fit in bin for each Orientation
  it_sums <- apply(it_dimsums,2,prod)

  # Dimensions of cube of Max Items for each Orientation
  cube_dims <- item_perms * it_dimsums

  # Remaining Dimensions in bin for each Orientation
  rem_dims <- bin_dims - cube_dims

  # Potential Cubes of remaining volume by Orientation
  #
  # The remaining space in the bin can be partitioned
  # into 6 triads of maximal, non-overlapping volumes.
  # The naming conventions below allow a structured
  # enumeration of triad components & how the 6 triads vary.
  #
  # Each triad is comprised of three rectangular prisms.
  #
  # Prisms are labeled using their dimension of remaining space,
  # i.e. the gap between the cube of items and the wall of the bin.
  # So the prism with an x length of rem-dim-x is the x-rem prism.
  # With three spatial dimensions, we get a triad of three prisms:
  # x-prism, y-prism, & z-prism.
  #
  # Together, the prisms have three regions of potential overlap,
  # one region between each pair of dimensions: x/y, y/z, & x/z
  # Consequently, each overlap region gets assigned to just one
  # prism at the expense of the other prism.
  # 3 overlaps x 2 choices = 6 possible allocations of overlap,
  # hence 6 triads of prisms.

  # Tensor of the constant coordinates for all 6 orientations
  # of the original item cube. cdap = Constant Dims for All Permutations
  cdap <- array(apply(rem_dims, 2, function(x) x * perms[,,1] +
                        bin_dims * (matrix(1,3,3) - perms[,,1])),c(3,3,6))
  dimnames(cdap) <-  list(Coord = c("x","y","z"),
                          Prism = c("remX","remY","remZ"),
                          ItCubePerm = c("p1","p2","p3","p4","p5","p6"))

  # For all six original item-cube orientations
  zomg <- array(apply(rem_dims,2,function(x) twpoth * x),c(3,3,8,6))

  # To match up dimensions for ease of subtraction
  cdapVIII <- array(c(cdap[,,1],cdap[,,1],cdap[,,1],cdap[,,1],cdap[,,1],cdap[,,1],cdap[,,1],cdap[,,1],
                      cdap[,,2],cdap[,,2],cdap[,,2],cdap[,,2],cdap[,,2],cdap[,,2],cdap[,,2],cdap[,,2],
                      cdap[,,3],cdap[,,3],cdap[,,3],cdap[,,3],cdap[,,3],cdap[,,3],cdap[,,3],cdap[,,3],
                      cdap[,,4],cdap[,,4],cdap[,,4],cdap[,,4],cdap[,,4],cdap[,,4],cdap[,,4],cdap[,,4],
                      cdap[,,5],cdap[,,5],cdap[,,5],cdap[,,5],cdap[,,5],cdap[,,5],cdap[,,5],cdap[,,5],
                      cdap[,,6],cdap[,,6],cdap[,,6],cdap[,,6],cdap[,,6],cdap[,,6],cdap[,,6],cdap[,,6]),c(3,3,8,6))

  # Dimensions of the EIGHT potential triads of remaining space
  # for each of the SIX possible orientations of the original item-cube
  triad_dims <- cdapVIII - zomg
  dimnames(triad_dims) <- list(Coord = c("x","y","z"),
                               Prism = c("remX","remY","remZ"),
                               SweepDims = c("yyy","yyn","yny","ynn","nyy","nyn","nny","nnn"),
                               ItCubePerm = c("p1","p2","p3","p4","p5","p6"))

  # How many items, in each of the six item orientations, fit along each axis of each prism ?
  triad_item_dimsums <- array(apply(triad_dims,c(2,3,4),function(x) x %/% item_perms),c(3,6,3,8,6))
  dimnames(triad_item_dimsums) <- list(Coord = c("x","y","z"),
                                       FillPerm = c("fp1","fp2","fp3","fp4","fp5","fp6"),
                                       Prism = c("remX","remY","remZ"),
                                       SweepDims = c("yyy","yyn","yny","ynn","nyy","nyn","nny","nnn"),
                                       ItCubePerm = c("p1","p2","p3","p4","p5","p6"))

  # How many total items/volume is that for each prism, for each fill perm?
  triad_item_sums <- apply(triad_item_dimsums, c(2,3,4,5), prod)

  # Which FillPerm(s), for each prism, hold the most items?
  max_FillPerms <- apply(triad_item_sums,c(2,3,4),max)

  # How many items total is that across each possible SweepDims combo?
  triad_totals <- apply(max_FillPerms, c(2,3),sum)

  # Which SweepDims holds the most additional items?
  triad_BIS <- apply(triad_totals,2,max)

  # Finally, for each of the six original item orientations,
  # how many items is that in addition to the original item-cube ?
  total_items <- it_sums + triad_BIS

  # What are the indices of the best from each dimension?
  # To find these we have to work backwards...
  best_perm <- which.max(total_items)
  best_sweep <- apply(triad_totals,1,function(x) which.max(x))[best_perm]
  best_maxfp <- apply(triad_item_sums[,,best_sweep,best_perm],2,function(x) which.max(x))

  # Remaining Space triad of prisms that yielded the most filled items.
  final_triad_dims <- triad_dims[,,best_sweep,best_perm]

  # How many items was that fit in best_perm?
  final_cube_qty <- it_sums[best_perm]

  # How many items fit in the best triad of prisms?
  # First, how many items along each dimension...
  btid <- array(0,c(3,3), dimnames = list(Coord = c("x","y","z"),
                                          Prism = c("remX","remY","remZ")))
  for (c in 1:3) {
    btid[,c] <- triad_item_dimsums[,best_maxfp[c],c,best_sweep,best_perm]
  }
  # Triad quantities are left separate for recursing down
  per_triad_qty <- apply(btid,2,prod)

  itr <- itr + 1
  tp1 <- pack_bin(item_dims,final_triad_dims[,1],per_triad_qty[1], itr)
  tp2 <- pack_bin(item_dims,final_triad_dims[,2],per_triad_qty[2], itr)
  tp3 <- pack_bin(item_dims,final_triad_dims[,3],per_triad_qty[3], itr)

  return(c(final_cube_qty + tp1[1] + tp2[1] + tp3[1], max(tp1[2],tp2[2],tp3[2])))

}

