

# Read in sample item/bin data -------------------------------------------
items <- read.csv("./data/sample_items.csv")


# Add columns to hold pack_bin output ----------------------------------
items$FitQty <- 0
items$Itrs <- 0


# Run everything in functions.R and helpers.R before continuing -----------
source("R/helper_funs.R")
source("R/helper_objs.R")
source("R/pack_bin.R")


# Test run of pack_bin -------------------------------------------------
for (i in 1:nrow(items)){
  tempy <- pack_bin(as.vector(c(items$item_length[i], items$item_width[i], items$item_height[i])),
                     as.vector(c(items$bin_length[i], items$bin_width[i], items$bin_height[i])))
  items$FitQty[i] <- tempy[1]
  items$Itrs[i] <- tempy[2]
}
rm(i)
rm(tempy)

# View(items)
