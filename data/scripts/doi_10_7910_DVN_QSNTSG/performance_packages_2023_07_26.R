# library(devtools)
# devtools::install(build_vignettes = TRUE)
library(terra)
library(ape)
library(bench)

library(phyloraster)
library(phyloregion)
library(epm)

### Loading and preparing data to suit packages
{
  # let's load the distribution data for 104 tree frog species
  shp <- terra::vect(system.file("extdata", "shps_iucn_spps_rosauer.shp",
                                 package = "phyloraster"))

  # then we will transform into rasters with high and low spatial resolution
  x.high <- shp2rast(shp, sps.col = "BINOMIAL", ymask = TRUE, background = 0,
                     resolution = 0.05)
  x.low <- shp2rast(shp, sps.col = "BINOMIAL", ymask = TRUE, background = 0,
                    resolution = 0.1)
  res(x.high)
  res(x.low)

  # to run the tests with the PD functions, we will need to load a phylogenetic
  # tree for amphibians from
  # Jetz and Pyron 2018 (https://doi.org/10.1038/s41559-018-0515-5)

  library(ape)
  # tree <- ape::read.tree(system.file("extdata", "tree.nex", package="phyloraster"))
  tree <- read.nexus("../bench_mark_files/tree_consenso_10000_mcc.nexus")
}

# Data transformation to fit with phyloraster
{
    library(phyloraster)
    names(x.low) <- gsub(" ", "_", names(x.low)) # changing the name to match with phylogenetic tree
    data.l <- phylo.pres(x.low, tree, full_tree_metr=F) # reordering raster according to tree order and extract branch length
    inv.R.l <- inv.range(data.l$x, filename = paste0(tempfile(), "invr_l.tif"), overwrite=T)

    names(x.high) <- gsub(" ", "_", names(x.high)) # changing the name to match with phylogenetic tree
    data.h <- phylo.pres(x.high, tree, full_tree_metr=F) # reordering raster according to tree order  and extract branch length
    inv.R.h <- inv.range(data.h$x, filename = paste0(tempfile(), "invr_h.tif"), overwrite=T)
    inv.R.hm <- inv.range(data.h$x)

    # MB
    object.size(data.h$x)
    object.size(data.l$x)
    object.size(data.h$tree)
    object.size(data.h$edge.path)
    object.size(data.h$branch.length)
    object.size(data.h$n.descendants)
  }

# Data transformation to fit with EcoPhyloMapper
{
    library(epm)
    datepm.h <- createEPMgrid(data.h$x, resolution = 0.05) # epmGRid object
    data.h$tree <- as(data.h$tree, "phylo") # phylo4 to phylo
    datepm.h <- addPhylo(datepm.h, data.h$tree) # add phylogenetic tree

    datepm.l <- createEPMgrid(data.l$x, resolution = 0.1) # epmGRid object
    data.l$tree <- as(data.l$tree, "phylo") # phylo4 to phylo
    datepm.l <- addPhylo(datepm.l, data.l$tree) # add phylogenetic tree

    # MB
    object.size(datepm.h)
    object.size(datepm.l)
  }

# Data transformation to fit with phyloregion
{
  library(phyloregion)

  # folder to store the high resolution rasters (0.05°)
  path <- tempdir()
  dir.create(paste0(path, "/rasters_high_res"), recursive = T)
  files.h <- paste0(paste0(path, "/rasters_high_res/"), names(data.h$x), ".tif")
  terra::writeRaster(data.h$x, files.h, overwrite=T)

  # folder to store the low resolution rasters (0.1°)
  dir.create(paste0(path, "/rasters_low_res"), recursive = T)
  files.l <- paste0(paste0(path, "/rasters_low_res/"), names(data.l$x), ".tif")
  terra::writeRaster(data.l$x, files.l, overwrite=T)

  # loading the data as suggested in the phyloregion vignette
  gdir.h <- paste0(path, "/rasters_high_res") #"/media/gabriela/HDD/Documents/Doutorado_UESC/Artigo_phylogrid/comparacoes_30_01_2023/rasters_high_res"
  files.h <- file.path(gdir.h, dir(gdir.h)) # high resolution
  com.h <- raster2comm(files.h)

  # loading the data as suggested in the phyloregion vignette
  gdir.l <- paste0(path, "/rasters_low_res") #"/media/gabriela/HDD/Documents/Doutorado_UESC/Artigo_phylogrid/comparacoes_30_01_2023/rasters_low_res"
  files.l <- file.path(gdir.l, dir(gdir.l)) # low resolution
  com.l <- raster2comm(files.l)

  # MB
  object.size(com.h$comm_dat)
  object.size(com.l$comm_dat)
}
gc()

############## COMPARISONS - BENCHMARKS ##############
library(bench)

########## Weighted Endemism ##########

### Weighted endemism - data import
{
  # LOW resolution
  we.low.data <- bench::mark(
    phyloraster.we.low.data = {
      x.low <- shp2rast(shp, sps.col = "BINOMIAL", ymask = TRUE, background = 0,
                         resolution = 0.1) # calculating shp2rast function processing
      # rast.we(data.h$x) # here I used data.h$x instead of x.high to standardize the data used for all packages in the same test
      inv.R.l <- inv.range(data.l$x, filename = paste0(tempfile(), "invr_l.tif"), overwrite=T)

    },
    epm.we.low.data = {
      datepm.l <- createEPMgrid(x.low, resolution = 0.1)
      # epm::gridMetrics(datepm.h, metric = "weightedEndemism")
    },
    phyloregion.we.low.data = {
      gdir.l <- gdir.l #"/media/gabriela/HDD/Documents/Doutorado_UESC/Artigo_phylogrid/comparacoes_30_01_2023/rasters_low_res"
      files.l <- file.path(gdir.l, dir(gdir.l)) # high resolution
      com.l <- raster2comm(files.l)
      # phyloregion::weighted_endemism(com.h$comm_dat)
    },
    check = F
  )
  summary(we.low.data)
  saveRDS(we.low.data, "../bench_mark_files/bench.mark.we.low.data.RDS")
  rm(we.low.data)

  # HIGH resolution
  {
    we.high.data.phyloraster <- bench::mark(
      phyloraster.we.high.data = {
        x.high <- shp2rast(shp, sps.col = "BINOMIAL", ymask = TRUE, background = 0,
                           resolution = 0.05) # calculating shp2rast function processing
        # rast.we(data.h$x) # here I used data.h$x instead of x.high to standardize the data used for all packages in the same test
        inv.R.h <- inv.range(data.h$x, filename = paste0(tempfile(), "invr_h.tif"), overwrite=T)
      },
      check = F
    )
    summary(we.high.data.phyloraster)
    saveRDS(we.high.data.phyloraster, "../bench_mark_files/bench.mark.we.high.data.phyloraster.RDS")
    rm(we.high.data.phyloraster)

    we.high.data.epm <- bench::mark(
      epm.we.high.data = {
        datepm.h <- createEPMgrid(x.high, resolution = 0.05)
        # epm::gridMetrics(datepm.h, metric = "weightedEndemism")
      },
      check = F
    )
    summary(we.high.data.epm)
    saveRDS(we.high.data.epm, "../bench_mark_files/bench.mark.we.high.data.epm.RDS")
    rm(we.high.data.epm)

    we.high.data.phyloregion <- bench::mark(
      phyloregion.we.high.data = {
        gdir.h <- gdir.h# "/media/gabriela/HDD/Documents/Doutorado_UESC/Artigo_phylogrid/comparacoes_30_01_2023/rasters_high_res"
        files.h <- file.path(gdir.h, dir(gdir.h)) # high resolution
        com.h <- raster2comm(files.h)
        # phyloregion::weighted_endemism(com.h$comm_dat)
      },
      check = F
    )
    summary(we.high.data.phyloregion)
    saveRDS(we.high.data.phyloregion, "../bench_mark_files/bench.mark.we.high.data.phyloregion.RDS")
    rm(we.high.data.phyloregion)
  }
}
gc()

### Weighted endemism - metric calculation
{
  # LOW resolution
  we.low.metric <- bench::mark(
    phyloraster.we.low.metric = {
      rast.we(data.l$x, inv.R.l)
    },
    epm.we.low.metric = {
      epm::gridMetrics(datepm.l, metric = "weightedEndemism")
    },
    phyloregion.we.low.metric = {
      phyloregion::weighted_endemism(com.l$comm_dat)
    },
    check = F
  )
  summary(we.low.metric)
  saveRDS(we.low.metric, "../bench_mark_files/bench.mark.we.low.metric.RDS")
  rm(we.low.metric)

  # HIGH resolution
  we.high.metric.phyloraster <- bench::mark(
    phyloraster.we.high.metric = {
      rast.we(data.h$x, inv.R.h)
    },
    check = F
  )
  summary(we.high.metric.phyloraster)
  saveRDS(we.high.metric.phyloraster, "../bench_mark_files/bench.mark.we.high.metric.phyloraster.RDS")
  rm(we.high.metric.phyloraster)

  we.high.metric.epm <- bench::mark(
    epm.we.high.metric = {
      epm::gridMetrics(datepm.h, metric = "weightedEndemism")
    },
    check = F
  )
  summary(we.high.metric.epm)
  saveRDS(we.high.metric.epm, "../bench_mark_files/bench.mark.we.high.metric.epm.RDS")
  rm(we.high.metric.epm)

  we.high.metric.phyloregion <- bench::mark(
    phyloregion.we.high.metric = {
      phyloregion::weighted_endemism(com.h$comm_dat)
    },
    check = F
  )
  saveRDS(we.high.metric.phyloregion, "../bench_mark_files/bench.mark.we.high.metric.phyloregion.RDS")
  summary(we.high.metric.phyloregion)
  rm(we.high.metric.phyloregion)

}
gc()


###### RAM vs. DISK - inv.R
### Weighted endemism - metric calculation
# HIGH resolution
{
  we.high.metric.phyloraster.disk <- bench::mark(
    phyloraster.we.high.metric.no.ir = {
      rast.we(data.h$x)
    },

    phyloraster.we.high.metric.ram = {
      rast.we(data.h$x, inv.R.hm)
    },

    phyloraster.we.high.metric.disk = {
      rast.we(data.h$x, inv.R.h)
    },

    check = F
  )
  saveRDS(we.high.metric.phyloraster.disk, "../bench_mark_files/bench.mark.we.high.metric.phyloraster.disk.RDS")
  summary(we.high.metric.phyloraster.disk)
  rm(we.high.metric.phyloraster.disk)
}
gc()

########## Phylogenetic Diversity ##########

### Phylogenetic diversity - data import
{
  # LOW resolution
  shp$BINOMIAL <- gsub(" ", "_", shp$BINOMIAL) # changing the name to match with phylogenetic tree

  pd.low.data <- bench::mark(
    phyloraster.pd.low.data = {
      x.low <- shp2rast(shp, sps.col = "BINOMIAL", ymask = TRUE, background = 0,
                        resolution = 0.1) # calculating shp2rast function processing
      # rast.we(data.h$x) # here I used data.h$x instead of x.high to standardize the data used for all packages in the same test
      data.l <- phylo.pres(x.low, tree)
      # rast.pd(data.l$x, data.l$branch.length)
    },
    epm.pd.low.data = {
      datepm.l <- createEPMgrid(data.l$x, resolution = 0.1)
      data.l$tree <- as(data.l$tree, "phylo")
      datepm.l <- addPhylo(datepm.l, data.l$tree)
      # epm::gridMetrics(datepm.l, metric = "pd")
    },
    phyloregion.pd.low.data = {
      gdir.l <- gdir.l # "/media/gabriela/HDD/Documents/Doutorado_UESC/Artigo_phylogrid/comparacoes_30_01_2023/rasters_low_res"
      files.l <- file.path(gdir.l, dir(gdir.l)) # low resolution
      com.l <- raster2comm(files.l)
      # phyloregion::PD(com.l$comm_dat, data.l$subtree)
    },
    check = F
  )
  summary(pd.low.data)
  saveRDS(pd.low.data, "../bench_mark_files/bench.mark.pd.low.data.RDS")
  rm(pd.low.data)

  # HIGH resolution
  shp$BINOMIAL <- gsub(" ", "_", shp$BINOMIAL) # changing the name to match with phylogenetic tree

  pd.high.data.phyloraster <- bench::mark(
    phyloraster.pd.high.data = {
      x.high <- shp2rast(shp, sps.col = "BINOMIAL", ymask = TRUE, background = 0,
                        resolution = 0.1) # calculating shp2rast function processing
      data.h <- phylo.pres(x.high, tree)
      # rast.pd(data.h$x, data.h$branch.length)
    },
    check = F
  )
  summary(pd.high.data.phyloraster)
  saveRDS(pd.high.data.phyloraster, "../bench_mark_files/bench.mark.pd.high.data.phyloraster.RDS")
  rm(pd.high.data.phyloraster)

  pd.high.data.epm <- bench::mark(
    epm.pd.high.data = {
      datepm.h <- createEPMgrid(data.h$x, resolution = 0.05)
      data.h$tree <- as(data.h$tree, "phylo")
      datepm.h <- addPhylo(datepm.h, data.h$tree)
      # epm::gridMetrics(datepm.h, metric = "pd")
    },
    check = F
  )
  summary(pd.high.data.epm)
  saveRDS(pd.high.data.epm, "../bench_mark_files/bench.mark.pd.high.data.epm.RDS")
  rm(pd.high.data.epm)

  pd.high.data.phyloregion <- bench::mark(
    phyloregion.pd.high.data = {
      gdir.h <- gdir.h # "/media/gabriela/HDD/Documents/Doutorado_UESC/Artigo_phylogrid/comparacoes_30_01_2023/rasters_high_res"
      files.h <- file.path(gdir.h, dir(gdir.h)) # low resolution
      com.h <- raster2comm(files.h)
      # phyloregion::PD(com.h$comm_dat, data.h$subtree)
    },
    check = F
  )
  summary(pd.high.data.phyloregion)
  saveRDS(pd.high.data.phyloregion, "../bench_mark_files/bench.mark.pd.high.data.phyloregion.RDS")
  rm(pd.high.data.phyloregion)

}
gc()

### Phylogenetic diversity - metric calculation
{
  # LOW resolution
  pd.low.metric <- bench::mark(
    phyloraster.pd.low.metric = {
      rast.pd(data.l$x, edge.path = data.l$edge.path, branch.length = data.l$branch.length)
    },
    epm.pd.low.metric = {
      epm::gridMetrics(datepm.l, metric = "pd")
    },
    phyloregion.pd.low.metric = {
      phyloregion::PD(com.l$comm_dat, data.l$tree)
    },
    check = F
  )
  summary(pd.low.metric)
  saveRDS(pd.low.metric, "../bench_mark_files/bench.mark.pd.low.metric.RDS")
  rm(pd.low.metric)

  # HIGH resolution
  pd.high.metric.phyloraster <- bench::mark(
    phyloraster.pd.high.metric = {
      rast.pd(data.h$x, edge.path = data.h$edge.path, branch.length = data.h$branch.length)
    },
    check = F
  )
  summary(pd.high.metric.phyloraster)
  saveRDS(pd.high.metric.phyloraster, "../bench_mark_files/bench.mark.pd.high.metric.phyloraster.RDS")
  rm(pd.high.metric.phyloraster)
  # pd.high.metric.phyloraster <- readRDS("../bench_mark_files/bench.mark.pd.high.metric.phyloraster.RDS")
  # pd.high.metric.phyloraster

  pd.high.metric.epm <- bench::mark(
    epm.pd.high.metric = {
      epm::gridMetrics(datepm.h, metric = "pd")
    },
    check = F
  )
  summary(pd.high.metric.epm)
  saveRDS(pd.high.metric.epm, "../bench_mark_files/bench.mark.pd.high.metric.epm.RDS")
  rm(pd.high.metric.epm)

  pd.high.metric.phyloregion <- bench::mark(
    phyloregion.pd.high.metric = {
      phyloregion::PD(com.h$comm_dat, data.h$tree)
    },
    check = F
  )

  summary(pd.high.metric.phyloregion)
  saveRDS(pd.high.metric.phyloregion, "../bench_mark_files/bench.mark.pd.high.metric.phyloregion.RDS")
  rm(pd.high.metric.phyloregion)

}
gc()
