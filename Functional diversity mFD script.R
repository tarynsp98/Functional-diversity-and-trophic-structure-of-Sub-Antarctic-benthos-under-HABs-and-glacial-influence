#Functional diversity of benthic communities in Sub-Antarctic fjords using fuzzy coded trait matrix

install.packages("mFD")
library(mFD)
Data = Data_Ms

sp_trt = Data_Ms
sp_trt <- sp_trt[ , -1,]
sp_trt = sp_trt[ ,-34]
row.names(sp_trt) <- Data_ms[[1]] 
trait_cat = Data_ms #change sheet from big dataset


#Trait cleaning if model indicate redundant or non-informative traits in data
sd_cols <- sapply(sp_trt, function(x) sd(x, na.rm = TRUE))
sp_trt <- sp_trt[, sd_cols > 0, drop = FALSE]
trait_cat <- subset(trait_cat, trait_name %in% colnames(sp_trt))


nz_cols <- sapply(sp_trt, function(x) sum(x > 0, na.rm = TRUE))
sp_trt <- sp_trt[, nz_cols > 1, drop = FALSE]
trait_cat <- subset(trait_cat, trait_name %in% colnames(sp_trt))

# Species traits summary:
traits_summ <- mFD::sp.tr.summary(
  tr_cat     = trait_cat,   
  sp_tr      = sp_trt, 
  stop_if_NA = TRUE)

#Species distance matrix
sp_dist_benthos <- mFD::funct.dist(
  sp_tr         = sp_trt,
  tr_cat        = trait_cat,
  metric        = "gower",
  scale_euclid  = "noscale",
  ordinal_var   = "classic",
  weight_type   = "equal",
  stop_if_NA    = TRUE)

sp_dist_benthos

#transform to matrix
summary(as.matrix(sp_dist_benthos))

#save as excel file or csv
m <- as.matrix(sp_dist_benthos)
write.csv(m, "distanciasoficial.csv")

#estimate functional space quality to define dimensions to compute indices
quality_fspaces_benthos <- mFD::quality.fspaces(
  sp_dist             = sp_dist_benthos,
  fdendro             = "average",
  maxdim_pcoa         = 9,
  deviation_weighting = c("absolute", "squared"),
  fdist_scaling       = c(TRUE, FALSE))

quality_fspaces_benthos$"quality_fspaces"

#plot the quality of functional spaces and visualize better fit in MAD

mFD::quality.fspaces.plot(
  fspaces_quality = quality_fspaces_fruits, 
  quality_metric  = "mad",
  fspaces_plot    = c("tree_average", "pcoa_4d", "pcoa_5d", "pcoa_6d", 'pcoa_7d'))


fspaces_quality_fruits <- mFD::quality.fspaces(
  sp_dist             = sp_dist_fruits,
  maxdim_pcoa         = 10,
  deviation_weighting = "absolute",
  fdist_scaling       = FALSE,
  fdendro             = "average")

round(fspaces_quality_fruits$"quality_fspaces", 3)            # Quality metrics of spaces

mFD::quality.fspaces.plot(
  fspaces_quality            = quality_fspaces_fruits,
  quality_metric             = "mad",
  fspaces_plot               = c("tree_average", "pcoa_2d", "pcoa_3d", 
                                 "pcoa_4d", "pcoa_5d", "pcoa_6d"),
  name_file                  = NULL,
  range_dist                 = NULL,
  range_dev                  = NULL,
  range_qdev                 = NULL,
  gradient_deviation         = c(neg = "darkblue", nul = "grey80", pos = "darkred"),
  gradient_deviation_quality = c(low = "yellow", high = "red"),
  x_lab                      = "Trait-based distance")


#get species coordinates in the functional space
sp_faxes_coord_benthos <- quality_fspaces_benthos$"details_fspaces"$"sp_pc_coord"
sp_faxes_coord_benthos

range_sp_coord  <- range(sp_faxes_coord_benthos)

#get ranges
range_faxes <- range_sp_coord +
  c(-1, 1) * (range_sp_coord[2] - range_sp_coord[1]) * 0.05

range_faxes

# get species coordinates along the two studied axes:
sp_faxes_coord_xy <- sp_faxes_coord_benthos[, c("PC4", "PC5")]

sp_faxes_coord_xy

#define your weighted matrix (presence/absence-abundance-etc)
abund = #your data
  abund <- abund[ , -1,] #delete species name column
row.names(abund) <- your data[[1]] #incorporate species names as rownames

#Create a subset for each sampling station

# Puerto Fontaine
sp_pres1 <- colnames(abund)[abund["Fontaine", ] > 0]

# Create object for mFD::vertices
sp_filter_fontaine <- list(
  `species names`      = sp_pres1,
  `species coordinates` = sp_faxes_coord_xy[sp_pres1, , drop = FALSE],
  `species weight`      = as.data.frame(rep(1, length(sp_pres1))),  # peso igual para todas
  `species relative weight` = as.data.frame(rep(1/length(sp_pres1), length(sp_pres1)))
)

#Seno Ballena
sp_pres2<- colnames(abund)[abund["Ballena", ] > 0]
sp_filter_ballena <- list(
  `species names`      = sp_pres2,
  `species coordinates` = sp_faxes_coord_xy[sp_pres2, , drop = FALSE],
  `species weight`      = as.data.frame(rep(1, length(sp_pres2))),  # peso igual para todas
  `species relative weight` = as.data.frame(rep(1/length(sp_pres2), length(sp_pres2)))
)

#Bahía Parry
sp_pres3<- colnames(abund)[abund["Parry", ] > 0]
sp_filter_parry <- list(
  `species names`      = sp_pres3,
  `species coordinates` = sp_faxes_coord_xy[sp_pres3, , drop = FALSE],
  `species weight`      = as.data.frame(rep(1, length(sp_pres3))),  # peso igual para todas
  `species relative weight` = as.data.frame(rep(1/length(sp_pres3), length(sp_pres3)))
)

#Isla Piazzi
sp_pres4<- colnames(abund)[abund["Piazzi", ] > 0]
sp_filter_piazzi <- list(
  `species names`      = sp_pres4,
  `species coordinates` = sp_faxes_coord_xy[sp_pres4, , drop = FALSE],
  `species weight`      = as.data.frame(rep(1, length(sp_pres4))),  # peso igual para todas
  `species relative weight` = as.data.frame(rep(1/length(sp_pres4), length(sp_pres4)))
)

#get species coordinates (Puerto Fontaine):
sp_faxes_coord_fontaine <- sp_filter_fontaine$`species coordinates`

sp_faxes_coord_fontaine

benthos_asb_sp_coord2D_b1 <- sp_faxes_coord_fontaine[, c("PC4", "PC5")]


# get species coordinates (Seno Ballena):
sp_faxes_coord_ballena <- sp_filter_ballena$`species coordinates`

sp_faxes_coord_ballena

benthos_asb_sp_coord2D_b2 <- sp_faxes_coord_ballena[, c("PC4", "PC5")]

# get species coordinates (Bahía Parry):
sp_faxes_coord_parry <- sp_filter_parry$`species coordinates`

sp_faxes_coord_parry

benthos_asb_sp_coord2D_b3 <- sp_faxes_coord_parry[, c("PC4", "PC5")]

# get species coordinates (Isla iazzi):
sp_faxes_coord_piazzi <- sp_filter_piazzi$`species coordinates`

sp_faxes_coord_piazzi

benthos_asb_sp_coord2D_b4 <- sp_faxes_coord_piazzi[, c("PC4", "PC5")]

#get species that are vertices in the functional space for each location
vert_nm_fontaine <- mFD::vertices(sp_faxes_coord = sp_faxes_coord_fontaine,
                                  order_2D = TRUE, 
                                  check_input = TRUE)
vert_nm_fontaine
# Seno Ballena
vert_nm_ballena <- mFD::vertices(sp_faxes_coord = sp_faxes_coord_ballena,
                                 order_2D = TRUE, 
                                 check_input = TRUE)
vert_nm_ballena
# Bahía Parry
vert_nm_parry <- mFD::vertices(sp_faxes_coord = sp_faxes_coord_parry,
                               order_2D = TRUE, 
                               check_input = TRUE)

vert_nm_parry 

# Isla Piazzi
vert_nm_piazzi <- mFD::vertices(sp_faxes_coord = sp_faxes_coord_piazzi,
                                order_2D = TRUE, 
                                check_input = TRUE)

vert_nm_piazzi

#list of coordinates in the best fit PCs
asb_sp_coord2D <- list(
  Fontaine = sp_faxes_coord_fontaine[, c("PC4", "PC5")],
  Ballena   = sp_faxes_coord_ballena[, c("PC4", "PC5")],
  Parry     = sp_faxes_coord_parry[, c("PC4", "PC5")],
  Piazzi    = sp_faxes_coord_piazzi[, c("PC4", "PC5")]
)

#  Convex hull vertices in N dimensions
vertices_nD <- list(
  Fontaine = vert_nm_fontaine,
  Ballena   = vert_nm_ballena,
  Parry     = vert_nm_parry,
  Piazzi = vert_nm_piazzi
)

#For plotting
assemblages <- c("fontaine","ballena","parry","piazzi")

## color palette (example Okabe-Ito)
cols <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442") #Fontaine, Ballena, Parry, Piazzi

## poligon
fill_ch  <- setNames(cols, assemblages)
color_ch <- setNames(rep("black",4), assemblages)
alpha_ch <- setNames(rep(0.3,4), assemblages)

## species
shape_sp <- setNames(rep(21,4), assemblages)
size_sp  <- setNames(rep(1,4), assemblages)
color_sp <- setNames(rep("grey70",4), assemblages)
fill_sp  <- setNames(rep("grey70",4), assemblages)

## vertices
shape_vert <- setNames(c(21,22,24,25), assemblages)
size_vert  <- setNames(rep(3,4), assemblages)
color_vert <- setNames(rep("black",4), assemblages)
fill_vert  <- setNames(cols, assemblages)

#FRic plot to visualize functional space occupation by assemblages
plot_try <- mFD::fric.plot(
  ggplot_bg = plot_k,
  asb_sp_coord2D = list(
    "fontaine" = sp_faxes_coord_fontaine,
    "ballena" = sp_faxes_coord_ballena,
    "parry" = sp_faxes_coord_parry,
    "piazzi" = sp_faxes_coord_piazzi
  ),
  asb_vertices_nD = list(
    "fontaine" = vert_nm_fontaine,
    "ballena" = vert_nm_ballena,
    "parry" = vert_nm_parry,
    "piazzi" = vert_nm_piazzi
  ),
  
  fill_ch  = fill_ch,
  color_ch = color_ch,
  alpha_ch = alpha_ch,
  
  plot_sp  = TRUE,
  shape_sp = shape_sp,
  size_sp  = size_sp,
  color_sp = color_sp,
  fill_sp  = fill_sp,
  
  shape_vert = shape_vert,
  size_vert  = size_vert,
  color_vert = color_vert,
  fill_vert  = fill_vert
)

plot_try


#get alpha-functional indices

abund_mat <- as.matrix(abund)
# Use alpha.fd.multidim() function to get inputs to plot FDiv:
alpha_fd_indices_fruits <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_benthos[, c("PC1", "PC2", "PC3", "PC4", "PC5")],
  asb_sp_w         = abund_mat,
  ind_vect         = c("fdis", "fmpd", "fnnd", "feve", "fric", "fdiv", "fori", 
                       "fspe", "fide"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)

fd_ind_values_benthos <- alpha_fd_indices_benthos$"functional_diversity_indices"
fd_ind_values_benthos
#save as excel file
a = as.matrix(fd_ind_values_benthos)
write.csv(a, "indices.csv")

#get functionally unique and specialized species 

FUSE_res<-mFD::fuse(
  sp_dist=sp_dist_benthos, 
  sp_faxes_coord=sp_faxes_coord_benthos, 
  nb_NN=5, 
  GE=yourdata$IUCN_num, 
  standGE=TRUE)

FUSE_res
#save as excel file
w = as.matrix(FUSE_res)
write.csv(w, "Fuse.csv")


