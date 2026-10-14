#----------------------------------------------------------#
#
#         HerbAcum - BABE herbivory control trees
#
#               Katerina Sam  25Apr2025
#
#        Building of the phylogeny for the analyses
#
#----------------------------------------------------------#

install.packages("devtools", dependencies = TRUE)
library(devtools)
install_github("jinyizju/V.PhyloMaker2")

# Load the V.PhyloMaker2 package
library(V.PhyloMaker2)
ls("package:V.PhyloMaker2")
tree <- GBOTB.extended.LCVP
class(tree)

# Define the vector of plant taxa you are interested in
plant_taxa <- c("Acacia parramattensis", "Acer mono", "Acer palmatum", 
                "Acer platanoides", "Acer pseudoplatanus", 
                "Argyrodendron peralatum", "Baccaurea ramiflora", 
                "Betula maximowicziana", "Breynia oblingifolia", 
                "Bursaria spinosa", "Carpinus betulus", "Carpinus cordata", 
                "Celtis latifolia", "Chrysophyllum roxburghii", 
                "Cleidion brevipetiolatum", "Cleistanthus myrianthus", 
                "Cryptocarya sp.", "Diospyros kaki", 
                "Dysoxylum arborescens", 
                "Endiandra leptodendron", "Erythrospermum candidum", 
                "Eucalyptus pruinosa", "Eucalyptus tereticornis", 
                "Ficus erythrosperma", "Fraxinus excelsior", 
                "Fraxinus lanuginosa", "Garcinia cowa", 
                "Gymnacranthera paniculata", "Haplostichanthus ramiflorus", 
                "Magnolia kobus", "Myristica globosa", "Orophea laui", 
                "Pimelodendron amboinicum", "Pittosporopsis kerrii", 
                "Pometia pinnata", "Prunus ssiori", "Quercus robur", 
                "Rockinghamia angustifolia", "Saprosma ternata", 
                "Shorea wangtianshuea", "Syringa reticulata",
                "Syzygium graveolens", "Tilia cordata", "Ulmus glabra")

sp.list <- data.frame(
  species = plant_taxa,
  genus = sapply(strsplit(plant_taxa, " "), `[`, 1),
  stringsAsFactors = FALSE
)

tips <- tips.info.LCVP

sp.list$family <- tips$family[
  match(sp.list$genus, tips$genus)
]

# inspect problems
sp.list[is.na(sp.list$family), ]

sp.list$family[sp.list$genus == "Gymnacranthera"] <- "Myristicaceae"
sp.list$family[sp.list$genus == "Pometia"] <- "Sapindaceae"
sp.list$family[sp.list$genus == "Rockinghamia"] <- "Ericaceae"

# verify all is fixed
sp.list[is.na(sp.list$family), ]

result <- phylo.maker(
  sp.list = sp.list,
  tree = GBOTB.extended.LCVP,
  nodes = nodes.info.2.LCVP,
  scenarios = "S3"
)

phylo_tree <- result$scenario.3
plot(phylo_tree, cex = 0.6)

# Save the tree to a file (optional)
# Specify the file path and format (e.g., "tree.nwk" for Newick format)

write.tree(phylo_tree, file = "C:/Users/ksam/Documents/GitHub/BABE_Herb4MO_CON/phylogenetic_tree_.nwk")

library(ape)
phylo_tree <- read.tree("C:/Users/ksam/Documents/GitHub/BABE_Herb4MO_CON/phylogenetic_tree.nwk")
species_list <- phylo_tree$tip.label
species_list


# Draw icons to the phylogeny
# Load the table
# CURRENTLY I HAVE NO IDEA WHERE THIS FILE IS

species_sites <- read.csv("C:/Users/ksam/Documents/GitHub/BABE_Herb4MO_CON/data/input/species_sites.csv")
species_sites

# Match species names between the tree and the table
species_sites$PlantSpeciesAnal <- trimws(species_sites$PlantSpeciesAnal)
phylo_tree$tip.label <- trimws(phylo_tree$tip.label)

# Create a vector to map species to their sites
species_order <- match(phylo_tree$tip.label, species_sites$PlantSpeciesAnal)
ordered_sites <- species_sites$Site[species_order]  # Ensure the correct column name

length(species_sites$Site)
range(species_order, na.rm = TRUE)

names(species_sites)

# Assign symbols and colors for each site
site_symbols <- c(LAK = 17, TOM = 16, BUB = 15, KAK = 17, DRO = 16, EUC = 15)
site_colors  <- c(LAK =  "blue", TOM = "turquoise" , BUB =  "lightgoldenrod1" , KAK = "orange", DRO = "red" , EUC =  "purple")

# Plot the tree
plot(tree, 
     show.tip.label = TRUE, 
     edge.color = "black", 
     type = "phylogram", 
     cex = 0.6, 
     label.offset = 5)

# Add symbols corresponding to study sites behind species names
for (i in 1:length(tree$tip.label)) {
  site <- ordered_sites[i]
  if (!is.na(site)) {  # Check for any missing data
    tiplabels(pch = site_symbols[site], 
              col = site_colors[site], 
              adj = 1.2, 
              cex = 1,  # Reduce the symbol size (adjust as needed)
              frame = "n", 
              tip = i)
  }
}

# Add a legend to the plot
legend("topright", legend = names(site_colors), 
       pch = site_symbols, col = site_colors, 
       title = "Study Sites", 
       bty = "n",  # No box around the legend
       pt.cex = 1.5,  # Size of the points
       cex = 0.8)  # Text size


