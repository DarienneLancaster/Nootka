## Map Rockfish species vs depth boxplot 
rockfishonly <- ROV %>% filter(Genus == "Sebastes")

get_common_name <- function(full_name) {
  common_names <- c("Copper Rockfish", "Puget Sound Rockfish", "Widow Rockfish", 
                    "Yellowtail Rockfish", "Quillback Rockfish", "Black Rockfish", 
                    "Vermilion Rockfish", "Canary Rockfish", "Unknown Rockfish")
  latin_names <- c("Scorpaenidae Sebastes caurinus", "Scorpaenidae Sebastes emphaeus",
                   "Scorpaenidae Sebastes entomelas", "Scorpaenidae Sebastes flavidus",
                   "Scorpaenidae Sebastes maliger", "Scorpaenidae Sebastes melanops",
                   "Scorpaenidae Sebastes miniatus", "Scorpaenidae Sebastes pinniger",
                   "Scorpaenidae Sebastes unknown")
  
  if (full_name %in% latin_names) {
    return(common_names[which(latin_names == full_name)])
  } else {
    return("Unknown Rockfish")
  }
}

RockFish <- mutate(rockfishonly, CommonName = sapply(rockfishonly$FullName, get_common_name))
RockFish <- RockFish %>% select("Site_ID", "CommonName", "Depth")
RockFish$DepthM <- RockFish$Depth * 0.3048

ggplot(RockFish, aes(x = CommonName, y = DepthM)) +
  geom_boxplot() +
  labs(x = "", y = "Depth (m)") +
  theme_minimal()

# Perform pairwise comparisons using Wilcoxon rank-sum test with BH adjustment
pairwise_wilcox <- pairwise.wilcox.test(RockFish$DepthM, RockFish$CommonName, p.adjust.method = "BH")

# Print the result of pairwise comparisons

RockFish$DepthMNeg <- RockFish$DepthM*-1
ggplot(RockFish, aes(x = CommonName, y = DepthMNeg)) +
 # geom_violin(trim = FALSE, fill = "#69b3a2", color = "black") +
  geom_boxplot(width = 0.2,  color = "black", outlier.shape = NA) +
  #coord_flip() +  # Flip coordinates to have violin plot on the left
  theme_minimal() +
  labs(title = "Depth Distribution by Species", x = "Species", y = "Depth (m)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

