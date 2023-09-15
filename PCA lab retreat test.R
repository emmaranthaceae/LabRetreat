#lab retreat github test script

######################################################################
######                Frankenstein PCA Plots               	  	######
######################################################################

##################################################################################################
### Section 1: Information about the script -----
#PCA plotted different ways: boxplots along individual PCs, as points w/confidence 
#ellipses plotted on two PCs, separated by color for species, sepraated by color 
#for study sites. Code is very frankenstein-ed and not clean.

##################################################################################################


############ A. Script information      -----------------------

# Script Content:         This R script explores data from soils sampled across two sites
#                         for 12 species, as well as 3 soils representing greenhouse study soils
#                         from a follow-up study with the same species and makes PCA figures 
#                         for the soil samples.

#                         Data plotted is species-specific soil samples (soil individual species were growing on; 
#                         in single-species stands), greenhouse soils (three soils used for a follow-up study, 
#                         which all twelve species were grown on). One species ("LJJ") had extreme values

#                         Plots generated:
#                         1. PCA of all species' soil samples, excluding greenhouse study soils
#                         2. PCA of all species' soil samples excluding LJJ and gh soils
#                         3. PCA of all species soil samples, but plotted w/color and ellipses 
#                         according to study site soils were collected from


# Data required:         The data files:  R_master_clean_p.xlsx



# Authors of this script:  Emma Rose Fryer
# Main contact (and email): emmarfryer@gmail.com
# Date created:           March 2023
# Last changes:           September 2023


##################################################################################################
### Section 2: Package installation, functions and data import -----
##################################################################################################


############ Working directory ############

setwd("/Users/emmar/Nextcloud/Cal Poly MS/Thesis Project/R/Prelim data/ugh_again")


############ Packages ############

library(ggbiplot)
library(ggfortify)
library(ggpubr)
library(dplyr)
library(graphics)
library(ggplot2)
library(base)
library(grid)
library(Matrix)
library(plyr)
library(ggpubr)
library(spam)
library(utils)
library(tibble)
library(car)
library(viridis)
library(fields)
library(ggtext)
library(ggthemes)
library(performance)
library(readxl)
library(stats)


############ Functions ############

#import data 
soil_data <- read_excel("R_master_clean_p.xlsx", 
                        sheet = "FieldSoils", col_types = c("text",
                                                            "text", "text", "numeric", "numeric", 
                                                            "numeric", "numeric", "numeric", "numeric", 
                                                            "numeric", "numeric", "numeric", 
                                                            "numeric", "numeric", "numeric", 
                                                            "numeric", "numeric", "numeric", 
                                                            "numeric", "numeric", "numeric", "numeric",
                                                            "numeric", "numeric", "numeric", "numeric",
                                                            "numeric", "numeric", "text", "numeric", "numeric",
                                                            "numeric", "numeric", "numeric", "numeric", "numeric",
                                                            "numeric", "numeric", "numeric", "numeric", "numeric"))

soildf <- as.data.frame(soil_data, row.names = NULL)


####Run and plot PCA without the greenhouse soils' samples in PCA####

#remove data not wanted for PCA:
mydataa <-mutate(soildf, "Sample ID" = NULL, "Nameq" = NULL, "Cameq" = NULL,
                 "Mgmeq" = NULL, "orgmatterENR" = NULL, "SAR" = NULL, "ESP" = NULL, "orgmatterpercent" = NULL, "KpercentCEC" = NULL, "MgpercentCEC" = NULL, "CapercentCEC" = NULL,  "HpercentCEC" = NULL,
                 "NapercentCEC" = NULL,"soiltexture" = NULL, "percentsand" = NULL, "percentsilt" = NULL,
                 "sulfur" = NULL, "slope" = NULL, "aspect" = NULL, "eastness" = NULL, "northness" = NULL, 
                 "latitude" = NULL, "longitude" = NULL, "site_num" = NULL, "euc_ex" = NULL, "euc_s" = NULL, "euc_ns" = NULL)

#remove greenhouse study soils:
cantua<-mydataa[!(mydataa$origin=="EXSO" | mydataa$origin=="SODIC" | mydataa$origin=="NSOD"),]

#PCA, but with revision to keep only PC1-PC4 (based on biplot/data exploration in code below)
cantuapca <- prcomp(cantua[,3:14], scale=T)
summary(cantuapca)

###data/PCA exploration:

#rotation, center, etc.:
cantuapca$rotation
cantuapca$center
cantuapca$x

#compute standard deviation of each principal component
std_dev <- cantuapca$sdev

#compute variance
pr_var <- std_dev^2

#check variance of first 10 components
pr_var[1:4]

#proportion of variance explained
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:4]

#scree plot
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")

#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#contribution of variables to each PC:
var_coord_func <- function(loadings, comp.sdev){
  loadings*comp.sdev
}

loadings <- cantuapca$rotation 
loadings
#to save:
#write.csv(loadings,"loadingspooled_withoutgreenhouse.csv", row.names = TRUE)


#bind PC values to your dataset
mydat=cbind(cantua, cantuapca$x)
mydata <- mydat
head(mydata)
mydata <- as.data.frame(mydata)


#biplot for PC1, PC2 to get idea of data and PCA before creating "nice" figure:
biplot(cantuapca, scale=0, col=c("white","red"),xlab="PC1", ylab="PC2") + theme_minimal()



############ clean plots ############ 
##generate formatted text labels for each species for PCA plot:
BOl <- expression(paste(italic("Benitoa occidentalis")))
LJAl <- expression(paste(italic("Lepidium jaredii"), " ssp. ", italic("album")))
CAl <- expression(paste(italic("Caulanthus anceps")))
DHl <- expression(paste(italic("Deinandra halliana")))
EXl <- expression(paste(italic("Extriplex "), "\"succulenta\" sp. nov."))
MMl <- expression(paste(italic("Monolopia major")))
MRl <- expression(paste(italic("Madia radiata")))
PCl <- expression(paste(italic("Phacelia ciliata")))
LMl <- expression(paste(italic("Layia munzii")))
MSl <- expression(paste(italic("Monolopia stricta")))
LJJl <- expression(paste(italic("Lepidium jaredii"), " ssp. ", italic("jaredii")))
LCl <- expression(paste(italic("Leptosyne calliopsidea")))


#combine labels:
labspecies = c("BO" = BOl, "LJA" = LJAl,"CA" = CAl,"DH" = DHl,"EX" = EXl, "MM" = MMl, "MR" = MRl, "PC" = PCl, "LM" = LMl, 
               "MS" = MSl, "LJJ" = LJJl, "LC" = LCl)


#### PC boxplots (plot points along ONE PC only:
PC1plot <- ggplot(mydata, mapping = aes(x = PC1, y = origin)) + 
  geom_boxplot(outlier.shape = NA) + 
  labs(title = "PC1 coordinates, by species", y = "Species")+ 
  geom_point(position = position_dodge(0.75), size = 1) + theme_few() + 
  scale_y_discrete(labels = labspecies, limits=rev) 

PC1plot
#to save:
#ggsave("PC1plotfinal.png", width = 8, height = 4.5, dpi = 1200)


PC2plot <- ggplot(mydata, mapping = aes(x = PC2, y = origin)) + 
  geom_boxplot(outlier.shape = NA) + 
  labs(title = "PC2 coordinates, by species", y = "Species")+ 
  geom_point(position = position_dodge(0.75), size = 1) + theme_few() + 
  scale_y_discrete(labels = labspecies, limits=rev)

PC2plot

#to save:
#ggsave("PC2plotfinal.png", width = 8, height = 4.5, dpi = 1200)


#check assumptions:
pc1comps <- lm(PC1 ~ origin, data = mydata)
pc2comps <- lm(PC2 ~ origin, data = mydata)

leveneTest(pc1comps)
leveneTest(pc2comps)

res1 <-kruskal.test(PC1 ~ origin, data = mydata)
res1

res2 <-kruskal.test(PC2 ~ origin, data = mydata)
res2



############ PCA plots without greenhouse soils############ 

#PCA by species without greenhouse soils:

#labels (based on biplot PCA generaated above; need to manually write and 
#place w/in figure):
df <- tibble(plotlab = c("low Na, Na:K, P, CEC",
                         "high Mg, K, P <br>low Ca, Ca:Mg, COLE",
                         "high Ca, Ca:Mg, COLE <br>low Mg, Na:K, K, P",
                         "high Na, Na:K, P, CEC"
),
x = c(-5, 13, -5, 13),
y = c(6, 6, -6, -6),
hjust = c(0, 1, 0, 1),
vjust = c(1, 1, 0, 0)
)

#assign colors to species for plot; using color-blind friendly palette "viridis":
#https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
cols <- c("BO" ="#4B0055", "CA" = "#45256B", 
          "DH" = "#30437F", "EX" = "#005F8E", "LJA" = "#007896", "LJJ" = "#009097", "LC" = "#00A691", "LM" = "#00B983", 
          "MM" = "#3AC96D", "MR" = "#8BD650", "MS" = "#C8DF32", "PC" = "#FDE333")

ssptitle <- expression(paste("All Species"))

#species labels are left here in case you use only this plot's code and didn't 
#run these labels in code for the plot above.
BOl <- expression(paste(italic("Benitoa occidentalis")))
LJAl <- expression(paste(italic("Lepidium jaredii"), " ssp. ", italic("album")))
CAl <- expression(paste(italic("Caulanthus anceps")))
DHl <- expression(paste(italic("Deinandra halliana")))
EXl <- expression(paste(italic("Extriplex "), "\"succulenta\" sp. nov."))
MMl <- expression(paste(italic("Monolopia major")))
MRl <- expression(paste(italic("Madia radiata")))
PCl <- expression(paste(italic("Phacelia ciliata")))
LMl <- expression(paste(italic("Layia munzii")))
MSl <- expression(paste(italic("Monolopia stricta")))
LJJl <- expression(paste(italic("Lepidium jaredii"), " ssp. ", italic("jaredii")))
LCl <- expression(paste(italic("Leptosyne calliopsidea")))

#combine labels:
labelss = c("BO" = BOl, "LJA" = LJAl,"CA" = CAl,"DH" = DHl,"EX" = EXl, "MM" = MMl, "MR" = MRl, "PC" = PCl, "LM" = LMl, 
            "MS" = MSl, "LJJ" = LJJl, "LC" = LCl)
#assign color-blind friendly fill for confidence ellipses:
filll <- c("BO" ="#4B0055", "CA" = "#45256B", 
           "DH" = "#30437F", "EX" = "#005F8E", "LJA" = "#007896", "LJJ" = "#009097", "LC" = "#00A691", "LM" = "#00B983", 
           "MM" = "#3AC96D", "MR" = "#8BD650", "MS" = "#C8DF32", "PC" = "#FDE333")


allspeciesnoghPCA <- ggplot(mydata, mapping = aes(x = PC1, y = PC2)) + geom_point(mydata, mapping = aes(x = PC1, y = PC2, color = origin)) + 
  stat_ellipse(mydata, mapping = aes(x = PC1, y = PC2, fill = origin), geom = "polygon", 
               level = 0.95, alpha = 0.1) +
  scale_fill_manual(values = cols, name = "Soil sample sources", 
                    labels = labelss,
                    guide = guide_legend(override.aes = list(fill = filll))) +
  scale_color_manual(values = cols, name = "Soil sample sources", 
                     labels = labelss,
                     guide = guide_legend(override.aes = list(size = c(1.75, 1.75, 1.75, 1.75, 1.75, 1.75, 1.75, 1.75, 1.75, 1.75, 1.75, 1.75), 
                                                              shape = c(16,16,16,16,16,16,16,16,16,16,16,16)))) + 
  labs(title = ssptitle, subtitle = "Principal component analysis of field-collected soils") +
  theme_few() + 
  geom_richtext(df, mapping = aes(x, y, label = plotlab,  hjust = hjust, vjust = vjust), size =3.5,
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0, 4), "pt"))

allspeciesnoghPCA

############ same pca without LJJ soil samples because they're so extreme ############ 
spdatanoljj <- mydata[!(mydata$origin=="LJJ"),]


#labels
df <- tibble(plotlab = c("low Na, Na:K, P, CEC",
                         "high Mg, K, P <br>low Ca, Ca:Mg",
                         "high Ca, Ca:Mg <br>low Mg, Na:K, K, P",
                         "high Na, Na:K, P, CEC"
),
x = c(-5, 5, -5, 5),
y = c(6, 6, -6, -6),
hjust = c(0, 1, 0, 1),
vjust = c(1, 1, 0, 0)
)

colss <- c("BO" ="#4B0055", "CA" = "#45256B", 
           "DH" = "#30437F", "EX" = "#005F8E", "LJA" = "#007896", "LC" = "#00A691", "LM" = "#00B983", 
           "MM" = "#3AC96D", "MR" = "#8BD650", "MS" = "#C8DF32", "PC" = "#FDE333")


labelsss = c("BO" = BOl, "LJA" = LJAl,"CA" = CAl,"DH" = DHl,"EX" = EXl, "MM" = MMl, "MR" = MRl, "PC" = PCl, "LM" = LMl, 
             "MS" = MSl, "LC" = LCl)
fillls <- c("BO" ="#4B0055", "CA" = "#45256B", 
            "DH" = "#30437F", "EX" = "#005F8E", "LJA" = "#007896", "LC" = "#00A691", "LM" = "#00B983", 
            "MM" = "#3AC96D", "MR" = "#8BD650", "MS" = "#C8DF32", "PC" = "#FDE333")



allspeciesnoghnoljjsamePCA <- ggplot(spdatanoljj, mapping = aes(x = PC1, y = PC2)) + geom_point(spdatanoljj, mapping = aes(x = PC1, y = PC2, color = origin)) + 
  stat_ellipse(spdatanoljj, mapping = aes(x = PC1, y = PC2, fill = origin), geom = "polygon", 
               level = 0.95, alpha = 0.1) +
  scale_fill_manual(values = colss, name = "Soil sample sources", 
                    labels = labelsss,
                    guide = guide_legend(override.aes = list(fill = fillls))) +
  scale_color_manual(values = colss, name = "Soil sample sources", 
                     labels = labelsss,
                     guide = guide_legend(override.aes = list(size = c(1.75,1.75,1.75, 1.75, 1.75, 1.75, 1.75, 1.75, 1.75, 1.75, 1.75), 
                                                              shape = c(16,16,16,16,16,16,16,16,16,16,16)))) + 
  labs(title = ssptitle, subtitle = "Principal component analysis of field-collected soils") +
  theme_few() + 
  geom_richtext(df, mapping = aes(x, y, label = plotlab,  hjust = hjust, vjust = vjust), size =3.5,
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0, 4), "pt"))

allspeciesnoghnoljjsamePCA


############ PCA by site, again without greenhouse soils ############ 


#labels
df <- tibble(plotlab = c("low Na, Na:K, P, CEC",
                         "high Mg, K, P <br>low Ca, Ca:Mg, COLE",
                         "high Ca, Ca:Mg, COLE <br>low Mg, Na:K, K, P",
                         "high Na, Na:K, P, CEC"
),
x = c(-5, 13, -5, 13),
y = c(6, 6, -6, -6),
hjust = c(0, 1, 0, 1),
vjust = c(1, 1, 0, 0)
)

cols <- c("cantua" ="#4B0055", "carrizo" = "#005F8E")
ssptitle <- expression(paste("All soils by site"))


labelss = c("cantua" = "Cantua Creek", "carrizo" = "Carrizo Plain")
filll <- c("cantua" ="#4B0055", "carrizo" = "#45256B")

bothsitesPCA <- ggplot(mydata, mapping = aes(x = PC1, y = PC2)) + geom_point(mydata, mapping = aes(x = PC1, y = PC2, color = site)) + 
  stat_ellipse(mydata, mapping = aes(x = PC1, y = PC2, fill = site), geom = "polygon", 
               level = 0.95, alpha = 0.1) +
  scale_fill_manual(values = cols, name = "Soil sample sources", 
                    labels = labelss,
                    guide = guide_legend(override.aes = list(fill = filll))) +
  scale_color_manual(values = cols, name = "Soil sample sources", 
                     labels = labelss,
                     guide = guide_legend(override.aes = list(size = c(1.75, 1.75), 
                                                              shape = c(16,16)))) + 
  labs(title = ssptitle, subtitle = "Principal component analysis of field-collected soils") +
  theme_few() + 
  geom_richtext(df, mapping = aes(x, y, label = plotlab,  hjust = hjust, vjust = vjust), size =3.5,
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0, 4), "pt"))

bothsitesPCA










##################################################################################################
### Section 3: Data analysis 
##################################################################################################