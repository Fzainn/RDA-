install.packages("languageR")
install.packages("packfor", repos = "http://R-Forge.R-project.org")
library(packfor)
library(vegan)
library(readxl)
library(languageR)


# apico <- read_excel("metadata_marium_apicomplexa_14-11-2022.xlsx")
# abund <- apico[,-c(1,14,15,16,17,18,19)]
# 
# env <- apico[,-c(2:13)]
# 
# abund_hellinger <- decostand(abund, method = "hellinger")
# 
# languageR::pairscor.fnc(env)

data(dune)
data(dune.env)

abund_hellinger <- decostand(dune, method = "hellinger")

#pairwise coorelation:  image with the pairwise correlation between phenotypes 
#and provides the corresponding source matrix
languageR::pairscor.fnc(dune.env)


#create the model
my_rda <- rda(abund_hellinger ~ A1 + Moisture + Management + Use , data = dune.env)

#Multicollinearity can be verified using the vif.cca() function
#it undermines the statistical significance of an independent variable
sqrt(vif.cca(my_rda))


# Predicting transformed species using all variables
# contained in env
# to make the syntax easier, you can use the formulation
# y ~ ., data = x
# which means: y as a function of all variables in x

my_rda <- rda(abund_hellinger ~ ., data = dune.env)


# First part -> contribution to explained variance
# Second part -> contribution to total variance
summary(my_rda)







#triplot
plot(my_rda)

#use RsquareAdj() function for adjusting vectors ,as they are biased
RsquareAdj(my_rda)


#permutation test: we have to test 3 things
#global RDA significance, Axis significance, terms(explanatory vars) significance


#1st one is global: We use the function anova.cca()
# Argument permutations gives the minimal number of permutations requested to assess 
# if the F value of a test is obviously significant or not
anova.cca(my_rda, permutations = 999)

anova.cca(my_rda, by = "axis")  # Test which axis are significant
anova.cca(my_rda, by = "terms")  # Test which terms are significant




#if it seems in our data there are some vars are redundantm, performe global model R2 to reduce the the no.of explanatory vars

# Global model with all environmental variables
my_rda <- rda(abund_hellinger ~ ., data = dune.env)

# global R2
global_r2 <- RsquareAdj(my_rda)$adj.r.squared

global_r2


# Forward selection
# fs <- forward.sel(abund_hellinger, # Y matrix
#                   dune.env, # X matrix
#                   adjR2thresh = global_r2, # Set the adj.R2 threshold
#                   alpha = 0.001, # Set alpha level
#                   nperm = 999  # Number of permutations
# )   

my_rda$CA$eig[my_rda$CA$eig > mean(my_rda$CA$eig)]













