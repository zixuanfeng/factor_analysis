library("FactoMineR")
library("factoextra")
library(Factoshiny)
library(corrplot)

#######################################
#MCA
#######################################

SOB <- read.csv("~/Desktop/SOB.csv", 
                header=TRUE, na.strings=c("NA","NaN", "") )
data<-SOB
data<-na.omit(data)

#data summary
summary(data)

#apply MCA
res.mca<-MCA(data, ncp = 5, graph = TRUE)

#screeplot
fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 20))

#eigen values
eig.val <- get_eigenvalue(res.mca)

#MCA-Biplot
fviz_mca_biplot(res.mca, 
                repel = TRUE, # Avoid text overlapping (slow if many point)
                ggtheme = theme_minimal())

#corrleation plot
corrplot(res.mca$var$coord, is.corr=FALSE)


#f you want to highlight the correlation between variables 
#(active & supplementary) and dimensions, 
#use the function fviz_mca_var() with the argument choice = “mca.cor”:
fviz_mca_var(res.mca, choice = "mca.cor",
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())


#Bi-plot variable categories
fviz_mca_var(res.mca, 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())


#The function dimdesc() [in FactoMineR] can be used to 
#identify the most correlated variables with a given dimension:
res.desc <- dimdesc(res.mca, axes = c(1,2))
res.desc[[1]]




#######################################
#MFA
#######################################
SOB <- read.csv("~/Desktop/SOB.csv", 
                header=TRUE, na.strings=c("NA","NaN", "") )
data<-SOB
data<-na.omit(data)

#create model
res.mfa <- MFA(data, 
               group = c(6,4,6,2,1), 
               type = c("n",rep("n",4)),
               ncp=5,
               name.group = c("demo", "process",  
                              "social", "project", "SOB"),
               graph = FALSE)

print(res.mfa)

#screeplot
fviz_screeplot(res.mfa, addlabels = TRUE, ylim = c(0, 15))

#To plot the groups of variables, type this:
fviz_mfa_var(res.mfa, "group")

#contribution plot
fviz_contrib(res.mfa, "group", axes = 1)
fviz_contrib(res.mfa, "group", axes = 2)



fviz_mfa_var(res.mfa, "quali.var", palette = "jco", 
             col.var.sup = "violet", repel = TRUE,
             geom = c("point", "text"), legend = "bottom")



