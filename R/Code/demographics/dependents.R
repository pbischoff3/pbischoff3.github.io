# Pull in outside data ####
source("./R/Code/01-cleaning_data.R")

# MDS For Loop ####
x=1
dependents_plots <- list()
for(i in c("si","cm","im","sd","se","gm","sci_comp","per_comm_orient","sci_comm_orient")){
  dependents_plots[[x]] <- mysubsetMDS(i) %>% 
    ggplot(aes(x=MDS1,y=MDS2,color=factor(dependents)))+
    geom_point()+
    labs(title = i)+
    stat_ellipse()
  x <- x+1
}

# Science Identity ####
dependents_plots[[1]]
ggsave("./R/Images/dependents/si.png",dpi = 300,width = 8)

adonis(si ~ demo$dependents)

# Career Motivation ####
dependents_plots[[2]]
ggsave("./R/Images/dependents/cm.png",dpi = 300,width = 8)

adonis(cm ~ demo$dependents)

# Intrinsic Motivation ####
dependents_plots[[3]]
ggsave("./R/Images/dependents/im.png",dpi = 300,width = 8)

adonis(im ~ demo$dependents)

# Self-Determination ####
dependents_plots[[4]]
ggsave("./R/Images/dependents/sd.png",dpi = 300,width = 8)

adonis(sd ~ demo$dependents)

# Self Efficacy ####
dependents_plots[[5]]
ggsave("./R/Images/dependents/se.png",dpi = 300,width = 8)

adonis(se ~ demo$dependents)

# Grade Motivation ####
dependents_plots[[6]]
ggsave("./R/Images/dependents/gm.png",dpi = 300,width = 8)

adonis(gm ~ demo$dependents)

# Competency in Science ####
dependents_plots[[7]]
ggsave("./R/Images/dependents/sci_comp.png",dpi = 300,width = 8)

adonis(sci_comp ~ demo$dependents)

# Personal Communal Orientation ####
dependents_plots[[8]] +
  coord_cartesian(xlim = c(-5.25,-4.25))
ggsave("./R/Images/dependents/per_comm_orient.png",dpi = 300,width = 8)

adonis(per_comm_orient ~ demo$dependents)

# Science Communal Orientation ####
dependents_plots[[9]]
ggsave("./R/Images/dependents/sci_comm_orient1.png",dpi = 300,width = 8)

zerorows <- which(rowSums(sci_comm_orient) == 0)

temp <- sci_comm_orient[-zerorows, ]

dec <- decorana(temp, mk=2)
dec <- summary(dec)
dec %>% as.matrix()
x=dec$site.scores[,1]
y=dec$site.scores[,2]

demo_DCA <- demo[-zerorows,]

demo_DCA$DCA1 <- x
demo_DCA$DCA2 <- y

demo_DCA %>% ggplot(mapping= aes(x=DCA1, y=DCA2,color=dependents)) +
  geom_point()+
  stat_ellipse()

ggsave("./R/Images/dependents/sci_comm_orient2.png", dpi = 300, width = 8)

adonis(sci_comm_orient ~ demo$dependents)
