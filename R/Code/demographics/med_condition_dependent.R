# Pull in outside data ####
source("./R/Code/01-cleaning_data.R")

# MDS For Loop ####
x=1
med_condition_dependent_plots <- list()
for(i in c("si","cm","im","sd","se","gm","sci_comp","per_comm_orient","sci_comm_orient")){
  med_condition_dependent_plots[[x]] <- mysubsetMDS(i) %>% 
    ggplot(aes(x=MDS1,y=MDS2,color=factor(med_condition_dependent)))+
    geom_point()+
    labs(title = i)+
    stat_ellipse()
  x <- x+1
}

# Science Identity ####
med_condition_dependent_plots[[1]]
ggsave("./R/Images/med_condition_dependent/si.png",dpi = 300,width = 8)

adonis(si ~ demo$med_condition_dependent)

# Career Motivation ####
med_condition_dependent_plots[[2]]
ggsave("./R/Images/med_condition_dependent/cm.png",dpi = 300,width = 8)

adonis(cm ~ demo$med_condition_dependent)

# Intrinsic Motivation ####
med_condition_dependent_plots[[3]]
ggsave("./R/Images/med_condition_dependent/im.png",dpi = 300,width = 8)

adonis(im ~ demo$med_condition_dependent)

# Self-Determination ####
med_condition_dependent_plots[[4]]
ggsave("./R/Images/med_condition_dependent/sd.png",dpi = 300,width = 8)

adonis(sd ~ demo$med_condition_dependent)

# Self Efficacy ####
med_condition_dependent_plots[[5]]
ggsave("./R/Images/med_condition_dependent/se.png",dpi = 300,width = 8)

adonis(se ~ demo$med_condition_dependent)

# Grade Motivation ####
med_condition_dependent_plots[[6]]
ggsave("./R/Images/med_condition_dependent/gm.png",dpi = 300,width = 8)

adonis(gm ~ demo$med_condition_dependent)

# Competency in Science ####
med_condition_dependent_plots[[7]]
ggsave("./R/Images/med_condition_dependent/sci_comp.png",dpi = 300,width = 8)

adonis(sci_comp ~ demo$med_condition_dependent)

# Personal Communal Orientation ####
med_condition_dependent_plots[[8]] +
  coord_cartesian(xlim = c(-6,-5))
ggsave("./R/Images/med_condition_dependent/per_comm_orient.png",dpi = 300,width = 8)

adonis(per_comm_orient ~ demo$med_condition_dependent)

# Science Communal Orientation ####
med_condition_dependent_plots[[9]]
ggsave("./R/Images/med_condition_dependent/sci_comm_orient1.png",dpi = 300,width = 8)

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

demo_DCA %>% ggplot(mapping= aes(x=DCA1, y=DCA2,color=med_condition_dependent)) +
  geom_point()+
  stat_ellipse()

ggsave("./R/Images/med_condition_dependent/sci_comm_orient2.png", dpi = 300, width = 8)

adonis(sci_comm_orient ~ demo$med_condition_dependent)
