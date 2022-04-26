# Pull in outside data ####
source("./R/Code/01-cleaning_data.R")

# MDS For Loop ####
x=1
med_condition_plots <- list()
for(i in c("si","cm","im","sd","se","gm","sci_comp","per_comm_orient","sci_comm_orient")){
  med_condition_plots[[x]] <- mysubsetMDS(i) %>% 
    ggplot(aes(x=MDS1,y=MDS2,color=factor(med_condition)))+
    geom_point()+
    labs(title = i)+
    stat_ellipse()
  x <- x+1
}

# Science Identity ####
med_condition_plots[[1]]
ggsave("./R/Images/med_condition/si.png",dpi = 300,width = 8)

adonis(si ~ demo$med_condition)

# Career Motivation ####
med_condition_plots[[2]]
ggsave("./R/Images/med_condition/cm.png",dpi = 300,width = 8)

adonis(cm ~ demo$med_condition)

# Intrinsic Motivation ####
med_condition_plots[[3]]
ggsave("./R/Images/med_condition/im.png",dpi = 300,width = 8)

adonis(im ~ demo$med_condition)

# Self-Determination ####
med_condition_plots[[4]]
ggsave("./R/Images/med_condition/sd.png",dpi = 300,width = 8)

adonis(sd ~ demo$med_condition)

# Self Efficacy ####
med_condition_plots[[5]]
ggsave("./R/Images/med_condition/se.png",dpi = 300,width = 8)

adonis(se ~ demo$med_condition)

# Grade Motivation ####
med_condition_plots[[6]]
ggsave("./R/Images/med_condition/gm.png",dpi = 300,width = 8)

adonis(gm ~ demo$med_condition)

# Competency in Science ####
med_condition_plots[[7]]
ggsave("./R/Images/med_condition/sci_comp.png",dpi = 300,width = 8)

adonis(sci_comp ~ demo$med_condition)

# Personal Communal Orientation ####
med_condition_plots[[8]] +
  coord_cartesian(xlim = c(-7.5,-6.5))
ggsave("./R/Images/med_condition/per_comm_orient.png",dpi = 300,width = 8)

adonis(per_comm_orient ~ demo$med_condition)

# Science Communal Orientation ####
med_condition_plots[[9]]
ggsave("./R/Images/med_condition/sci_comm_orient1.png",dpi = 300,width = 8)

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

demo_DCA %>% ggplot(mapping= aes(x=DCA1, y=DCA2,color=med_condition)) +
  geom_point()+
  stat_ellipse()

ggsave("./R/Images/med_condition/sci_comm_orient2.png", dpi = 300, width = 8)

adonis(sci_comm_orient ~ demo$med_condition)
