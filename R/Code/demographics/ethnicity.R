# Pull in outside data ####
source("./R/Code/01-cleaning_data.R")

# MDS For Loop ####
x=1
ethnicity_plots <- list()
for(i in c("si","cm","im","sd","se","gm","sci_comp","per_comm_orient","sci_comm_orient")){
  set.seed(123)
  ethnicity_plots[[x]] <- mysubsetMDS(i) %>% 
    ggplot(aes(x=MDS1,y=MDS2,color=factor(ethnicity)))+
    geom_point()+
    labs(title = i)+
    stat_ellipse()
  x <- x+1
}

# Science Identity ####
ethnicity_plots[[1]]
ggsave("./R/Images/ethnicity/si.png",dpi = 300,width = 8)

set.seed(123)
adonis(si ~ demo$ethnicity)

# Career Motivation ####
ethnicity_plots[[2]]
ggsave("./R/Images/ethnicity/cm.png",dpi = 300,width = 8)

set.seed(123)
adonis(cm ~ demo$ethnicity)

# Intrinsic Motivation ####
ethnicity_plots[[3]]
ggsave("./R/Images/ethnicity/im.png",dpi = 300,width = 8)

set.seed(123)
adonis(im ~ demo$ethnicity)

# Self-Determination ####
ethnicity_plots[[4]]
ggsave("./R/Images/ethnicity/sd.png",dpi = 300,width = 8)

set.seed(123)
adonis(sd ~ demo$ethnicity)

# Self Efficacy ####
ethnicity_plots[[5]]
ggsave("./R/Images/ethnicity/se.png",dpi = 300,width = 8)

set.seed(123)
adonis(se ~ demo$ethnicity)

# Grade Motivation ####
ethnicity_plots[[6]]
ggsave("./R/Images/ethnicity/gm.png",dpi = 300,width = 8)

set.seed(123)
adonis(gm ~ demo$ethnicity)

# Competency in Science ####
ethnicity_plots[[7]]
ggsave("./R/Images/ethnicity/sci_comp.png",dpi = 300,width = 8)

set.seed(123)
adonis(sci_comp ~ demo$ethnicity)

# Personal Communal Orientation ####
ethnicity_plots[[8]] +
  coord_cartesian(xlim = c(-20,20))
ggsave("./R/Images/ethnicity/per_comm_orient.png",dpi = 300,width = 8)

set.seed(123)
adonis(per_comm_orient ~ demo$ethnicity)

# Science Communal Orientation ####
ethnicity_plots[[9]]
ggsave("./R/Images/ethnicity/sci_comm_orient1.png",dpi = 300,width = 8)

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

demo_DCA %>% ggplot(mapping= aes(x=DCA1, y=DCA2,color=ethnicity)) +
  geom_point()+
  stat_ellipse()

ggsave("./R/Images/ethnicity/sci_comm_orient2.png", dpi = 300, width = 8)

set.seed(123)
adonis(sci_comm_orient ~ demo$ethnicity)
