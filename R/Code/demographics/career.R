# Pull in outside data ####
source("./R/Code/01-cleaning_data.R")

# MDS For Loop ####
x=1
career_plots <- list()
for(i in c("si","cm","im","sd","se","gm","sci_comp","per_comm_orient","sci_comm_orient")){
  career_plots[[x]] <- mysubsetMDS(i) %>% 
    ggplot(aes(x=MDS1,y=MDS2,color=factor(career)))+
    geom_point()+
    labs(title = i)+
    stat_ellipse()
  x <- x+1
}

# Science Identity ####
career_plots[[1]]
ggsave("./R/Images/career/si.png",dpi = 300,width = 8)

adonis(si ~ demo$career)

# Career Motivation ####
career_plots[[2]]
ggsave("./R/Images/career/cm.png",dpi = 300,width = 8)

adonis(cm ~ demo$career)

# Intrinsic Motivation ####
career_plots[[3]]
ggsave("./R/Images/career/im.png",dpi = 300,width = 8)

adonis(im ~ demo$career)

# Self-Determination ####
career_plots[[4]]
ggsave("./R/Images/career/sd.png",dpi = 300,width = 8)

adonis(sd ~ demo$career)

# Self Efficacy ####
career_plots[[5]]
ggsave("./R/Images/career/se.png",dpi = 300,width = 8)

adonis(se ~ demo$career)

# Grade Motivation ####
career_plots[[6]]
ggsave("./R/Images/career/gm.png",dpi = 300,width = 8)

adonis(gm ~ demo$career)

# Competency in Science ####
career_plots[[7]]
ggsave("./R/Images/career/sci_comp.png",dpi = 300,width = 8)

adonis(sci_comp ~ demo$career)

# Personal Communal Orientation ####
career_plots[[8]] +
  coord_cartesian(xlim = c(-11,-2))
ggsave("./R/Images/career/per_comm_orient.png",dpi = 300,width = 8)

adonis(per_comm_orient ~ demo$career)

# Science Communal Orientation ####
career_plots[[9]]
ggsave("./R/Images/career/sci_comm_orient1.png",dpi = 300,width = 8)

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

demo_DCA %>% ggplot(mapping= aes(x=DCA1, y=DCA2,color=career)) +
  geom_point()+
  stat_ellipse()

ggsave("./R/Images/career/sci_comm_orient2.png", dpi = 300, width = 8)

adonis(sci_comm_orient ~ demo$career)
