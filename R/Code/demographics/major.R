# Pull in outside data ####
source("./R/Code/01-cleaning_data.R")

# MDS For Loop ####
x=1
major_plots <- list()
for(i in c("si","cm","im","sd","se","gm","sci_comp","per_comm_orient","sci_comm_orient")){
  major_plots[[x]] <- mysubsetMDS(i) %>% 
    ggplot(aes(x=MDS1,y=MDS2,color=factor(major)))+
    geom_point()+
    labs(title = i)+
    stat_ellipse()
  x <- x+1
}

# Science Identity ####
major_plots[[1]]
ggsave("./R/Images/major/si.png",dpi = 300,width = 8)

adonis(si ~ demo$major)

# Career Motivation ####
major_plots[[2]]
ggsave("./R/Images/major/cm.png",dpi = 300,width = 8)

adonis(cm ~ demo$major)

# Intrinsic Motivation ####
major_plots[[3]]
ggsave("./R/Images/major/im.png",dpi = 300,width = 8)

adonis(im ~ demo$major)

# Self-Determination ####
major_plots[[4]]
ggsave("./R/Images/major/sd.png",dpi = 300,width = 8)

adonis(sd ~ demo$major)

# Self Efficacy ####
major_plots[[5]]
ggsave("./R/Images/major/se.png",dpi = 300,width = 8)

adonis(se ~ demo$major)

# Grade Motivation ####
major_plots[[6]]
ggsave("./R/Images/major/gm.png",dpi = 300,width = 8)

adonis(gm ~ demo$major)

# Competency in Science ####
major_plots[[7]]
ggsave("./R/Images/major/sci_comp.png",dpi = 300,width = 8)

adonis(sci_comp ~ demo$major)

# Personal Communal Orientation ####
major_plots[[8]] +
  coord_cartesian(xlim = c(-5.1,-4))
ggsave("./R/Images/major/per_comm_orient.png",dpi = 300,width = 8)

adonis(per_comm_orient ~ demo$major)

# Science Communal Orientation ####
major_plots[[9]]
ggsave("./R/Images/major/sci_comm_orient1.png",dpi = 300,width = 8)

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

demo_DCA %>% ggplot(mapping= aes(x=DCA1, y=DCA2,color=major)) +
  geom_point()+
  stat_ellipse()

ggsave("./R/Images/major/sci_comm_orient2.png", dpi = 300, width = 8)

adonis(sci_comm_orient ~ demo$major)
