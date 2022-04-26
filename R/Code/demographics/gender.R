# Pull in outside data ####
source("./R/Code/01-cleaning_data.R")

# MDS For Loop ####
x=1
gender_plots <- list()
for(i in c("si","cm","im","sd","se","gm","sci_comp","per_comm_orient","sci_comm_orient")){
  gender_plots[[x]] <- mysubsetMDS(i) %>% 
    ggplot(aes(x=MDS1,y=MDS2,color=factor(gender)))+
    geom_point()+
    labs(title = i)+
    stat_ellipse()
  x <- x+1
}

# Science Identity ####
gender_plots[[1]]
ggsave("./R/Images/gender/si.png",dpi = 300,width = 8)

adonis(si ~ demo$gender)

# Career Motivation ####
gender_plots[[2]]
ggsave("./R/Images/gender/cm.png",dpi = 300,width = 8)

adonis(cm ~ demo$gender)

# Intrinsic Motivation ####
gender_plots[[3]]
ggsave("./R/Images/gender/im.png",dpi = 300,width = 8)

adonis(im ~ demo$gender)

# Self-Determination ####
gender_plots[[4]]
ggsave("./R/Images/gender/sd.png",dpi = 300,width = 8)

adonis(sd ~ demo$gender)

# Self Efficacy ####
gender_plots[[5]]
ggsave("./R/Images/gender/se.png",dpi = 300,width = 8)

adonis(se ~ demo$gender)

# Grade Motivation ####
gender_plots[[6]]
ggsave("./R/Images/gender/gm.png",dpi = 300,width = 8)

adonis(gm ~ demo$gender)

# Competency in Science ####
gender_plots[[7]]
ggsave("./R/Images/gender/sci_comp.png",dpi = 300,width = 8)

adonis(sci_comp ~ demo$gender)

# Personal Communal Orientation ####
gender_plots[[8]] +
  coord_cartesian(xlim = c(-5.5,-4.5))
ggsave("./R/Images/gender/per_comm_orient.png",dpi = 300,width = 8)

adonis(per_comm_orient ~ demo$gender)

# Science Communal Orientation ####
gender_plots[[9]]
ggsave("./R/Images/gender/sci_comm_orient1.png",dpi = 300,width = 8)

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

demo_DCA %>% ggplot(mapping= aes(x=DCA1, y=DCA2,color=gender)) +
  geom_point()+
  stat_ellipse()

ggsave("./R/Images/gender/sci_comm_orient2.png", dpi = 300, width = 8)

adonis(sci_comm_orient ~ demo$gender)
