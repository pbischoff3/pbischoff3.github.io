# Pull in outside data ####
source("./R/Code/01-cleaning_data.R")

# MDS For Loop ####
x=1
parents_grad_college_plots <- list()
for(i in c("si","cm","im","sd","se","gm","sci_comp","per_comm_orient","sci_comm_orient")){
  set.seed(123)
  parents_grad_college_plots[[x]] <- mysubsetMDS(i) %>% 
    ggplot(aes(x=MDS1,y=MDS2,color=factor(parents_grad_college)))+
    geom_point()+
    labs(title = i)+
    stat_ellipse()
  x <- x+1
}

# Science Identity ####
parents_grad_college_plots[[1]]
ggsave("./R/Images/parents_grad_college/si.png",dpi = 300,width = 8)

set.seed(123)
adonis(si ~ demo$parents_grad_college)

# Career Motivation ####
parents_grad_college_plots[[2]]
ggsave("./R/Images/parents_grad_college/cm.png",dpi = 300,width = 8)

set.seed(123)
adonis(cm ~ demo$parents_grad_college)

# Intrinsic Motivation ####
parents_grad_college_plots[[3]]
ggsave("./R/Images/parents_grad_college/im.png",dpi = 300,width = 8)

set.seed(123)
adonis(im ~ demo$parents_grad_college)

# Self-Determination ####
parents_grad_college_plots[[4]]
ggsave("./R/Images/parents_grad_college/sd.png",dpi = 300,width = 8)

set.seed(123)
adonis(sd ~ demo$parents_grad_college)

# Self Efficacy ####
parents_grad_college_plots[[5]]
ggsave("./R/Images/parents_grad_college/se.png",dpi = 300,width = 8)

set.seed(123)
adonis(se ~ demo$parents_grad_college)

# Grade Motivation ####
parents_grad_college_plots[[6]]
ggsave("./R/Images/parents_grad_college/gm.png",dpi = 300,width = 8)

set.seed(123)
adonis(gm ~ demo$parents_grad_college)

# Competency in Science ####
parents_grad_college_plots[[7]]
ggsave("./R/Images/parents_grad_college/sci_comp.png",dpi = 300,width = 8)

set.seed(123)
adonis(sci_comp ~ demo$parents_grad_college)

# Personal Communal Orientation ####
parents_grad_college_plots[[8]] +
  coord_cartesian(xlim = c(-5,-3.75))
ggsave("./R/Images/parents_grad_college/per_comm_orient.png",dpi = 300,width = 8)

set.seed(123)
adonis(per_comm_orient ~ demo$parents_grad_college)

# Science Communal Orientation ####
parents_grad_college_plots[[9]]
ggsave("./R/Images/parents_grad_college/sci_comm_orient1.png",dpi = 300,width = 8)

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

demo_DCA %>% ggplot(mapping= aes(x=DCA1, y=DCA2,color=parents_grad_college)) +
  geom_point()+
  stat_ellipse()

ggsave("./R/Images/parents_grad_college/sci_comm_orient2.png", dpi = 300, width = 8)

set.seed(123)
adonis(sci_comm_orient ~ demo$parents_grad_college)
