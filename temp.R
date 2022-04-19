### *Major*
Alright, awesome. Data is loaded, and MDS is ready to be run. We created this for-loop that will allow us to get the plots of the MDS data according to gender and whatever grouping of questions we are exploring particularly.
```{r for loop, message=FALSE, warning=FALSE, include=FALSE}
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
```

Let's check out the relationship in Science Identity questions and majors.
```{r si major, message=FALSE, warning=FALSE}
major_plots[[1]]
```
<br> 
Well, that is kind of a mess. Let's run the Adonis test to see if there is a significant difference in Science Identity answers versus majors. 
```{r si adonis, message=FALSE, warning=FALSE}
adonis(si ~ demo$major)
```
Well gosh, check that out. We have a significant difference here. We will *mark that down for later analysis*.

Next, lets measure significance in difference in Career Motivation and majors. The same Adonis test is run on that.
```{r message=FALSE, warning=FALSE}
major_plots[[2]]
adonis(cm ~ demo$major)
```
This shows that there is no significance.
<br>
  
  Now, we will measure significance in difference in Intrinsic Motivation and majors, with an Adonis being run.
```{r message=FALSE, warning=FALSE}
major_plots[[3]]
adonis(im ~ demo$major)
```
No significance here, either. 
<br>
  
  Next, Self-Determination and majors.
```{r message=FALSE, warning=FALSE}
major_plots[[4]]
adonis(sd ~ demo$major)
```
Again, not significant.
<br>
  
  Here is Self-Efficacy and majors.
```{r message=FALSE, warning=FALSE}
major_plots[[5]]
adonis(se ~ demo$major)
```
This is not significant.
<br>
  
  This is Grade Motivation and majors.
```{r message=FALSE, warning=FALSE}
major_plots[[6]]
adonis(gm ~ demo$major)
```
This is not significant.
<br>
  
  Here is Science Competency and major.
```{r message=FALSE, warning=FALSE}
major_plots[[7]]
adonis(sci_comp ~ demo$major)
```
This one is not significant, either.
<br>
  
  Now, Personal Communal Orientation and majors.
```{r message=FALSE, warning=FALSE}
major_plots[[8]] +
  coord_cartesian(xlim = c(-6,-5))
adonis(per_comm_orient ~ demo$major)
```
This one is not significant.
<br>
  
  Lastly, lets explore Science Communal Orientation and majors.
```{r message=FALSE, warning=FALSE}
major_plots[[9]]
```
<br> Well heck, that didn't work. Let's run a different type of MDS and try that.
```{r message=FALSE, warning=FALSE}
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
```
Now, let's see if it is significant. 
```{r message=FALSE, warning=FALSE}
adonis(sci_comm_orient ~ demo$major)
```
Lo and behold, this is not significant. 

<br><br>

Now, what does this all mean? All of the differences in the aspects of the survey and majors were not significant except for the Science Identity questions. This tells us that in our sample, there is a difference in how different majors answer Science Identity. To understand this, further model analysis needs to be run, something that will be run in later explorations of this data set.

### *Gender*

### *Career Goals*

### *Ethnicity*

### *First Destination Students*

### *Number of Dependents*

### *Do you have a medical condition?*

### *Do you care for someone who has a medical condition?*

## **Conclusion**
