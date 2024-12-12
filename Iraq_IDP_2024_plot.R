library(tidyverse)
library(wbstats)
library(sf)
library(ggplot2)
library(ggrepel)
library(readxl)

path='C:/Users/PC/Documents/Wikipedia/Carto/data/population_irak/'
setwd(path)
list.files(pattern='.shp')



map=st_read('irq_admbnda_adm1_cso_20190603.shp')

data_clean=read_xlsx('iraq_census.xlsx',sheet='internally_displaced_persons')

map_data=merge(map,data_clean,
               by.x='ADM1_EN',
               by.y='gov')

n_gov=map %>% as.data.frame() %>% select(ADM1_EN) %>% unique() %>% nrow()

map_data$geometry
n_gov

for(k in 1:n_gov){
  print(paste0(map_data$ADM1_EN[k],': averaged'))
  local= map_data$geometry[[k]][[1]]
  
  dim1=local[,1] %>% mean()
  dim2=local[,2] %>% mean()
  
  map_data$dim1[k]=dim1
  map_data$dim2[k]=dim2
}


# conversion de la variable continue en variable discrète

breaks = seq(0,250000, by=40000)
labs=seq(0,250,by=40)
labs_plot=paste0('[',labs[1:6], 'k-',labs[2:7],'k]')

map_data$ref_disc=cut(map_data$IDP,
                      breaks=breaks,
                      labs=labs,
                      dig.lab=5,
                      labels = labs_plot,
                      include.lowest=T)

map_data$ref_disc = factor(map_data$ref_disc, levels=labs_plot)

# Paramétrage de la palette

pal=hcl.colors(6,'Fall',rev=F,alpha=1)

ggplot()+
  geom_sf(data=map_data,size=1.5,
          color='black',
          aes(fill=ref_disc),
          show.legend=T,
          alpha=0.9)+
  ggtitle('Iraq')+
  coord_sf()+
  theme_minimal()+
  scale_fill_manual(values=pal,
                    drop=F,
                    na.value='grey',
                    label=labs_plot,
                    # Legend
                    guide=guide_legend(direction = 'horizontal',
                                       nrow=1,
                                       label.position='bottom'))+
  theme_light()+
  theme(plot.caption=element_text(size=10,face='italic'),
        legend.position='bottom')+
  labs(title='Irak - Répartition des déplacés internes',
       subtitle='Par gouvernorat, 2024',
       caption='Source: UNHCR',
       fill='')+
  geom_label_repel(data=map_data,aes(x=dim1,y=dim2,label=ADM1_EN),
                   size=3.1, nudge_y=0.5, alpha=0.8, max.overlaps=20)+
  xlab('')+
  ylab('')









