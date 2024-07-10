setwd('D:/JuypterNotebook/Github/CasualXGBoost-for-TER/')

library(ggplot2)
library(ggsci)
library(tidyr)
library(viridis)
library(patchwork)

theme_zg <- function(..., bg='white'){
  require(grid)
  theme_classic(...) +
    theme(rect=element_rect(fill=bg),
          plot.margin=unit(rep(0.5,4), 'lines'),
          panel.background=element_rect(fill='transparent', color='black'),
          panel.border=element_rect(fill='transparent', color='transparent'),
          panel.grid=element_blank())
}

windowsFonts(HEL=windowsFont("Helvetica CE 55 Roman"),
             Times=windowsFont("Times New Roman"),
             ARL=windowsFont("Arial"))

file_path <- "./data/XGBoost/Weight/"  #文件路径
csv_path_list <- list.files(file_path, pattern = ".csv", full.names = TRUE)  #获取该文件夹中所有csv格式的文件
csv_num = length(csv_path_list) 

test = read.csv( csv_path_list[1])

base_wight = data.frame(matrix(NA,8,ncol(test)+1))
colnames(base_wight) = c('type',colnames(test))
causal_wight = base_wight

for (i in 1:(csv_num/2)) {
  
  csv_base = csv_path_list[i]
  csv_causal = csv_path_list[i+8]
  
  pft = strsplit(strsplit(csv_base,'_')[[1]][2],'.c')[[1]][1]
  
  w_base = read.csv(csv_base)
  w_causal = read.csv(csv_causal)
  
  w_base_mean = apply(w_base,2,'mean')
  w_causal_mean = apply(w_causal,2,'mean')
  
  base_wight[i,1] = pft
  causal_wight[i,1] = pft
  
  base_wight[i,2:14] = w_base_mean
  causal_wight[i,2:14] = w_causal_mean
  
}

base_wight[is.na(base_wight)] <- 0
causal_wight[is.na(causal_wight)] <- 0

for (i in 1:8) {
  
  sum_base = sum(base_wight[i,2:14])
  sum_causal =  sum(causal_wight[i,2:14])
  
  base_wight[i,2:14] = base_wight[i,2:14]/sum_base
  causal_wight[i,2:14] = causal_wight[i,2:14]/sum_causal
  
}

base_wight[9,1] = 'ALL'
causal_wight[9,1] = 'ALL'

base_wight[9,2:14] = colMeans(base_wight[1:8,2:14])
causal_wight[9,2:14] = colMeans(causal_wight[1:8,2:14])
####
base_plot = gather(base_wight, key = 'variables', value = 'value',-'type') 

causal_plot = gather(causal_wight, key = 'variables', value = 'value',-'type') 

v_order = c('LAI','CI','Air_temperature','NPV','Precipation','PV',
            'SOC_0','SOC_10','SOC_30','SWC','BS','Soil_temperature','VPD')

pft_order = c('EBF','ENF','DBF','MF','SAV','SHR','GRA','CRO','ALL')

summary(causal_plot)

p1 <- ggplot(base_plot, aes(x=factor(type,level = pft_order),
                            y = factor(variables,level = rev(v_order)),
                            fill = 100*abs(value) )) +
  theme_zg() +
  theme(axis.title.y=element_text(family="Times",angle=90, colour="black",size=24),
        axis.title.x = element_text(family="Times",colour="black",size=24,
                                    vjust = -1),
        axis.text.x = element_text(family="Times",colour="black",size=16),
        axis.text.y = element_text(family="Times",colour="black",size=16),
        title = element_text(family="Times",face = "bold",colour="black",size=16),
        legend.text= element_text(family="Times",colour="black",size=16),
        legend.title = element_text(family="Times",colour="black",size=20),
        legend.position = 'bottom',
        legend.key.width = unit(1.325,'cm')) +
  geom_raster() +
  scale_fill_viridis(limit = c(0,30), breaks = seq(0,30,10),
                     labels = seq(0,30,10)) +
  ggtitle('Baseline XGBoost model') + 
  ylab(expression('Variables')) + 
  xlab(expression('Plant functional types')) +
  guides(fill = guide_colorbar(title = 'Normalized \nXGBoost weight (%)',title.vjust = 1))

p1

p2 <- ggplot(causal_plot, aes(x=factor(type,level = pft_order),
                              y = factor(variables,level = rev(v_order)),
                              fill = 100*abs(value) )) +
  theme_zg() +
  theme(axis.title.y=element_text(family="Times",angle=90, colour="black",size=24),
        axis.title.x = element_text(family="Times",colour="black",size=24,
                                    vjust = -1),
        axis.text.x = element_text(family="Times",colour="black",size=16),
        axis.text.y = element_text(family="Times",colour="black",size=16),
        title = element_text(family="Times",face = "bold",colour="black",size=16),
        legend.text= element_text(family="Times",colour="black",size=16),
        legend.title = element_text(family="Times",colour="black",size=20),
        legend.position = 'bottom',
        legend.key.width = unit(1.325,'cm')) +
  geom_raster() +
  scale_fill_viridis(limit = c(0,30), breaks = seq(0,30,10),
                     labels = seq(0,30,10))  +
  ggtitle('Causal XGBoost model') + 
  ylab(expression('Variables')) + 
  xlab(expression('Plant functional types')) +
  guides(fill = guide_colorbar(title = 'Normalized \nXGBoost weight (%)',title.vjust = 1))

p2

layout <-"
AB
"
p3 = p1 + p2 + plot_layout(design = layout)+ plot_annotation(tag_levels = 'a',
                                                             tag_prefix = '(',tag_suffix = ')') &
  theme(plot.tag.position = c(0.05, 0.95),
        plot.tag = element_text(size = 32, hjust = 0, vjust = 0))
p3
