setwd('D:/JuypterNotebook/Github/CasualXGBoost-for-TER/')

library(ggplot2)
library(patchwork)
library(ggpmisc)
library(tidyr)
library(viridis)

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

allset_SHAP = read.csv('./data/SHAP.csv')

p1 <- ggplot(allset_SHAP, aes(x = Air_temperature, y = Air_temperature_base,
                         color = type,fill= type)) + theme_zg() +
  geom_point(alpha = 0.1, size =2) +
  scale_color_lancet() +
  scale_fill_lancet() +
  scale_y_continuous(breaks = seq(-100,100,20),limits = c(-120,120),expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(-50,50,10),limits = c(-20,35),expand = c(0, 0)) +
  theme(axis.title.y=element_text(family="Times",angle=90, colour="black",size=20),
        axis.title.x = element_text(family="Times",colour="black",size=20),
        axis.text.x = element_text(family="Times",colour="black",size=16),
        axis.text.y = element_text(family="Times",colour="black",size=16),
        title = element_text(family="Times",face = "bold",colour="black",size=16),
        legend.text= element_text(family="Times",colour="black",size=16),
        legend.title = element_text(family="Times",colour="black",size=16),
        legend.position = c(0.45,0.1),
        legend.key.width = unit(25,'pt'),
        legend.direction = "horizontal",
        plot.margin = margin(t = 10,  # 顶部边缘距离
                             r = 20,  # 右边边缘距离
                             b = 10,  # 底部边缘距离
                             l = 10)) +
  xlab(expression(paste('Air temperature ( ',degree,'C )'))) +
  ylab(expression(paste('SHAP value for air temperature',' [ g C m'^-2,' month'^-1,' ]'))) +
  ggtitle('Baseline XGBoost model') +
  guides(color=guide_legend(title = NULL),fill=guide_legend(title = NULL)) +
  geom_smooth(aes(color = type, fill = type), method="gam", se =T)

p1

p2 <- ggplot(allset_SHAP, aes(x = Air_temperature, y = Air_temperature_causal,
                              color = type, fill= type)) + theme_zg() +
  geom_point(alpha = 0.1, size =2) +
  scale_color_lancet() +
  scale_fill_lancet(alpha = 0.1) +
  scale_y_continuous(breaks = seq(-100,100,20),limits = c(-120,120),expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(-50,50,10),limits = c(-20,35),expand = c(0, 0)) +
  theme(axis.title.y=element_text(family="Times",angle=90, colour="black",size=20),
        axis.title.x = element_text(family="Times",colour="black",size=20),
        axis.text.x = element_text(family="Times",colour="black",size=16),
        axis.text.y = element_text(family="Times",colour="black",size=16),
        title = element_text(family="Times",face = "bold",colour="black",size=16),
        legend.text= element_text(family="Times",colour="black",size=16),
        legend.title = element_text(family="Times",colour="black",size=16),
        legend.position = c(0.45,0.1),
        legend.key.width = unit(25,'pt'),
        legend.direction = "horizontal",
        plot.margin = margin(t = 10,  # 顶部边缘距离
                             r = 20,  # 右边边缘距离
                             b = 10,  # 底部边缘距离
                             l = 10)) +
  xlab(expression(paste('Air temperature ( ',degree,'C )'))) +
  ylab(expression(paste('SHAP value for air temperature',' [ g C m'^-2,' month'^-1,' ]'))) +
  ggtitle('Causal XGBoost model') +
  guides(color=guide_legend(title = NULL),fill=guide_legend(title = NULL)) +
  geom_smooth(aes(color = type, fill = type), method="gam", se =T)

p2
layout <-"
AB
"
p3 = p1 + p2 + plot_layout(design = layout)+ plot_annotation(tag_levels = 'a',
                                                             tag_prefix = '(',tag_suffix = ')') &
  theme(plot.tag.position = c(0.9, 0.165),
        plot.tag = element_text(size = 32, hjust = 0, vjust = 0))
p3
ggsave(p3 , path = './Plot/',
       file='Figure_10.png', width=14, height=7, dpi = 600)

#### heat plot
pft_order = c('EBF','ENF','DBF','MF','SAV','SHR','GRA','CRO','ALL')

stat = data.frame(matrix(NA,9,27))
colnames(stat) = c('type',colnames(allset_SHAP)[26:51])

for (i in 1:9) {
  
  if (i != 9){
    sub = allset_SHAP[allset_SHAP$type == pft_order[i],]
  }else{
    sub = allset_SHAP
  }
  shap = abs(sub[,26:51])
  stat[i,1] =  pft_order[i]
  stat[i,2:27] = apply(abs(sub[,26:51]),2,'mean')
  
}

base_stat = stat[,1:14]
causal_stat = stat[,c(1,15:27)]

base_stat$sum = apply(base_stat[2:14],1,'sum')
causal_stat$sum = apply(causal_stat[2:14],1,'sum')

base_stat[,2:14] = base_stat[,2:14]/base_stat$sum
causal_stat[,2:14] = causal_stat[,2:14]/causal_stat$sum

colnames(base_stat)[2:14] = colnames(allset_SHAP)[13:25]
colnames(causal_stat)[2:14] = colnames(allset_SHAP)[13:25]

base_plot = gather(base_stat[,1:14], key = 'variables', value = 'value',-'type') 
causal_plot = gather(causal_stat[,1:14], key = 'variables', value = 'value',-'type') 

v_order = c('LAI','CI','Air_temperature','NPV','Precipation','PV',
            'SOC_0','SOC_10','SOC_30','SWC','BS','Soil_temperature','VPD')

summary(base_plot)
p4 <- ggplot(base_plot, aes(x=factor(type,level = pft_order),
                            y = factor(variables,level = rev(v_order)),
                            fill = abs(value) )) +
  theme_zg() +
  theme(axis.title.y=element_text(family="Times",angle=90, colour="black",size=20),
        axis.title.x = element_text(family="Times",colour="black",size=20,vjust = -1),
        axis.text.x = element_text(family="Times",colour="black",size=16),
        axis.text.y = element_text(family="Times",colour="black",size=16),
        title = element_text(family="Times",face = "bold",colour="black",size=16),
        legend.text= element_text(family="Times",colour="black",size=16),
        legend.title = element_text(family="Times",colour="black",size=16),
        legend.position = 'bottom',
        legend.key.width = unit(1.325,'cm')) +
  geom_raster() +
  scale_fill_viridis(limit = c(0,0.6), breaks = seq(0,0.6,0.2),
                     labels = seq(0,0.6,0.2)) +
  ggtitle('Baseline XGBoost model') +
  ylab(expression('Variables')) + 
  xlab(expression('Plant functional types')) +
  guides(fill = guide_colorbar(title = 'Normalized mean |SHAP|',title.vjust = 0.75))

p4

p5 <- ggplot(causal_plot, aes(x=factor(type,level = pft_order),
                              y = factor(variables,level = rev(v_order)),
                              fill = abs(value) )) +
  theme_zg() +
  theme(axis.title.y=element_text(family="Times",angle=90, colour="black",size=20),
        axis.title.x = element_text(family="Times",colour="black",size=20,vjust = -1),
        axis.text.x = element_text(family="Times",colour="black",size=16),
        axis.text.y = element_text(family="Times",colour="black",size=16),
        title = element_text(family="Times",face = "bold",colour="black",size=16),
        legend.text= element_text(family="Times",colour="black",size=16),
        legend.title = element_text(family="Times",colour="black",size=16),
        legend.position = 'bottom',
        legend.key.width = unit(1.325,'cm')) +
  geom_raster() +
  scale_fill_viridis(limit = c(0,0.6), breaks = seq(0,0.6,0.2),
                     labels = seq(0,0.6,0.2)) +
  ggtitle('Causal XGBoost model') +
  ylab(expression('Variables')) + 
  xlab(expression('Plant functional types')) +
  guides(fill = guide_colorbar(title = 'Normalized mean |SHAP|',title.vjust = 0.75))
p5

p6 = p4 + p5 + plot_layout(design = layout)+plot_annotation(tag_levels = 'a',
                                                            tag_prefix = '(',tag_suffix = ')') &
  theme(plot.tag.position = c(0.05, 0.95),
        plot.tag = element_text(size = 32, hjust = 0, vjust = 0))
p6
ggsave(p6 , path = './plot/',
       file='Figure_5.png', width=16, height=10, dpi = 400)