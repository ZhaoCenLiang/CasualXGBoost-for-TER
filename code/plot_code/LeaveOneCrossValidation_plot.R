setwd('D:/JuypterNotebook/Github/CasualXGBoost-for-TER/')

library(ggplot2)
library(Metrics)
library(EnvStats)
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

file_path <- "./data/XGBoost/LeaveOneCrossValidation/"  #文件路径
csv_path_list <- list.files(file_path, pattern = ".csv", full.names = TRUE)  #获取该文件夹中所有csv格式的文件
csv_num = length(csv_path_list) 

mean_stat = data.frame()
m = 0

for (i in 1:csv_num) {
  
  csv_name = csv_path_list[i]
  cat(c(i,csv_name),'\n') #显示处理中的文件名称
  sheet = read.csv(csv_name)
  site_ID = as.numeric( substr(strsplit(csv_name,'_')[[1]][4],1,3) ) 
  pft = strsplit(csv_name,'_')[[1]][2]
  
  m = m + 1
  n = m + nrow(sheet) - 1
  
  base_mean = apply(sheet[,c(seq(2,40,2))],1,mean)
  causal_mean = apply(sheet[,c(seq(3,41,2))],1,mean)
  
  mean_stat[m:n,1] = site_ID
  mean_stat$TER_flux[m:n] = sheet$RECO
  mean_stat$TER_base[m:n] = base_mean 
  mean_stat$TER_causal[m:n] = causal_mean 
  mean_stat$type[m:n] = pft 
  
  m = n
  
}

stat_ec = summarySE(mean_stat, measurevar = 'TER_flux',
                    groupvars = c('V1','type'),na.rm = T)
stat_causal =  summarySE(mean_stat, measurevar = 'TER_causal',
                         groupvars = c('V1','type'),na.rm = T)
stat_base = summarySE(mean_stat, measurevar = 'TER_base',
                      groupvars = c('V1','type'),na.rm = T)

stat_plot = cbind(stat_ec , stat_causal[,4])
colnames(stat_plot)[8] = 'TER_causal'
mae_causal = round(mae(stat_plot$TER_flux,stat_plot$TER_causal),2)
rmae_causal = 100* round(mae_causal/mean(stat_plot$TER_flux),4)
p1 <- ggplot(stat_plot ,aes(x=TER_flux, y = TER_causal, color = type)) + theme_zg() +
  geom_point(alpha = 0.5,size =3) + geom_smooth(method = 'lm', se = FALSE, level = 0.95, alpha = 0.25) +
  scale_color_lancet() +
  scale_x_continuous(breaks = seq(0,500,50),limits = c(0,300),expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0,500,50),limits = c(0,300),expand = c(0, 0)) +
  stat_poly_eq(data = mean_stat,
               aes(x = TER_flux, y = TER_causal,
                   color = NULL,
                   label = paste(..eq.label..,..rr.label..,..p.value.label.., sep = '~~')),
               formula = y ~ x,  parse = T,
               size = 6,
               family = "Times") +
  xlab(expression(paste('TER observarions',' [ g C m'^-2,' month'^-1,' ]'))) +
  ylab(expression(paste('Causal-ML TER estimation ',' [ g C m'^-2,' month'^-1,' ]'))) +
  theme(axis.title.y=element_text(family="Times",angle=90, colour="black",size=20),
        axis.title.x = element_text(family="Times",colour="black",size=20),
        axis.text.x = element_text(family="Times",colour="black",size=16),
        axis.text.y = element_text(family="Times",colour="black",size=16),
        title = element_text(family="Times",face = "bold",colour="black",size=16),
        legend.text= element_text(family="Times",colour="black",size=16),
        legend.title = element_text(family="Times",colour="black",size=16),
        legend.position = c(0.525,0.1),
        legend.direction = "horizontal",
        plot.margin = margin(t = 10,  # 顶部边缘距离
                             r = 20,  # 右边边缘距离
                             b = 10,  # 底部边缘距离
                             l = 10)) +
  guides(color=guide_legend(title = NULL)) + 
  geom_abline (intercept = 0, slope = 1, linetype = 2) +
  annotate("text", x= -Inf, y= Inf, hjust = -0.165, vjust = 5,  
           label=paste("rMAE =", rmae_causal,'%'),family="Times",size = 6) +
  annotate("text", x= -Inf, y= Inf, hjust = -0.2, vjust = 7,  
           label=paste("MAE =", mae_causal),family="Times",size = 6) 

p1  

stat_plot2 = cbind(stat_ec , stat_base[,4])
colnames(stat_plot2)[8] = 'TER_base'
mae_base = round(mae(stat_plot2$TER_flux,stat_plot2$TER_base),2)
rmae_base = 100* round(mae_base/mean(stat_plot2$TER_flux),4)
p2 <- ggplot(stat_plot2 ,aes(x=TER_flux, y = TER_base, color = type)) + theme_zg() +
  geom_point(alpha = 0.5,size =3) + geom_smooth(method = 'lm', se = FALSE, level = 0.95, alpha = 0.25) +
  scale_color_lancet() +
  scale_x_continuous(breaks = seq(0,500,50),limits = c(0,300),expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0,500,50),limits = c(0,300),expand = c(0, 0)) +
  stat_poly_eq(data = mean_stat,
               aes(x = TER_flux, y = TER_base,
                   color = NULL,
                   label = paste(..eq.label..,..rr.label..,..p.value.label.., sep = '~~')),
               formula = y ~ x,  parse = T,
               size = 6,
               family = "Times")+
  xlab(expression(paste('TER observarions',' [ g C m'^-2,' month'^-1,' ]'))) +
  ylab(expression(paste('Baseline-ML TER estimation ',' [ g C m'^-2,' month'^-1,' ]'))) +
  theme(axis.title.y=element_text(family="Times",angle=90, colour="black",size=20),
        axis.title.x = element_text(family="Times",colour="black",size=20),
        axis.text.x = element_text(family="Times",colour="black",size=16),
        axis.text.y = element_text(family="Times",colour="black",size=16),
        title = element_text(family="Times",face = "bold",colour="black",size=16),
        legend.text= element_text(family="Times",colour="black",size=16),
        legend.title = element_text(family="Times",colour="black",size=16),
        legend.position = c(0.525,0.1),
        legend.direction = "horizontal",
        plot.margin = margin(t = 10,  # 顶部边缘距离
                             r = 20,  # 右边边缘距离
                             b = 10,  # 底部边缘距离
                             l = 10)) +
  guides(color=guide_legend(title = NULL)) + 
  geom_abline (intercept = 0, slope = 1, linetype = 2) +
  annotate("text", x= -Inf, y= Inf, hjust = -0.165, vjust = 5,  
           label=paste("rMAE =", rmae_base,'%'),family="Times",size = 6) +
  annotate("text", x= -Inf, y= Inf, hjust = -0.2, vjust = 7,  
           label=paste("MAE =", mae_base),family="Times",size = 6) 

p2  
layout <-"
AB
"
p3 <- p2 + p1 +plot_layout(design = layout)+ plot_annotation(tag_levels = 'a',
                                                             tag_prefix = '(',tag_suffix = ')') &
  theme(plot.tag.position = c(0.9, 0.175),
        plot.tag = element_text(size = 32, hjust = 0, vjust = 0))
p3

#####
delta_stat = data.frame()
m = 0

for (i in 1:csv_num) {
  
  csv_name = csv_path_list[i]
  cat(c(i,csv_name),'\n') #显示处理中的文件名称
  sheet = read.csv(csv_name)
  site_ID = as.numeric( substr(strsplit(csv_name,'_')[[1]][4],1,3) ) 
  pft = strsplit(csv_name,'_')[[1]][2]
  
  m = m + 1
  n = m + 19
  
  R2_base = c()
  mae_base = c()
  r_base = c()
  
  R2_causal = c()
  mae_causal = c()
  r_causal = c()
  
  for (j in 1:20) {
    
    a = 2*j # base
    lineM_base = lm(sheet$RECO~sheet[,a])
    line_s_base = summary(lineM_base)
    round_r_base = cor(sheet$RECO,sheet[,a])
    round_mae_base = round(mae(sheet$RECO,sheet[,a]),2)
    
    R2_base = c(R2_base, line_s_base$r.squared)
    r_base = c(r_base,round_r_base)
    mae_base = c(mae_base, round_mae_base)
    
    b = 2*j + 1 # causal
    lineM_causal = lm(sheet$RECO~sheet[,b])
    line_s_causal = summary(lineM_causal)
    round_r_causal = cor(sheet$RECO,sheet[,b])
    round_mae_causal = round(mae(sheet$RECO,sheet[,b]),2)
    
    R2_causal = c(R2_causal, line_s_causal$r.squared)
    r_causal = c(r_causal,round_r_causal)
    mae_causal = c(mae_causal, round_mae_causal)
    
  }
  
  delta_stat[m:n,1] = site_ID
  delta_stat$round[m:n] = seq(1,20)
  delta_stat$TER_flux[m:n] = mean(sheet$RECO, na.rm = T)
  
  delta_stat$r_base[m:n] = r_base
  delta_stat$mae_base[m:n] = mae_base
  delta_stat$R2_base[m:n] = R2_base 
  
  delta_stat$r_causal[m:n] = r_causal
  delta_stat$mae_causal[m:n] = mae_causal
  delta_stat$R2_causal[m:n] = R2_causal
  
  delta_stat$type[m:n] = pft 
  
  m = n
  
}
summary(delta_stat)

delta_stat$rmae_base = 100*delta_stat$mae_base/delta_stat$TER_flux
delta_stat$rmae_causal =  100*delta_stat$mae_causal/delta_stat$TER_flux
delta_stat[delta_stat$rmae_base > 100 | delta_stat$rmae_causal > 100, 11:12] = NA

summary(delta_stat)
mae_plot = gather(delta_stat[,10:12],key = 'rmae', value = 'value',-'type')
pft_order = c('EBF','ENF','DBF','MF','SAV','SHR','GRA','CRO')
p5 <- ggplot(mae_plot, aes(x = factor(type,level = pft_order),
                           y = value, fill = rmae)) + theme_zg() + 
  stat_boxplot(geom="errorbar",size=0.5)+
  geom_boxplot(outlier.shape = NA,notch = T) + 
  scale_y_continuous(breaks = seq(0,100,20),expand = c(0, 0)) +
  coord_cartesian(ylim=c(0,117.5))+
  xlab(expression(paste('Plant functional types'))) +
  ylab(expression(paste('rMAE of leave-one-site-out evaluation (%)'))) +
  scale_fill_discrete(type = c('#FA8072','#87CEFA'), label = c('Baseline XGBoost','Causal XGBoost')) +
  theme(axis.title.y=element_text(family="Times",angle=90, colour="black",size=18),
        axis.title.x = element_text(family="Times",colour="black",size=18,
                                    vjust = -1),
        axis.text.x = element_text(family="Times",colour="black",size=14),
        axis.text.y = element_text(family="Times",colour="black",size=14),
        title = element_text(family="Times",face = "bold",colour="black",size=14),
        legend.text= element_text(family="Times",colour="black",size=14),
        legend.title = element_text(family="Times",colour="black",size=14),
        legend.position = c(0.69,0.935),
        legend.direction = "horizontal",
        plot.margin = margin(t = 10,  # 顶部边缘距离
                             r = 20,  # 右边边缘距离
                             b = 10,  # 底部边缘距离
                             l = 10)) +
  guides(fill=guide_legend(title = NULL) ) +
  stat_compare_means(data = mae_plot, aes(x = factor(type,level = pft_order),
                                          y = value, group = rmae),
                     label.y = 92.5, label = 'p.signif', method = "t.test",paired = T,
                     family="Times",size = 5) +
  stat_n_text(y.pos = 100) 

p5

delta_stat$delta_rmae = delta_stat$rmae_causal - delta_stat$rmae_base
ll_rmae = summarySE(delta_stat,measurevar = 'delta_rmae',groupvars = 'type',na.rm = T)

bar_plot = ll_rmae
bar_plot$col = NA
bar_plot$col[bar_plot$delta > 0] = 'neg'
bar_plot$col[bar_plot$delta <= 0] = 'pos'

p8 = ggplot(bar_plot, aes(x = factor(type,level = pft_order),
                          y = delta_rmae,
                          fill = col)) + theme_zg() +
  geom_bar(stat="identity",color = 'black') +
  scale_y_continuous(breaks = seq(-10,10,2.5),limits = c(-5,5),expand = c(0, 0)) +
  geom_errorbar(aes(ymin = delta_rmae-ci, ymax = delta_rmae+ci),width = 0.382) + 
  xlab(expression(paste('Plant functional types'))) +
  ylab(expression(paste(Delta,' rMAE of leave-one-site-out evaluation (%)'))) +
  theme(axis.title.y=element_text(family="Times",angle=90, colour="black",size=20),
        axis.title.x = element_text(family="Times",colour="black",size=20),
        axis.text.x = element_text(family="Times",colour="black",size=16),
        axis.text.y = element_text(family="Times",colour="black",size=16),
        title = element_text(family="Times",face = "bold",colour="black",size=16),
        legend.text= element_text(family="Times",colour="black",size=16),
        legend.title = element_text(family="Times",colour="black",size=16),
        legend.position = c(0.7,0.1),
        legend.key.width = unit(25,'pt'),
        legend.direction = "horizontal",
        plot.margin = margin(t = 20,  # 顶部边缘距离
                             r = 20,  # 右边边缘距离
                             b = 10,  # 底部边缘距离
                             l = 10))+
  guides(fill=guide_legend(title = expression(paste(Delta,' rMAE'[Causal - Baseline]) ))) +
  scale_fill_discrete(type = c('#FA8072','#43CD80'),label = c('+','-')) +
  geom_hline(yintercept = 0, color = 'black')

p8

p9 <- p5 + p8 +plot_layout(design = layout)+ plot_annotation(tag_levels = 'a',
                                                             tag_prefix = '(',tag_suffix = ')') &
  theme(plot.tag.position = c(0.1125, 0.925),
        plot.tag = element_text(size = 32, hjust = 0, vjust = 0))
p9
