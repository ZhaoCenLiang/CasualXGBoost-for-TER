setwd('D:/JuypterNotebook/Github/CasualXGBoost-for-TER/')

library(ggplot2)
library(ggsci)
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

PCMCI_pft = read.csv('./data/PCMCI_byPFT.csv')
PCMCI_pft[9,1] = 'ALL'
PCMCI_pft[9,4:16] = colMeans(PCMCI_pft[1:8,4:16],na.rm = T)
gather_pft = gather(PCMCI_pft[,c(1,4:16)], key = 'variables', value = 'value',
                    -'PFT')
gather_pft$PFT <- gsub('MF.','MF',gather_pft$PFT)

value_order_pft = gather_pft[gather_pft$PFT == 'ALL',]
value_order_pft = value_order_pft[order(abs(value_order_pft$value)),]
pft_order = c('EBF','ENF','DBF','MF','SAV','SHR','GRA','CRO','ALL')

# modify the display effect
# gather_pft$value[gather_pft$value > 0.4 | gather_pft$value < -0.4] = 0.4 

p1 <- ggplot(gather_pft, aes(x=factor(PFT,level = pft_order),
                             y = factor(variables,level = value_order_pft$variables),
                             fill = abs(value) )) +
  theme_zg() +
  theme(axis.title.y=element_text(family="Times",angle=90, colour="black",size=24),
        axis.title.x = element_text(family="Times",colour="black",size=24,
                                    vjust = -1),
        axis.text.x = element_text(family="Times",colour="black",size=16),
        axis.text.y = element_text(family="Times",colour="black",size=16),
        legend.text= element_text(family="Times",colour="black",size=16),
        legend.title = element_text(family="Times",colour="black",size=20),
        legend.position = 'bottom',
        legend.key.width = unit(1.75,'cm')) +
  geom_raster() +
  scale_fill_viridis(limit = c(0,0.4), breaks = seq(0,0.4,0.1),
                     labels = c('0','0.1','0.2','0.3','0.4')) +
  ylab(expression('Variables')) + 
  xlab(expression('Plant functional types')) +
  guides(fill = guide_colorbar(title = 'Causal effect',title.vjust = 0.75))
p1
