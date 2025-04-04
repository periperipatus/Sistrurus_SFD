dodge=position_dodge(0.8)
basesize=8
baselwd=0.2
peri_figure <- theme(panel.background = element_blank(), 
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     plot.background = element_blank(),
                     axis.title.x = element_text(size = basesize+1), 
                     axis.text.x = element_text(size =basesize, 
                                                colour = "black"), 
                     axis.title.y = element_text(size = basesize+1),
                     axis.text.y = element_text(size = basesize, 
                                                colour = "black"),
                     axis.line.y = element_line(colour = "black",
                                                linewidth = baselwd), 
                     axis.line.x = element_line(colour = "black",
                                                linewidth = baselwd),
                     axis.ticks = element_line(colour = "black",
                                               linewidth = 0.1),
                     plot.title = element_text(hjust = 0.5, 
                                               size=basesize+1),
                     plot.subtitle = element_text(hjust = 0.5, 
                                                  size=basesize),
                     strip.text.x=element_text(size=basesize+1,
                                               margin = margin(2,0,2,0, unit="mm")),
                     strip.background = element_blank(),
                     legend.text = element_text(size = basesize), 
                     legend.title = element_text(size =basesize),
                     legend.key=element_blank(),
                     legend.margin = margin(0.5,0,0,0),
                     plot.margin = margin(t = 5,  # Top margin
                                          r = 5,  # Right margin
                                          b = 5,  # Bottom margin
                                          l = 5, unit="mm"),
                     legend.key.size=unit(3,"mm"))



geom.text.size=(5/14)*basesize


peri_tile <- theme(panel.background = element_blank(), 
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     plot.background = element_blank(), 
                     axis.title.x = element_text(size = basesize+1), 
                     axis.text.x = element_text(size =basesize, 
                                                colour = "black"), 
                     axis.title.y = element_text(size = basesize+1),
                     axis.text.y = element_text(size = basesize, 
                                                colour = "black"),
                     axis.line.y = element_blank(), 
                     axis.line.x = element_blank(),
                     axis.ticks = element_blank(),
                     plot.title = element_text(hjust = 0.5, 
                                                 basesize+1),
                     plot.subtitle = element_text(hjust = 0.5, 
                                                  size=basesize),
                     strip.text.x=element_text(size=basesize+1,
                                               margin = margin(2,0,2,0, unit="mm")),
                     legend.text = element_text(size = basesize), 
                     legend.title = element_text(size =basesize),
                     legend.key=element_blank(),
                     legend.margin = margin(0.5,0,0,0),
                     plot.margin = margin(t = 5,  # Top margin
                                          r = 5,  # Right margin
                                          b = 5,  # Bottom margin
                                          l = 5, unit="mm"),
                      legend.key.size=unit(3,"mm"))


#basesize=12
#baselwd=1
#peri_poster <- theme(panel.background = element_blank(), 
#                     panel.grid.major = element_blank(), 
#                     panel.grid.minor = element_blank(),
#                     plot.background = element_blank(),
#                     axis.title.x = element_text(size = basesize+1), 
#                     axis.text.x = element_text(size =basesize, 
#                                                colour = "black"), 
#                     axis.title.y = element_text(size = basesize+1),
#                     axis.text.y = element_text(size = basesize, 
#                                                colour = "black"),
#                     axis.line.y = element_line(colour = "black",
#                                                linewidth = baselwd), 
#                     axis.line.x = element_line(colour = "black",
#                                                linewidth = baselwd),
#                     axis.ticks = element_line(colour = "black",
#                                               linewidth = 0.1),
#                     plot.title = element_text(hjust = 0.5, 
#                                               size=basesize+1),
#                     plot.subtitle = element_text(hjust = 0.5, 
#                                                  size=basesize),
#                     strip.text.x=element_text(size=basesize+1,
#                                               margin = margin(2,0,2,0, unit="mm")),
#                     strip.background = element_blank(),
#                     legend.text = element_text(size = basesize), 
#                     legend.title = element_text(size =basesize),
#                     legend.key=element_blank(),
#                     legend.margin = margin(0.5,0,0,0),
#                     plot.margin = margin(t = 5,  # Top margin
#                                          r = 5,  # Right margin
#                                          b = 5,  # Bottom margin
#                                          l = 5, unit="mm"),
#                     legend.key.size=unit(3,"mm"))
#geom.text.size=(5/14)*basesize
