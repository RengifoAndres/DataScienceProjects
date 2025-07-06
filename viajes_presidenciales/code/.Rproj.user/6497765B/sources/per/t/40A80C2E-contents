# Replication files for Difference-in-Differences with a Continuous Treatment
# by Brantly Callaway, Andrew Goodman-Bacon and Pedro H. C. Sant'Anna
#-----------------------------------------------------------------------------
# Empirical application based on Acemoglu and Finkelstein (2008)
#-----------------------------------------------------------------------------
# This portion of the codes compute our nonparametric plots
#-----------------------------------------------------------------------------
# Clear memory
rm(list=ls())
# Load packages
library(tidyr)
library(dplyr)
library(fixest)
library(broom)
library(stringr)
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(here)
#library(np)
#remotes::install_github('JeffreyRacine/npiv')
#library(npiv)
library(latex2exp)
library(patchwork)
library(ggtext)
#---------------------------------------------------------------------------------------
# Load data
load(here("data/processed/splines_estimates1.RData"))
# Load ggplot theme
source(here("codes/00_ggplot_theme.R"))
#---------------------------------------------------------------------------------------
# Compute range
range_Y <- range(results_cdid_SR$p_lci_att, results_cdid_SR$p_uci_att,
                 results_cdid_LR$p_lci_att, results_cdid_LR$p_uci_att)
range_Y
#-----------------------------------------------------------------------------
# Plot ATT(d|d) for Short-Run
p_att_SR <- ggplot(data = results_cdid_SR,
                   mapping = aes(x = d, y = att)) +
  geom_ribbon(aes(ymin= p_lci_att, ymax=  p_uci_att),
              alpha = 0.2,
              size = 1,
              fill = "#E69F00")+
  geom_line(linewidth = 1.2,
            alpha = 2,
            colour = "#E69F00") +
  geom_hline(yintercept = 0,
             colour="black",
             linewidth = 0.5,
             linetype = "dotted")+
  ylim(range(-0.04, 0.12))+
  scale_y_continuous(breaks = seq(-0.04, 0.12, 0.02), limits = c(-0.04,0.12))+
  xlab("County prospectivity score") +
  ylab("Average differences in log(employment)") +
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title = element_text(color="black",
                                  size = 12))+
  theme(plot.title=ggtext::element_markdown(size=14,
                                            #face = "bold",
                                            color="black",
                                            hjust=0,
                                            lineheight=1.2)
  )
p_att_SR
# Plot ATT(d|d) for Long-Run
p_att_LR <- ggplot(data = results_cdid_LR,
                   mapping = aes(x = d, y = att)) +
  geom_ribbon(aes(ymin= p_lci_att, ymax=  p_uci_att),
              alpha = 0.1,
              size = 1,
              fill = "darkblue")+
  geom_line(linewidth = 1.2,
            alpha = 2,
            colour = "darkblue") +
  geom_hline(yintercept = 0,
             colour="black",
             linewidth = 0.5,
             linetype = "dotted")+
  ylim(range(-0.04, 0.12))+
  scale_y_continuous(breaks = seq(-0.04, 0.12, 0.02), limits = c(-0.04,0.12))+
  xlab("County prospectivity score") +
  ylab("Average differences in log(employment)") +
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title = element_text(color="black",
                                  size = 12))+
  theme(plot.title=ggtext::element_markdown(size=14,
                                            #face = "bold",
                                            color="black",
                                            hjust=0,
                                            lineheight=1.2)
  )
p_att_LR
p_att_all <- ggplot(data = results_cdid_all,
                    mapping = aes(x = d, y = att)) +
  geom_ribbon(aes(ymin= p_lci_att, ymax=  p_uci_att),
              alpha = 0.3,
              size = 1,
              fill = "#009E73")+
  geom_line(linewidth = 1.2,
            alpha = 2,
            colour = "#009E73") +
  geom_hline(yintercept = 0,
             colour="black",
             linewidth = 0.5,
             linetype = "dotted")+
  ylim(range(-0.04, 0.12))+
  scale_y_continuous(breaks = seq(-0.04, 0.12, 0.02), limits = c(-0.04,0.12))+
  xlab("County prospectivity score") +
  ylab("Average differences in log(employment)") +
  theme(axis.text.y = element_text(size = 18))+
  theme(axis.text.x = element_text(size = 18)) +
  theme(axis.title = element_text(color="black",
                                  size = 18))+
  theme(plot.title=ggtext::element_markdown(size=20,
                                            #face = "bold",
                                            color="black",
                                            hjust=0,
                                            lineheight=1.2)
  )
p_att_all
#-----------------------------------------------------------------------------
# Do a combine plot (Figure 2 in the paper)
p_att_combined <- ggplot(data = results_cdid_LR,
                         mapping = aes(x = d, y = att)) +
  geom_ribbon(aes(ymin= p_lci_att, ymax=  p_uci_att),
              alpha = 0.1,
              size = 1,
              fill = "darkblue")+
  geom_line(linewidth = 1.2,
            alpha = 2,
            colour = "darkblue") +
  geom_hline(yintercept = 0,
             colour="black",
             linewidth = 0.5,
             linetype = "dotted")+
  geom_ribbon(data = results_cdid_SR, aes(ymin= p_lci_att, ymax=  p_uci_att),
              alpha = 0.2,
              size = 1,
              fill = "#E69F00")+
  geom_line(data = results_cdid_SR,mapping = aes(x = d, y = att),
            linewidth = 1.2,
            alpha = 2,
            colour = "#E69F00") +
  
  ylim(range(-0.04, 0.12))+
  scale_y_continuous(breaks = seq(-0.04, 0.12, 0.02), limits = c(-0.04,0.12))+
  scale_x_continuous(breaks = seq(2, 6, 0.5))+
  xlab("County prospectivity score") +
  ylab("Average differences in log(employment)") +
  theme(axis.text.y = element_text(size = 20))+
  theme(axis.text.x = element_text(size = 20)) +
  theme(axis.title = element_text(color="black",
                                  size = 20))+
  theme(plot.title=ggtext::element_markdown(size=20,
                                            #face = "bold",
                                            color="black",
                                            hjust=0,
                                            lineheight=1.2)
  )

p_att_combined



#-----------------------------------------------------------------------------
# Save Plots
# ggsave(here("plots/ATT_function_SR.pdf"),
#        plot = p_att_SR,
#        width = 12, height = 6, units = "in", dpi = 300)
# 
# ggsave(here("plots/ATT_function_LR.pdf"),
#        plot = p_att_LR,
#        width = 12, height = 6, units = "in", dpi = 300)

ggsave(here("plots/Appendix_Fig_dose_response.pdf"),
       plot = p_att_all,
       width = 12, height = 5, units = "in", dpi = 300)

ggsave(here("plots/Fig2.pdf"),
       plot = p_att_combined,
       width = 12, height = 5, units = "in", dpi = 300)
# #-----------------------------------------------------------------------------
