library(data.table)
library(ggplot2)
library(lme4)
library(bit64)

fdf <- fread('../cmcl-data-formike.csv',sep=',',header=T)
#fdf <- fread('/scratch/users/gdoyle/cmcl-data-formike.csv',sep=',',header=T)

#Adding audience information
fdf$audience <- factor(NA,levels=c('invisible','\nbaseline','hashtagged'),exclude=NULL)
fdf$audience[(fdf$mentions>0)&(fdf$invisible==1)&(fdf$hashtags==0)] <- 'invisible'
fdf$audience[(fdf$mentions==0)&(fdf$invisible==0)&(fdf$hashtags>0)] <- 'hashtagged'
fdf$audience[(fdf$mentions==0)&(fdf$invisible==0)&(fdf$hashtags==0)] <- '\nbaseline'

library(dplyr)

maxrlevel <- 6
break_list <- -1:maxrlevel
sem <- function(x) { return(sqrt(var(x)/length(x))) }

#Simpson's paradox per-word

mss <- fdf %>% 
  filter(!is.na(rlevel)) %>%
  filter(words>0) %>%
  filter(maxdesc<maxrlevel) %>%
  filter(rlevel<maxrlevel) %>%
  mutate(binned.rlevel = cut(rlevel, 
                             breaks = break_list, 
                             labels = break_list[2:length(break_list)],
                             include.lowest=F)) %>%
  filter(!is.na(binned.rlevel)) %>%
  mutate(binned.rlevel = as.numeric(as.character((binned.rlevel)))) %>%
  mutate(binned.maxdesc = cut(maxdesc, 
                             breaks = break_list, 
                             labels = break_list[2:length(break_list)],
                             include.lowest=F)) %>%
  filter(!is.na(binned.maxdesc)) %>%
  mutate(binned.maxdesc = as.numeric(as.character((binned.maxdesc)))) %>%
  group_by(binned.rlevel, uid, binned.maxdesc) %>%
  summarise(n = n(), 
            ci = NA,
            lppl = mean(lppl))

ms <- mss %>%
  group_by(binned.rlevel,binned.maxdesc) %>%
  summarise(ci = sem(lppl)*1.96,
            lppl = mean(lppl), 
            n = sum(n))

pdf("../figures/cmcl-rlevel-pw.pdf", width=5, height=3.5)

qplot(log2(binned.rlevel+1), lppl, 
      #group = terminal, col = terminal, 
      ymin = lppl - ci, ymax = lppl + ci, 
      position = position_dodge(width = .1), 
      geom="pointrange",
      group = as.factor(binned.maxdesc),
      col = as.factor(binned.maxdesc),
      data = ms,xlab='log2 of reply level',ylab='per-word perplexity',title='per-word') +
  geom_smooth(data= mss,method='lm',
              aes(x = log2(binned.rlevel+1), 
                  y = lppl, 
                  group = binned.maxdesc)) + theme_classic() + scale_color_discrete(name='maximum\ndepth')
                  
dev.off()

# Simpson's paradox per-tweet

mss <- fdf %>% 
  filter(!is.na(rlevel)) %>%
  filter(words>0) %>%
  filter(maxdesc<maxrlevel) %>%
  filter(rlevel<maxrlevel) %>%
  mutate(binned.rlevel = cut(rlevel, 
                             breaks = break_list, 
                             labels = break_list[2:length(break_list)],
                             include.lowest=F)) %>%
  filter(!is.na(binned.rlevel)) %>%
  mutate(binned.rlevel = as.numeric(as.character((binned.rlevel)))) %>%
  mutate(binned.maxdesc = cut(maxdesc, 
                             breaks = break_list, 
                             labels = break_list[2:length(break_list)],
                             include.lowest=F)) %>%
  filter(!is.na(binned.maxdesc)) %>%
  mutate(binned.maxdesc = as.numeric(as.character((binned.maxdesc)))) %>%
  group_by(binned.rlevel, uid, binned.maxdesc) %>%
  summarise(n = n(), 
            ci = NA,
            lppl = mean(words*lppl))

ms <- mss %>%
  group_by(binned.rlevel,binned.maxdesc) %>%
  summarise(ci = sem(lppl)*1.96,
            lppl = mean(lppl), 
            n = sum(n))

pdf("../figures/cmcl-rlevel-pt.pdf", width=5, height=3.5)

qplot(log2(binned.rlevel+1), lppl, 
      #group = terminal, col = terminal, 
      ymin = lppl - ci, ymax = lppl + ci, 
      position = position_dodge(width = .1), 
      geom="pointrange",
      group = as.factor(binned.maxdesc),
      col = as.factor(binned.maxdesc),
      data = ms,xlab='log2 of reply level',ylab='per-tweet perplexity') +
  geom_smooth(data= mss,method='lm',
              aes(x = log2(binned.rlevel+1), 
                  y = lppl, 
                  group = binned.maxdesc)) + theme_classic() + scale_color_discrete(name='maximum\ndepth')
                  
dev.off()

#########################################
# Audience size (reply/baseline/hashtagged)

fdf$audience <- NULL
fdf$audience <- factor(NA,levels=c('invisible','\nbaseline','hashtagged'),exclude=NULL)
fdf$audience[(fdf$mentions>0)&(fdf$invisible==1)&(fdf$hashtags==0)] <- 'invisible'
fdf$audience[(fdf$mentions==0)&(fdf$invisible==0)&(fdf$hashtags>0)] <- 'hashtagged'
fdf$audience[(fdf$mentions==0)&(fdf$invisible==0)&(fdf$hashtags==0)] <- '\nbaseline'

mss <- fdf %>% 
  filter(!is.na(audience)) %>%
  filter(words>0) %>%
  group_by(audience, uid) %>%
  summarise(n = n(), 
            ci = NA,
            lppl = mean(lppl))

ms <- mss %>%
  summarise(ci = sem(lppl)*1.96,
            lppl = mean(lppl), 
            n = sum(n))

pdf("../figures/cmcl-audience-pw.pdf", width=2.5, height=3.5)

qplot(audience, lppl, 
      #group = terminal, col = terminal, 
      ymin = lppl - ci, ymax = lppl + ci, 
      geom="pointrange",
      data = ms,xlab='tweet type',ylab='per-word perplexity',title='per-word') +
      theme_classic()
                  
dev.off()

#Per-tweet

mss <- fdf %>% 
  filter(!is.na(audience)) %>%
  filter(words>0) %>%
  group_by(audience, uid) %>%
  summarise(n = n(), 
            ci = NA,
            lppl = mean(words*lppl))

ms <- mss %>%
  summarise(ci = sem(lppl)*1.96,
            lppl = mean(lppl), 
            n = sum(n))

pdf("../figures/cmcl-audience-pt.pdf", width=2.5, height=3.5)

qplot(audience, lppl, 
      #group = terminal, col = terminal, 
      ymin = lppl - ci, ymax = lppl + ci, 
      geom="pointrange",
      data = ms,xlab='tweet type',ylab='per-tweet perplexity',title='per-word') +
      theme_classic()
                  
dev.off()

