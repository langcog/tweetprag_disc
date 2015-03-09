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
      data = ms,xlab='log2 (reply level + 1)',ylab='per-word perplexity (bits)',title='per-word') +
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
      data = ms,xlab='log2 (reply level + 1)',ylab='per-tweet perplexity (bits)') +
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

###################################################################
# Mentions

maxmentions <- 6
break_list <- -1:maxmentions

fdf$visibility <- ifelse(fdf$invisible,'invisible','visible')

mss <- fdf %>% 
  filter(words>0) %>%
  filter(mentions<maxmentions) %>%
  mutate(binned.mentions = cut(mentions, 
                             breaks = break_list, 
                             labels = break_list[2:length(break_list)],
                             include.lowest=F)) %>%
  filter(!is.na(binned.mentions)) %>%
  mutate(binned.mentions = as.numeric(as.character((binned.mentions)))) %>%
  group_by(binned.mentions, uid, visibility) %>%
  summarise(n = n(), 
            ci = NA,
            lppl = mean(lppl))

ms <- mss %>%
  group_by(binned.mentions,visibility) %>%
  summarise(ci = sem(lppl)*1.96,
            lppl = mean(lppl), 
            n = sum(n))

pdf("../figures/cmcl-mentions-pw.pdf", width=5, height=3.5)

qplot(binned.mentions, lppl, 
      group = visibility, col = visibility, 
      ymin = lppl - ci, ymax = lppl + ci, 
      position = position_dodge(width = .1), 
      geom="pointrange",
      group = as.factor(binned.mentions),
      #col = as.factor(binned.mentions),
      data = ms,xlab='# of mentions',ylab='per-word perplexity',title='per-word') +
  stat_smooth(data= mss,method='lm', formula=y~poly(x,2),
              aes(x = binned.mentions, 
                  y = lppl, 
                  group = visibility)) + theme_classic() #+ scale_color_discrete(name='visibility')
  
dev.off()

# per-tweet mentions

mss <- fdf %>% 
  filter(words>0) %>%
  filter(mentions<maxmentions) %>%
  mutate(binned.mentions = cut(mentions, 
                             breaks = break_list, 
                             labels = break_list[2:length(break_list)],
                             include.lowest=F)) %>%
  filter(!is.na(binned.mentions)) %>%
  mutate(binned.mentions = as.numeric(as.character((binned.mentions)))) %>%
  group_by(binned.mentions, uid, visibility) %>%
  summarise(n = n(), 
            ci = NA,
            lppl = mean(words*lppl))

ms <- mss %>%
  group_by(binned.mentions,visibility) %>%
  summarise(ci = sem(lppl)*1.96,
            lppl = mean(lppl), 
            n = sum(n))

pdf("../figures/cmcl-mentions-pt.pdf", width=5, height=3.5)

qplot(binned.mentions, lppl, 
      group = visibility, col = visibility, 
      ymin = lppl - ci, ymax = lppl + ci, 
      position = position_dodge(width = .1), 
      geom="pointrange",
      group = as.factor(binned.mentions),
      #col = as.factor(binned.mentions),
      data = ms,xlab='# of mentions',ylab='per-tweet perplexity',title='per-tweet') +
  stat_smooth(data= mss,method='lm', formula=y~poly(x,2),
              aes(x = binned.mentions, 
                  y = lppl, 
                  group = visibility)) + theme_classic() #+ scale_color_discrete(name='visibility')
  
dev.off()

########################################################
# Numbers of tweets

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
  group_by(binned.rlevel, seed) %>%
  summarise(n = n(), 
            ci = NA,
            lppl = mean(lppl))

#########################################################
# Redoing number of mentions to have same data as rlevel/maxdesc tests

maxmentions <- 6
break_list <- 0:maxmentions

fdf$visibility <- ifelse(fdf$invisible,'invisible','visible')

mss <- fdf %>% 
  filter(!is.na(rlevel)) %>%
  filter(words>0) %>%
  filter(maxdesc<maxrlevel) %>%
  filter(rlevel<maxrlevel) %>%
  filter(mentions<maxmentions) %>%
  filter(mentions>0) %>%
  mutate(binned.mentions = cut(mentions, 
                             breaks = break_list, 
                             labels = break_list[2:length(break_list)],
                             include.lowest=F)) %>%
  filter(!is.na(binned.mentions)) %>%
  mutate(binned.mentions = as.numeric(as.character((binned.mentions)))) %>%
  group_by(binned.mentions, uid, visibility) %>%
  summarise(n = n(), 
            ci = NA,
            lppl = mean(lppl))

ms <- mss %>%
  group_by(binned.mentions,visibility) %>%
  summarise(ci = sem(lppl)*1.96,
            lppl = mean(lppl), 
            n = sum(n))

pdf("../figures/cmcl-mentions-pw2.pdf", width=5, height=3.5)

qplot(log2(binned.mentions), lppl, 
      group = visibility, col = visibility, 
      ymin = lppl - ci, ymax = lppl + ci, 
      position = position_dodge(width = .1), 
      geom="pointrange",
      group = as.factor(binned.mentions),
      #col = as.factor(binned.mentions),
      data = ms,xlab='log2 (# of mentions)',ylab='per-word perplexity (bits)',title='per-word') +
  stat_smooth(data= mss,method='lm',
              aes(x = log2(binned.mentions), 
                  y = lppl, 
                  group = visibility)) + theme_classic() #+ scale_color_discrete(name='visibility')
  
dev.off()

  
