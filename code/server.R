##############################################################
# Starting from the file for Mike

library(data.table)
library(ggplot2)
library(dplyr)
library(lme4)
library(bit64)
library(shiny)

#fdf <<- fread('data-vis-4.csv',sep=',',header=T)
#fdf <- fread('../data/data-vis-4.csv',sep=',',header=T)
# df <- filter(fdf, root %in% as.integer64("558770074945744896"))


#fdf <- fdf %>% 
#  filter(!is.na(rlevel), 
#         maxdesc > 3)

get_end <- function(pos, tid, treplyid) {
  out <- array(NA)
  for (i in 2:length(pos)) {
    tryCatch(out[i] <- pos[tid == treplyid[i]],error=function(e) {print(c("Root",as.character(tid[1]),"not available")); return(NA)})
  }
  return(as.double(out))
}


get_xpos <- function(tid, treplyid, lppl) {
 x_pos <- array(lppl[1],1) 
 
 for (i in 2:length(tid)) {
   if (treplyid[i]!='9218868437227407266') {
     tryCatch(x_pos[i] <- x_pos[tid == treplyid[i]] + lppl[i],error=function(e) {print(c("Root",as.character(tid[1]),"not available")); return(NA)})
   }
 }
 
 #If there are any gaps (NAs from above) in the perplexity calculation or less than 3 perplexities, make it all NAs
 if (sum(is.na(x_pos))>0|sum(!is.na(x_pos))<3) {
   x_pos <- array(NA,length(tid))
 }
 
 return(as.double(x_pos))
  
}

get_partnum <- function(uid) {
  return(match(uid,unique(uid)))
}

get_match <- function(var, tid, treplyid) {
  out <- array(NA)
  for (i in 2:length(var)) {
    tryCatch(out[i] <- var[tid == treplyid[i]]==var[i],error=function(e) {print(c("Root",as.character(tid[1]),"not available")); return(NA)})
  }
  return(as.logical(out))
}

shinyServer(function(input, output, session) {
  
  observeEvent(input$redraw, function() {
    #maxdescrange <- seq(input$maxdescs[1],input$maxdescs[2])
    #tdf <- fdf[maxdesc>=input$maxdescs[1]&maxdesc<=input$maxdescs[2],]
    #utr <- unique(tdf$root)
    
    updateSelectizeInput(session,inputId = "root",
                         selected = as.character(utr)[sample(1:length(utr),input$numtrees,replace=F)])    
  })
  
  observeEvent(input$maxdescs, function() {
    maxdescrange <- seq(input$maxdescs[1],input$maxdescs[2])
    tdf <- fdf[maxdesc>=input$maxdescs[1]&maxdesc<=input$maxdescs[2],]
    utr <<- unique(tdf$root)
    
    updateSelectizeInput(session,inputId = "root", 
                         choices = as.character(utr),
                         selected = as.character(utr)[sample(1:length(utr),input$numtrees,replace=F)])
    
    updateSliderInput(session,inputId="numtrees",max=min(20,length(utr)))
  })
  
  observeEvent(input$numtrees, function() {
    if(!exists("utr")) {
      maxdescrange <- seq(input$maxdescs[1],input$maxdescs[2])
      tdf <- fdf[maxdesc>=input$maxdescs[1]&maxdesc<=input$maxdescs[2],]
      utr <- unique(tdf$root)
    }
    
    updateSelectizeInput(session,inputId = "root", 
                         choices = as.character(utr),
                         selected = as.character(utr)[sample(1:length(utr),input$numtrees,replace=F)])
  })
  
  output$tree <- renderPlot({ 
    
    df <- filter(fdf, root %in% as.integer64(input$root))
    
    df$xvar <- switch(input$tweetvar,"pw"=df$lppl,"pt"=df$lppl*df$words,"w"=df$words)
    
    #svar <- switch()
    #slabel <- switch()
  
    df <- df %>%
      group_by(root) %>%
      mutate(x_pos = get_xpos(tid, treplyid, xvar),
             x_end = get_end(x_pos, tid, treplyid),
             y_pos = seq(0, -n() + 1, -1), 
             y_end = get_end(y_pos, tid, treplyid),
             partnum = get_partnum(uid)) %>%
      ungroup() %>%
      filter(!is.na(x_pos)) %>%
      mutate(uid = as.factor(as.character(uid)))
    
    if(nrow(df)>0) {
    if (input$plottype=='multi') {
    q <- qplot(x_pos, y_pos, size=xvar, color=uid, data=df) + 
      geom_segment(aes(x = x_pos, 
                       y = y_pos, 
                       xend = x_end, 
                       yend = y_end, size=1),
                   data = df[treplyid!='9218868437227407266',]) + 
      ylab("Tweets Sorted by Time") + 
      labs(color="User ID") +
      #ylim(c(-10,.5)) + 
      facet_wrap(~root)
    } else if (input$plottype=='single') {
      q <- qplot(x_pos, -rlevel, size=xvar, color=partnum, data=df) + 
        geom_segment(aes(x = x_pos, 
                         y = -rlevel, 
                         xend = x_end, 
                         yend = ifelse(rlevel>0,-rlevel+1,NA), size=1),
                     data = df[treplyid!='9218868437227407266',]) + 
        ylab("Reply Level") +
        labs(color="Participant #") +
        scale_color_gradient(high="#ee0088",low="#00ccee",guide="legend")
        #scale_color_brewer(palette="BrBG")
        #xlab(switch(input$tweetvar,"pw"="Cumulative per-word perplexity","pt"="Cumulative per-tweet perplexity","w"="Cumulative words")) +
        #theme_bw() #+ 
        #ylim(c(-10,.5)) 
    }
    
    q <- q +
      xlab(switch(input$tweetvar,"pw"="Cumulative per-word perplexity","pt"="Cumulative per-tweet perplexity","w"="Cumulative words")) +
      theme_bw() +
      labs(size=switch(input$tweetvar,"pw"="per-word\nentropy","pt"="per-tweet\nentropy","w"="# words"))
    
    
    if ("single" %in% input$lines) {
      q <- q + geom_smooth(aes(group=1,size=2),method="lm",se=F,color="red") + guides(linetype=F)
    }
    if ("multi" %in% input$lines) {
      q <- q + geom_smooth(aes(group=root),method="lm",group=df$root,linetype="dashed",se=F,color="red") + guides(linetype=F)
    }
    } else {
      q <- qplot(0,0) + annotate("text", x = 0, y = 0, label = "Error loading this tree.")
    }
    q
#       geom_abline(aes(slope = -max(x_pos) / max(y_pos), intercept = 0), lty = 2)
  })

  output$trans <- renderPlot({
    df <- filter(fdf, root %in% as.integer64(input$root))
    
    df$xvar <- switch(input$tweetvar,"pw"=df$lppl,"pt"=df$lppl*df$words,"w"=df$words)
    
    #svar <- switch()
    #slabel <- switch()
    
    df <- df %>%
      group_by(root) %>%
      mutate(x_pos = get_xpos(tid, treplyid, xvar),
             prevx = get_end(xvar, tid, treplyid),
             same = get_match(uid,tid,treplyid),
             partnum = get_partnum(uid)) %>%
      ungroup() %>%
      filter(!is.na(x_pos)) %>%
      mutate(uid = as.factor(as.character(uid)))
    
    if(nrow(df)>0) {
      if(input$resid) {
        df <- df %>%
          group_by(root) %>%
          mutate(meanx = mean(xvar)) %>%
          ungroup() %>%
          mutate(xvar = xvar - meanx,
                 prevx = prevx-meanx)
      }
      if (input$plottype=='multi') {
        q <- qplot(prevx,xvar, color=same, data=df) +
          labs(color="Same user?") +
          #ylim(c(-10,.5)) + 
          facet_wrap(~root)
      } else if (input$plottype=='single') {
        q <- qplot(prevx,xvar, color=same, data=df) +
          labs(color="Same user?")
        #scale_color_brewer(palette="BrBG")
        #xlab(switch(input$tweetvar,"pw"="Cumulative per-word perplexity","pt"="Cumulative per-tweet perplexity","w"="Cumulative words")) +
        #theme_bw() #+ 
        #ylim(c(-10,.5)) 
      }
      
      if ("single" %in% input$lines) {
        q <- q + geom_smooth(aes(group=1),method="lm",se=F,color="black") + guides(linetype=F)
      }
      if ("multi" %in% input$lines) {
        q <- q + geom_smooth(aes(group=same),method="lm",se=F) + guides(linetype=F)
      }    
      axislabel <- switch(input$tweetvar,"pw"="per-word entropy","pt"="per-tweet entropy","w"="words")
      if (input$resid) {
        axislabel <- paste("residual",axislabel)
      }
      q <- q + xlab(paste("Preceding",axislabel)) + ylab(paste("Current",axislabel)) + theme_bw()
      q
    } else {
      q <- qplot(0,0) + annotate("text", x = 0, y = 0, label = "Error loading this tree.")
    }
  })
})

