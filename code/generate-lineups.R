get.aes <- function(r){
  c("Color", "Shape")[which(as.logical(r[1:2]))]
}

# function to create a list of chosen statistics
get.stats <- function(r){
  c("Reg. Line", "Error Bands", "Ellipses")[which(as.logical(r[3:5]))]
}

gen.plot <- function(dd, aes, stats, colorp=NULL, shapep=NULL){
  pointsize <- 1.5
  if(is.null(colorp)) colorp <- best.combo(length(unique(dd$group)), colors, colortm)
  if(is.null(shapep)) shapep <- best.combo(length(unique(dd$group)), shapes, shapetm)
  
  plot <- ggplot(data=dd, aes(x=x, y=y)) + theme_lineup() + facet_wrap(~.sample, ncol=5)
  
  # lines and ellipses are not data structure, so reduce contrast to emphasize points!
  # Set other geoms/aids
  if("Reg. Line"%in%stats){
    plot <- plot + geom_smooth(method="lm", color="grey30", se=F, fullrange=TRUE)
  }
  if("Error Bands"%in%stats){
    #     xrange <- range(dd$x)
    tmp <- dd %>% group_by(.sample) %>% do({
      model <- lm(y~x, data=.)
      range <- diff(range(.$x))
      newdata <- data.frame(x=seq(min(dd$x)-.1*range, max(dd$x)+.1*range, length.out=400))
      data.frame(.sample=unique(.$.sample), x=newdata$x,
                 predict.lm(model, newdata=newdata, interval="prediction", level=0.9))
    })
    if("Shade Error Bands"%in%stats & "Error Bands"%in%stats){
      plot <- plot +
        geom_line(data=tmp, aes(x=x, y=lwr), linetype=2, inherit.aes=F) +
        geom_line(data=tmp, aes(x=x, y=upr), linetype=2, inherit.aes=F) +
        geom_ribbon(data=tmp, aes(x=x, ymin=lwr, ymax=upr), fill="black", color="transparent", alpha=.1, inherit.aes=F)
    } else {
      plot <- plot +
        geom_line(data=tmp, aes(x=x, y=lwr), linetype=2, inherit.aes=F) +
        geom_line(data=tmp, aes(x=x, y=upr), linetype=2, inherit.aes=F)
    }
  }
  
  if("Ellipses"%in%stats){
    if("Color"%in%aes){
      if("Shade Ellipses"%in%stats){
        plot <- plot + stat_ellipse(geom="polygon", level=.9, aes(fill=factor(group), colour=factor(group))) + #, alpha=0.1) +
          scale_fill_manual(values=colorp)
      } else {
        plot <- plot + stat_ellipse(geom="polygon", level=.9, aes(colour=factor(group)), fill="transparent") #, alpha=0.2)
      }
    } else if("Shape"%in%aes){
      plot <- plot + stat_ellipse(geom="polygon", level=.9, aes(group=factor(group)),
                                  colour="grey15", fill="transparent")
    } else {
      plot <- plot + stat_ellipse(geom="polygon", level=.9, aes(group=factor(group)),
                                  colour="grey15", fill="transparent")
    }
  }
  
  if("Shade Ellipses"%in%stats & "Ellipses" %in% stats){
    plot <- plot + stat_ellipse(geom="polygon", level=.9, aes(group=factor(group)), alpha=0.1, fill="black", color="transparent")
  }
  
  # points on top of everything
  # Set Aesthetics
  if(length(aes)==0){
    plot <- plot + geom_point(size=pointsize, shape=1) +
      scale_shape_discrete(solid=F)
  } else if(length(aes)==1){
    if("Color"%in%aes){
      plot <- plot + geom_point(aes(color=factor(group)), size=pointsize, shape=1) +
        scale_color_manual(values=colorp)
    } else {
      plot <- plot + geom_point(aes(shape=factor(group)), size=pointsize) +
        scale_shape_manual(values=shapep)
    }
  } else {
    plot <- plot + geom_point(aes(color=factor(group), shape=factor(group)), size=pointsize) +
      scale_color_manual(values=colorp) +
      scale_shape_manual(values=shapep)
  }
  
  
  plot
}

save.pics <- function(df, datastats, plotparms, plotname, palname = NULL, colorp = "", shapep = "", testplot=FALSE){
  if(is.null(shapep) & is.null(colorp)){
    plotobj <- gen.plot(df, aes=get.aes(plotparms), stats=get.stats(plotparms))
  } else if(is.null(colorp)){
    plotobj <- gen.plot(df, aes=get.aes(plotparms), stats=get.stats(plotparms), shapep = shapep)
  } else if(is.null(shapep)){
    plotobj <- gen.plot(df, aes=get.aes(plotparms), stats=get.stats(plotparms), colorp = colorp)
  } else {
    plotobj <- gen.plot(df, aes=get.aes(plotparms), stats=get.stats(plotparms), colorp = colorp, shapep = shapep)
  }
  
  plotobj <- plotobj + theme(aspect.ratio = 1)
  plotobj  
}

#----- Plot Generation (Actual Plots) ----
load("./data/DataWithNewGroups.RData")


plot.opts <- data.frame(expand.grid(i=unique(data$set), j=1:10))

plot.names <- c("plain","color", "shape", "colorShape", "colorEllipse", "colorShapeEllipse", "trend", "trendError", "colorTrend", "colorEllipseTrendError")
plot.parms <- expand.grid(
  color = c(0,1),
  shape = c(0,1),
  reg = c(0,1),
  err = c(0,1),
  ell = c(0,1)
)[c(
  1, # control
  2, # color
  3, # shape
  4, # color + shape
  18, # color + ellipse
  20, # color + shape + ellipse
  5, # trend
  13, # trend + error
  6, # color + trend
  30 # color + ellipse + trend + error
),]

generateLineup <-function(data, type, colorp = "", shapep = "", params) {

  j <- which(plot.names == type)
  save.pics(data, datastats=params, plotparms=plot.parms[j,], plotname=plot.names[j], colorp = colorp, shapep = shapep)
}


