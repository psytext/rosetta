

#' Makes simple slop plots of moderatedMediationSem object
#'
#' @param data data frame containg the variables of the model 
#' @param xvar predictor variable name
#' @param yvar depedendent variable name
#' @param mod moderator name
#' @param mvars vector of mediators names
#' @param parEst parameter estimates from lavaan results
#' @param vorw character ("v" or "w") indicating for which path the simple slopes should be computed.
#' @param digits number of printed digits
#' @import ggplot2
#' @return empty, directly plots all simple slopes and all indices of mediation
#' @export
#'
simpleSlopes <- function(data,xvar,yvar,mod, mvars, parEst, vorw, int, vdichotomous,
                              modLevels, path = NULL, digits = 3) {
  
  xmin <- min(data[,xvar], na.rm = TRUE)
  xmax <- max(data[,xvar], na.rm = TRUE)
  miny <- min(data[,yvar], na.rm = TRUE)
  maxy <- max(data[,yvar], na.rm = TRUE)
  
  # compute simple slopes 
  
    if (vdichotomous) {
      modmin <- 0;
      modmax <- 1
    } else {
      modmin <- mean(data[,mod]) - sd(data[,mod]);
      modmax <- mean(data[,mod]) + sd(data[,mod]);
    }
  
  a <- subset(parEst, grepl("a1", parEst$label))[,"est"]
  b <- subset(parEst, grepl("b", parEst$label))[,"est"]
  vw <- subset(parEst, grepl(vorw, parEst$label))[,"est"]
  int <- subset(parEst, grepl(int, parEst$label))[,c("ci.lower","est","ci.upper")]
  
  if (vorw == "v") vw <- rep(vw,length(mvars))

  title <- paste0("Simple slopes in ", path , " path for indirect effect ")
  
  cat("\n","Simple slopes in ", path , " path(s) for indirect effect ", "\n")
  cat(" ---------------------------------------------", "\n" );

  for (i in 1:length(mvars)) {
    
    incmin <- vw*modmin
    slopemin <- a[i]*b[i] + vw[i]*int[i,]*modmin
    incmax  <- vw*modmax
    slopemax <- a[i]*b[i] + vw[i]*int[i,]*modmax

    if (vdichotomous) {
      legendLabel <- modLevels 
      maxLab <- max(stringr::str_length(modLevels[1]),stringr::str_length(modLevels[2]))
      minLabel <- stringr::str_pad(modLevels[1],maxLab, pad = " ")
      maxLabel <- stringr::str_pad(modLevels[2],maxLab, pad = " ")
    }
    else {
      legendLabel <- c("1 SD below mean", "1 SD above mean") 
      minLabel <- c("for 1 sd below mean of moderator: ")
      maxLabel <- c("for 1 sd above mean of moderator: ")
      data$moderator <- data[,mod]
      
      data$indexOfMed1 <- a[i]*b[i] + vw[i]*int[i,2]*data[,mod] 
      
      plot_indexOfmediation <- ggplot(data, aes(x=data$moderator,y=data$indexOfMed1)) +
        geom_point(size=.5) + geom_line() +
        coord_cartesian(ylim=c(-0.5, 0.5)) + 
        scale_y_continuous(breaks=seq(-0.5, 0.5, 0.1)) +
        ggtitle(paste0("Index of mediation for ",mvars[i])) +
        ylab("Index of Mediation") +
        xlab(paste0("Moderator: ",mod))
      
      plot(plot_indexOfmediation)
    }
    
    cat( paste0("\n", "through mediator: ", mvars[i]), "\n");
    tableRes <- matrix(c(minLabel,round(sort(as.numeric(slopemin)), digits = digits), 
                         maxLabel, round(sort(as.numeric(slopemax)), digits = digits)), 
                         nrow = 2, byrow = TRUE)
    colnames(tableRes) <- c("label", "ci.lower", "est", "ci.upper")
    pander::pander(tableRes)
    
    pred <- rep(0,4) 
    pred[1] <- incmin  + slopemin[2] * xmin;
    pred[2] <- incmin + slopemin[2] * xmax;
    pred[3] <- incmax  + slopemax[2] * xmin;
    pred[4] <- incmax  + slopemax[2] * xmax
    pred <- unlist(pred)
    
    plotdat1 <- as.data.frame(cbind(pred, c(xmin,xmax,xmin,xmax), c(modmin, modmin, modmax, modmax)))
    colnames(plotdat1) <- c(yvar, xvar, mod)
    plotdat1[,mod] <- as.factor(plotdat1[,mod])
    
    plot_simpleSlopes <- ggplot(plotdat1, aes(x=plotdat1[,xvar],y=plotdat1[,yvar], 
                                                colour=plotdat1[,mod],group = plotdat1[,mod])) + 
      geom_point() + geom_line() +
      labs(x = xvar, y = yvar) +
      ylim(min(miny,min(plotdat1[,1])), max(maxy,max(plotdat1[,1]))) +
      ggtitle(paste0(title, mvars[i])) +
      theme(plot.title = ggplot2::element_text(lineheight=.8, face="bold")) +
      scale_colour_discrete(name  = mod, labels=legendLabel)
    
     plot(plot_simpleSlopes)
   
     } 
  
  return()

} # end function 





