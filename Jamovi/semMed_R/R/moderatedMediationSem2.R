

#' Analyze moderated mediation model using SEM
#'
#' @param data data frame
#' @param xvar predictor variable
#' @param mvars vector of names of mediator variables
#' @param yvar dependent variable
#' @param xmmod moderator of effect predictor on mediators
#' @param mymod moderator of effect mediators on dependent variable
#' @param cmvars covariates for mediators
#' @param cyvars covariates for dependent variable
#' @param nboot number of bootstrap samples 
#'
#' @return moderatedMediationSem object
#' @export
#'
#' @examples
#' load("dat1.rda")
#' res <- moderatedMediationSem(dat = dat1, xvar="x1", mvars= c("m1","m2","m3"),
#'        yvar = "y1", xmmod = "mod1", mymod= "bimod2",
#'        cmvars =c("c1","c2"), cyvars =c("c1","c2"), nboot=50)
#' print(res)
#' plot(res)

moderatedMediationSem <- function(data = NULL,
                                  xvar,
                                  mvars,
                                  yvar,
                                  xmmod = NULL,
                                  mymod = NULL,
                                  cmvars = NULL,
                                  cyvars = NULL,
                                  nboot = 1000) {

  res <- list(input = as.list(environment()),
              intermediate = list(),
              output = list());

  res$intermediate$dataName <- as.character(deparse(substitute(data)));

  res$intermediate$numberOfMediators <-
    nm <- length(mvars);

  ## check if there is a moderator for the x - m path
  if (!is.null(xmmod)) {
    if (length(xmmod) > 1) {
      stop("This function can only handle one moderator for the x-m path. You provided ", length(xmmod),
           " (argument 'xmmod' contained ", vecTxtQ(xmmod), ").");
    }
    if (is.factor(data[,xmmod])) {
      if (nlevels(data[,xmmod]) > 2) {
        stop("This function can not yet deal with categorical moderators with more than two levels.");
      } else {
        res$intermediate$xdichotomous <-
          xdichotomous <- TRUE;
        data[,"xmodOriginal"] <- data[,xmmod];
        data[,xmmod] <- as.numeric(data[,xmmod]) - 1;
      }
    } else {
      res$intermediate$xdichotomous <-
        xdichotomous <- FALSE;
    }

    xmint <- paste0("xmInteraction",c(1:nm));
    xmterms <- paste0(paste0("data$",xmmod,"*","data$",mvars));

    for (i in 1:nm) {
      data[,xmint[i]] <- eval(parse(text = xmterms[i]));
    }

  }

  ### check if there is a moderator for the m - y path;
  if (!is.null(mymod)) {
    if (length(mymod) > 1) {
      stop("This function can only handle one moderator for the m-y path. You provided ", length(mymod),
           " (argument 'mymod' contained ", vecTxtQ(mymod), ").");
    }
    if (is.factor(data[,mymod])) {
      if (nlevels(data[,mymod]) > 2) {
        stop("This function can not yet deal with categorical moderators with more than two levels.");
      }
      else {
        res$intermediate$ydichotomous <-
          ydichotomous <- TRUE;
        data[,"ymodOriginal"] <- data[,mymod];
        data[,mymod] <- as.numeric(data[,mymod]) - 1;
      }
    } else {
      res$intermediate$ydichotomous <-
        ydichotomous <- FALSE;
    }

    myint <- paste0("myInteraction",c(1:nm));
    myterms <- paste0(paste0("data$",mymod,"*","data$",mvars));

    for (i in 1:nm) {
      data[,myint[i]] <- eval(parse(text = myterms[i] ));
    }
  }

  ### Build lavaan model
  res$intermediate$model <-
    buildModMedSemModel(xvar=xvar,
                        mvars= mvars,
                        yvar = yvar,
                        xmmod = xmmod,
                        mymod = mymod,
                        cmvars = cmvars,
                        cyvars = cyvars);
 
  ### Run SEM
  res$intermediate$result <- result <-
    lavaan::sem(res$intermediate$model,
        data=data,
        fixed.x = FALSE,
        std.lv = TRUE,
        se="bootstrap",
        bootstrap=nboot);
  
  
  ### Extract R squared values
  res$output$Rsq <-
    lavaan::inspect(res$intermediate$result, "r2");

  ### Extract parameter estimates for direct effects
  res$intermediate$parameterEstimates <-
    lavaan::parameterestimates(result);
  res$output$parameterEstimates.direct <-
    dplyr::filter(lavaan::parameterestimates(result),
           lhs %in% yvar & rhs %in% xvar)[, -c(1:3)];

  ### ... And for indirect effects
  a2 <- 1:nm;
  ind <- paste0("ind", a2 );
  indinter <- paste0("indinter", a2 );
  res$output$parameterEstimates.indirect.raw <-
    dplyr::filter(lavaan::parameterestimates(result),
           lhs %in% c(ind,indinter, "total"))[, -c(1:3)];

  ### ... And the standardized indirect effects
   aa <- lavaan::lavInspect(result, "std")$beta[yvar,mvars] * lavaan::lavInspect(result, "std")$beta[mvars, xvar];
   names(aa) <- mvars
   res$output$parameterEstimates.indirect.standardized <- aa
  

  
  class(res) <- "moderatedMediationSem";

  return(res);

}



#' print method of object of class moderatedMediationSem
#'
#' @param x object of class moderatedMediationSem
#' @param ... 
#' @param digits  number of digits  
#'
#' @export
#'
print.moderatedMediationSem <- function(x, ..., digits=2) {

  cat("###   The model contains", length(x$input$mvars),"mediators:",x$input$mvars, "
      and",length(x$input$xmmod), "moderators for the x-m path(s):",x$input$xmmod, "
      and",length(x$input$mymod), "moderators for the m-y path(s):",x$input$mymod, "
      and",length(x$input$cmvars), "covariates for the mediators:",x$input$cmvars, "
      and",length(x$input$cyvars), "covariates for the dependent variable:",x$input$cyvars)
  cat("\n\n")
  
  cat("### Explained variance (R-square) of the mediators and dependent variable:\n\n");
  print(x$output$Rsq);
  cat("\n")

  cat("### Direct effect:\n\n");
  print(x$output$parameterEstimates.direct);
  cat("\n")
  
  cat("### Indirect effects (unstandardized):\n\n");
  print(x$output$parameterEstimates.indirect.raw);
  cat("\n")

  cat("### Indirect effects (standardized):\n\n");
  ind <- (as.data.frame(x$output$parameterEstimates.indirect.standardized));
  colnames(ind) <- "ind"
  print(ind)
  cat("\n")

}




#' Makes simple slop plots of moderatedMediationSem object
#'
#' @param x 
#' @param ... 
#' @param digits 
#'
#' @return simple slope plots for each mediator and simple slopes parameter estimates
#' @export
#'
plot.moderatedMediationSem <- function(x,...,digits = 3) {
  
  data <- x$input$data
  xmmod <- x$input$xmmod
  mymod <- x$input$mymod
  xvar <- x$input$xvar
  yvar <- x$input$yvar
  mvars <- x$input$mvars
  parEst <- x$intermediate$parameterEstimates
  
  if ((!length(xmmod)) & (!length(mymod))) 
            return(cat("No plots can be given, because no moderators have been specified"))
  
  
  ## test if moderator exists for x=m path and if it is dichotomous factor
  if (length(xmmod)) {
    xdichotomous <- FALSE
    if (is.factor(data[,xmmod])) {
      if (length(levels(data[,xmmod])) > 2) {
        stop("This function can not yet plot moderation with a moderator (x-m path) that is a factor with more than two levels.");
      }
      else {
        xmodLevels <- levels(data[,xmmod]);
        data[,xmmod] <- as.numeric(data[,xmmod]) - 1;
        xdichotomous <- TRUE;
      }
    }
    simpleSlopes(data=data, xvar="x1", yvar = "y1", mod = xmmod, mvars = mvars, parEst = parEst, vorw = "w", 
                 int = "im",vdichotomous = xdichotomous, modLevels = ymodLevels, path = "x-m")
  }
  
  ## test if moderator exists for m=y path and if it is dichotomous factor
  
  if (length(mymod)) {
    ydichotomous <- FALSE
    if (is.factor(data[,mymod])) {
      if (length(levels(data[,mymod])) > 2) {
        stop("This function can not yet plot moderation with a moderator (x-y path) that is a factor with more than two levels.")}
      else {
        ymodLevels <- levels(data[,mymod]);
        data[,mymod] <- as.numeric(data[,mymod]) - 1;
        ydichotomous <- TRUE;
      }
    }
    simpleSlopes(data=data, xvar="x1", yvar = "y1", mod = mymod, mvars = mvars, parEst = parEst, vorw = "v", 
                 int = "iy",vdichotomous = ydichotomous, modLevels = ymodLevels, path = "m-y")
  }
  
  return(cat("**"))
  
}  # end function
 






