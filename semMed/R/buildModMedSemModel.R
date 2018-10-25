

#' Builds model for moderated mediation anaysis using SEM

#' @param xvar independent variable (predictor)
#' @param mvars vector of names of mediators
#' @param yvar dependent variable
#' @param xmmod moderator of a path(s)
#' @param mymod moderator of b path(s)
#' @param cmvars covariates for predicting the mediators
#' @param cyvars covariates for predicting the dependent variable
#' @export
#' @return lavaan model to be used in moderatedMediationSem
#' @examples model <- buildModMedSemModel(xvar="x1",  yvar = "y1", mvars= c("m1","m2","m3"),xmmod = "mod1",
#'           mymod = "mod2",cmvars = "c1",cyvars = "c2")
buildModMedSemModel <- function(xvar,
                                mvars,
                                yvar,
                                xmmod = NULL,
                                mymod = NULL,
                                cmvars = NULL,
                                cyvars = NULL) {

  nm <- length(mvars);
  ncm <- length(cmvars);
  ncy <- length(cyvars);

  ### first index predictor in x - m path (= 1 because only one predictor)
  a1 <- 1:length(xvar);

  ### second indices mediators for x - m paths
  a2 <- 1:nm;

  ### second index predictor in m - y path (= 1 because only one dependent)
  b2 <- 1:length(yvar);

  ### first indices mediators for m - y paths
  b1 <- a2;

  c1 <- 1:length(cmvars);
  c2 <- 1:length(cyvars);
  h <- as.vector(outer(1:nm, 1:ncm, paste0));

  ######################################################################
  ### naming the parameters
  ######################################################################

  ### path from x to m
  a <- paste0("a",a1,a2)

  ### path from moderator*x to m
  if (!is.null(xmmod)) {
    w1 <- paste0("w",a2) 
    w2 <- paste0("im",a2)
  }

  ### path from m to y
  b <- paste0("b",b1,b2)

  ### path from moderator*m to y
  if(!is.null(mymod)) {
    v1 <- paste0("v",1)
    v2 <- paste0("iy",a2)
  }

  ### path from x to y
  c <- paste0("c",a1,b2);

  ### path from c1 to m
  d <- paste0("d",h);

  ### path from c2 to y
  f <- paste0("f",c2,b2);

  ### construct covariances between covariates for M
  model_cov1 <- " ";
  if (length(cmvars) > 1) {
    ha <- expand.grid(cmvars,cmvars);
    hb <- matrix(data=c(1:(ncm**2)),nrow=ncm,ncol=ncm);
    s <- as.vector(lower.tri(hb));
    ha <- ha[s,];
    model_cov1 <- paste0(ha[,1], " ~~ " , ha[,2], collapse = " \n ");
  }

  ### construct covariances between mediators and covariates for Y
  model_cov2 <- " ";
  vars <- c(mvars, cyvars);
  if (length(vars) > 1) {
    nmy <- nm + ncy;
    ha <- expand.grid(vars,vars);
    hb <- matrix(data=c(1:(nmy**2)),nrow=nmy,ncol=nmy);
    s <- as.vector(lower.tri(hb));
    ha <- ha[s,];
    model_cov2 <- paste0(ha[,1], " ~~ " , ha[,2], collapse = " \n ");
  }

  ### indirect effects
  ind <- paste0("ind", a2 );
  modmedx <- paste0("modmedx", a2 );
  modmedm <- paste0("modmedm", a2 );

  ### initialize path from mod on x - m path
  modela2 <- " ";

  ### initialize path from mod on m - y path
  modelb2 <- " ";

  ### initialize path from mod on  m path
  modelw <- " ";

  ### initialize path from int on m path
  modelw2 <- " ";

  ### initialize path from mod on  m path
  modelv <- " ";

  ### initialize path from int on m path
  modelv2 <- " "

  ### initialize indirect effects with mod x on  m path
  modeli1 <- " ";

  ### initialize indirect effects with mod m on y path
  modeli2 <- " ";

  #### initialize path from c1 to m
  modeld <- " ";

  ### initialize path from c2 to y
  modelf <- " ";

  ### construct interaction terms

  if(!is.null(xmmod)) {
    xmint <- paste0("xmInteraction",c(1:nm));
  }
  if(!is.null(mymod)) {
    myint <- paste0("myInteraction",c(1:nm));
  }

  modela1 <- paste0(mvars, " ~ " ,a,"*",xvar ,  collapse = " \n ");

  if(!is.null(xmmod)) {
    modelw <- paste0( mvars, " ~ " ,w1,"*",xmmod ,  collapse = " \n ");
    modelw2 <- paste0( mvars, " ~ " ,w2,"*",xmint ,  collapse = " \n ");
  }

  modelb1 <- paste0( yvar, " ~ " ,b,"*",mvars , collapse = " \n ");

  if(!is.null(mymod)) {
    modelv <- paste0( yvar, " ~ " ,v1,"*",mymod , collapse = " \n ");
    modelv2 <- paste0( yvar, " ~ " ,v2,"*",myint , collapse = " \n ");
  }

  modelc <- paste0( yvar, " ~ " ,c,"*",xvar , collapse = " \n ");

  if (!is.null(cmvars)) {
    modeld <- paste0( rep(mvars,ncm), " ~ " ,d,"*",rep(cmvars, each=nm) , collapse = " \n ");
  }
  if (!is.null(cyvars)) {
    modelf <- paste0( yvar, " ~ " ,f,"*",cyvars , collapse = " \n ");
  }

  modeli <- paste0(ind , " := " , a, " * ", b, collapse = " \n ");

  if(!is.null(xmmod)) {
    modeli1 <- paste0(modmedx , " := " , w2, " * ", b, collapse = " \n ");
  }
  if(!is.null(mymod)) {
    modeli2 <- paste0(modmedm , " := " , v2, " * ", a, collapse = " \n ");
  }

  modelt <- paste0("total"," := " , (paste0(ind,  collapse = " + ")));

  model <- paste0(modela1," \n ",modela2," \n ",
                  modelb1," \n ", modelb2," \n ",
                  modelc, " \n ", modeld, " \n ",
                  modelw, " \n ", modelw2, " \n ",
                  modelv, " \n ", modelv2, " \n ",
                  modelf, " \n ",
                  model_cov1," \n ", model_cov2, " \n ",
                  modeli, " \n ", modeli1, " \n ", modeli2, " \n ",
                  modelt);

  return(model)
}







