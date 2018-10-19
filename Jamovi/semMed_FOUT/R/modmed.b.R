
# This file is a generated template, your changes will not be overwritten

modmedClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "modmedClass",
    inherit = modmedBase,
    private = list(
        .run = function() {

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)
          
          # read the option values into shorter variable names
          
          dep  <- self$options$dep
          meds <- self$options$meds
          pred <- self$options$pred
          xmmod <- self$options$xmmod
          mymod <- self$options$mymod
          covsm <- self$options$covsm
          covsy <- self$options$covsy
          
          # get the data
          
          data <- self$data
          
          # convert to appropriate data types
          
          data[[dep]] <- jmvcore::toNumeric(data[[dep]])
          data[[pred]] <- jmvcore::toNumeric(data[[pred]])
          
          for (med in meds)
            data[[med]] <- jmvcore::toNumeric(data[[med]])
          
          if (!is.null(covsm)) { 
             for (covm in covsm)
             data[[covm]] <- jmvcore::toNumeric(data[[covm]])
          }
          
          if (!is.null(covsy)) { 
             for (covy in covsy)
             data[[covy]] <- jmvcore::toNumeric(data[[covy]])
          }
          
          # data is now all of the appropriate type we can begin!
          
          data <- na.omit(data)
          
          if (self$results$isFilled())
            return()
          
          ready <- ! is.null(self$options$dep) &&  ! is.null(self$options$pred) &&  ! is.null(self$options$meds)
          
          if (ready) {
            
            #data <- private$.cleanData()
            results <- private$.compute(data)
            
            # private$.populateMedTable(results)
            # private$.populatePathsTable(results)
            # private$.prepareEstPlot(results)
        }
        
        }, # end run
        
        #### Compute results ----
        .compute = function(data) {
          
          
          model <- private$.buildModMedSemModel()
          se <- self$options$estMethod
          bootstrap <- self$options$bootstrap
           
          suppressWarnings({
             
          fit <- lavaan::sem(model = model, data=data, se=se, bootstrap=bootstrap)
             
          })   # suppressWarnings
          
          return(list('fit'=fit))
           
        },
        
        #### Build model
        .buildModMedSemModel = function() {
          
          dep <- self$options$dep
          pred <- self$options$pred
          meds <- self$options$meds
          xmmod <- self$options$xmmod
          mymod <- self$options$mymod
          covsm <- self$options$covsm
          covsy <- self$options$covsy
          
          nm <- length(meds)
          ncm <- length(covsm)
          ncy <- length(covsy)
          
          ### first index predictor in x - m path (= 1 because only one predictor)
          a1 <- 1:length(pred);
          
          ### second indices mediators for x - m paths
          a2 <- 1:nm;
          
          ### second index predictor in m - y path (= 1 because only one dependent)
          b2 <- 1:length(dep);
          
          ### first indices mediators for m - y paths
          b1 <- a2;
          
          c1 <- 1:length(covsm);
          c2 <- 1:length(covsy);
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
          if (length(covsm) > 1) {
            ha <- expand.grid(covsm,covsm);
            hb <- matrix(data=c(1:(ncm**2)),nrow=ncm,ncol=ncm);
            s <- as.vector(lower.tri(hb));
            ha <- ha[s,];
            model_cov1 <- paste0(ha[,1], " ~~ " , ha[,2], collapse = " \n ");
          }
          
          ### construct covariances between mediators and covariates for Y
          model_cov2 <- " ";
          vars <- c(meds, covsy);
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
          
          modela1 <- paste0(meds, " ~ " ,a,"*",pred ,  collapse = " \n ");
          
          if(!is.null(xmmod)) {
            modelw <- paste0( meds, " ~ " ,w1,"*",xmmod ,  collapse = " \n ");
            modelw2 <- paste0( meds, " ~ " ,w2,"*",xmint ,  collapse = " \n ");
          }
          
          modelb1 <- paste0( dep, " ~ " ,b,"*",meds , collapse = " \n ");
          
          if(!is.null(mymod)) {
            modelv <- paste0( dep, " ~ " ,v1,"*",mymod , collapse = " \n ");
            modelv2 <- paste0( dep, " ~ " ,v2,"*",myint , collapse = " \n ");
          }
          
          modelc <- paste0( dep, " ~ " ,c,"*",pred , collapse = " \n ");
          
          if (!is.null(covsm)) {
            modeld <- paste0( rep(meds,ncm), " ~ " ,d,"*",rep(covsm, each=nm) , collapse = " \n ");
          }
          if (!is.null(covsy)) {
            modelf <- paste0( dep, " ~ " ,f,"*",covsy , collapse = " \n ");
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
          
        } # end buildmodmedsemmodel
          
        )
)
