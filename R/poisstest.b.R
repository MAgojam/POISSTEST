
# This file is a generated template, your changes will not be overwritten

POISSTESTClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "POISSTESTClass",
    inherit = POISSTESTBase,
    public = list(
        initialize=function(...) {
            super$initialize(...)
            require("DescTools") 
        }
    ),
    private = list(
        .run = function() {

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)
            
            
            
            if ( is.null(self$data) || is.null(self$options$dep) )
            {print("Select your variables")} 
            else {
            
                if (self$options$switch == "fromraw") {
                
                dep <- self$options$dep

                results <- poisson.test(x=sum(self$data[,dep]),
                            T=length(self$data[,dep]),
                            r=self$options$testValue, 
                            alternative =self$options$alt, 
                            conf.level = self$options$conf)   
                

                self$results$text$setContent(results)   
                
                table <- self$results$poi
                
                table$setRow(rowNo=1, values=list(
                    var=self$options$dep,
                    Numberofevents=results$statistic,
                    Intervalbase=results$parameter,
                    Eventrate=results$estimate,
                    p=results$p.value
                ))
                
                confi <- data.frame(DescTools::PoissonCI(x=sum(self$data[,dep]), 
                                                         n=length(self$data[,dep]), 
                                                         conf.level=self$options$conf, 
                                                         method=ci))
                
                
                tableci <- self$confi$poisstest
                
                tabTitStr <- ('Confidence Interval for Variances - {dep}')
                tabTit <- jmvcore::format(tabTitStr, dep=self$options$dep)
                tableci$setTitle(tabTit)
                
                ciTitle <- paste0(self$options$ci*100, '% Confidence Interval')
                tableci$getColumn('Lower')$setSuperTitle(ciTitle)
                tableci$getColumn('Upper')$setSuperTitle(ciTitle)
                tableci$setRow(rowNo=1, values=list(
                    var=self$options$dep,
                    lambda=results$estimate[[1]],
                    method=row.names(confi)[1],
                    Lower=confi$lwr.ci[1],
                    Upper=confi$upr.ci[1],
                ))
                
                tableci$setRow(rowNo=2, values=list(
                    var="",
                    var=self$options$dep,
                    lambda=results$estimate[[2]],
                    method=row.names(confi)[2],
                    Lower=confi$lwr.ci[2],
                    Upper=confi$upr.ci[2],
                ))
                
                tableci$setRow(rowNo=3, values=list(
                    var="",
                    var=self$options$dep,
                    lambda=results$estimate[[3]],
                    method=row.names(confi)[3],
                    Lower=confi$lwr.ci[3],
                    Upper=confi$upr.ci[3],
                ))
                    
                    
                }
            }
        })
    )
