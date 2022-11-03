
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
            
            dep <- self$options$dep
            testvalue <- self$options$testValue
            lmbda <- self$options$lambda
            time <- self$options$intervalValue

            if ( is.null(self$data) || is.null(self$options$dep) )
            {print("Select your variables")} 
            else {
            
                if (self$options$switch == "fromraw") { #Use raw data
                
                results <- poisson.test(x=sum(self$data[,dep]),
                            T=length(self$data[,dep]),
                            r=self$options$testValue, 
                            alternative =self$options$alt, 
                            conf.level = self$options$conf)   
                

               # self$results$text$setContent(results)   
                
               confi <- data.frame(DescTools::PoissonCI(x=sum(self$data[,dep]), 
                                                         n=length(self$data[,dep]), 
                                                         conf.level=self$options$conf, 
                                                         method=c("exact","score","wald")))
            
             #self$results$text$setContent(confi)      
                
            } else { #use summary data
                    
                results <- poisson.test(x=lmbda*time,
                                            T=time,
                                            r=self$options$testValue, 
                                            alternative =self$options$alt, 
                                            conf.level = self$options$conf)   
                    
                    
                    # self$results$text$setContent(results)   
                    
                    confi <- data.frame(DescTools::PoissonCI(x=lmbda*time, 
                                                  n=time, 
                                                  conf.level=self$options$conf, 
                                                  method=c("exact","score","wald")))
                    
                    #self$results$text$setContent(confi)      
            }
                
                table <- self$results$poissontest   #name of the last element comes from xx.r.yaml name
                
                
                pval <- ifelse(results$p.value <.001, "<.001", results$p.value)
                side <- ifelse(self$options$alt=="two.sided", "=", ifelse(self$options$alt=="greater", "<=", ">="))
                
                
                tabTitStr <- paste0("Poisson Test: H0: lambda",side, testvalue)
                tabTit <- jmvcore::format(tabTitStr, dep=self$options$dep)
                table$setTitle(tabTit)
                
                ciTitle <- paste0(self$options$conf*100, "% Conf. Interval - ", self$options$ci)
                table$getColumn("Lower")$setSuperTitle(ciTitle)
                table$getColumn('Upper')$setSuperTitle(ciTitle)
                
                table$setRow(rowNo=1, values=list(
                    var=self$options$dep,
                    NumberofEvents=results$statistic,
                    Intervalbase=results$parameter,
                    Eventrate=results$estimate,
                    p=pval,
                    Lower=confi$lwr.ci[1],
                    Upper=confi$upr.ci[1]
                ))
                
                
                tableci <- self$results$poissonci
                
                tabTitStr <- ("Confidence Intervals for Poisson Parameter - {dep}")
                tabTit <- jmvcore::format(tabTitStr, dep=self$options$dep)
                tableci$setTitle(tabTit)
                
                ciTitle <- paste0(self$options$conf*100, "% Confidence Interval")
                tableci$getColumn("Lower")$setSuperTitle(ciTitle)
                tableci$getColumn('Upper')$setSuperTitle(ciTitle)
                
                tableci$setRow(rowNo=1, values=list(
                    var=self$options$dep,
                    lambda=confi$est[1],
                    method=row.names(confi)[1],
                    Lower=confi$lwr.ci[1],
                    Upper=confi$upr.ci[1]
                ))
                
                tableci$setRow(rowNo=2, values=list(
                    var="",
                    var=self$options$dep,
                    lambda=confi$est[2],
                    method=row.names(confi)[2],
                    Lower=confi$lwr.ci[2],
                    Upper=confi$upr.ci[2]
                ))
                
                tableci$setRow(rowNo=3, values=list(
                    var="",
                    var=self$options$dep,
                    lambda=confi$est[3],
                    method=row.names(confi)[3],
                    Lower=confi$lwr.ci[3],
                    Upper=confi$upr.ci[3]
                ))
                
                
            }
        })
    )
