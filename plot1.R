################################################################################
## plot1.R
##
## plot1.R - Creates a bar plot of 'Total U.S. PM[2.5] Emissions'
##
##
## Author - Gregg O'Marr
## Date - 21 September 2014
## Version - 1.1
###############################################################################

NEI <- readRDS( "./summarySCC_PM25.rds" )       ## Read-in data.frame

## Reduce data by dropping unnecessary variables
NEI_Reduced <- NEI[ , c( 'Emissions', 'year' ) ]

## Split data.frame on year and return a list
NEI_EmissionsByYear <- split( NEI_Reduced[ , 'Emissions' ], 
                              NEI_Reduced$year )

## Apply sum over the NEI_EmissionsByYear list
totalPM25ByYear <- lapply( NEI_EmissionsByYear, 
                           FUN = sum )

png( file = "plot1.png", 
     width = 480, 
     height = 480) ## Open PNG file device

barplot(                                            ## Create plot
      unlist( totalPM25ByYear ) * 1e-6,             ## Set x-coords for plot 
      names.arg = names(totalPM25ByYear),           ## Set y-coords for plot
      col = c("lightblue", 
              "mistyrose",
              "lightcyan", 
              "lavender"),                          ## Set plot color to black
      xlab = "",                                    ## Set xlab to 'NO' label
      ylab = mtext( 
                  text = expression( 
                        'PM'[2.5]*' Emissions '*bolditalic( '( megatons )' ) ),
                  cex = 1.25,
                  side = 2,
                  line = 2 ),       ## Annotate with x-axis label
      ylim = c( 0.00, 8.00 ),
      main = mtext( 
                  text = expression( 
                        bold( 
                              'Total U.S. PM'[2.5]*' Emissions') ), 
                  line = 2, 
                  cex = 2 ) )
dev.off()                                           ## Close PNG file device