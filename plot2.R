################################################################################
## plot2.R
##
## plot2.R - Creates a bar plot of 'Total PM[2.5] Emissions: (Baltimore, MD),
##                PM[2.5] Emissions vs. Years'
##
##
## Author - Gregg O'Marr
## Date - 21 September 2014
## Version - 1.1
###############################################################################

NEI <- readRDS( "./summarySCC_PM25.rds" )       ## Read-in data.frame

## Reduce data by dropping unnecessary variables
NEI_Reduced <- NEI[ , c( 'fips', 
                         'Emissions', 
                         'year' ) ]

## Reduce data further by only selecting rows with Baltimore zipcode '24510'
NEI_Baltimore <- NEI_Reduced[ 
                        NEI_Reduced[ , 'fips' ] == '24510', 
                        c( 'Emissions', 'year' ) ]

## Split data.frame on year and return a list
NEI_BaltimoreByYear <- split( NEI_Baltimore[ , 'Emissions'], 
                              NEI_Baltimore$year )

## Apply sum over the NEI_EmissionsByYear listv
totalPM25BaltimoreByYear <- lapply( NEI_BaltimoreByYear, 
                                    FUN = sum )

## Open 'PNG' file device to create image to export to file
png( file = "plot2.png", 
     width = 480, 
     height = 480) ## Open PNG file device

## Create a bar plot
barplot(                                           
      unlist( totalPM25BaltimoreByYear ) * 1e-3,      ## Set x-coords for plot 
      names.arg = names(totalPM25BaltimoreByYear),    ## Set y-coords for plot
      col = "lightblue",                          ## Set plot color to lightblue
      xlab = "",                                    ## Set xlab to 'NO' label
      ylab = mtext( 
            text = expression( 
                  'PM'[2.5]*' Emissions '*bolditalic( '( kilotons )' ) ),
            cex = 1.25,
            side = 2,
            line = 2 ),       ## Annotate with x-axis label
      ylim = c( 0.00, 4.00 ),
      main = c(mtext( 
                  text = expression( 
                        bold( 
                              'Total PM'[2.5]*' Emissions') ), 
                  line = 2, 
                  cex = 2 ),
               mtext( 
                  text = expression( 
                        bold( 
                              '(Baltimore, MD)') ), 
                  line = 0, 
                  cex = 1.75 ) ) )
dev.off()                                           ## Close PNG file device