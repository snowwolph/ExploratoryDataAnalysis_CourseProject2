################################################################################
## plot3.R
##
## plot3.R - Creates a bar plot of 'Total PM[2.5] Emissions: (Baltimore, MD), 
##                PM[2.5] Emissions vs. Years by Type of Source
##
##
## Author - Gregg O'Marr
## Date - 21 September 2014
## Version - 1.1
###############################################################################

require( ggplot2 )      ## Plotting framework
require( plyr )     ## Reshape the data with ddply()

NEI <- readRDS( "./summarySCC_PM25.rds" )       ## Read-in data.frame

## Reduce data by dropping unnecessary variables
NEI_Reduced <- NEI[ , c( 'fips', 
                         'Emissions', 
                         'type', 
                         'year' ) ]

## Reduce data further by only selecting rows with Baltimore zipcode '24510'
NEI_Baltimore <- NEI_Reduced[ 
                              NEI_Reduced[ , 'fips' ] == '24510', 
                              c( 'Emissions', 'type', 'year' ) ]

NEI_Baltimore$type <- as.factor( NEI_Baltimore$type )
NEI_Baltimore$year <- as.factor( NEI_Baltimore$year )

## Split the data.frame on year & type and then summarize by summing Emissions
baltimorePM25Summarized <- ddply( NEI_Baltimore, 
                                  .( year, type ), 
                                  summarize, 
                                  Emissions = sum( Emissions ) )

## Open 'PNG' file device to create image to export to file
png( 
      file = "plot3.png",     ## 'PNG' file name
      width = 480, 
      height = 480)           

                                     
g <- qplot( 
            year, 
            Emissions * 1e-3,                   ## Scaling factor
            data = baltimorePM25Summarized, 
            stat = 'identity', 
            geom = 'bar',                       ## Bar plot
            fill = type, 
            position = 'dodge' ) + 
      
      labs(
            x = expression( 
                        bold( 'Years' ) ) ) +   ## X-axis label
      
      labs(
            y = expression(                     ## Y-axis label
                        PM[2.5]*' Emissions '*bolditalic( '( kilotons )' ) ) ) + 
      
      labs(
            fill = expression(
                              bold( PM[2.5]*' Source') ) ) + ## Legend label
      
      ggtitle(
          expression( 
                  atop( 
                      bold( 'Total PM'[2.5]*' Emissions'),   ## Main title
                      atop( 
                            bold( '(Baltimore, MD)') ) ) ) ) ## Sub title

print( g )

dev.off()                                           ## Close PNG file device