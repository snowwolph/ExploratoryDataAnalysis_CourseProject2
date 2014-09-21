################################################################################
## plot4.R
##
## plot4.R - Creates a bar plot of 'Total U.S. PM[2.5] Emissions: 
##                (Coal Combustion-Related), PM[2.5] Emissions vs. Years by 
##                Coal Source
##
##
## Author - Gregg O'Marr
## Date - 21 September 2014
## Version - 1.1
###############################################################################

require( ggplot2 )      ## Plotting framework
require( plyr )     ## Reshape the data with ddply()


NEI <- readRDS( "./summarySCC_PM25.rds" )       ## Read-in data.frame
SCC <- readRDS( "./Source_Classification_Code.rds" )       ## Read-in data.frame

## Reduce data by dropping unnecessary variables
NEI_Reduced <- NEI[ , c( 'SCC', 
                         'Emissions', 
                         'type', 
                         'year' ) ]

NEI_Reduced$year <- as.factor( NEI_Reduced$year )

## Create a data.frame to merge with NEI data.frame
SCC_CoalCombustion <- SCC[ 
                        grep( 
                              "Coal$", 
                              SCC$EI.Sector ), 
                        c( 'SCC', 
                           'Data.Category', 
                           'EI.Sector', 
                           'SCC.Level.Two', 
                           'SCC.Level.Three' ) ]

## Merge county and Baltimore data.frames
NEI_CoalComb <- merge( 
                        NEI_Reduced, 
                        SCC_CoalCombustion, 
                        sort = FALSE )

## Split the data.frame on year & EI.Sector and then summarize 
##    by summing Emissions
NEI_CoalCombSummarized <- ddply( NEI_CoalComb, 
                                 .( year, EI.Sector ), 
                                 summarize, 
                                 Emissions = sum( Emissions ) )

## Replace text strings with no char
NEI_CoalCombSummarized$EI.Sector <- gsub( "Fuel Comb - | - Coal", 
                                            "", 
                                            NEI_CoalCombSummarized$EI.Sector )

## Open 'PNG' file device to create image to export to file
png( 
      file = "plot4.png",     ## 'PNG' file name
      width = 480, 
      height = 480)           


g <- qplot( 
            year, 
            Emissions * 1e-3,                   ## Scaling factor
            data = NEI_CoalCombSummarized, 
            stat = 'identity', 
            geom = 'bar',                       ## Bar plot
            fill = EI.Sector, 
            position = 'dodge' ) + 
      
     ## xlim( 1997, )
      
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
                              bold( 'Total U.S. PM'[2.5]*' Emissions'),   ## Main title
                              atop( 
                                    bold( '(Coal Combustion-Related)') ) ) ) ) ## Sub title

print( g )

dev.off()                                           ## Close PNG file device