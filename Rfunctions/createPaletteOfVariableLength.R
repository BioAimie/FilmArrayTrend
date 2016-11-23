createPaletteOfVariableLength <- function(dataFrame, colorVariable) {
  
  colorNames <- unique(dataFrame[,colorVariable])[order(unique(dataFrame[,colorVariable]))]
  numberOfColors <- length(colorNames)
  colorVector <- c('midnightblue','steelblue4','royalblue1','steelblue1','slategray1','orangered3','tomato2','orange','sienna1','mediumorchid3','mediumpurple4','orchid',
                   'plum3','green4','seagreen3','seagreen1','palegreen','turquoise4','lightseagreen','cyan','mediumvioletred','maroon1','hotpink','lightcoral','goldenrod',
                   'khaki','lightgoldenrodyellow','red4','red','firebrick1')
  myPal <- colorRampPalette(colorVector[1:numberOfColors])(numberOfColors)
  names(myPal) <- colorNames
  return(myPal)
}