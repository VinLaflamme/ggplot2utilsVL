
#' Prints a ggplot2 object into a .svg and a .tiff files
#'
#' \code{printGraTiffVL} prints a ggplot2 into  an svg and a tiff files with the desired resolution and size. Produces plot with optimal quality and with Cairo type anti-aliasing.
#'
#' @param graphObj ggplot2 object to print.
#' @param fileN Path to the files to be created. Must exclude the file extension.
#' @param width Width of the image in the desired measurement unit.
#' @param height Height of the image in the desired measurement unit.
#' @param resol Resolution of the image, in dpi.
#' @param unit Measurement unit for width and height. Defaults to inches.
#' @param compress compression algorithm, one of c("None", "BZip", "Fax", "Group4", "JPEG", "JPEG2000", "Lossless", "LZW", "RLE", "Zip"). Defaults to "LZW".
#' @param showF Boolean. Decides whether the .tiff image file should be opened after its creation in an external program. Defaults to FALSE.
#' @param IMloc Path to the ImageMagick executable.
#' @param grayscale If TRUE, the graph will be saved in grayscale (necessary for word documents)
#' @return the path to the .tiff file.
#' @export
#' @seealso \url{http://www.imagemagick.org/script/command-line-options.php} \url{http://www.imagemagick.org/script/index.php}

printGraTiffVL<-function(graphObj,fileN,width=7,height=7,resol = 600,unit="in",compress="LZW",showF=F,IMloc="C:\\Program Files\\ImageMagick-7.0.3-Q16\\convert.exe", grayscale=F){
  switch(EXPR = unit,"in" = {
    widthIN<-width
    widthCM<-cm(width)
    widthPX<-widthIN * resol
    heightIN<-height
    heightCM<-cm(height)
    heightPX<-heightIN * resol
  },"cm"={
    widthIN<-width /cm(1)
    widthCM<-width
    widthPX<-widthIN * resol
    heightIN<-height /cm(1)
    heightCM<-height
    heightPX<-heightIN * resol
  },{
    widthIN<-width
    widthCM<-cm(width)
    widthPX<-widthIN * resol
    heightIN<-height
    heightCM<-cm(height)
    heightPX<-heightIN * resol
    warning("Measurement unit taken as inches.")
  })

  fileNsvg<-paste0(fileN,".svg")
  svg(filename = fileNsvg,width = widthIN,height = heightIN)
  print(graphObj)
  dev.off()

  normSVGpath<-normalizePath(fileNsvg,winslash = "/")
  normSVGpath<-shortPathName(normSVGpath)
  normTIFFpath<-sub(x = normSVGpath,pattern = ".svg",replacement = ".tiff")
  normTIFFpath<-sub(x = normTIFFpath,pattern = ".SVG",replacement = ".tiff")
  normIMloc<- normalizePath(IMloc,winslash = "/")
  normIMloc<-shortPathName(normIMloc)
  sysSTR<-paste0(normIMloc, " -size ", round(widthPX,digits = 0),"x",round(heightPX,digits = 0), " -density ", round(resol,digits = 0),"x",round(resol,digits = 0), " -units PixelsPerInch -depth 8")
  if(!is.null(compress)) sysSTR<-paste0(sysSTR, " -compress ", compress)
  if(grayscale) sysSTR<-paste0(sysSTR, " -colorspace gray ")
  sysSTR<-paste0(sysSTR, ' "', normSVGpath, '" "', normTIFFpath,'"')
  shell(cmd = sysSTR)
  normSVGpath2<-normalizePath(fileNsvg)
  normSVGpath2<-sub(x = normSVGpath2,pattern = ".svg",replacement = ".tiff")
  normSVGtiff2<-sub(x = normSVGpath2,pattern = ".SVG",replacement = ".tiff")
  if(!(basename(normTIFFpath)==basename(normSVGtiff2))){
    if(file.exists(normSVGtiff2)) file.remove(normSVGtiff2)
    file.rename(from = normTIFFpath,to=normSVGtiff2)
    normTIFFpath<-normSVGtiff2
  }
  if(showF) shell(cmd = shortPathName(normTIFFpath),wait = F)
  return(normTIFFpath)
}

#' Returns the coordinates of a polygon for graphing the area under a curve.
#'
#' @param datat the data table containing the points of the function whose area under the curve is to be plotted.
#' @param startC a vector containing the x and y coordinates of the leftmost point on the curve to be plotted.
#' @param endC a vector containing the x and y coordinates of the righttmost point on the curve to be plotted.
#' @param xColumn a string indicating the name of the column in datat which corresponds to the x values.
#' @param yColumn a string indicating the name of the column in datat which corresponds to the y values.
#' @param yintercept y intercept of the horizontal line at the bottom of the polygon to be plotted.
#' @return a data table containing the coordinates of the polygon.
#' @export

areaUnderCurveDT<-function(datat,startC,endC,xColumn,yColumn,yintercept=0){
  datatc<-copy(datat)
  if(!all(c(xColumn,yColumn) %in% names(datatc))) stop("Bad column names")
  datatc<-setnames(x = datatc[,c(xColumn,yColumn),with=F],old = c("xxx","yyy"))[between(xxx,lower = startC[1],upper = endC[1])]
  if(anyDuplicated(x = datatc[["xxx"]])) stop("duplicated entries in x")
  datatout<-setnames(rbind(as.list(c(startC[1],yintercept)),as.list(startC),datatc,as.list(endC),as.list(c(endC[1],yintercept))),old = c(xColumn,yColumn))[]
  return(datatout)
}


#' @import ggplot2
#' @import data.table
#' @import grDevices
#'
NULL
