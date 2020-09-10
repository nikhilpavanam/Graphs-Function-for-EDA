Graphsfunction <- function(data, newvar = 0, graphsdir = 0)
{
  options(warn=-1)
  if(!is.data.frame(data))
  {
    stop("The given object is not a data frame") 
  }
  
  
  {if (graphsdir == 0) #set export directory as working directory if no argument is mentioned
  {
    direct=setwd()
  }
    else
    {
      direct=setwd(graphsdir)
    }}
  
  
  {if (newvar == 0)
  {
    d=data
  }
    else
    {
      d=data[,newvar]
    }}
  
  for(i in 1:ncol(d))
  {
    
    {if(is.numeric(d[,i]))
    {
      png(paste(names(data)[i], ".png", sep=""))
      
      par(mfrow=c(2,1))
      
      boxplot(d[,i], main = paste("Boxplot of", names(d)[i]),
              ylab = names(d)[i], col = "lightgreen", border = "grey5", horizontal = T)
      
      hist(d[,i], main = paste("Histogram of", names(d)[i]),
           xlab = names(d)[i], ylab = "Frequency", col = "skyblue2", border=F)
      
      dev.off()
    }
      else
        
      {
        
        d[,i]=as.factor(d[,i])
        slices <- as.numeric(unname(table(d[,i])))
        lbls <- names(table(d[,i]))
        
        png(paste(names(data)[i], ".png", sep=""))
        
        par(mfrow=c(2,1))
        
        barplot(table(d[,i]), freq=T,main = paste("Barplot of", names(d)[i]),
                xlab = names(d)[i], ylab = "Frequency",col = "orange",
                border = "black", horizontal = T)
        
        pie(slices,labels = lbls, col=rainbow(length(lbls)),
            main = paste("PieChart of", names(d)[i]))
        
        dev.off()
        
      }}
  }
}

