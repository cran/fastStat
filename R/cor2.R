#' Correlation Analysis
#'
#' @param data dataframe
#' @param x1 x1
#' @param x2 x2
#' @param method 1,2,3
#' @importFrom stats cor cor.test
#' @export
#'
cor2<-function(data,x1,x2,method='spearman'){
    method=match.arg(method)
    if (method==1) method='pearson'
    if (method==2) method='spearman'
    if (method==3) method='kendall'
    if (missing(x1)){
        x1=colnames(data)
    }
    if (missing(x2)){
        x2=colnames(data)
    }
    x1=unique(x1)
    x2=unique(x2)
    res=matrix(rep(NA,length(x1)*length(x2)),
               nrow = length(x1),
               dimnames = list(x1,x2))
    for (i in x1) {
        for (j in x2) {
            dd=data[,c(i,j)]
            dd=na.omit(dd)
            res[i,j]=round(cor(x=to.numeric(dd[,1]),
                               y=to.numeric(dd[,2]),
                               method=method),2)
        }
    }
    res
}
