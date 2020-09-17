#' Correlation Analysis with Sinificant and Correlation Value
#'
#' @param data a dataframe or matrix
#' @param method a character string indicating which correlation coefficient (or covariance) is to be computed. One of "pearson" (default, 1), "kendall" (2), or "spearman" (3): can be abbreviated.
#' @return correlation analysis with significant star.
#' @export
#'
#' @examples
#' cor_sig_star(mtcars)
cor_sig_star<- function(data,method="pearson") {
    if (method==1) method='pearson'
    if (method==2) method='kendall'
    if (method==3) method='spearman'
    message('')
    message(method,' method')
    res.cor = round(cor(data),3)
    for (i in 1:(nrow(res.cor)-1)) {
        for (j in (i+1):nrow(res.cor)) {
            sig = as.numeric(cor.test(data[,rownames(res.cor)[i]],
                                      data[,rownames(res.cor)[j]],
                                      method=method)$p.value)
            if (sig <= 0.001) {
                star = "***"
            }
            if (sig <= 0.01 & sig > 0.001) {
                star = "** "
            }
            if (sig <= 0.05 & sig > 0.01) {
                star = "*  "
            }
            if (sig > 0.05) {
                star = "   "
            }
            res.cor[i,j]=paste0(res.cor[i,j],'(',round(sig,3),')',star)
        }
        res.cor[(i+1):nrow(res.cor),i]=''
    }
    for (i in 1:nrow(res.cor)) {
        res.cor[i,i]=rownames(res.cor)[i]
    }
    message('')
    message('***: p      <=0.001')
    message(' **: p 0.01 ~ 0.001')
    message('  *: p 0.05 ~ 0.01')
    message('')
    rownames(res.cor)=NULL
    colnames(res.cor)[1]=' '
    as.data.frame(res.cor)
}
