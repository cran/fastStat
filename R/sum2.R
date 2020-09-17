sum2 <- function(x,...){
    by2 <- list(...)
    if (length(by2)==0){
        sum(x)
    }else{
        for (i in 1:length(by2)) {
            if (i==1) group_res=c()
            group_res=paste0(group_res,by2[[i]])
        }
        table(rep(group_res,x))
    }
}


