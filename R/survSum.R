
#' Calculate Survival Rate and Time
#'
#' @param fit fit by survfit() function
#' @name survSum
#' @return a dataframe
#' @export
#'
#' @examples
#' library(survival)
#' fit=survfit(Surv(futime, fustat)~rx, data=ovarian)
#' 
#' #survival table
#' surv_table(fit)
#' 
#' #median survival rate
#' surv_median_time(fit)
#' 
#' #one year survaival rate
#' surv_year_rate(fit,365)
#' 
#' #two years survaival rate
#' surv_year_rate(fit,365*2)
#' 
surv_table <- function(fit){
    if (class(fit) != "survfit") stop('fit must be "survfit" by function survfit.')
    df0=data.frame(
        time=fit$time,
        n.risk=fit$n.risk,
        n.event=fit$n.event,
        n.censor=fit$n.censor,
        surv=fit$surv,
        lower=fit$lower,
        upper=fit$upper)
    x=fit$strata
    if (!is.null(x)){
        strata0=lapply(1:length(x), function(i) rep(names(x)[i],x[i]))
        strata=do.call(c,strata0)
        df1=cbind(strata,df0)
    }else{
        df1=df0
    }
    invisible(df1)
    median_surv_time <- function(df1){
        if (all(sum(df1$surv >= 0.5))){
            res=rep('NULL',length(colnames(df1)))
            res_df=data.frame(t(res))
            colnames(res_df)=colnames(df1)
            return(res_df)
        }
        df1[sum(df1$surv >= 0.5),]
    }
}
#' @export
#' @rdname survSum
surv_median_time <- function(fit){
    if (class(fit) != "survfit") stop('fit must be "survfit" by function survfit.')
    df0=data.frame(
        time=fit$time,
        n.risk=fit$n.risk,
        n.event=fit$n.event,
        n.censor=fit$n.censor,
        surv=fit$surv,
        lower=fit$lower,
        upper=fit$upper)
    x=fit$strata
    if (!is.null(x)){
        strata0=lapply(1:length(x), function(i) rep(names(x)[i],x[i]))
        strata=do.call(c,strata0)
        df1=cbind(strata,df0)
    }else{
        df1=df0
    }
    invisible(df1)
    median_surv_time <- function(df1){
        if (all(sum(df1$surv >= 0.5))){
            res=rep('NULL',length(colnames(df1)))
            res_df=data.frame(t(res))
            colnames(res_df)=colnames(df1)
            return(res_df)
        }
        df1[sum(df1$surv >= 0.5),]
    }
    if ('strata' %in% colnames(df1)){
        res=lapply(unique(df1$strata), function(i) median_surv_time(df1[df1$strata==i,]))
    }else{
        res=median_surv_time(df1)
    }
    do.call(rbind,res)
}
#' @export
#' @rdname survSum
#' @param year year
surv_year_rate <- function(fit,year){
    if (class(fit) != "survfit") stop('fit must be "survfit" by function survfit.')
    df0=data.frame(
        time=fit$time,
        n.risk=fit$n.risk,
        n.event=fit$n.event,
        n.censor=fit$n.censor,
        surv=fit$surv,
        lower=fit$lower,
        upper=fit$upper)
    x=fit$strata
    if (!is.null(x)){
        strata0=lapply(1:length(x), function(i) rep(names(x)[i],x[i]))
        strata=do.call(c,strata0)
        df1=cbind(strata,df0)
    }else{
        df1=df0
    }
    invisible(df1)
    median_surv_time <- function(df1){
        df1[sum(df1$time <= year),]
    }
    if ('strata' %in% colnames(df1)){
        res=lapply(unique(df1$strata), function(i) median_surv_time(df1[df1$strata==i,]))
    }else{
        res=median_surv_time(df1)
    }
    do.call(rbind,res)
}
