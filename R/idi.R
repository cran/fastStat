
#' Perform IDI for logistic and cox regression
#' @description Easy to perform integrated discrimination improvement
#'     index (IDI) for logistic and cox regression.
#' @param f1 base model
#' @param f2 the other model
#' @param timepoint one time point for cox regression, default is median time.
#' @param ... ignore
#' @importFrom do model.x model.y model.data
#' @importFrom survC1 kmcens
#' @importFrom stats as.formula median predict update
#' @return IDI results
#' @export
#' @examples
#' library(rms)
#' # logistic
#'
#' data(lung)
#'
#' lung$status=lung$status-1
#' f1=lrm(status~age+sex,lung)
#' f2=lrm(status~age+sex+ph.ecog,lung)
#' idi(f1,f2)
#'
#' # survival
#'
#' head(lung)
#'
#' range(lung$time)
#'
#' lung=lung[complete.cases(lung),]
#'
#' f1=cph(Surv(time,status)~age+sex,lung)
#' f2=cph(Surv(time,status)~age+sex+ph.ecog,lung)
#' idi(f1,f2)
idi <- function(f1,f2,...) UseMethod('idi')

#' @method idi lrm
#' @export
#' @name idi
idi.lrm <- function(f1,f2,...){
    x=union(model.x(f1),model.x(f2))
    y=model.y(f1)
    formu=as.formula(paste(y,'~',paste(x,collapse = ' + ')))
    fu=update(f1,formu,model=TRUE)
    data=model.data(fu)
    cOutcome=match(y,colnames(data))
    p1=predict(f1,type = 'fitted')
    p2=predict(f2,type = 'fitted')
    jdg=is.na(p1)+is.na(p2) <1
    p1=p1[jdg]
    p2=p2[jdg]
    PredictABEL::reclassification(data=data,
                                  cOutcome=cOutcome,
                                  predrisk1=p1,
                                  predrisk2=p2,
                                  cutoff=seq(0,1,0.1))
}

#' @method idi cph
#' @export
#' @name idi
idi.cph <- function(f1,f2,timepoint=NULL,...){
    if (is.null(timepoint)) timepoint=median(model.data(f1)[,model.y(f1)[1]])
    covs0=model.data(f1)[model.x(f1)]
    covs1=model.data(f2)[model.x(f2)]
    indata=model.data(f1)[model.y(f1)]
    x<-survIDINRI::IDI.INF(indata = indata,
                           covs0 = covs0,
                           covs1 = covs1,
                           t0 = timepoint)
    survIDINRI::IDI.INF.OUT(x)
}

#' @method idi coxph
#' @export
#' @name idi
idi.coxph <- function(f1,f2,timepoint=NULL,...){
    if (is.null(timepoint)) timepoint=median(model.data(f1)[,model.y(f1)[1]])
    covs0=model.data(f1)[model.x(f1)]
    covs1=model.data(f2)[model.x(f2)]
    indata=model.data(f1)[model.y(f1)]
    x<-survIDINRI::IDI.INF(indata = indata,
                           covs0 = covs0,
                           covs1 = covs1,
                           t0 = timepoint)
    survIDINRI::IDI.INF.OUT(x)
}

