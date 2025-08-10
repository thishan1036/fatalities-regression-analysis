################################################################################
#
# smallstuff2.R
# Additional functions to be added to the smallstuff package
#
################################################################################
regression_line<-function(model,nm=NULL,...) {
  if (is.null(nm)) {
    mn=floor(min(model.matrix(model)[,2]))
    mx=ceiling(max(model.matrix(model)[,2]))
    nm=all.vars(model$formula)[2]
  } else {
    dat=as.character(model$call$data)
    v=eval(parse(text=paste(dat,"$",nm)))
    mn=floor(min(v))
    mx=ceiling(max(v))
  }
  x=data.table(nm=seq(mn,mx,length.out=1000))
  setnames(x,"nm",nm)
  if (inherits(model,"glm")) {
    lines(predict(model,x,"r")~x[[1]],x,...)
    return(invisible())
  }
  if(inherits(model,"lm")) {
    lines(predict(model,x)~x[[1]],x,...)
    return(invisible())
  }
  stop("Model is not supported")
}
KNN<-function(formula,data=NULL,K=1,subset=NULL) {
  if (!inherits(formula,"formula")) stop("Must give a fomula")
  if (!is.null(data)) {
    if (!inherits(data,"data.frame")) stop("Incorrect data")
    if (!is.null(subset)) {
      if (!inherits(subset,c("logical","integer","numeric")))
        stop("subset must be logical or numeric")
      if (inherits(subset,"logical") & length(subset)!=nrow(data))
        stop("subset has incorrect length")
      if (inherits(subset,"numeric")) subset=round2(subset)
      if (!inherits(subset,"logical") & (min(subset) < 1 | max(subset) > nrow(data)))
        stop("subset values incorrect")
      data=data[subset,]
    }
  }
  preds=attributes(terms(formula,data=data))$term.labels
  data2=model.frame(formula,data)
  resp=model.response(data2)
  data.table::setDT(data2)
  out=list(call=match.call(),data=data2[], K=K, preds=preds)
  class(out)="KNN"
  out
}
predict.KNN<-function(model,new=NULL,K=NULL) {
  if (is.null(new)) new=model$data
  if (is.null(K)) K=model$K
  preds=model$preds
  for (i in 1:length(preds)) {
    if (!(preds[i] %in% colnames(new))) stop("Variables missing")
  }
  new2=data.table::copy(new)
  data.table::setDT(new2)
  predTrain=model$data[,..preds]
  predTest=new2[,..preds]
  #Changing categorical variables to numeric
  for (i in 1:length(preds)) {
    tmp=preds[i]
    if (inherits(predTrain[[i]],c("character","factor"))) {
      predTrain[,(tmp):=as.numeric(factor(predTrain[[i]]))]
      predTest[,(tmp):=as.numeric(factor(predTest[[i]]))]
    }
  }
  yhat=class::knn(predTrain,predTest,model.response(model$data),k=K,prob=TRUE)
  prob=attributes(yhat)$prob
  attr(yhat,"prob")=NULL
  dt=data.table::data.table(class=yhat,winning=prob)
  #2 levels
  if (length(levels(yhat))==2) {
    vars=c(paste0("prob",levels(yhat)))
    dt[,(vars):=.(prob,prob)]
    dt[class==levels(yhat)[1],(vars[2]):=1-winning]
    dt[class==levels(yhat)[2],(vars[1]):=1-winning]
  }
  dt[]
}
print.KNN<-function(model) print(model$call)

ROCcurve<-function(mod,new=NULL,K=NULL) {
  if (!inherits(mod,c("glm","lda","qda","KNN"))) stop("Model not supported")
  fm=eval(mod$call$formula)
  att=attributes(terms(fm,data=eval(mod$call$data)))
  Ktxt=""
  if (!is.null(new)) {
    nw=data.table::as.data.table(new)
    if (!inherits(nw,"data.frame")) stop("new must be a data frame type")
    resp=all.vars(fm)[att$response]
    if (!(resp %in% colnames(nw))) stop("new must contain the response variable")
    response=nw[,..resp]
    datname=as.character(match.call()$new)
  } else {
    datname=as.character(mod$call$data)
  }
  if (inherits(mod,"glm")) {
    if (is.null(new)) {
      prob=fitted(mod)
      response=factor(model.frame(mod)[,1],ordered=T)
    } else prob=predict(mod,nw,"r")
  } else if (inherits(mod, "KNN")) {
    set.seed(45L)
    if (is.null(K)) K=mod$K
    Ktxt=paste0(";  K=",K)
    if (is.null(new)) {
      response=model.response(mod$data)
      pred=predict(mod,K=K)
    } else {
      pred=predict(mod,nw,K=K)
    }
    yhat=pred[,class]
    var=paste0("prob",levels(yhat)[2])
    prob=pred[,..var]
  } else {
    if (is.null(new)) {
      prob=predict(mod)$posterior[,2]
      response=factor(model.frame(mod)[,1],ordered=T)
    } else prob=predict(mod,nw)$posterior[,2]
  }
  pr=ROCR::prediction(prob,response)
  plot(ROCR::performance(pr,"tpr","fpr"),
       main=c(paste0("ROC curve for ",class(mod)[1]," on ",datname,Ktxt),
              deparse(fm)))
  auc=ROCR::performance(pr,"auc")@y.values[[1]]
  graphics::text(.5,.5,paste0("AUC = ",round2(auc,3)))
}
#Plotting a hierarchical clustering in differing numbers of groups#####
plot_cut<-function(hi,g,n,col=NULL,...) {
  if (!inherits(hi,"communities")) stop("hi must be a communities object")
  if (!inherits(g,"igraph")) stop("g must be an igraph object")
  if (!is_hierarchical(hi)) stop("Not a hierarchical community structure")
  if (hi$algorithm!="fast greedy") stop("Not a hierarchical community structure")
  if (!inherits(n,c("integer","numeric"))) stop("n must be numeric")
  if (!isInt(n)) stop("n must be an integer")
  if (n<1 | n>hi$vcount) stop("n must contain a valid number of vertices")
  ca=igraph::cut_at(hi,n)
  names(ca)=seq_along(ca)
  if (!is.null(V(g)$name)) v=V(g)$name else v=names(ca)
  dt=data.table::data.table(v=v,group=ca)
  dt2=dt[,.(v=.(v)),group]
  if (is.null(col)) col=ca
  plot(hi,g,col=col,mark.groups=dt2$v,
       edge.color=c("black","red")[crossing2(ca,g)+1],...)
  invisible(ca)
}
model.frame.KNN<-function (model) 
{
  oc <- model$call
  oc[[1L]] <- quote(stats::model.frame)
  eval(oc)
}
CVerror<-function(mod, k=nrow(stats::model.frame(mod)),K=NULL) {
  if (!inherits(mod,c("glm","lda","qda","KNN"))) {
    stop("Not a supported model")
  }
  if (!isInt(k)|k<1) stop("k must be an integer greater than or equal to 1")
  if (inherits(mod,"KNN")&is.null(K)) K=mod$K
  dat=stats::model.frame(mod)
  n=nrow(dat)
  split=sample(rep(1:k,length.out=n))
  if (!inherits(mod,"glm")) {
    resp=model.response(dat)
  }
  errk=NULL
  for (i in 1:k) {
    mod2=eval(pryr::modify_call(mod$call,list(subset=which(split!=i))))
    if (inherits(mod,"glm")) {
      errk[i]=logistErrorRate(mod2,dat[split==i,])$errorRate 
    } else if (inherits(mod,"KNN")) {
        errk[i]=mean(predict(mod2,dat[split==i,],K)$class!=resp[split==i])*100 
    } else {
      errk[i]=mean(predict(mod2,dat[split==i,])$class!=resp[split==i])*100
    }
  }
  mean(errk)
}
#Create a normal curve overlay for a histogram
noverlay<-function(dat,...) {
  if (!inherits(dat,c("integer","numeric"))) stop("Data must be numeric")
  m1=floor(min(dat,na.rm=T))
  m2=ceiling(max(dat,na.rm=T))
  x=seq(m1,m2,length.out=1000)
  y=dnorm(x,mean(dat,na.rm=T),smallstuff::pop.sd(dat))
  lines(y~x,...)
}
#Obtain z-scores for a variable, or for all numeric variables in a dataset
#using the population standard deviation
popScale<-function(x) {
  if (!inherits(x,c("integer","numeric","data.frame"))) stop("Incorrect data type")
  if (inherits(x,"data.frame")) {
    idx=rep(FALSE,ncol(x))
    for (i in 1:ncol(x)) {
      if (inherits(x[[i]],c("integer","numeric"))) idx[i]=TRUE
    }
    if (sum(idx)==0) stop("data does not have any numeric columns")
    return(scale(x[,idx,drop=FALSE],scale=sapply(x[,idx,drop=FALSE],pop.sd)))
  }
  return(scale(x,scale=pop.sd(x)))
}

