#####################################################
# Custom functions 
# 5 / 15 / 2013
#
# Written by Matt Townley
# townleym@uw.edu
#
# Inspired by and adapted from:
# http://macsci.jelmerborst.nl/files/r_startup_script.php
# http://gettinggeneticsdone.blogspot.com/2013/06/customize-rprofile.html
# 
# No warranty implied!
#
# "Share and enjoy"
#####################################################

# set repository 
options("repos" = c(CRAN = "http://cran.fhcrc.org"))

# create a new invisible environment so that my functions
# don't clutter up GobalEnv

.env = new.env()

print.functions <- function(){
	cat("plot.t(tstat, df, pval) - plots rejection region for t.test \n",sep="")
	cat("zstat(xbar, mu, s, n) - Calculates z statistic \n", sep="")
	cat("ci(data=NULL, xbar, s, n, prob) - calculates 95% CI \n", sep="")
	cat("robsum(model, exp, vcovmat,level, t) - retuns robust standard errors from an lm() object \n", sep="")	
	cat("pdesc(frame, sigfig) - returns pretty descriptives \n", sep="")
	cat("\n---------------\n\n",sep="")

}

.env$plot.t = function(tstat=-2.0422, df=30, pval=NULL) {

	if (!is.null(pval)) {
		
		qu=qt(p=pval, df=df, lower.tail=F)
		
		limit = 3.5
		strt=qu
		stp=limit

		x05 = qt(p=0.95, df=df)
		x01 = qt(p=0.99, df=df)

		cord.x <- c(strt,seq(strt,stp,0.01),stp)
		cord.y <- c(0,dt(seq(strt,stp,0.01), df=df),0)
		
		limit=abs(limit)
		curve(dt(x,df=df), xlim=c(-limit,limit), 
				xlab="t", ylab="Density", cex.axis=0.85)
		polygon(cord.x,cord.y,col='skyblue')
		title.text = paste("P[t > T], df =", df)
		title(main=title.text)
		tstat=round(qu, 3)
		legend(x=1, y=0.35, legend=c(paste("t =", tstat), paste("p =", pval)), 
			bty="n", cex=0.85)

		segments(x0=x05, y0=0, x1=x05, y1=0.25, lwd=0.75, lty=2, col="black")	
		text(x=x05, y=0.25, labels=quote(alpha==0.05), pos=3, cex=0.75)
		segments(x0=x01, y0=0, x1=x01, y1=0.2, lwd=0.75, lty=2, col="black")		
		text(x=x01, y=0.2, labels=quote(alpha==0.01), pos=3, cex=0.75)
	

	} else
	
	{
		qu=tstat
		if (qu < 0) {
			limit = -3.5
			strt=limit
			stp=qu
			
			x05 = qt(p=0.05, df=df)
			x01 = qt(p=0.01, df=df)
			title.text = paste("P[t < T], df =", df)	
			pval=round(pt(q=qu, df=df),3)			
			
		} else {
			limit = 3.5
			strt=qu
			stp=limit
	
			x05 = qt(p=0.95, df=df)
			x01 = qt(p=0.99, df=df)
			title.text = paste("P[t > T], df =", df)	
			pval=round(1-pt(q=qu, df=df),3)
			}


		cord.x <- c(strt,seq(strt,stp,0.01),stp)
		cord.y <- c(0,dt(seq(strt,stp,0.01), df=df),0)
		
		limit=abs(limit)
		curve(dt(x,df=df), xlim=c(-limit,limit), 
				xlab="t", ylab="Density", cex.axis=0.85)
		polygon(cord.x,cord.y,col='skyblue')
		title(main=title.text)
		
		tstat=round(tstat, 3)
		legend(x=1, y=0.35, legend=c(paste("t =", tstat), paste("p =", pval)), 
			bty="n", cex=0.85)
				
		segments(x0=x05, y0=0, x1=x05, y1=0.25, lwd=0.75, lty=2, col="black")	
		text(x=x05, y=0.25, labels=quote(alpha==0.05), pos=3, cex=0.75)
		segments(x0=x01, y0=0, x1=x01, y1=0.2, lwd=0.75, lty=2, col="black")		
		text(x=x01, y=0.2, labels=quote(alpha==0.01), pos=3, cex=0.75)
		
	}
	
}


.env$zstat=function(xbar=NULL, s=NULL, mu=NULL, n=NULL) {
	zstat<-NULL
	z=(xbar - mu) / (s / sqrt(n))
	zstat$z=z
	zstat$pnorm=pnorm(abs(z), lower.tail=F)
	zstat$pt=pt(abs(z), lower.tail=F, df=n-1)	
	return(zstat)
}

.env$ci=function(data=NULL, xbar=NULL, s=NULL, n=NULL, prob=0.95) {
	if (length(data) < 1 ) {

	alpha = 1 - prob
	se = s/sqrt(n)
	
	cutie = qt((1 - alpha/2), df=n-1, lower.tail=T)
	lci = xbar-(cutie*se)
	uci = xbar+(cutie*se)

	r.vec = cbind("n"=n,"mean"=xbar,"se"=se,"l ci"=lci,"u ci"=uci)
	return(signif(r.vec,3))

	}

	n<-length(data)
	m<-mean(data,na.rm=T)
	se<-sd(data,na.rm=T)/sqrt(n)
	lci<-xbar-(qt(.975,df=n-1)*se)
	uci<-xbar+(qt(.975,df=n-1)*se)

	r.vec<-cbind("n"=n,"mean"=xbar,"se"=se,"l ci"=lci,"u ci"=uci)
	return(signif(r.vec,3))
	
	
}

.env$pdesc = function(frame=NULL, sigfig=3) {

	classes<-sapply(frame, class)
	int<-grep("integer", classes)
	num<-grep("numeric", classes)
	char<-grep("character", classes)
	fac<-grep("factor", classes)

	numcols=sort(c(int, num))

	descmat=cbind(
	mean=signif(sapply(frame[,numcols], mean), sigfig),
	sd=signif(sapply(frame[,numcols], sd), sigfig),
	t(signif(sapply(frame[,numcols], function(x) {quantile(x, probs=c(0, 0.25, 0.50, 0.75, 1))}), sigfig)),
	nacount=round(sapply(frame[,numcols], function(x) {length(which(is.na(x))==T)}), 0),
	n = round(sapply(frame[,numcols], function(x) {length(x)}), 0)
	)
	
	return(descmat)
	
}

.env$robsum<-function(model, exp=FALSE, vcovmat=NULL,level=0.95, t=FALSE) {
  if (is.null(vcovmat) & !require(sandwich))
    stop("Need either sandwich package or supplied vcov matrix")

 if(is.null(vcovmat))
    vcovmat<-vcovHC(model, type="HC0")

 se<-sqrt(diag(vcovmat))

 if (t)
    cv <- qt((1-level)/2, df=model$df.residual, lower.tail=FALSE)
 else
    cv <- qnorm( (1-level)/2, lower.tail=FALSE)

.env$s.model<-summary(model)

	print(model$call)
	if (exp)
	cbind("est(e)"=exp(coef(model)), se, "lci(e)"=exp(coef(model)-cv*se), "uci(e)"=exp(coef(model)+cv*se), p=s.model$coefficients[,4])
	else
	cbind( est=coef(model), se, lci=coef(model)-cv*se, uci=coef(model)+cv*se, p=s.model$coefficients[,4] )

}
