# OBJECTS


# METHODS
setGeneric("cumFreq", function(data, names, width, colors, xlab, xlim=0, lty=1) standardGeneric("cumFreq"))
setMethod("cumFreq", signature("matrix", "character", "numeric", "character", "character", "numeric", "numeric"),
	function(data, names, width, colors, xlab, xlim=0, lty=1) {
		max_number <- max(apply(data, 2, max))
		min_number <- min(apply(data, 2, min))
		breaks <- seq(min_number-1, max_number+1, by = width)

		if (length(xlim) == 1 && xlim == 0) {
			xlim <- c(min_number, max_number)
		}

		dataList <- list()
		for (i in 1:length(data[1,])) {
			data.cut <- cut(data[,i], breaks=breaks)
			dataList[[i]] <- data.cut
		}
		cumList <- list()
		for (i in 1:length(dataList)) {
			data.cum <- c("0"=0, cumsum(table(dataList[[i]])))
			data.cum <- data.cum / length(dataList[[i]])
			cumList[[i]] <- data.cum
		}
		plot(breaks, cumList[[1]], type="l", col=colors[1], ylim=c(0, 1), main="Cummulative frequencies", ylab="Frequency", xlab=xlab, xlim=xlim, lty=lty[1])
		for (i in 2:length(cumList)) {
			lines(breaks, cumList[[i]], col=colors[i], lty=lty[i])
		}
		legend("bottomright", legend=names, col=colors, lty=lty, lwd=2)
})


setMethod("cumFreq", signature("list", "character", "numeric", "character", "character", "numeric", "numeric"),
	function(data, names, width, colors, xlab, xlim=0, lty=1) {
		max_number <- 0
		min_number <- 0
		for (item in data) {
			max_number <- ifelse(max_number < max(item), max(item), max_number)
			min_number <- ifelse(min_number > min(item), min(item), min_number)
		}
#		print(max_number)
#		print(min_number)
		if (length(xlim) == 1 && xlim == 0) {
			xlim <- c(min_number, max_number)
		}
		breaks <- seq(min_number-1, max_number+1, by = width)
		dataList <- list()
		i <- 1
		for (item in data) {
			data.cut <- cut(item, breaks)
			dataList[[i]] <- data.cut
			i <- i + 1
		}
#		print(str(dataList))
		cumList <- list()
		for (i in 1:length(dataList)) {
			data.cum <- c("0"=0, cumsum(table(dataList[[i]])))
			data.cum <- data.cum / length(dataList[[i]])
			cumList[[i]] <- data.cum
		}
#		print(cumList)
		plot(breaks, cumList[[1]], type="l", col=colors[1], ylim=c(0,1), main="Cummulative frequencies", ylab="Frequency", xlab=xlab, xlim=xlim, lty=lty[1])
		for (i in 2:length(cumList)) {
			lines(breaks, cumList[[i]], col=colors[i], lty=lty[i])
		}
		legend("bottomright", legend=names, col=colors, lty=lty, lwd=2)
})


setGeneric("cumFreq.flipped", function(data, names, width, colors, xlab, xlim=0, lty=1) standardGeneric("cumFreq.flipped"))
setMethod("cumFreq.flipped", signature("matrix", "character", "numeric", "character", "character", "numeric", "numeric"),
	function(data, names, width, colors, xlab, xlim=0, lty=1) {
		max_number <- max(apply(data, 2, max))
		min_number <- min(apply(data, 2, min))
		breaks <- seq(min_number-1, max_number+1, by = width)

		if (length(xlim) == 1 && xlim == 0) {
			xlim <- c(min_number, max_number)
		}

		dataList <- list()
		for (i in 1:length(data[1,])) {
			data.cut <- cut(data[,i], breaks=breaks)
			dataList[[i]] <- data.cut
		}
		cumList <- list()
		for (i in 1:length(dataList)) {
			data.cum <- c("0"=0, cumsum(table(dataList[[i]])))
			data.cum <- data.cum / length(dataList[[i]])
			cumList[[i]] <- data.cum
		}
		plot(cumList[[1]], breaks, type="l", col=colors[1], ylim=xlim, main="Cummulative frequencies", xlab="Frequency", ylab=xlab, xlim=c(0,1), lty=lty[1])
		for (i in 2:length(cumList)) {
			lines(cumList[[i]], breaks, col=colors[i], lty=lty[i])
		}
		legend("topleft", legend=names, col=colors, lty=lty, lwd=2)
})


setMethod("cumFreq.flipped", signature("list", "character", "numeric", "character", "character", "numeric", "numeric"),
	function(data, names, width, colors, xlab, xlim=0, lty=1) {
		max_number <- 0
		min_number <- 0
		for (item in data) {
			max_number <- ifelse(max_number < max(item), max(item), max_number)
			min_number <- ifelse(min_number > min(item), min(item), min_number)
		}
#		print(max_number)
#		print(min_number)
		if (length(xlim) == 1 && xlim == 0) {
			xlim <- c(min_number, max_number)
		}
		breaks <- seq(min_number-1, max_number+1, by = width)
		dataList <- list()
		i <- 1
		for (item in data) {
			data.cut <- cut(item, breaks)
			dataList[[i]] <- data.cut
			i <- i + 1
		}
#		print(str(dataList))
		cumList <- list()
		for (i in 1:length(dataList)) {
			data.cum <- c("0"=0, cumsum(table(dataList[[i]])))
			data.cum <- data.cum / length(dataList[[i]])
			cumList[[i]] <- data.cum
		}
#		print(cumList)
		plot(cumList[[1]], breaks, type="l", col=colors[1], ylim=xlim, main="Cummulative frequencies", xlab="Frequency", ylab=xlab, xlim=c(0,1), lty=lty[1])
		for (i in 2:length(cumList)) {
			lines(cumList[[i]], breaks, col=colors[i], lty=lty[i])
		}
		legend("topleft", legend=names, col=colors, lty=lty, lwd=2)
})




################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################

setGeneric("plot.getPairLenDiff", function(data, colors, lty, names, xlab, xlim, width) standardGeneric("plot.getPairLenDiff"))
setMethod("plot.getPairLenDiff", signature("matrix", "character", "numeric", "character", "character", "numeric", "numeric"),
	function(data, colors, lty, names, xlab, xlim, width) {
		print("chkpnt1")
		par(mfrow=c(2, 1))
		cumFreq(data=data, colors=colors, lty=lty, names=names, xlab=xlab, xlim=xlim, width=width)
		abline(h=0.05, col="brown")
		abline(h=0.95, col="brown")
		abline(h=0.25, col="red")
		abline(h=0.75, col="red")
		boxplot(x=data, names=names, ylim=xlim, ylab=xlab)
		print("Finish!")
})

setGeneric("scattWithHats", function(data, x, y, cat) standardGeneric("scattWithHats"))
setMethod("scattWithHats", signature("data.frame", "character", "character", "character"),
	function(data, x, y, cat) {
		histx <- ggplot(data, aes(x=as.numeric(as.character(data[,x])), group=eval(parse(text=cat)), col=data[,cat])) + geom_density(show.legend=FALSE) + theme(legend.position="none") + labs(y="", x="")
		empty <- ggplot() + theme_void()
		histy <- ggplot(data, aes(x=as.numeric(as.character(data[,y])), group=eval(parse(text=cat)), col=eval(parse(text=cat)))) + geom_density() + coord_flip() + scale_y_reverse() + labs(y="", x="", color=cat)
		scxy <- ggplot(data, aes(x=as.numeric(as.character(data[,x])), y=as.numeric(as.character(data[,y])), col=data[,cat])) + geom_point(show.legend=FALSE) + theme_classic() + theme(legend.position="none") + labs(x="", y="")
		grid.arrange(empty, histx, histy, scxy, widths=c(2,4), heights=c(1,4))
})

setGeneric("scattWithStrings", function(data, x, y, cat, xlimx=0, ylimy=0) standardGeneric("scattWithStrings"))
setMethod("scattWithStrings", signature("data.frame", "character", "character", "character"),
	function(data, x, y, cat, xlimx=0, ylimy=0) {
		layout(matrix(c(1,2,3,4), 2, 2, byrow=TRUE, ), widths=c(5, 2), heights=c(3, 5))
		cat_names <- names(table(data[,cat]))
		subset_list_x <- list()
		subset_list_y <- list()
		for(name in cat_names){
			subset_list_x[[name]] <- data[data[,cat]==name,x]
			subset_list_x[[name]] <- subset_list_x[[name]][!is.na(subset_list_x[[name]])]
			subset_list_y[[name]] <- data[data[,cat]==name,y]
			subset_list_y[[name]] <- subset_list_y[[name]][!is.na(subset_list_y[[name]])]
		}
		data_x <- data[,x]
		data_x <- data_x[!is.na(data_x)]
		width_x <- max(data_x) / 200
		data_y <- data[,y]
		data_y <- data_y[!is.na(data_y)]
		width_y <- max(data_y) / 200
		cumFreq(subset_list_x, names=names(subset_list_x), width=width_x, colors=as.character(1:length(cat_names)), xlab=x, xlim=xlimx, lty=rep(1, length(cat_names)))
		plot.new()
		if(length(xlimx) > 1 & length(ylimy) > 1){
			plot(as.numeric(as.character(data[,x])), as.numeric(as.character(data[,y])), col=data[,cat], pch=16, xlab=x, ylab=y, xlim=xlimx, ylim=ylimy)
		}
		else {
			plot(as.numeric(as.character(data[,x])), as.numeric(as.character(data[,y])), col=data[,cat], pch=16, xlab=x, ylab=y)
		}
		cumFreq.flipped(subset_list_y, names=names(subset_list_y), width=width_y, colors=as.character(1:length(cat_names)), xlab=y, xlim=ylimy, lty=rep(1, length(cat_names)))
	#		ggplot(data, aes(x=as.numeric(as.character(data[,x])), y=as.numeric(as.character(data[,y])), col=data[,cat])) + geom_point(show.legend=FALSE) + theme_classic() + theme(legend.position="none") + labs(x="", y="")
})

setMethod("scattWithStrings", signature("matrix", "character", "character", "character"),
	function(data, x, y, cat) {

})
