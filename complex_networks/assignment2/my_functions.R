getRandomGraphData = function(numberRandomGraphs, N, p) {
    randomGraphData <- data.frame();

    for (i in 1:numberRandomGraphs) {
        randomGraph <- igraph::sample_gnp(N, p); # ER model
        row = list(apl = igraph::average.path.length(randomGraph),
                   maxDegree = max(igraph::degree(randomGraph)),
                   transitivity = igraph::transitivity(randomGraph));

        randomGraphData = rbind(randomGraphData, row);
    }
    (randomGraphData)
}

plotDistribution = function(randomGraphVariable,
                            jazzNetStatistic,
                            xlims,
                            title) {

    randomGraphVariable_mean = mean(randomGraphVariable)

    hist(randomGraphVariable,
         main=title);
    abline(v=randomGraphVariable_mean, lty=1, col=2);
    legend(x = "topright",
        legend = c("distribution mean"),
        lty = c(1),
        col = c(2),
        lwd = 2);

    hist(randomGraphVariable,
         xlim = xlims,
         main=sprintf("%s compared to jazzNet", title));
    abline(v=randomGraphVariable_mean, lty=1, col=2);
    abline(v=jazzNetStatistic, lty=2, col=4);
    legend(x = "topright",
        legend = c("distribution mean", "jazz network"),
        lty = c(1, 2),
        col = c(2, 4),
        lwd = 2);
}
