library("igraph")

random_net <- sample_gnp(n = 50, p=0.1, directed = FALSE, loops = FALSE)
plot(random_net)
degree(random_net)
degree_distribution(random_net)

mean_distance(graph = random_net, directed = FALSE)
average.path.length(random_net, directed=FALSE, unconnected=TRUE)


#mean_distance calculates the average path length in a graph, by calculating the shortest paths between all pairs of vertices (both ways for directed graphs). This function does not consider edge weights currently and uses a breadth-first search.
mean_distance(graph = random_net, directed = FALSE)

mean(c(distances(random_net, mode="out")))
mean(c(shortest.paths(random_net)))

#### ANSWER! (https://stackoverflow.com/questions/42352578/mean-distance-vs-mean-of-disances-in-igraph)
random_net_lengths <- distances(random_net)[lower.tri(distances(random_net))]
mean(random_net_lengths)
random_net_lengths[!is.finite(random_net_lengths)] <- NA
mean(random_net_lengths, na.rm=TRUE)
boxplot(random_net_lengths)

# "gnm" for nodes and edeges or "gnp" for nodes and probability of egding
random_net1 <- erdos.renyi.game(n=50, p.or.m = 60, type=c("gnm"), directed = FALSE, loops = FALSE)
random_net1_lengths <- distances(random_net1)[lower.tri(distances(random_net1))]
plot(random_net1)
mean(random_net1_lengths)
random_net1_lengths[!is.finite(random_net1_lengths)] <- NA
mean(random_net1_lengths, na.rm=TRUE)

random_net2 <- erdos.renyi.game(n=50, p.or.m = 60, type=c("gnm"), directed = FALSE, loops = FALSE)
random_net2_lengths <- distances(random_net2)[lower.tri(distances(random_net2))]
plot(random_net2)
mean(random_net2_lengths)
random_net2_lengths[!is.finite(random_net2_lengths)] <- NA
mean(random_net2_lengths, na.rm=TRUE)

boxplot(x = list(random_net1 = random_net1_lengths,
                 random_net2 = random_net2_lengths))

length(random_net1_lengths)
length(random_net2_lengths)
wilcox.test(random_net1_lengths, random_net2_lengths)



#---- LOOP ----#
oberved_net <- erdos.renyi.game(n=50, p.or.m = 60, type=c("gnm"), directed = FALSE, loops = FALSE)
oberved_net_lengths <- distances(oberved_net)[lower.tri(distances(oberved_net))]
plot(oberved_net)
mean(oberved_net_lengths)
oberved_net_lengths[!is.finite(oberved_net_lengths)] <- NA
mean(oberved_net_lengths, na.rm=TRUE)

number_of_random_networks = 1000
number_of_nodes_in_random_networks = 50
number_of_edges_in_random_networks = 60
p_values_vector <- c()
for (i in 1:number_of_random_networks) {
  print(paste0(i*100/number_of_random_networks, "%"))
  random_net <- erdos.renyi.game(n=number_of_nodes_in_random_networks, p.or.m = number_of_edges_in_random_networks, type=c("gnm"), directed = FALSE, loops = FALSE)
  random_net_lengths <- distances(random_net)[lower.tri(distances(random_net))]
  mean(random_net_lengths)
  random_net_lengths[!is.finite(random_net_lengths)] <- NA
  p_values_vector[i] <- c(wilcox.test(oberved_net_lengths, random_net_lengths)$p.value)
}
par(mfrow=c(1,2))
hist(-log10(p_values_vector), breaks = 30, col="lightblue", main = "P value"); abline(v=-log10(0.05), col="black", lwd=1, lty=2); mtext(paste0((length(p_values_vector[p_values_vector<0.05])/number_of_random_networks)*100, "% < 0.05"), side=3)
print(paste0(length(p_values_vector[p_values_vector<0.05]), " out of ", number_of_random_networks, " (", (length(p_values_vector[p_values_vector<0.05])/number_of_random_networks)*100, "%)"," are significant to 0.05 level"))

p_values_vector_adjusted <- p.adjust(p = p_values_vector)
hist(-log10(p_values_vector_adjusted), breaks = 30, col="salmon", main = "P adjusted"); abline(v=-log10(0.05), col="black", lwd=1, lty=2); mtext(paste0((length(p_values_vector_adjusted[p_values_vector_adjusted<0.05])/number_of_random_networks)*100, "% < 0.05"), side=3)
print(paste0(length(p_values_vector_adjusted[p_values_vector_adjusted<0.05]), " out of ", number_of_random_networks, " (", (length(p_values_vector_adjusted[p_values_vector_adjusted<0.05])/number_of_random_networks)*100, "%)"," are significant to 0.05 level after adjusting"))

