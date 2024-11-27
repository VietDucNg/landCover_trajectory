#### stack all the LULC maps as one ####
# get a list of raster name
raster_names = Sys.glob('data/*.tiff')
raster_names
# stack
lulc_rast = stack(raster_names)
lulc_rast
plot(lulc_rast)

#### convert LULC raster to table ####
lulc_df = as.data.frame(rasterToPoints(lulc_rast))
head(lulc_df)

# create a column of concatened sequence
lulc_df$seq = as.vector(paste(lulc_df[,3],lulc_df[,4],lulc_df[,5],lulc_df[,6],lulc_df[,7],lulc_df[,8],lulc_df[,9],lulc_df[,10]))
lulc_df$seq

# get the number of unique sequences
unique_traj = sort(unique(lulc_df$seq))
length(unique_traj)  # 1692 different trajectories

# get the frequency of these trajectories
traj_freq = as.data.frame(table(lulc_df$seq))
# sort the sequences from more to less frequent
traj_freq = traj_freq[order(traj_freq$Freq,decreasing = TRUE),]
traj_freq

#### Creation of a state sequence object
#### short state names (for printed output) and long state labels (for the graphics' legend). 
alphabet = sort(unique(unlist(lulc_df[,3:10])))
label = c('a1','b1','c1','d1','e1','f1','g1','h1','i1','k1','l1')
short_label = c('a','b','c','d','e','f','g','h','i','k','l')
color = c("darkolivegreen","green","yellow","red","orange","grey",'darkgreen','blue','lightblue','purple','brown')
lulc_seq = seqdef(lulc_df, 3:10, alphabet = alphabet, states = short_label,
                  cpal = color, labels = label)

### visualize the sequence
# plot 10 sequences (chosen to show diversity)
some_seq = lulc_seq[c(41,72,174,322,632,672,817,885,927,999),]
seqiplot(some_seq, with.legend = T, border = T, main = "Some selected sequences")

# Plot all the sequences in the data set, sorted by states from start.
seqIplot(lulc_seq, sortv = "from.start", with.legend = T, main = "All sequences 2001-2021")

# Plot the 10 most frequent sequences.
seqfplot(lulc_seq, with.legend = T, main="10 most common sequences")


#### Explore the sequence data set by computing and visualizing descriptive statistics

# plot the state distributions by time step. 
seqdplot(lulc_seq, with.legend = T, border = NA,main="Land cover (states) distribution", ylab="Proportion of study area")

# plot the transversal entropy index (Landscape entropy over time)
seqHtplot(lulc_seq, main = "Entropy", ylab="Entropy index value",xlab=("Time"))

# Plot dominant land cover of the transversal state distributions.
seqmsplot(lulc_seq, with.legend = T, main ="Most frequent land cover")

# Plot the mean time spent in each land cover category.
seqmtplot(lulc_seq, with.legend = T, main = "Permanence", ylab="Number of 2 years periods")


