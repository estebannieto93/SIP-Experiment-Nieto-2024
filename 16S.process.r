
lapply(c('ggfortify', 'psych','msa', 'ggplot2','RColorBrewer', 'reshape2', 'ape', 'phyloseq', 'vegan', 'cowplot', 'dplyr', 'gplots', 'dada2', 'phangorn'), require,character.only=TRUE) #add as necessary

fully.process<-function(forward_read_names="_L001_R1_001.fastq.gz", reverse_read_names="_L001_R2_001.fastq.gz", seed=1, displayx=10, filetype=".jpg",trimleft=10, maxeef=4, maxeer=4,trainingset="silva_nr_v138_train_set.fa.gz", truncLenF=260, truncLenR=220, tree=FALSE,multithread=TRUE, loopnr=1){

path= getwd()
fnFs= sort(list.files(path, pattern= forward_read_names))
fnRs= sort(list.files(path, pattern= reverse_read_names))

sample.names = sapply(strsplit(fnFs, "_"), `[`, 1)  #extract sample names
fnFs = file.path(path, fnFs) #specify global path to the sequence files
fnRs = file.path(path, fnRs)

filt_path= file.path(path, "filtered") # Place filtered files in filtered/ subdirectory
filtFs=file.path(filt_path, paste0(sample.names, forward_read_names))#Create a subdirectory and file names for the filtered files
filtRs= file.path(filt_path, paste0(sample.names, reverse_read_names))

out=filterAndTrim(fnFs, filtFs, fnRs, filtRs, truncLen=c(truncLenF,truncLenR),trimLeft=10, maxN=0, maxEE=c(maxeef,maxeer), truncQ=2, rm.phix=TRUE, compress=TRUE, multithread=TRUE)

errF = learnErrors(filtFs, multithread=TRUE)
errR = learnErrors(filtRs, multithread=TRUE)

derepFs = derepFastq(filtFs, verbose=TRUE)
derepRs = derepFastq(filtRs, verbose=TRUE)

names(derepFs) = sample.names
names(derepRs) = sample.names

dadaFs = dada(derepFs, err=errF, multithread=TRUE)
dadaRs = dada(derepRs, err=errR, multithread=TRUE)

mergers = mergePairs(dadaFs, derepFs, dadaRs, derepRs, verbose=TRUE, minOverlap = 10)

seqtab = makeSequenceTable(mergers)

seqtab.nochim = removeBimeraDenovo(seqtab, method="consensus", multithread=TRUE, verbose=TRUE)

getN = function(x) sum(getUniques(x))
track = cbind(out, sapply(dadaFs, getN), sapply(mergers, getN), rowSums(seqtab), rowSums(seqtab.nochim))
colnames(track) = c("Original Reads", "Filtered Reads", "Denoised Reads", "Merged Reads", "tabled", "Non-chimeric Reads")
rownames(track) = sample.names
track1= track[, -6]
write.table(track1, paste0(loopnr,"tracking_table.txt"), sep="\t")


taxa = assignTaxonomy(seqtab.nochim, trainingset, multithread=TRUE)
unname(head(taxa))
taxa=as.data.frame(taxa)
taxa[] <- lapply(taxa, as.character)
taxa$Phylum[is.na(taxa$Phylum)]<-taxa$Kingdom[is.na(taxa$Phylum)]
taxa$Class[is.na(taxa$Class)]<-taxa$Phylum[is.na(taxa$Class)]
taxa$Order[is.na(taxa$Order)]<-taxa$Class[is.na(taxa$Order)]
taxa$Family[is.na(taxa$Family)]<-taxa$Order[is.na(taxa$Family)]
taxa$Genus[is.na(taxa$Genus)]<-taxa$Family[is.na(taxa$Genus)]
taxa[] <- lapply(taxa, as.factor)
taxa=as.matrix(taxa)

merged = phyloseq(otu_table(seqtab.nochim, taxa_are_rows=FALSE),  tax_table(taxa))
merged@sam_data=sample_data(as.data.frame(sample_names(merged)))
sample_names(sample_data(merged))=sample_names(merged)
sample_data(merged)[ , 2] <- sample_data(merged)[ ,1]
colnames(merged@sam_data)=c("names", "dummy")
merged = subset_taxa(merged, Kingdom== "Bacteria")
saveRDS(merged, paste0(loopnr,"merged.rds"))

write.table(sort(sample_sums(merged)), paste0(loopnr,"reads.per.sample.txt"), sep="\t")}

fully.process()




  