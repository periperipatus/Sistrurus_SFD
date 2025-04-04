picols<- c("indexStart_indexStop-firstPosWithData_lastPosWithData-WinStart_WinStop","Chr","WinCenter","tW","tP","tF","tH","tL","Tajima","fuf","fud","fayh","zeng","nSites","geneID")
hwecols<- c("Chromo","Position","Major","Minor","hweFreq","Freq","F","LRT","p-value")

gene_categories<- function(dat, geneID){
  require(dplyr)
  out<- dat %>%
    mutate(cat_upper=ifelse(geneID %in% upper,"upper","")) %>%
    mutate(cat_lower=ifelse(geneID %in% lower,"lower","")) %>%
    mutate(cat_immune=ifelse(geneID %in% immune,"immune","")) %>%
    mutate(cat_DEG=ifelse(geneID %in% DEG, "DEG","")) %>%
    mutate(immune_DEG=paste(cat_upper, cat_lower, cat_immune, cat_DEG, sep="_")) %>%
    mutate(immune_DEG=gsub("__|___","_",immune_DEG)) %>%
    mutate(immune_DEG=gsub("^_|_$","",immune_DEG)) %>%
    mutate(immune_DEG=gsub("_"," ",immune_DEG)) %>%
    mutate(immune_DEG=trimws(immune_DEG))
  return(out)
}

read_hwe.gz<- function(path,pops){
  hwe.out<- list()
  for(p in pops){
    print(p)
    ind<- readLines(paste0(path,"/lists/final_172_",p,".txt"))
    print("reading synonymous sites")
    
    files<- list.files(paste0(path,"/FGD/degeneracy/",p,"/"),pattern="syn.hwe")
    
    syn.out<- list()
    for(f in files){
      #f=files[1]
      syn.hwe<- read.table(paste0(path,"/FGD/degeneracy/",p,"/",f),header=TRUE)
      colnames(syn.hwe)<- hwecols
      if(nrow(syn.hwe)==0){
        print(paste(f,"has no sites"))
      }else if(nrow(syn.hwe)>0){
        print(paste(f,"has some sites"))
        gene<- word(f,2,2, sep="\\.")
        syn.hwe$geneID<- gene
        syn.hwe$n_ind<- length(ind)
        syn.hwe$loc<- paste0(syn.hwe$Chromo,":",syn.hwe$Position, syn.hwe$gene)
        #using the hwe allele freq to calculate heterozygosity from https://github.com/clairemerot/angsd_pipeline/blob/master  /01_scripts/Rscripts/Hobs_sliding.r
        syn.hwe$Hexp<-2*(syn.hwe$hweFreq)*(1-syn.hwe$hweFreq) #Hexp = 2*(hweFreq)(1-hweFreq)
        #syn.hwe$Hexp<-2*(syn.hwe$Freq)*(1-syn.hwe$Freq)
        syn.hwe$Hexp_n <- round(syn.hwe$Hexp*length(ind),0)
        syn.hwe$Hobs<-syn.hwe$Hexp-(syn.hwe$F*syn.hwe$Hexp)
        syn.hwe$Hobs_n<- round(syn.hwe$Hobs*length(ind),0)
        
        #prop homozygotes at minor 
        syn.hwe$Homoz_minor<-syn.hwe$Freq*syn.hwe$Freq+syn.hwe$Freq*(1-syn.hwe$Freq)*syn.hwe$F
        syn.hwe$n_Homoz_minor<-round(syn.hwe$Homoz_minor*length(ind),0)
        
        #the proportion of homozygotes at the major
        syn.hwe$Homoz_major<-(1-syn.hwe$Freq)*(1-syn.hwe$Freq)+syn.hwe$Freq*(1-syn.hwe$Freq)*syn.hwe$F
        syn.hwe$n_Homoz_major<-round(syn.hwe$Homoz_major*length(ind),0)
        syn.hwe$polymorphic<- ifelse(syn.hwe$hweFreq>0.001, 1,0)
        syn.hwe$pop<- p
        syn.hwe$type<- "synonymous"
        syn.out[[f]]<- syn.hwe
      }
    }
    syn.hwe<- do.call("rbind", syn.out)
    
    print("reading non-synonymous sites")
    files<- list.files(paste0(path,"/FGD/degeneracy/",p,"/"),pattern="ns.hwe")
    ns.out<- list()
    for(f in files){
      #f=files[1]
      ns.hwe<- read.table(paste0(path,"/FGD/degeneracy/",p,"/",f),header=TRUE)
      colnames(ns.hwe)<- hwecols
      if(nrow(ns.hwe)==0){
        print(paste(f,"has no sites"))
      }else{
        print(paste(f,"has some sites"))
        gene<- word(f,2,2, sep="\\.")
        ns.hwe$n_ind<- length(ind)
        ns.hwe$geneID<- gene
        ns.hwe$loc<- paste0(ns.hwe$Chromo,":",ns.hwe$Position, ns.hwe$gene)
        #using the hwe allele freq to calculate heterozygosity from https://github.com/clairemerot/angsd_pipeline/blob/master  /01_scripts/Rscripts/Hobs_sliding.r
        #where p=minor q=major
        #2pq Heterozygosity under HWE
        ns.hwe$Hexp<-2*(ns.hwe$hweFreq)*(1-ns.hwe$hweFreq) #Hexp = 2*(hweFreq)(1-hweFreq)
        #ns.hwe$Hexp<-2*(ns.hwe$Freq)*(1-ns.hwe$Freq)
        ns.hwe$Hexp_n <- round(ns.hwe$Hexp*length(ind),0)
        ns.hwe$Hobs<-ns.hwe$Hexp-(ns.hwe$F*ns.hwe$Hexp)
        ns.hwe$Hobs_n<- round(ns.hwe$Hobs*length(ind),0)
        
        #prop homozygotes at minor 
        ns.hwe$Homoz_minor<-ns.hwe$Freq*ns.hwe$Freq+ns.hwe$Freq*(1-ns.hwe$Freq)*ns.hwe$F
        ns.hwe$n_Homoz_minor<-round(ns.hwe$Homoz_minor*length(ind),0)
        
        #the proportion of homozygotes at the major
        ns.hwe$Homoz_major<-(1-ns.hwe$Freq)*(1-ns.hwe$Freq)+ns.hwe$Freq*(1-ns.hwe$Freq)*ns.hwe$F
        ns.hwe$n_Homoz_major<-round(ns.hwe$Homoz_major*length(ind),0)
        ns.hwe$polymorphic<- ifelse(ns.hwe$hweFreq>0.001, 1,0)
        ns.hwe$pop<- p
        ns.hwe$type<- "non-synonymous"
        ns.out[[f]]<- ns.hwe
      }
    }
    ns.hwe<- do.call("rbind", ns.out)
    
    hwe.out[[p]]<- rbind(ns.hwe, syn.hwe)
    
  }
  
  hwe.degen<- do.call("rbind", hwe.out)
  return(hwe.degen)
}


plot_categories<- function(dat,pops){
  require(dplyr)
  out<- dat %>%  
    mutate(pop=factor(pop, levels=c(pops))) %>% 
    mutate(type2=ifelse(type=="synonymous","S","N")) %>%
    mutate(type3=ifelse(type=="synonymous","syn","ns")) %>%
    mutate(site_category=paste(immune_DEG, type)) %>%
    mutate(pop_site=paste(pop, type3)) %>%
    mutate(gene_pop=paste0(geneID, pop))
    return(out)
}


read_pestPG<- function(path,pops){
  require(dplyr)
  #path=paste0(datadir,"/FGD/degeneracy/")
  #path=paste0(datadir,"/geneDesert/angsd_variants/")
  #pops=pop3
  pi.out<- list()
  
  for(p in pops){
    #p="STER"
    print(p)
    #Synonymous pi
    print("reading synonymous")
    syn.pi<- list()
    files<- list.files(path=paste0(path,p,"/"),pattern="syn.pestPG")
    for(f in files){
      #f<- files[1]
      dat<- read.table(paste0(path,p,"/",f), sep="\t")
      gene<- word(f,2,2,sep="\\.")
      dat$geneID<- gene
      syn.pi[[f]]<- dat
    }
    syn.pi<- do.call("rbind",syn.pi)
    colnames(syn.pi)<- picols
    syn.pi<- syn.pi[!duplicated(syn.pi$geneID),]
    syn.pi<- syn.pi[!is.na(syn.pi$geneID),]
    syn.pi$type<- "synonymous"
    syn.pi$pop<- p   
    
    #non-synonymous pi
    print("reading non-synonymous")
    ns.pi<- list()
    files<- list.files(path=paste0(path,p,"/"),pattern="ns.pestPG")
    for(f in files){
      #f<- files[1]
      dat<- read.table(paste0(path,p,"/",f), sep="\t")
      gene<- word(f,2,2,sep="\\.")
      dat$geneID<- gene
      ns.pi[[f]]<- dat
    }
    ns.pi<- do.call("rbind",ns.pi)
    
    colnames(ns.pi)<- picols
    ns.pi<- ns.pi[!duplicated(ns.pi$geneID),]
    ns.pi<- ns.pi[!is.na(ns.pi$geneID),]
    ns.pi$type<- "non-synonymous"
    ns.pi$pop<- p
    
    pi<- rbind(ns.pi,syn.pi)
    
    pi.out[[p]]<- pi
    
  }
  pi.dm<- do.call("rbind",pi.out)
  pi.dm <- pi.dm %>%
    mutate(watterson_theta=tW/nSites,
           pi=tP/nSites) %>%
    mutate(log10pi=log10(pi)) %>%
    mutate(log10pi=ifelse(pi==0,NA,log10pi))
  
  pi.dm<- gene_categories(pi.dm, geneID)
  pi.dm<- plot_categories(pi.dm, pops)
}

