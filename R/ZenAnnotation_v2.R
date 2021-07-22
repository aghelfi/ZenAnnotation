Zen.Annotation <- function(input_fasta,dir_source_input,dir_output){
  input_fasta <- as.character(readline(prompt="Enter fasta file (.fasta or .fa): "))
  test_input_fasta <- file.exists(input_fasta)
  while(test_input_fasta == FALSE){
    input_fasta <- as.character(readline(prompt="Enter fasta file (.fasta or .fa): "))
    test_input_fasta <- file.exists(input_fasta)
  }
  dir_source_input <- as.character(readline(prompt="Enter source dir: "))
  test_dir_source_input <- dir.exists(dir_source_input)
  while(test_dir_source_input == FALSE){
    dir_source_input <- as.character(readline(prompt="Enter source dir: "))
    test_dir_source_input <- dir.exists(dir_source_input)
  }
  dir_output <- as.character(readline(prompt="Enter output dir: "))
  test_dir_output <- dir.exists(dir_output)
  while(test_dir_output == FALSE){
    dir_output <- as.character(readline(prompt="Enter output dir: "))
    test_dir_output <- dir.exists(dir_output)
  }
  dir_database <- as.character(readline(prompt="Enter KusakiDB ZEN dir: "))
  dir_database <- paste(dir_database,"kusa_zen.dmnd",sep="/")
  test_dir_database <- file.exists(dir_database)
  while(test_dir_database == FALSE){
    dir_database <- as.character(readline(prompt="Enter KusakiDB ZEN dir: "))
    dir_database <- paste(dir_database,"kusa_zen.dmnd",sep="/")
    test_dir_database <- file.exists(dir_database)
  }
  cat("Connect to KusakiDB-Docker database \n")
  database_IP <- as.character(readline(prompt="Enter IP to access KusakiDB-docker database (type 0 for localhost): "))
  con <- dbConnect(MariaDB(),user = 'root',password = 'password',host = database_IP,dbname = 'KUSAKIDB',port=3308)
  list_tables <- dbListTables(con)
  len_list_tables <- length(list_tables)
  if(len_list_tables == 15){
    cat("\nKusakiDB-Docker successfully connected\n\nEnter parameters for sequence alignment\n")
    seq_id <- as.numeric(readline(prompt="Set sequence identity (%) [50]: "))
    if(is.na(seq_id) | seq_id < 10 | seq_id > 100){
      seq_id <- 50
    }
    query_cov <- as.numeric(readline(prompt="Set minimum query coverage (%) [50]: "))
    if(is.na(query_cov) | query_cov < 10 | query_cov > 100){
      query_cov <- 50
    }
    target_cov <- as.numeric(readline(prompt="Set minimum target coverage (%) [50]: "))
    if(is.na(target_cov) | target_cov < 10 | target_cov > 100){
      target_cov <- 50
    }
    e_value <- as.numeric(readline(prompt="Set maximum E-value [1e-6]: "))
    if(is.na(e_value) | e_value > 10 | e_value < 1e-100){
      e_value <- 1e-6
    }
    threads <- as.numeric(readline(prompt="Set maximum number of threads [10]: "))
    if(is.na(threads) | threads < 1 | threads > 40){
      threads <- 10
    }
    options_input_align <- paste("\n\n[1] Sequence identity: ",seq_id,"\n[2] Query coverage: ", query_cov,"\n[3] Target coverage: ",target_cov,"\n[4] E-value: ",e_value,"\n[5] Number of threads: ",threads, "\n\nConfirm options? (Yes or No) \n\n",sep="")
    confirm_options_input_align <- toupper(as.character(readline(prompt=options_input_align)))
    while(confirm_options_input_align == "NO" | confirm_options_input_align == "N"){
      change_input <- as.integer(readline(prompt="Select which option you would like to change: "))
      if(change_input == 1){
        seq_id <- as.numeric(readline(prompt="Set sequence identity (%) [50]: "))
        if(is.na(seq_id) | seq_id < 10 | seq_id > 100){
          seq_id <- 50
        }
        options_input_align <- paste("\n\n[1] Sequence identity: ",seq_id,"\n[2] Query coverage: ", query_cov,"\n[3] Target coverage: ",target_cov,"\n[4] E-value: ",e_value,"\n[5] Number of threads: ",threads, "\n\nConfirm options? (Yes or No) \n\n",sep="")
        confirm_options_input_align <- toupper(as.character(readline(prompt=options_input_align)))
      } else if(change_input == 2){
        query_cov <- as.numeric(readline(prompt="Set minimum query coverage (%) [50]: "))
        if(is.na(query_cov) | query_cov < 10 | query_cov > 100){
          query_cov <- 50
        }
        options_input_align <- paste("\n\n[1] Sequence identity: ",seq_id,"\n[2] Query coverage: ", query_cov,"\n[3] Target coverage: ",target_cov,"\n[4] E-value: ",e_value,"\n[5] Number of threads: ",threads, "\n\nConfirm options? (Yes or No) \n\n",sep="")
        confirm_options_input_align <- toupper(as.character(readline(prompt=options_input_align)))
      } else if(change_input == 3){
        target_cov <- as.numeric(readline(prompt="Set minimum target coverage (%) [50]: "))
        if(is.na(target_cov) | target_cov < 10 | target_cov > 100){
          target_cov <- 50
        }
        options_input_align <- paste("\n\n[1] Sequence identity: ",seq_id,"\n[2] Query coverage: ", query_cov,"\n[3] Target coverage: ",target_cov,"\n[4] E-value: ",e_value,"\n[5] Number of threads: ",threads, "\n\nConfirm options? (Yes or No) \n\n",sep="")
        confirm_options_input_align <- toupper(as.character(readline(prompt=options_input_align)))
      } else if(change_input == 4){
        e_value <- as.numeric(readline(prompt="Set maximum E-value [1e-6]: "))
        if(is.na(e_value) | e_value > 10 | e_value < 1e-100){
          e_value <- 1e-6
        }
        options_input_align <- paste("\n\n[1] Sequence identity: ",seq_id,"\n[2] Query coverage: ", query_cov,"\n[3] Target coverage: ",target_cov,"\n[4] E-value: ",e_value,"\n[5] Number of threads: ",threads, "\n\nConfirm options? (Yes or No) \n\n",sep="")
        confirm_options_input_align <- toupper(as.character(readline(prompt=options_input_align)))
      } else if(change_input == 5){
        threads <- as.numeric(readline(prompt="Set maximum number of threads [10]: "))
        if(is.na(threads) | threads < 1 | threads > 40){
          threads <- 10
        }
        options_input_align <- paste("\n\n[1] Sequence identity: ",seq_id,"\n[2] Query coverage: ", query_cov,"\n[3] Target coverage: ",target_cov,"\n[4] E-value: ",e_value,"\n[5] Number of threads: ",threads, "\n\nConfirm options? (Yes or No) \n\n",sep="")
        confirm_options_input_align <- toupper(as.character(readline(prompt=options_input_align)))
      }
    }
    if(confirm_options_input_align == "YES" | confirm_options_input_align == "Y"){
      setwd(dir_output)
      if(!dir.exists("zen_v2.0")){
        system("mkdir zen_v2.0")
      }
      if(!dir.exists("temp")){
        system("mkdir temp")
      }
      filename1query <- paste(dir_source_input,input_fasta,sep="/")
      line1query <- "awk '{print $1}' FILENAME > temp/query.fasta"
      line1query <- sub("FILENAME", filename1query, line1query)
      system(line1query)
      if(file.exists("temp/query.fasta")){
        line_flawed <- " awk '{if(NR==1) {print $0} else {if($0 ~ /^>/) {print RS $0} else {printf $0}}}' temp/query.fasta  | awk '{print $1}' |awk '$0 ~ \">\" {id = $0; getline; print id OFS $0}'|perl -pe 's/\r(?!\n)//g'| rev | cut -c2- | rev | egrep \"[*]\" | awk -F\" \" '{print $1 \"\t Yes\" }' | cut -c2-  > temp/flawed_stop_codons.tsv "
        system(line_flawed)
        test_stop <- "wc -l < temp/flawed_stop_codons.tsv"
        size_test_stop <- as.numeric(system(test_stop, intern=T))
        if(size_test_stop > 0){
          stop <- fread("temp/flawed_stop_codons.tsv", sep="\t", header=F)
          len_stop <- dim(stop)[1]
        }else{
          len_stop <- 0
        }
        #
        line_fgrep <- "fgrep '>' temp/query.fasta | cut -c2-"
        all_queries <- as.data.frame(as.character(system(line_fgrep, intern=T)))
        colnames(all_queries) <- "queryid_pep"
        # set path for diamond
        temp_run_plast <- "/home/plantgenomics/r_packages/diamond blastp -q temp/query.fasta -d USERSDATABASEDIR -o temp/output_search.tsv -f 6 --threads USERSTHREADS -b6 -c1 --more-sensitive --top 20 --evalue USERSEVALUE --id USERSSEQID --query-cover USERSQUERYCOVER --subject-cover USERSTARGETCOVER --quiet"
        run_plast <- gsub ("USERSSEQID", seq_id, temp_run_plast)
        run_plast <- gsub ("USERSQUERYCOVER", query_cov, run_plast)
        run_plast <- gsub ("USERSTARGETCOVER", target_cov, run_plast)
        run_plast <- gsub ("USERSEVALUE", e_value, run_plast)
        run_plast <- gsub ("USERSTHREADS", threads, run_plast)
        run_plast <- gsub ("USERSDATABASEDIR", dir_database, run_plast)
        write.csv(run_plast, "temp/zen_v2_log.txt", row.names=F)
        system (run_plast)
        test_file <- "wc -l < temp/output_search.tsv"
        size_test_file <- as.numeric(system(test_file, intern=T))
        if(size_test_file > 0){
          outtable <- fread("temp/output_search.tsv", sep="\t",header=F)
          colnames(outtable) <-  c("queryid_pep", "kusakidb_id", "sequence_identity", "length", "mis", "gaps", "startquery", "endquery", "starttarget", "endtarget", "e_value", "bitscore")
          outtable <- outtable[grep("89399_0:005319", outtable$kusakidb_id, invert=T),]# manual curation: bat genes annotated as Arabidopsis in orthoDB
          uniq <- outtable[!duplicated(outtable[, c('queryid_pep')]),c('queryid_pep','bitscore')]
          colnames(uniq)[2] <- "bittarget"
          outtable <- merge(outtable,uniq,by="queryid_pep")
          outtable$frac <- outtable$bitscore/outtable$bittarget
          outtable <- outtable[outtable$frac > 0.99,]
          plants <- outtable[grep("KORTHO",outtable$kusakidb_id, invert=T),]
          #
          list_kusakidb_id <- unique(plants$kusakidb_id)
          formated_list_kusakidb_id <- paste(as.character(list_kusakidb_id), collapse=",")
          line1 <- "SELECT * FROM kusakidb WHERE kusakidb_id IN (KUSAKIDB_ID);"
          line1 <- sub("KUSAKIDB_ID", formated_list_kusakidb_id, line1)
          tanota1 <- dbGetQuery(con,line1)
          plants_t1 <- merge(tanota1,plants,by="kusakidb_id")
          predicted_species <- as.data.frame(table(plants_t1$ncbi_taxid)) # attention
          predicted_species <- as.character(predicted_species[order(predicted_species$Freq,decreasing=T),][1,1])
          line_taxon <- "select * from taxonomy where ncbi_taxid=PREDICTED;"
          line_taxon <- sub("PREDICTED", predicted_species, line_taxon)
          taxon <- dbGetQuery(con,line_taxon)
          taxon_hierarchy <- dbGetQuery(con,"select * from taxonomy;")
          sel_taxons <- merge(plants_t1,taxon_hierarchy,by="ncbi_taxid")
          ts_gen <- c();ts_fam <- c();ts_ord <- c();ts_cla <- c();ts_phy <- c()
          ts_gen <- sel_taxons[sel_taxons$genus_name == taxon$genus_name,]
          if(dim(ts_gen)[1] > 0){
            ts_gen$index <- 1
          }
          remaining_list <- as.data.frame(setdiff(sel_taxons$queryid_pep, ts_gen$queryid_pep))
          colnames(remaining_list) <- "queryid_pep"
          sel_taxons <- merge(remaining_list,sel_taxons,by="queryid_pep")
          ts_fam <- sel_taxons[sel_taxons$family_name==taxon$family_name,]
          if(dim(ts_fam)[1] > 0){
            ts_fam$index <- 2
          }
          remaining_list <- as.data.frame(setdiff(sel_taxons$queryid_pep, ts_fam$queryid_pep))
          colnames(remaining_list) <- "queryid_pep"
          sel_taxons <- merge(remaining_list,sel_taxons,by="queryid_pep")
          ts_ord <- sel_taxons[sel_taxons$order_name==taxon$order_name,]
          if(dim(ts_ord)[1] > 0){
            ts_ord$index <- 3
          }
          remaining_list <- as.data.frame(setdiff(sel_taxons$queryid_pep, ts_ord$queryid_pep))
          colnames(remaining_list) <- "queryid_pep"
          sel_taxons <- merge(remaining_list,sel_taxons,by="queryid_pep")
          ts_cla <- sel_taxons[sel_taxons$class_name==taxon$class_name,]
          if(dim(ts_cla)[1] > 0){
            ts_cla$index <- 4
          }
          remaining_list <- as.data.frame(setdiff(sel_taxons$queryid_pep, ts_cla$queryid_pep))
          colnames(remaining_list) <- "queryid_pep"
          if( dim(remaining_list)[1] > 0){
            sel_taxons <- merge(remaining_list,sel_taxons,by="queryid_pep")
            ts_phy <- sel_taxons[sel_taxons$phylum_name==taxon$phylum_name,]
            if(dim(ts_phy)[1] > 0){
              ts_phy$index <- 5
            }
          }
          plants_t1_taxon <- rbind(ts_gen,ts_fam,ts_ord,ts_cla,ts_phy)
          plants_t1_taxon[plants_t1_taxon$evidence_existence == "Yes",'evidence_existence'] <- 1
          plants_t1_taxon[plants_t1_taxon$evidence_existence == "No",'evidence_existence'] <- 2
          plants_t1_taxon <- plants_t1_taxon[order(plants_t1_taxon$index,plants_t1_taxon$evidence_existence),]
          plants_t1_taxon <- plants_t1_taxon[!duplicated(plants_t1_taxon[, c('queryid_pep')]),]
          plants_t1_taxon[plants_t1_taxon$evidence_existence == 1,'evidence_existence'] <- "Yes"
          plants_t1_taxon[plants_t1_taxon$evidence_existence == 2,'evidence_existence'] <- "No"
          rm(ts_gen,ts_fam,ts_ord,ts_cla,ts_phy,plants_t1)
          # source2desc
          list_source_accession <- unique(plants_t1_taxon$source_accession)
          formated_list_source_accession <- paste(shQuote(list_source_accession), collapse=",")
          line2 <- "SELECT * FROM source2desc WHERE source_accession IN (KUSAKIDB_ID);"
          line2 <- sub("KUSAKIDB_ID", formated_list_source_accession, line2)
          tanota2 <- dbGetQuery(con,line2)
          plants_t2 <- merge(plants_t1_taxon,tanota2,by="source_accession",all.x=T)
          rm(plants_t1_taxon,list_source_accession,formated_list_source_accession)
          # orthodb2desc
          list_orthodb_unique_id <- unique(plants_t2$orthodb_unique_id)
          formated_list_orthodb_unique_id <- paste(shQuote(list_orthodb_unique_id), collapse=",")
          line3 <- "SELECT * FROM orthodb2desc WHERE orthodb_unique_id IN (KUSAKIDB_ID);"
          line3 <- sub("KUSAKIDB_ID", formated_list_orthodb_unique_id, line3)
          tanota3 <- dbGetQuery(con,line3)
          plants_t3 <- merge(plants_t2,tanota3,by="orthodb_unique_id",all.x=T)
          rm(plants_t2,formated_list_orthodb_unique_id,list_orthodb_unique_id)
          # version
          list_source_db <- unique(plants_t3$source_db)
          formated_list_source_db <- paste(shQuote(list_source_db), collapse=",")
          line4 <- "SELECT * FROM version WHERE source_db IN (KUSAKIDB_ID);"
          line4 <- sub("KUSAKIDB_ID", formated_list_source_db, line4)
          tanota4 <- dbGetQuery(con,line4)
          plants_t4 <- merge(plants_t3,tanota4,by="source_db",all.x=T)
          rm(plants_t3,list_source_db,formated_list_source_db)
          # PlantTFDB
          list_plantTFDB_family <- unique(plants_t4$plantTFDB_family)
          list_plantTFDB_family <- list_plantTFDB_family[list_plantTFDB_family != "NA"]
          len_list_plantTFDB_family <- length(list_plantTFDB_family)
          if(len_list_plantTFDB_family > 0){
            formated_list_plantTFDB_family <- paste(shQuote(list_plantTFDB_family), collapse=",")
            line5 <- "SELECT * FROM tf_fam2desc WHERE plantTFDB_family IN (KUSAKIDB_ID);"
            line5 <- sub("KUSAKIDB_ID", formated_list_plantTFDB_family, line5)
            tanota5 <- dbGetQuery(con,line5)
            plants_t5 <- merge(plants_t4,tanota5,by="plantTFDB_family",all.x=T)
            rm(plants_t4,list_plantTFDB_family,formated_list_plantTFDB_family)
          } else {
            plants_t5 <- plants_t4
            plants_t5$plantTFDB_family <- NA
            plants_t5$plantTFDB_description <- NA
            rm(plants_t4)
          }
          # comments
          line6 <- "SELECT * FROM comments WHERE kusakidb_id IN (KUSAKIDB_ID);"
          line6 <- sub("KUSAKIDB_ID", formated_list_kusakidb_id, line6)
          tanota6 <- dbGetQuery(con,line6)
          kusa_entries <- merge(plants_t5,tanota6,by="kusakidb_id",all.x=T)
          rm(plants_t5)
          kid2query <- kusa_entries[,c('kusakidb_id','queryid_pep')]
          # goslim
          line7 <- "SELECT K.kusakidb_id,G.goslim_id,G.goslim_term,G.goslim_domain FROM kusakidb AS K, goslim AS G, kusa2goslim AS A WHERE A.goslim_id=G.goslim_id AND K.kusakidb_id=A.kusakidb_id AND K.kusakidb_id IN (KUSAKIDB_ID);"
          line7 <- sub("KUSAKIDB_ID", formated_list_kusakidb_id, line7)
          tanota7_goslim <- dbGetQuery(con,line7)
          tanota7_goslim <- merge(kid2query,tanota7_goslim,by="kusakidb_id")
          tanota7_goslim <- unique(subset(tanota7_goslim, select=-c(kusakidb_id)))
          # go
          line8 <- "SELECT K.kusakidb_id,G.go_id,G.go_term,G.go_domain FROM kusakidb AS K, go AS G, kusa2go AS A WHERE A.go_id=G.go_id AND K.kusakidb_id=A.kusakidb_id AND K.kusakidb_id IN (KUSAKIDB_ID);"
          line8 <- sub("KUSAKIDB_ID", formated_list_kusakidb_id, line8)
          tanota8_go <- dbGetQuery(con,line8)
          tanota8_go <- merge(kid2query,tanota8_go,by="kusakidb_id")
          tanota8_go <- subset(tanota8_go, select=-c(kusakidb_id))
          # interpro
          line9 <- "SELECT K.kusakidb_id,G.interpro_id,G.interpro_description FROM kusakidb AS K, interpro AS G, kusa2interpro AS A WHERE A.interpro_id=G.interpro_id AND K.kusakidb_id=A.kusakidb_id AND K.kusakidb_id IN (KUSAKIDB_ID);"
          line9 <- sub("KUSAKIDB_ID", formated_list_kusakidb_id, line9)
          tanota9_interpro <- dbGetQuery(con,line9)
          tanota9_interpro <- merge(kid2query,tanota9_interpro,by="kusakidb_id")
          tanota9_interpro <- subset(tanota9_interpro, select=-c(kusakidb_id))
          # pfam
          line10 <- "SELECT K.kusakidb_id,G.pfam_id,G.pfam_description FROM kusakidb AS K, pfam AS G, kusa2pfam AS A WHERE A.pfam_id=G.pfam_id AND K.kusakidb_id=A.kusakidb_id AND K.kusakidb_id IN (KUSAKIDB_ID);"
          line10 <- sub("KUSAKIDB_ID", formated_list_kusakidb_id, line10)
          tanota10_pfam <- dbGetQuery(con,line10)
          tanota10_pfam <- merge(kid2query,tanota10_pfam,by="kusakidb_id")
          tanota10_pfam <- subset(tanota10_pfam, select=-c(kusakidb_id))
          #
          write.table(tanota7_goslim,"zen_v2.0/zen_goslim_v2.0.tsv",sep="\t",row.names=F,col.names=T,quote=F)
          write.table(tanota8_go,"zen_v2.0/zen_go_v2.0.tsv",sep="\t",row.names=F,col.names=T,quote=F)
          write.table(tanota9_interpro,"zen_v2.0/zen_interpro_v2.0.tsv",sep="\t",row.names=F,col.names=T,quote=F)
          write.table(tanota10_pfam,"zen_v2.0/zen_pfam_v2.0.tsv",sep="\t",row.names=F,col.names=T,quote=F)
          dbDisconnect(con)
          #
          nonplants <- outtable[grep("KORTHO",outtable$kusakidb_id),]
          list_nonplants <- as.data.frame(setdiff(nonplants$queryid_pep,kusa_entries$queryid_pep))
          len_nonplants <- dim(list_nonplants)[1]
          if(len_nonplants > 0){
            colnames(list_nonplants) <- "queryid_pep"
            nonplants <- merge(list_nonplants,nonplants, by="queryid_pep")
            nonplants$orthodb_gene_id <- as.data.frame(str_split(nonplants$kusakidb_id, "[|]", simplify=T))[,2]
            nonplants$orthodb_unique_id <- as.data.frame(str_split(nonplants$kusakidb_id, "[|]", simplify=T))[,3]
            nonplants$orthodb_description <- as.data.frame(str_split(nonplants$kusakidb_id, "[|]", simplify=T))[,5]
            nonplants$ncbi_taxid <- as.data.frame(str_split(nonplants$kusakidb_id, "[|]", simplify=T))[,12]
            nonplants <- merge(nonplants,hokanotaxon,by="ncbi_taxid",all.x=T)
            nonplants <- subset(nonplants, select=-c(kusakidb_id))
            nonplants$orthodb_version <- 10
          } else if(len_nonplants == 0){
            nonplants <- data.frame(queryid_pep=character(),stringsAsFactors=FALSE)
          }
          zen_out <- merge(kusa_entries,nonplants,all=T)
          zen_out <- merge(all_queries,zen_out,all=T)
          zen_out <- subset(zen_out, select=-c(flawed_stop_codons))
          if(len_stop > 0){
            colnames(stop) <- c("queryid_pep","flawed_stop_codons")
            stop[stop$flawed_stop_codons == "Yes",'flawed_stop_codons'] <- "The gene has stop codons in the middle of the protein sequence"
            zen_out <- merge(zen_out,stop,by="queryid_pep", all.x=T)
          }else{
            stop <- data.frame(queryid_pep=character(),flawed_stop_codons=character(),stringsAsFactors=FALSE)
            zen_out$flawed_stop_codons <- NA
          }
          colnames(zen_out)[colnames(zen_out)=="species_name"] <- "species_target"
          colnames(zen_out)[colnames(zen_out)=="family_name"] <- "family_target"
          colnames(zen_out)[colnames(zen_out)=="phylum_name"] <- "phylum_target"
          colnames(zen_out)[colnames(zen_out)=="superkingdom_name"] <- "superkingdom_target"
          write.table(zen_out,"zen_v2.0/hayai_annotation_v2.0.tsv",sep="\t",row.names=F,col.names=T,quote=F)
          setwd(dir_output)
        }
      }
    }
  }else{
    cat("Cannot connect to KusakiDB-Docker RMariaDB \n")
  }
}
