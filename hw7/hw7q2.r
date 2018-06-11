load('cwsas_train_v2.rdata')
print(sample_sent[12])

model1$tseq_count

model1=hmm_train(train_sent$text2, train_sent$bmes_tag)
model1$ct_count

out1=hmm_predict(model1, sample_sent)
out1

out3 = hmm_predict(model1, c('',''))
out3
hmm_predict = function(model, allsent, sepchar=" ", addsmooth=1){
  
  # contruct word freq log matrix
  ct_count_smooth = model$ct_count + addsmooth
  gz = matrix_to_p_log_all(ct_count_smooth)
  
  # Init transistion
  trans = matrix_to_p_log_all(model$tseq_count +addsmooth)
  
  outtag = c()
  outsent = c()
  if(length(allsent)==0){
    return(list(outsent=outsent, outtag=outtag))
  }
  
  for(s in 1:length(allsent)){
    sentence = allsent[s]
    if(sentence=="" || sentence==" "){
      outtag = append(outtag, "")
      outsent = append(outsent, "")
    }
    else{
      indexis = utf8ToInt(sentence)
      
      # Use small matrix instead of requirements in the spec [1]
      gz2 = matrix_to_p_log_all(ct_count_smooth[indexis,])
      
      record = matrix(0, nrow = length(indexis), ncol = 4)
      
      prior = model$tprior_count + addsmooth
      prior = log(prior / sum(prior))
      
      outseq = ""
      if(length(indexis)>1){
        for(i in 1:(length(indexis)-1)){
          idx = indexis[i]
          # Use small matrix instead of requirements in the spec [2]
          #zf = get_zf(idx, gz, prior)  # zi -> fi
          zf = gz2[i,] + prior
          
          
          for(l in 1:4){
            record[i, l] = which.max(trans[,l] + zf)
            prior[l] = max(trans[,l] + zf)
          }  
          #print(prior)
        }
      }
      last_idx = tail(indexis, n=1)
      last_tag_index = which.max(prior + gz[last_idx,])
      
      #print(prior + gz[last_idx,])
      
      #last_tag_index = which.max(prior)
      last_tag = index_to_tag(last_tag_index)
      outseq = last_tag
      
      for(i in length(indexis):1){
        if(i != length(indexis)){
          last_tag_index = record[i, last_tag_index]
          last_tag = index_to_tag(last_tag_index)
          outseq = paste(last_tag, outseq, sep ="")  
        }
      }
      
      out_sentence = tag_seq_to_out(outseq, sentence,sepchar)
      
      outtag = append(outtag, outseq)
      outsent = append(outsent, out_sentence)
    }
  }
  return(list(outsent=outsent,outtag=outtag))
}


tag_seq_to_out = function(tag_seq, sentence, sepchar=" "){
  out = ''
  if(nchar(tag_seq)>0){
    tag_seq = strsplit(tag_seq, "")[[1]]
    sentence = strsplit(sentence, "")[[1]]
    for(i in 1:length(tag_seq)){
      tag = tag_seq[i]
      char = sentence[i]
      out = paste(out, char, sep = '')
      if((tag=='E' || tag=='S') && i != length(tag_seq)){
        out=paste(out,sepchar, sep = '')
      }
    }  
  }
  return(out)
}

get_zf = function(word_index, gz, prior){
  return(gz[word_index,] + prior)
}

matrix_to_p_log_all= function(matrix){
  p = matrix / sum(matrix)
  return(log(p))
}

matrix_to_p_log= function(matrix){
  row_sum = rowSums(matrix)
  p = matrix / row_sum
  return(log(p))
}

index_to_tag = function(id){
  if(id==1){return('S')}
  if(id==2){return('B')}
  if(id==3){return('M')}
  if(id==4){return('E')}
}

get_tag_index = function(tag){
  if(tag=="S"){return(1)}
  else if(tag=='B'){return(2)}
  else if(tag=='M'){return(3)}
  else if(tag=='E'){return(4)}
}


hmm_train = function(sentvec, tagvec){
  ct_count = matrix(0L, nrow = 70000, ncol = 4)
  tseq_count = matrix(0L, nrow = 4, ncol=4)
  tprior_count = matrix(0L, nrow=4, ncol=1)
  
  for(i in 1:length(sentvec)){
    sentence = sentvec[i]
    sentence_int = utf8ToInt(sentence)

    tag_sentence = tagvec[i]
    
    last_tag = ''
    for(x in 1:length(sentence_int)){
      index = sentence_int[x]
      tag = substring(tag_sentence, x, x)
      # Save for probability matrix
      tprior_count[get_tag_index(tag)] = tprior_count[get_tag_index(tag)] + 1
      # Save for ct count matrix
      ct_count[index, get_tag_index(tag)] =  ct_count[index, get_tag_index(tag)]  + 1
      
      if(last_tag!=''){
        tseq_count[get_tag_index(last_tag), get_tag_index(tag)] = tseq_count[get_tag_index(last_tag), get_tag_index(tag)] +1
      }
      last_tag = tag
      
    }
  }
  
  return(list(ct_count=ct_count, tseq_count=tseq_count, tprior_count=tprior_count))
}




