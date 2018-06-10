load('cwsas_train_v2.rdata')
print(sample_sent[12])

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

get_tag_index = function(tag){
  if(tag=="S"){return(1)}
  else if(tag=='B'){return(2)}
  else if(tag=='M'){return(3)}
  else if(tag=='E'){return(4)}
}

model_s1=hmm_train(train_sent$text2[1:100], train_sent$bmes_tag[1:100])

# Test 1
print(model_s1$tprior_count)

# Test 2
print(model_s1$tseq_count)

# Test 3
print(model_s1$ct_count[65290:65300,])
