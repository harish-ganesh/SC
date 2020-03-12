mat = list(c(0.3,0.6,0.1),
           c(0.2,0.4,0.4),
           c(0.1,0.6,0.3),
           c(0.7,0.2,0.1),
           c(0.4,0.6,0))

max_min_relation  <<- function(l1,l2){
  num = 0
  dnm = 0
  for(i in 1:(length(l1))){
    num = num + min(l1[i],l2[i])
    dnm =  dnm + max(l1[i],l2[i])
  }
  return(num/dnm)
}

get_cosine_relation_mat <<- function(mat){
  rel_mat = mat
  for(i in 1:(length(mat)-1)){
    rel_mat[[i]][i] = 1 
    for(j in c((i+1):length(mat))){
      val = max_min_relation(mat[[i]],mat[[j]])
      rel_mat[[i]][j] = val
      rel_mat[[j]][i] = val
    }
  }
  rel_mat[[length(mat)]][length(mat)] = 1
  return(rel_mat)
}

print(get_cosine_relation_mat(mat))
