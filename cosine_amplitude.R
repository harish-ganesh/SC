mat = list(c(0.3,0.6,0.1),
           c(0.2,0.4,0.4),
           c(0.1,0.6,0.3),
           c(0.7,0.2,0.1),
           c(0.4,0.6,0))

get_cosine_relation  <<- function(l1,l2){
  num = 0
  dnm1 = 0
  dnm2 = 0
  for(i in 1:(length(l1))){
    print(l1[i])
    num = num + l1[i] * l2[i]
    dnm1 =  dnm1 + l1[i] * l1[i]
    dnm2 =  dnm2 + l2[i] * l2[i]
  }
  return(num/(sqrt(dnm1*dnm2)))
}

get_cosine_relation_mat <<- function(mat){
  rel_mat = mat
  for(i in 1:(length(mat)-1)){
    rel_mat[[i]][i] = 1
    for(j in c((i+1):length(mat))){
      val = get_cosine_relation(mat[[i]],mat[[j]])
      rel_mat[[i]][j] = val
      rel_mat[[j]][i] = val
    }
  }
  rel_mat[[length(mat)]][length(mat)] = 1
  return(rel_mat)
}

print(get_cosine_relation_mat(mat))
