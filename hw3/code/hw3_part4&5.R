#Question 4
non_max_com = V(gcc_undirected2)[which(cluster_fg$membership != which.max(sizes(cluster_fg)))]
sub_com = delete.vertices(gcc_undirected2, non_max_com)
sub_com_fg = fastgreedy.community(sub_com)
sub_color_vec_fg = sub_com_fg$membership+1
# plot(sub_com,vertex.label = NA, vertex.size = 3, vertex.color = sub_color_vec_fg, main = "Sub Community Structure Using Fast Greedy")
sub_com_lp = label.propagation.community(sub_com)
sub_color_vec_lp = sub_com_lp$membership+1
# plot(sub_com,vertex.label = NA, vertex.size = 3, vertex.color = sub_color_vec_lp, main = "Sub Community Structure Using Lable Propagation")
modularity(sub_com_lp)  #0
modularity(sub_com_fg)  #0.3595153

#Question 5
subComIndex = which(sizes(cluster_fg) > 100)
for(i in 1:length(subComIndex))
{
  nonSubNodes = (1 : vcount(gcc_undirected2))[cluster_fg$membership != subComIndex[i]]
  subCom = delete.vertices(gcc_undirected2, nonSubNodes)
  fgSub = fastgreedy.community(subCom)
  lpSub = label.propagation.community(subCom)
  
  fgSubSize = sizes(fgSub)
  print(fgSubSize)
  
  fgSubMod = modularity(fgSub)
  print(fgSubMod)

  lpSubSize = sizes(lpSub)
  print(lpSubSize)

  lpSubMod = modularity(lpSub)
  print(lpSubMod)
}

# Community sizes
#   1   2   3   4   5   6   7 
# 262 454 492 398  88 126  16 
# [1] 0.2230858
# Community sizes
#    1    2    3 
# 1832    3    1 
# [1] 0.0001310914
# Community sizes
#   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15 
# 134  67 262 113  65  59  31  15  13   4   7   4   7   6   4 
# [1] 0.4193492
# Community sizes
#   1   2   3   4   5   6   7   8   9  10  11  12  13 
# 607  15 115   6  11   6   6   4   3   7   2   4   5 
# [1] 0.3052929
# Community sizes
#   1   2   3   4   5   6   7   8   9 
# 502 358 346 142 303  32  10   5   3 
# [1] 0.3716341
# Community sizes
#    1    2 
# 1698    3 
# [1] 0.0002298961
# Community sizes
#   1   2   3   4   5   6   7   8   9 
# 279 182 281  88  53 159  69  98   4 
# [1] 0.3975836
# Community sizes
#    1    2    3    4    5 
# 1196    5    5    3    4 
# [1] 0.003770803
# Community sizes
#   1   2   3   4   5   6   7   8 
#  39 378 417 370  32 301 341 438 
# [1] 0.3626932
# Community sizes
#    1    2 
# 2304   12 
# [1] 0.004410772
# Community sizes
#   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15 
# 170  68  78 156  40  43  33  19   8   3   3   4   3   3   3 
# [1] 0.4785346
# Community sizes
#   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17 
# 393 126  35   4   2   4   5  15  13   3   5   6   3   3   5   3   3 
#  18  19 
#   3   3 
# [1] 0.3699817
# Community sizes
#   1   2   3   4   5   6   7   8   9  10  11  12  13  14 
# 296 198  88 169  77  29  65  10   6   3   3   4   8   7 
# [1] 0.5002231
# Community sizes
#   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17 
# 394 451   7  23  15   3   6   5  16   4   7   6   4   7   4   4   4 
#  18 
#   3 
# [1] 0.3928051
# Community sizes
#   1   2   3   4   5   6   7   8   9  10  11  12  13  14 
# 190  57 248 124  90  72  25 112  83   6   9   6   4   7 
# [1] 0.5053173
# Community sizes
#   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17 
#  40 552   4 184  45  22  40  44   3   6   7  28   9   4   5   4   4 
#  18  19  20  21  22  23  24  25  26 
#   1   4   4   3   3   5   3   6   3 
# [1] 0.4077668