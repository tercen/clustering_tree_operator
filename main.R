library(tercen)
library(dplyr)
library(dendextend)
library(tidyr)
library(reshape2)

data = (ctx = tercenCtx())  %>% 
  select(.ci, .ri, .y) %>% 
  reshape2::acast(
    .ci ~ .ri,
    value.var = '.y',
    fill = 0,
    fun.aggregate = mean
  )

hc <- hclust(dist(t(data[,])))
dend <- as.dendrogram(hc)
coord <- get_nodes_xy(dend, type = c("rectangle"), center = TRUE, horiz = FALSE)

granularity <- 4 * ncol(data)
ncolo <- 4 * ncol(data)
rng.x <- range(coord[, 1])
x.coord <- cut(
  coord[, 1],
  breaks = seq(rng.x[1], rng.x[2], length.out = ncolo + 1),
  include.lowest = TRUE
)
rng.y <- range(coord[, 2])
y.coord <- cut(
  coord[, 2],
  breaks = seq(rng.y[1], rng.y[2], length.out = granularity),
  include.lowest = F
)
levels(y.coord) <- c("[0]", levels(y.coord))
y.coord[is.na(y.coord)] <- "[0]"

mat <- matrix(NA, nrow = granularity, ncol = ncolo)
rownames(mat) <- levels(y.coord)
colnames(mat) <- levels(x.coord)
for(i in 1:length(x.coord)) { mat[y.coord[i], x.coord[i]] <- 1}
any(rowSums(mat, na.rm = TRUE)[-1] > 1)

alive <- 1
for(i in 2:granularity) {
  if(any(!is.na(mat[i, ]))) {
    mat[i, ] <- mat[i, ] + alive
    cond <- which(is.na(mat[i, ]) & mat[i - 1, ] == alive)
    mat[i, ][cond] <- mat[i - 1, ][cond]
    alive <- alive + 1
    
    # flag two to merge
    a1 <- a2 <- c()
    for(l in which(mat[i, ] == alive)) {
      if(l != ncol(mat) & any(!is.na(mat[i, (l+1):ncol(mat)]))) {
        for(j in l:ncol(mat)) {
          if(j == ncol(mat)) { a1 <- c(a1, j); break }
          else if(is.na(mat[i, j])) { mat[i, j] <- alive - 1 }
          else if(mat[i, j] == (alive - 1)) {
            if(j != (l + 1)) a1 <- c(a1, j)
            else { mat[i, k] = (alive - 1); a1 <- c(a1, j) }
            break
          }
        }
      } else { a1 <- c(a1, l+1) }
      
      if(l != 1 & any(!is.na(mat[i, (l-1):1]))) {
        for(k in l:1) {
          if(k == 1) { a2 <- c(a2, k); break }
          else if(is.na(mat[i, k])) { mat[i, k] <- alive - 1 }
          else if(mat[i, k] == (alive - 1)) {
            if(k != (l - 1)) { a2 <- c(a2, k) }
            else { mat[i, k] = (alive - 1); a2 <- c(a2, k) } 
            break
          }
        }
      } else { a2 <- c(a2, l - 1) }
    }
    
    vc <- c()
    for(m in 1:length(a1)) { vc <- c(vc, (-a1[m]):(-a2[m])) }
    mat[i, vc][!is.na(mat[i, vc])] <- alive
    
    # check for multiple mergers
    na.idx <- which(is.na(mat[i, ]))
    for(ii in 2:length(na.idx)) {
      focal <- (mat[i, ][na.idx[ii-1]:na.idx[ii]])
      if(any(!is.na(focal))) {
        whichmx <- which(focal == max(focal, na.rm = TRUE))
        if(length(whichmx) > 1) {
          mat[i, ][na.idx[ii-1]:na.idx[ii]][whichmx[-1]] <- alive - 1
        }
      }
    }
  } else {
    cond <- which(is.na(mat[i, ]) & mat[i - 1, ] == alive)
    mat[i, ][cond] <- mat[i - 1, ][cond]
  }
}
image(mat<=0, bty="n")

colnames(mat) <- 1:ncol(mat)
rownames(mat) <- 1:nrow(mat)

df_out <- as.data.frame(mat) %>% 
  mutate(tree_dim1 = rownames(mat)) %>%
  gather(tree_dim2, presence, -tree_dim1) 

df_out$tree_dim1 <- formatC(as.numeric(df_out$tree_dim1), width = max(nchar(df_out$tree_dim1)), format = "d", flag = "0")
df_out$tree_dim2 <- formatC(as.numeric(df_out$tree_dim2), width = max(nchar(df_out$tree_dim2)), format = "d", flag = "0")

labs <- ctx$rselect()[[1]]
df_out$tip_labels <- labs[cut(as.numeric(df_out$tree_dim2), length(labs))]

table = tercen::dataframe.as.table(ctx$addNamespace(df_out))
table$properties$name = 'value'
for(i in 1:length(table$columns)) table$columns[[i]]$type = 'double'
table$columns[[1]]$type = 'double'

relation = SimpleRelation$new()
relation$id = table$properties$name

join = JoinOperator$new()
join$rightRelation = relation

result = OperatorResult$new()
result$tables = list(table)
result$joinOperators = list(join)

ctx$save(result) 

