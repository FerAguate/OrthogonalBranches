## Grow a tree of principal components with orthogonal branches

### Description

returns the first principal component (PC) for each group at the next level of a dendogram. That PC is orthogonal to the information at the current level. Also plots the dendogram with the groups colored.

### Usage

grow_tree(data_df, ...)

### Arguments
<pre><nowrap>
  data_sf            original dataframe. All columns must be numeric.
  
  tree_grw           an object of class 'Orthogonal tree' (the result of the function). By default is equal to NULL.
  
</nowrap></pre>

### Value

<pre><nowrap>
  PC_lst            list of length equal to the number of groups. Contains the first PC for each group.
  
  X_lst             Residual data by group.
  
  level             branch level.
  
  PCvar             percentage of variance explained by the first PC.
  
</nowrap></pre>

### Examples
```R
mydata <- iris[,-5]

lvl1 <- grow_tree(data_df = mydata)
lvl2 <- grow_tree(data_df = mydata, tree_grw = lvl1)
lvl3 <- grow_tree(data_df = mydata, tree_grw = lvl2)

cor(lvl1$PC_lst[[1]], lvl2$PC_lst[[1]]) # The correlation must be close to zero.
cor(lvl1$PC_lst[[1]], lvl2$PC_lst[[2]]) # The correlation must be close to zero.
```
