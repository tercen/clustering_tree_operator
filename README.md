# Hierarchical clustering tree operator

##### Description

`clustering_tree` operator returns a hierarchical clustering tree to be projected in Tercen.

##### Usage

Input projection|.
---|---
`row`        | factor, variables to cluster
`col`        | factor, variables to cluster (`dist_to` variable from a `dist` operator) 
`y-axis`        | numeric, pairwise distance (`dist` variable from a `dist` operator)

Output relations|.
---|---
`presence`        | numeric, to be projected on y-axis
`tree_dim1`        | factor, to be projected on rows
`tree_dim2`        | factor, to be projected on columns
`tip_labels`        | factor, leaf labels, to be projected on rows

