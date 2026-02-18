# Cluster Analysis

The Cluster module provides tools for clustering analysis of your data. This module helps you identify natural groupings in your data using various clustering algorithms.

## Configuration

### Column Selection
- **Select columns**: Choose the columns you want to include in the clustering analysis
- Both measurement and descriptive columns are available
- Use multiple columns for better clustering results

### Number of Clusters
- **Number of clusters**: Specify how many clusters you want to create
- Range: 2-10 clusters
- Start with a smaller number and adjust based on your data

## Algorithm Settings

### Clustering Algorithms
- **K-Means**: Fast and efficient for large datasets, assumes spherical clusters
- **Hierarchical**: Creates a tree-like structure, good for hierarchical data
- **DBSCAN**: Density-based, can find arbitrarily shaped clusters

### Data Scaling
- **Scale data**: Standardize data before clustering
- Recommended when variables have different scales
- Helps prevent variables with large ranges from dominating the analysis

## Usage

1. **Load data**: Make sure your data is loaded in the app
2. **Select columns**: Choose relevant columns for clustering
3. **Set parameters**: Configure number of clusters and algorithm
4. **Run analysis**: Click "Run Clustering" to perform the analysis
5. **View results**: Examine cluster assignments and statistics

## Tips

- Start with K-Means algorithm for a quick analysis
- Use data scaling when variables have different units
- Experiment with different numbers of clusters to find the optimal solution
- Consider the nature of your data when choosing an algorithm

## Interpreting Results

The results will show:
- Cluster assignments for each data point
- Algorithm used and parameters
- Summary statistics for each cluster

Use these results to understand the structure of your data and identify meaningful patterns.
