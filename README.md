# clustering-physiology
Attempts to find a model that most accurately classifies athletes based on ground reaction force data

### Force Plate Breakdown
This file breaks down the physics behind ordered breakdown of each metric. It also skims over the methodology behind why certain metrics were excluded from the model to reduce dimensionality from the beginning.

### Data Exploration
This is where you can view the data as a whole.  It was also used to help develop the initial logic for clustering on the Force Plate Breakdown File

### ML Files
1. Kmeans - This was used first to see if unsupervised learning identified similar clusters to what was mentioned in the Force Plate Breakdown file

2. Bagged CART - The first supervised learning technique used due to no tuning parameters, so most simple

3. CART - The second model chosen due to the ability to view the hierarchical decision making process and fairly limited tuning parameters.  Multipe CART models were run.
