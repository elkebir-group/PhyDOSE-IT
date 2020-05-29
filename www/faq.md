## Frequently asked questions

1. [PhyDOSE-IT inputs and toggles](#part1)

<a name="part1"></a>
### Part 1: PhyDOSE-IT inputs and toggles

#### 1. What file format is accepted?

PhyDOSE-IT accepts .txt or .tsv file formats. 
To see an example of an accepted input file, download one of the example datasets given.

#### 2. What is the quantile input k*?

PhyDOSE computes the value k of cells to sequence for every tree in the solution space. 
By default, PhyDOSE takes the maximum among the k's obtained from each tree. 
This corresponds to a quantile of 1.
To take the median k, users indicate that with a quantile of 0.5. 
Users are free to choose any quantile of k that they desire.
                     
#### 3. What is the false negative rate?

Current single-cell sequencing technology is error prone and may result in false negatives in the data.
The typical false negative rate of a sequencing technology is between 0.2 and 0.35.
If the user knows a-priori the expected false negative rate, they can indicate so using the slider.
 
#### 4. What is the confidence level slider?
 
The confidence level refers to the probability of successfully identifying the true tree from among the set of candidate trees when.
By lowering your confidence level, the number of cells to sequence will also decrease.
