README

Functions regarding the App:
AmountOfNewData.R
App.R
DAColNoise.R
DataAugmentationWithColoredNoise.R
DatasetBalancing.R
DatasetInput.R
GenerateNoise.R
GenerateNoiseForBalancing.R
GenerateVariableNoise.R
GenerateVariableNoiseForBalancing.R
NoiseTable.R


Analysis:
Analyse.R -> analyzes the different data augmentation approaches (chapter 4.3.3)
AnalyseRuntime.R -> analyzes the runtime of the algorithms
AnalysisAmount.R -> analyzes the different amounts for DA with colored noise (chapter 4.3.4)
BestNoise.R -> analyzes the different colors of noise (chapter 4.3.2)
BestVariableCombination.R -> here I analyzed different allocations of colors of noise assigned to variables. Not included in the thesis, because it took too long to check for all possible combinations
SamplingAnalysis.R -> analyzes whether sampling or no sampling is superior (chapter 4.3.1)
StatisticalAnalysis.R -> analyzes the statistics of the new datapoints (chapter 4.2)
In the functions for the analyses, the set seeds are the seeds I was using. To perform the analyses, your have to give the correct file path to every of the datasets.

Graphics:
With Generated Graphics.R, figure 10 in the thesis was generated.