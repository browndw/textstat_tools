
library(quanteda)
library(tidyverse)
library(nFactors)

# We'll load in a couple of functions first that we'll use later.
source("functions/mda_functions.R")
source("functions/helper_functions.R")

# Our list of files include MICSUP data that has been tagged using DocuScope.
# DocuScope is an extremely large dictionary of words and phrases that
# have been grouped into rhetorical categores.
#
# At the broadest level in the 9/19/2019 verions of the dictionary,
# it includes 37 categores or "clusters":
#
# AcademicTerms			    Description			    	  	InformationReportVerbs	Responsibility
# AcademicWritingMoves	Facilitate			    	  	InformationStates	    	Reasoning
# Character				      ForceStressed		    	  	InformationTopics	    	Strategic
# Citation				      FirstPerson		    		  	Inquiry				        	SyntacticComplexity
# CitationAuthorized	  Future				      	  	Interactive			      	Uncertainty
# CitationHedged		    InformationExposition	  	Metadiscourse		      	Updates
# ConfidenceHedged	  	InformationChange		    	Narrative			        	Orphaned
# ConfidenceHigh		    InformationChangeNegative	Negative	
# ConfidenceLow		    	InformationChangePositive	Positive	
# Contingent		      	InformationPlace		    	PublicTerms	
#
files_list <- list.files("data/text_data/micusp_ds", full.names = T, pattern = "*.txt")

# We'll read in our files
text_df <- readtext_lite(files_list)

# Create a corpus.
ds_corpus <- corpus(text_df)

# Tokenize it with limited sensitivity.
ds_tokens <- tokens(ds_corpus, remove_punct = T, what = "fasterword")

# The tags in this corpus are concatenated using a double hash: "##".
# So we're going to split our tokens on that delimiter.
ds_tokens <- tokens_split(ds_tokens, separator = "##", valuetype = "fixed",
             remove_separator = TRUE)

# Now we load in a dictionary that contains the DocuScope structure.
# We're going to count the "cluster" categories at the 1st level of the dictionary.
ds_dict <- dictionary(file = "dictionaries/ds_categories.yml", tolower = F)

# Now we'll count those tokens. Note that we're not converting strings to lower case.
ds_tokens <- tokens_lookup(ds_tokens, dictionary = ds_dict, levels = 1, case_insensitive = F)

# Finally we create our dfm.
ds_dfm <- dfm(ds_tokens, tolower = F)

# Clean up our environment.
rm(text_df, ds_tokens)

# Note have the dictionary has enabled radical dimension reduction.
# Rather than thousands of unique tokens, we have only a handful of categories.
topfeatures(ds_dfm)

# From these counts, we'll created a weighted dfm.
# Note that there alternative methods for nomalizing these counts other
# than quanteda's built in function. One could, for example,
# get a more accurate token count for each text from a non-tagged version
# of the files. That said, I tried out some alternatives, and the results
# we only very slighted changed...
ds_norm <- dfm_weight(ds_dfm, scheme = "prop")

# Clean up our environment.
rm(ds_dfm)

# Now we convert this dfm to a data frame.
ds_norm <- convert(ds_norm, to = "data.frame")

# And we'll do a little cleaning up. We'll convert the file names
# into a discipline category and, importantly, we'll drop the 
# "Orphaned" category. These are counts of features not recognized by the
# DocuScope dictionary. They're necessary for normalizing counts,
# but would throw off our factor analysis. So we discard the column here.
ds_norm <- ds_norm %>% mutate(document = str_replace(document, "(\\w{3})\\S+", "\\1")) %>%
  dplyr::select(-Orphaned) %>%
  mutate_if(is.character, as.factor) %>%
  rename(Group = document) %>% 
  mutate_if(is.numeric, ~ . * 100) %>% 
  as.data.frame()

##
# PAUSE HERE!
##

# Now that we have our data prepared, we can begin the multi-staged process
# of multidimensional analysis. We begin by creating a matrix without
# our "Group" variable. We'll call it simply "m".
m <- ds_norm %>% dplyr::select(-Group)

# Now we generate a correlation matrix of all 36 of our variables.
m_cor <- cor(m, method = "pearson")

# Factor analysis is prdicated on locating groups of variables that correlate
# positively or negatively. Using corrplot(), we can get a look at
# how our variables cluster.
corrplot::corrplot(m_cor, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, diag = F, tl.cex = 0.5)

# We're going to use our correlation matrix to filter out variables
# that don't correlate with any others. To do that, we need to
# reset out diagnonal to 0.
diag(m_cor) <- 0

# Now we'll generate a logial vector that test whether variables have
# at least a correlation of 0.2 to one other variable.
threshold <- apply(m_cor, 1, function(x) max(abs(x), na.rm = T) > .2)

# The vector shows only a couple of our variables don't meet this threshold.
print(threshold)

# We'll use the vector to trim our full matrix.
m_trim <- m[, threshold]

# Now that our data is prepared for factor analysis, we now
# need to decide how many factors we want to return.
fa.parallel(m_trim, fa="fa", main = "Scree Plot", show.legend=FALSE)

# The factor analysis is carried out using the factanal() function and a
# "promax" rotation. See Brezina pg. 165-166 for an explanation.
ds_fa <- factanal(m_trim, factors = 3, rotation="promax")

# We can extract our loadings and put them in a data.frame.
f_loadings <- as.data.frame(unclass(ds_fa$loadings))

# Let's see what we have.
View(f_loadings)

# Clean up our environment.
rm(f_loadings, ds_fa, m, m_cor, m_trim)

# File mda_functions.R contains functions that will do most of this process and return
# different data that we need for plotting and further analysis.
#
# We can get the loadings, just as we have done above.
ds_loadings <- get_loadings(ds_norm, 3,  get = "loadings")

# We can get the mean z-scores, which are used for plotting (see Brezina pg. 168).
ds_scores <- get_loadings(ds_norm, 3, get = "scores")

# And we can get the summed z-scores for each file, that we can use to test the
# explanatory value of our factors using analysis of variance.
ds_aov <- get_loadings(ds_norm, 3, get = "aov_scores")

# With the loadings and the scores, we can create stickplots of the kind
# Brezina shows on pg. 169. For this, you need the vegan package installed.
plot_scores(ds_loadings, ds_scores, 1)

# To save the plot, you would need to run three lines of code ALL together,
# and specifying your own path.
#
## png ('/Users/user/Downloads/miscusp_1.png', 800, 1800, res=300)
## plot_scores(ds_loadings, ds_scores, 1)
## dev.off()

# Now we can use analysis of variance and linear regression to assess
# the robustness of our factors.
f_aov <- aov(Factor1 ~ group1, data = ds_aov)
summary(f_aov)

f_lm <- lm(Factor1 ~ group1, data = ds_aov)
summary(f_lm)




