# rules-of-contagion
This repository contains code and data to accompany [The Rules of Contagion: Why Things Spread - and Why They Stop](https://amzn.to/2JJbwxo) by Adam Kucharski.

If you use any of the code/analysis under the GNU license, please cite 'Kucharski AJ, The Rules of Contagion: Why Things Spread - and Why They Stop, Wellcome Collection/Profile Books, 2020' as well as any relevant data sources (listed below).

### Guide to files

The main script to reproduce the analysis is in `scripts/main_plot_figures.R`. This script calls the following files:

> `R/plotting_functions.R` - Loads and plots data

> `R/functions_basic_sir.R` - Functions for SIR model

> `R/set_plot.R` - Customised plot settings

As you'd expect, data is the the `data` folder and plots are output in the `plots` folder.

### References for datasets

#### Chapter 1

* 1906 Bombay plague data: [Epidemiological observations in Bombay City, Journal of Hygiene, 1907](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2236259/)
* French Polynesia Zika GBS data: [Cao-Lormeau et al, Lancet, 2016](https://www.ncbi.nlm.nih.gov/pubmed/26948433)
* French Polynesia Zika case data: [Kucharski et al, PLOS NTD, 2016](https://journals.plos.org/plosntds/article?id=10.1371/journal.pntd.0004726)
* House prices: [Council of Mortgage Lenders](https://www.cml.org.uk/news/news-and-views/723/)
* VCR adoption: [Consumer Electronics Association](http://blog.pff.org/archives/2008/03/print/005037.html)

#### Chapter 2

* South Sea Bubble data: [Frehen et al, Journal of Financial Economics, 2013](http://som.yale.edu/faculty-research/our-centers-initiatives/international-center-finance/data/historical-southseasbubble)

* 1917 Typhoid data: [Cumming, JAMA, 1917](https://jamanetwork.com/journals/jama/article-abstract/442535)

#### Chapter 3

* UK Pandemic data: [Epidemiological report of pandemic (H1N1) 2009 in the UK](http://webarchive.nationalarchives.gov.uk/20140714084352/http://www.hpa.org.uk/webc/HPAwebFile/HPAweb_C/1284475321350)

#### Chapter 4

* Chicago gun violence data: [Green et al, JAMA, 2017](https://jamanetwork.com/journals/jamainternalmedicine/fullarticle/2594804)
* 1854 Cholera data: [cholera R package](https://cran.r-project.org/web/packages/cholera/index.html)
* Bangladesh diphtheria outbreak data: [Finger et al, BMC Medicine, 2019](https://bmcmedicine.biomedcentral.com/articles/10.1186/s12916-019-1288-7)
* 1976 Ebola data: [Camacho et al, Epidemics, 2014](https://www.sciencedirect.com/science/article/pii/S1755436514000528)

#### Chapter 5

* Higgs retweet data: [Stanford Network Analysis Project](https://snap.stanford.edu/data/higgs-twitter.html)
* 'The Perfect Bet' YouTube data: Royal Institution (personal communication)


