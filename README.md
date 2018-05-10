# THOR
Cupoyright 2012 Scott Walmsley
Testing for Heteroskedasticity of Sample Replicates : mass spectrometry count data cutoff MS data tool developed in R.

# Requirements:
1. R
2. PeptideProphet MSMS search engine results.
3. ProteinProphet MSMS search engine results.
4. ABACUS results (see Damian Fermin's software at:  http://abacustpp.sourceforge.net/).

# Usage:  Run from R command prompt:
```{r}
#Source all your files.

# Read in an abacus file
file = "ABACUS_Results.tsv"
dset = read.THOR(file)


# Run the Goldfeld-QUandt test for heteroskedastistity
r = gq.THOR(dset,STEP=2,PLOT=T)

# Estimate the GQ mediated cutoff.
est.THOR(r)
```