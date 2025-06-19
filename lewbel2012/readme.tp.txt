Todd Prono, "The Role of Conditional Heteroskedasticity in Identifying
and Estimating Linear Triangular Systems, with Applications to Asset
Pricing Models that Include a Mismeasured Factor", Journal of Applied
Econometrics, Vol. 29, No. 5, 2014, pp. 800-824.

The data file contains 2,166 weekly return observations (in percentage
terms) recorded over the period 07/05/1963--12/31/2004. 

The variables in this data file include:

(1) DATE--the end-of-week date for each return.

(2) rm, smb, hml, RF--weekly returns of the Fama/French factors from
    Kenneth French's website:
    http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/index.html.

(2) Rij (i,j = 1,...,5)--weekly returns on 25 Portfolios Formed on
    Size and Book-to-Market calculated from the daily returns on the
    aforementioned portfolios posted on Kenneth French's website.

(3) RIk (k = 1,...,30)--weekly returns on 30 Industry Portfolios
    calculated from the daily returns of the aforementioned portfolios
    posted on Kenneth French's website.

The data are organized by column, with the first column corresponding
to DATE and the last column corresponding to RI30, for a total of 60
columns. The data file is stored as text.

The file name is ff_sze_bm_ind_Prono2013.txt

Also included in this data submission are Stata do files (stored as
text files) that implement the proposed CUE and INT estimators for
both the CAPM and Fama/French 3-factor model. These files work on
excess returns, so rij = Rij - RF and rik = RIk - RF need to be
generated before the programs can be executed. The file names are:

(1) CUE_CAPM_Prono2013.txt
(2) CUE_FF_Prono2013.txt
(3) INT_CAPM_Prono2013.txt
(4) INT_FF_Prono2013.txt

All five files are ASCII files in DOS format. They are zipped in the
file tp-files.zip. Unix/Linux users should use "unzip -a".


