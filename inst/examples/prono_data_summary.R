# Summary of Prono's Actual Data vs What He Reports

cat("=== CRITICAL FINDING: PRONO'S DATA VS REPORTED STATISTICS ===\n\n")

cat("WHAT PRONO REPORTS IN THE PAPER:\n")
cat("- Average weekly excess market return: 0.097%\n\n")

cat("WHAT THE ACTUAL DATA SHOWS:\n")
cat("- Market return (rm): mean = 0.097%\n")
cat("- Risk-free rate (RF): mean = 0.119%\n") 
cat("- Market EXCESS return (rm - RF): mean = -0.022%\n\n")

cat("THE DISCREPANCY:\n")
cat("Prono appears to report the mean of 'rm' (0.097%) but calls it\n")
cat("the 'excess market return'. However, the actual excess return\n")
cat("(rm - RF) has a mean of -0.022%.\n\n")

cat("This explains why our implementation targets 0.097% - we were\n")
cat("matching what Prono reported, not what his data actually shows!\n\n")

cat("=== COMPLETE SUMMARY STATISTICS FROM PRONO'S DATA ===\n\n")

cat("1. MARKET RETURNS (Weekly, in %):\n")
cat("   rm (market return):        Mean = 0.097%, SD = 2.079%\n")
cat("   RF (risk-free rate):       Mean = 0.119%, SD = 0.056%\n")
cat("   rm-RF (market excess):     Mean = -0.022%, SD = 2.074%\n\n")

cat("2. FAMA-FRENCH FACTORS:\n")
cat("   SMB: Mean = 0.041%, SD = 1.181%\n")
cat("   HML: Mean = 0.097%, SD = 1.168%\n\n")

cat("3. 25 SIZE-B/M PORTFOLIOS (Raw Returns):\n")
cat("   Mean range: [0.161%, 0.351%]\n")
cat("   SD range: [1.947%, 2.962%]\n")
cat("   Corner portfolios:\n")
cat("     Small-Low:  Mean = 0.161%, SD = 2.923%\n")
cat("     Small-High: Mean = 0.351%, SD = 2.031%\n")
cat("     Big-Low:    Mean = 0.208%, SD = 2.382%\n")
cat("     Big-High:   Mean = 0.242%, SD = 2.210%\n\n")

cat("4. 25 SIZE-B/M PORTFOLIOS (Excess Returns):\n")
cat("   Small-Low:  Mean = 0.043%, SD = 2.927%\n")
cat("   Small-High: Mean = 0.232%, SD = 2.035%\n")
cat("   Big-Low:    Mean = 0.089%, SD = 2.384%\n")
cat("   Big-High:   Mean = 0.123%, SD = 2.210%\n\n")

cat("5. 30 INDUSTRY PORTFOLIOS:\n")
cat("   Mean range: [0.176%, 0.323%]\n")
cat("   SD range: [1.779%, 4.106%]\n\n")

cat("6. CORRELATIONS:\n")
cat("   Cor(rm, smb) = 0.005\n")
cat("   Cor(rm, hml) = -0.536\n")
cat("   Cor(smb, hml) = -0.158\n\n")

cat("7. EVIDENCE OF GARCH EFFECTS:\n")
cat("   Ljung-Box test on squared market residuals: p < 0.0001\n")
cat("   ACF of squared residuals at lag 1: 0.212\n")
cat("   Strong evidence of volatility clustering\n\n")

cat("=== IMPLICATIONS FOR OUR IMPLEMENTATION ===\n")
cat("1. Our target mean of 0.097% matches rm, not rm-RF\n")
cat("2. Our SD of ~2% correctly matches the market volatility\n")
cat("3. The true excess return mean is actually negative!\n")
cat("4. This doesn't affect the methodology, just the interpretation\n")