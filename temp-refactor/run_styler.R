# Apply styler with default tidyverse style
library(styler)

message("Applying styler formatting...")

# Note: Since our .lintr is set to 120 chars and we've disabled indentation linter,
# styler should not cause conflicts even with its default 80-char preference
styler::style_pkg()

message("Styler formatting complete")
