# sbatch src/bc-run-scripts/array_test.sh -o test_array 


library(magrittr)
library(optparse)


option_list = list(

  make_option(c("-j", "--index"), type='character',
              help="index for village quant combo")
)

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

print(opt)






