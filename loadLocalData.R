# load data if needed
if(!exists("edx") | !exists("validation")) {
    print("Loading data sets 'edx' and 'validation'...")
    load(file = "data//dslabs_movielens.Rdata")
    print("Done!")
} else {
    print("Data 'edx' and 'validation' already loaded")
}
