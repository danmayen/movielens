# load sample data if needed
if(!exists("edx_1000") | !exists("validation_1000")) {
    print("Loading data sets 'edx_1000' and 'validation_1000'...")
    load(file = "data//dslabs_movielens_extract.Rdata")
    print("Done!")
} else {
    print("Data 'edx_1000' and 'validation_1000' already loaded")
}
