pat <- "_[^_]*"

parse_src_filename <- function(symlink_filename) {
    arg1_ind <- regexpr(pat, symlink_filename)[[1]]
    if (arg1_ind == -1L)
        return(symlink_filename)
    return(paste(sep="", substr(symlink_filename, 1L, arg1_ind - 1L), ".R"))
}

parse_program_args <- function(symlink_filename) {
    symlink_basename <- substr(symlink_filename, 1L, 
                           regexpr("\\.[^.]*$", symlink_filename)[[1]] - 1L)
    arg1_ind <- regexpr(pat, symlink_basename)[[1]]
    if (arg1_ind == -1L)
        return(character())
    
    inds <- gregexpr(pat, symlink_basename, perl=TRUE)[[1]] + 1L
    lens <- attr(inds, "match.length") - 2L
    return(substring(symlink_filename, inds, inds + lens))
}

get_symlink_filepath <- function() {
    args <- commandArgs()
    for (i in 1:(length(args) - 1))
        if (args[[i]] == "-f")
            return(args[[i + 1]])
    return(NA)
}

get_src_filepath <- function(symlink_filepath, src_filename) {
    symlink_dir <- system(paste("dirname `readlink -f",symlink_filepath,"`"),
                          intern=TRUE)
    symlink_filename <- basename(symlink_filepath)
    return(paste(sep="", symlink_dir, "/", src_filename))
}

get_main_function_name <- function(src_filename) {
    for (i in nchar(src_filename):1)
        if (sub(src_filename, i, i) != ".")
            return(sub("-", "_", substr(src_filename, 1L, i - 2L)))
    return(NA)
}

test <- function(symlink_filename) {
    src_filename <- parse_src_filename(symlink_filename)
    print("program name:"); print(src_filename)
    program_args <- parse_program_args(symlink_filename)
    print("program args:"); print(program_args)
}

main <- function(args) {
    symlink_filepath <- get_symlink_filepath()
    symlink_filename <- basename(symlink_filepath)
    src_filename <- parse_src_filename(symlink_filename)
    program_args <- parse_program_args(symlink_filename)
    
    src_filepath <- get_src_filepath(symlink_filepath, src_filename)
    main_function_name <- get_main_function_name(src_filename)

    # override parsed program_args if args are present
    if (length(args) != 0L)
        program_args <- args

    # cat("run filepath: "); print(src_filepath)
    # cat("main function: "); print(main_function_name)
    # cat("program args:"); print(program_args)

    # turn on tracing if requested
    if (!!nchar(Sys.getenv('RTRACE')))
        traceOn()

    # call main function with appropriate parameters
    assign("i_am_wrapper", TRUE, envir=topenv())
    cmp_filepath = paste(src_filepath, "c", collapse="", sep="")
    if (substring(symlink_filename, nchar(symlink_filename)) == 'c')
        compiler::loadcmp(cmp_filepath)
    else
        source(src_filepath)
    do.call(main_function_name, as.list(program_args))

    # turn off tracing
    if (!!nchar(Sys.getenv('RTRACE')))
        traceOff()
}

main(commandArgs(trailingOnly=TRUE))
