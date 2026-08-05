################################################################
## Helpers shared by the bench/ scripts (each script stays runnable on
## its own; they source this file via their repo-root locator).
################################################################

## mean wall time over `reps` runs after one warm-up evaluation
timeit <- function(label, expr, reps = 5, warmup = TRUE) {
    expr <- substitute(expr)
    if (warmup) eval(expr, parent.frame())
    t <- system.time(for (i in seq_len(reps)) eval(expr, parent.frame()))
    cat(sprintf("%-58s %9.2f ms\n", label, 1000 * t[["elapsed"]] / reps))
}

## the module's leave-one-out mask algebra (prefix/suffix cumulative
## ANDs), for benches that simulate an interaction outside Shiny
loo_combine <- function(ms) {
    k <- length(ms)
    prefix <- vector("list", k); suffix <- vector("list", k)
    acc <- ms[[1]]; prefix[[1]] <- acc
    for (i in seq_len(k)[-1]) { acc <- acc & ms[[i]]; prefix[[i]] <- acc }
    acc <- ms[[k]]; suffix[[k]] <- acc
    for (i in rev(seq_len(k)[-k])) { acc <- acc & ms[[i]]; suffix[[i]] <- acc }
    out <- lapply(seq_len(k), function(i) {
        left  <- if (i > 1) prefix[[i - 1]] else NULL
        right <- if (i < k) suffix[[i + 1]] else NULL
        if (is.null(left)) right else if (is.null(right)) left
        else left & right
    })
    names(out) <- names(ms)
    out
}
