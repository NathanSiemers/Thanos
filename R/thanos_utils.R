################################################################
## Small shared utilities, defined INSIDE the Thanos namespace so a
## host app can neither break them nor be affected by them.
################################################################

## Null-default.  Base R has %||% since 4.4, but in source mode the
## Thanos namespace's parent is globalenv(): a host app that defines its
## OWN %||% there (with different semantics, e.g. treating character(0)
## or NA as NULL) would silently replace base's inside the module -- a
## remembered empty checkbox selection would turn back into "all
## levels".  Owning the definition makes the module immune.
`%||%` <- function(x, y) if (is.null(x)) y else x

## Column name -> id-safe fragment for input/output ids and selectors.
## INJECTIVE: names made only of [A-Za-z0-9_] map to themselves; every
## other byte becomes "-HH" (its hex code).  A clean name never contains
## "-", so two different column names can never share an id -- unlike a
## lossy "replace with _" scheme, where "TP53.mut" and "TP53_mut" (or
## "HLA-A" and "HLA.A") would collide on one DOM/input id.
.thanos_id_bytes <- charToRaw(
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_")
thanos_vid <- function(v) {
    b <- charToRaw(enc2utf8(v))
    safe <- b %in% .thanos_id_bytes
    if (all(safe)) return(v)
    out <- sprintf("-%02X", as.integer(b))
    out[safe] <- rawToChar(b[safe], multiple = TRUE)
    paste(out, collapse = "")
}
