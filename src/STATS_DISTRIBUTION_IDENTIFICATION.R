#/***********************************************************************
# * Licensed Materials - Property of IBM
# * IBM SPSS Products: Statistics Common
# * (C) Copyright IBM Corp. 2026
# ************************************************************************/
# STATS DISTRIBUTION IDENTIFICATION — R extension for IBM SPSS Statistics
# Author : Aruna Saraswathy
# 1.1.0  19-oct-2026  quality-control tools: physical bounds (LOWERBOUND/UPPERBOUND), tolerance
#                     intervals, recommendation stability (bootstrap), k-sample AD pooling test,
#                     distribution-based control limits, outlier sensitivity, /SAVE PROB NSCORE,
#                     Split File and Weight honoured, small-sample caution
# 1.0.1  19-oct-2026  package structure aligned with STATS PROCESS CAPABILITY: legacy .spd,
#                     full CDB syntax generation, PCA package-install guard, Windows temp
#                     dir fix, LC_NUMERIC fix, uncompressed .spe with MANIFEST first;
#                     dialog: Fit Options adds the distribution list; the HTML report
#                     opens automatically (OPENREPORT=NO in syntax turns it off)
# 1.0.0  19-oct-2026  first release (split out of STATS PROCESS CAPABILITY's
#                     Distribution Advisor, with new outputs)
# ── Preamble copied verbatim from STATS_PROCESS_CAPABILITY.R (temp dir, package guard) ──
# ── Windows temp-dir fix: ensures R charts are readable by SPSS on all accounts
# Tries C:/Temp first; falls back to C:/Users/Public/Temp; then user profile
if (.Platform$OS.type == "windows") {
    .td_candidates <- c(
        "C:/Temp",
        file.path(Sys.getenv("PUBLIC"),      "Temp"),
        file.path(Sys.getenv("USERPROFILE"), "RTemp")
    )
    for (.td in .td_candidates) {
        if (!dir.exists(.td)) dir.create(.td, recursive=TRUE, showWarnings=FALSE)
        if (dir.exists(.td)) { options(tmpdir = .td); break }
    }
    rm(.td_candidates, .td)
}

# ══════════════════════════════════════════════════════════════════════════════
# PACKAGE INSTALLATION — OneDrive / SharePoint backed local mirror
# Flow:
#   1. All packages already installed? → silent skip
#   2. Local extracted folder exists?  → install from it
#   3. Try download from SharePoint     → extract → install
#   4. Download fails (not signed in)?  → stop with clear user message
# ══════════════════════════════════════════════════════════════════════════════

.SPSS_FALLBACK_ZIP  <- paste0(
    "https://nestle.sharepoint.com/:u:/r/sites/SPSS_Statistics/",
    "Shared%20Documents/Packages/spss_cran_mirror.zip",
    "?csf=1&web=1&e=RHeShh"
)
.SPSS_LOCAL_REPO    <- "C:/spss_cran_mirror"
.SPSS_CRAN_MIRROR   <- "https://cloud.r-project.org"
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

.spss_ensure_packages <- function(pkgs) {
    missing_pkgs <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
    if (length(missing_pkgs) == 0) return(invisible(TRUE))   # all installed

    r_ver    <- paste0(R.version$major, ".",
                       strsplit(R.version$minor, "\\.")[[1]][1])
    repo_dir <- file.path(.SPSS_LOCAL_REPO, "bin", "windows", "contrib", r_ver)
    repo_url <- paste0("file:///", .SPSS_LOCAL_REPO)

    # Helper: check if a package .zip exists in the local repo folder
    pkg_in_repo <- function(pkg) {
        dir.exists(repo_dir) &&
        length(list.files(repo_dir, pattern = paste0("^", pkg, "_"),
                          ignore.case = TRUE)) > 0
    }

    # -- Step 1: try public CRAN (works for any user with internet access) -----
    cran_ok <- tryCatch({
        tmp_chk <- tempfile()
        old_to  <- getOption("timeout"); options(timeout = 15)
        on.exit({ unlink(tmp_chk); options(timeout = old_to) }, add = TRUE)
        res <- suppressWarnings(download.file(
            paste0(.SPSS_CRAN_MIRROR, "/src/contrib/PACKAGES.gz"),
            tmp_chk, quiet = TRUE, mode = "wb"))
        res == 0
    }, error = function(e) FALSE, warning = function(w) FALSE)

    if (cran_ok) {
        # Find a writable R library path — on Mac/Linux the default may be system-owned
        # (read-only), causing silent install failures even when CRAN is reachable.
        writable_lib <- tryCatch({
            candidate <- NULL
            for (p in .libPaths()) {
                if (dir.exists(p) && file.access(p, 2L) == 0L) { candidate <- p; break }
            }
            if (is.null(candidate)) {
                # No writable path — create a personal library
                ulib <- file.path(path.expand("~"), ".R", "library")
                dir.create(ulib, recursive = TRUE, showWarnings = FALSE)
                if (dir.exists(ulib)) {
                    .libPaths(c(ulib, .libPaths()))
                    candidate <- ulib
                }
            }
            candidate
        }, error = function(e) NULL)

        # If SPSS is using renv (SPSS 32+), use renv::install() which is the
        # correct API for renv-managed environments. Fall back to install.packages.
        use_renv <- isNamespaceLoaded("renv") &&
                    tryCatch(is.function(renv::install), error=function(e) FALSE)

        for (pkg in missing_pkgs) {
            if (use_renv) {
                tryCatch(
                    renv::install(pkg, repos = .SPSS_CRAN_MIRROR, prompt = FALSE),
                    error = function(e) NULL
                )
            }
            # Always also try standard install.packages (covers non-renv and as fallback)
            if (!requireNamespace(pkg, quietly = TRUE)) {
                tryCatch(
                    suppressWarnings(
                    install.packages(pkg, repos = .SPSS_CRAN_MIRROR,
                                     lib   = writable_lib,
                                     dependencies = c("Depends","Imports","LinkingTo"),
                                     quiet = TRUE,
                                     type = if (.Platform$OS.type == "windows") "binary"
                                          else if (grepl("x86_64", R.version$arch %||% ""))
                                              "mac.binary.big-sur-x86_64"
                                          else getOption("pkgType"))),
                    error = function(e) NULL
                )
            }
        }

        # Intel Mac extra fallback: still-missing packages installed directly
        # to SPSS rpackage dir (bypasses renv) with explicit binary type.
        if (.Platform$OS.type != "windows" && grepl("x86_64", R.version$arch %||% "")) {
            still_missing2 <- missing_pkgs[!sapply(missing_pkgs, requireNamespace, quietly=TRUE)]
            if (length(still_missing2) > 0) {
                spss_rpkg <- Filter(function(p) grepl("rpackage", p, fixed=TRUE) &&
                                                dir.exists(p) && file.access(p, 2L) == 0L,
                                    .libPaths())
                if (length(spss_rpkg) > 0) {
                    for (pkg in still_missing2) {
                        tryCatch(
                            suppressWarnings(
                            install.packages(pkg, repos = .SPSS_CRAN_MIRROR,
                                             lib  = spss_rpkg[1],
                                             dependencies = c("Depends","Imports","LinkingTo"),
                                             quiet = TRUE,
                                             type  = "mac.binary.big-sur-x86_64")),
                            error = function(e) NULL
                        )
                    }
                }
            }
        }

        # Ensure the writable lib is on the search path before checking
        if (!is.null(writable_lib) && !(writable_lib %in% .libPaths()))
            .libPaths(c(writable_lib, .libPaths()))
        missing_pkgs <- missing_pkgs[!sapply(missing_pkgs, requireNamespace, quietly = TRUE)]
        if (length(missing_pkgs) == 0) return(invisible(TRUE))
    }

    # -- Step 2: CRAN blocked -> try organisation fallback mirror (Windows) ----
    # Download only if any missing package is absent from the local repo.
    # This means a second extension can reuse the existing folder without
    # re-downloading, but will trigger a fresh download if its packages are new.
    if (.Platform$OS.type == "windows") {
        need_download <- !all(sapply(missing_pkgs, pkg_in_repo))

        if (need_download) {
            zip_dest <- file.path(tempdir(), "spss_cran_mirror.zip")
            if (!dir.exists(.SPSS_LOCAL_REPO))
                dir.create(.SPSS_LOCAL_REPO, recursive = TRUE, showWarnings = FALSE)

            dl_ok <- tryCatch({
                suppressWarnings(
                    download.file(.SPSS_FALLBACK_ZIP, zip_dest,
                                  mode = "wb", quiet = TRUE))
                file.size(zip_dest) > 100000 &&
                    !inherits(tryCatch(unzip(zip_dest, list = TRUE),
                                       error = function(e) e), "error")
            }, error = function(e) FALSE)

            if (dl_ok) {
                message("[SPSS] Installing R packages from local mirror...")
                unzip(zip_dest, exdir = .SPSS_LOCAL_REPO, overwrite = TRUE)
                unlink(zip_dest)
            }
        }
    }

    if (dir.exists(repo_dir)) {
        for (pkg in missing_pkgs) {
            tryCatch(
                install.packages(pkg, repos = repo_url, type = "win.binary",
                                 dependencies = TRUE, quiet = TRUE),
                error = function(e) NULL
            )
        }
        missing_pkgs <- missing_pkgs[!sapply(missing_pkgs, requireNamespace, quietly = TRUE)]
        if (length(missing_pkgs) == 0) return(invisible(TRUE))
    }

    # -- Step 3: both failed -> generic message --------------------------------
    stop(paste0(
        "\n\n",
        "================================================================\n",
        "  STATS_PROCESS_CAPABILITY: Required R Packages Missing\n",
        "================================================================\n",
        "The following R packages could not be installed automatically:\n",
        "  ", paste(missing_pkgs, collapse = ", "), "\n\n",
        "Possible causes:\n",
        "  - No internet connection\n",
        "  - Firewall blocking access to cloud.r-project.org\n",
        "  - R package repository unavailable\n\n",
        "To resolve:\n",
        "  1. Check your internet connection and re-run the extension\n",
        "  2. Ask your IT administrator to whitelist cloud.r-project.org\n",
        "  3. Or ask your administrator to pre-install these R packages:\n",
        "     ", paste(missing_pkgs, collapse = ", "), "\n",
        if (.Platform$OS.type != "windows") paste0(
        "\nMac users: if installation keeps failing, open Terminal and run:\n",
        "  xcode-select --install\n",
        "Then re-run the extension. This installs the compiler needed\n",
        "to build R packages from source.\n") else "",
        "================================================================"
    ))
}

# ── Shared distribution library: copied verbatim from STATS_PROCESS_CAPABILITY.R at build time ──
# Do not edit here; edit the Process Capability source and rebuild both extensions.

strip_quotes <- function(s) {
    # Strip surrounding double or single quotes from literal-ktype strings
    if (is.null(s) || length(s) == 0 || !nzchar(as.character(s))) return(s)
    gsub('^["\']|["\']$', '', as.character(s))
}

to_bool <- function(x) {
    if (is.null(x) || length(x) == 0) return(FALSE)
    if (is.logical(x)) return(isTRUE(x))
    isTRUE(tolower(as.character(x)) %in% c("yes", "true", "1"))
}

setuplocalization <- function(domain) invisible(NULL)

gtxt  <- function(msg, ...) if (...length() == 0) msg else sprintf(msg, ...)

gtxtf <- function(msg, ...) sprintf(msg, ...)

StartProcedure <- function(procname, omsid) {
    ver <- tryCatch(as.numeric(substr(spsspkg.GetSPSSVersion(), 1, 2)),
                    error = function(e) 19)
    if (ver >= 19) spsspkg.StartProcedure(procname, omsid)
    else           spsspkg.StartProcedure(omsid)
}

Warn <- function(procname, omsid) {
    lcl <- list2env(list(procname = procname, omsid = omsid,
                         msglist = list(), sevlist = character(0), msgnum = 0L,
                         stopped = FALSE))

    lcl$warn <- function(msg = NULL, dostop = FALSE, inproc = FALSE) {
        if (!is.null(msg)) {
            assign("msgnum", lcl$msgnum + 1L, envir = lcl)
            m <- lcl$msglist; m[[lcl$msgnum]] <- msg
            assign("msglist", m, envir = lcl)
            assign("sevlist", c(lcl$sevlist, if (dostop) "FATAL" else "WARNING"), envir = lcl)
        }
        if (is.null(msg) || dostop) {
            lcl$display(inproc)
            if (dostop) {
                assign("stopped", TRUE, envir = lcl)
                # the real message (not a generic text) so the Viewer shows it
                stop(if (!is.null(msg)) msg else gtxt("Procedure stopped due to error."), call. = FALSE)
            }
        }
    }

    lcl$display <- function(inproc = FALSE) {
        if (lcl$msgnum == 0L) {
            if (inproc) spsspkg.EndProcedure()
            return()
        }
        procok <- if (!inproc) {
            tryCatch({ StartProcedure(lcl$procname, lcl$omsid); TRUE },
                     error = function(e) FALSE)
        } else TRUE

        if (procok) {
            # spsspivottable.Display instead of the raw BasePivotTable API: a table
            # built with only a row dimension can render with a title but no text.
            msgs <- vapply(lcl$msglist, as.character, character(1))
            tbl  <- data.frame(Severity = lcl$sevlist, Message = msgs,
                               stringsAsFactors = FALSE, check.names = FALSE)
            ok <- tryCatch({
                spsspivottable.Display(tbl, title = gtxt("Warnings"),
                    templateName = "STATSPROCAPWARN", outline = gtxt("Warnings"),
                    rowlabels = as.character(seq_len(lcl$msgnum)))
                TRUE }, error = function(e) FALSE)
            if (!ok) for (i in seq_len(lcl$msgnum)) message(msgs[i])
            spsspkg.EndProcedure()
        } else {
            for (i in seq_len(lcl$msgnum)) message(lcl$msglist[[i]])
        }
        # clear what was shown so a later display() does not repeat it
        assign("msglist", list(), envir = lcl); assign("sevlist", character(0), envir = lcl)
        assign("msgnum", 0L, envir = lcl)
    }
    return(lcl)
}

.pc_js_ascii <- function(js) {
    cp <- utf8ToInt(js)
    if (anyNA(cp)) return(NULL)
    bad <- cp > 126L | (cp < 32L & !(cp %in% c(9L, 10L, 13L)))
    if (!any(bad)) return(js)
    ch <- intToUtf8(cp, multiple=TRUE)
    b <- cp[bad]; hi <- b > 65535L
    esc <- sprintf("\\u%04x", b)
    if (any(hi)) { v <- b[hi] - 65536L
        esc[hi] <- sprintf("\\u%04x\\u%04x", 55296L + v %/% 1024L, 56320L + v %% 1024L) }
    ch[bad] <- esc
    paste(ch, collapse="")
}

.pc_plotly_js <- function() {
    cdn <- '<script src="https://cdn.plot.ly/plotly-2.35.0.min.js" charset="utf-8"></script>'
    d <- tryCatch(system.file("htmlwidgets", "lib", "plotlyjs", package="plotly"), error=function(e) "")
    f <- if (nzchar(d)) list.files(d, pattern="^plotly.*min\\.js$", full.names=TRUE) else character(0)
    if (!length(f)) return(cdn)
    # binary read: text-mode reading on Windows stops at the Ctrl-Z (0x1A) byte
    # that plotly.min.js contains, truncating the library
    js <- tryCatch({ r <- readBin(f[1], "raw", file.info(f[1])$size); r <- r[r != as.raw(0)]
                     x <- rawToChar(r); Encoding(x) <- "UTF-8"; .pc_js_ascii(x) }, error=function(e) NULL)
    if (is.null(js)) return(cdn)
    paste0("<script>", gsub("</script", "<\\/script", js, fixed=TRUE), "</script>")
}

D2_TABLE <- c(
    1.128, 1.693, 2.059, 2.326, 2.534, 2.704, 2.847, 2.970, 3.078, 3.173,
    3.258, 3.336, 3.407, 3.472, 3.532, 3.588, 3.640, 3.689, 3.735, 3.778,
    3.819, 3.858, 3.895, 3.931
)

d2_const <- function(n) {
    n <- round(n)
    if (n < 2) return(NA_real_)
    if (n <= 25) return(unname(D2_TABLE[as.character(n)]))
    # n > 25: exact d2(n) = E[range of n std normals] (previously d2(25) was
    # silently used, overstating Rbar/d2 sigma, e.g. by ~4% for n = 30)
    tryCatch(integrate(function(w) 1 - (1 - pnorm(w))^n - pnorm(w)^n,
                       -Inf, Inf, rel.tol = 1e-10)$value,
             error = function(e) NA_real_)
}

c4_const <- function(n) {
    n <- round(n)
    if (n < 2) return(NA_real_)
    # lgamma: gamma(n/2) overflows to Inf for n > 343, which made Sbar/c4 NaN
    sqrt(2 / (n - 1)) * exp(lgamma(n / 2) - lgamma((n - 1) / 2))
}

compute_within_sd <- function(data, groups, unbias = FALSE) {
    lvls <- unique(groups[!is.na(groups)])
    var_sum <- 0; df_sum <- 0
    for (g in lvls) {
        gd <- data[!is.na(groups) & groups == g]
        ng <- length(gd)
        if (ng >= 2) { var_sum <- var_sum + (ng - 1) * var(gd); df_sum <- df_sum + (ng - 1) }
    }
    if (df_sum <= 0) {   # all subgroups of size 1 = individual data: MRbar/d2(2) (1.2.2)
        mr <- compute_mrbar_sd(data)
        return(if (is.finite(mr) && mr > 0) mr else sd(data))
    }
    sp <- sqrt(var_sum / df_sum)
    # Minitab default ("use unbiasing constants"): Sp / c4(d + 1), d = sum(n_i - 1)
    if (isTRUE(unbias)) sp / c4_const(df_sum + 1) else sp
}

compute_rbar_sd <- function(data, groups) {
    lvls <- unique(groups[!is.na(groups)])
    ranges <- numeric(0); sizes <- numeric(0)
    for (g in lvls) {
        gd <- data[!is.na(groups) & groups == g]
        ng <- length(gd)
        if (ng >= 2) {
            ranges <- c(ranges, max(gd) - min(gd))
            sizes  <- c(sizes, ng)
        }
    }
    if (length(ranges) == 0) return(NA_real_)
    # mean of R_i / d2(n_i): identical to Rbar/d2(n) for equal subgroup sizes, and
    # unbiased for unequal sizes (d2 of the average size was used before)
    d2v <- vapply(sizes, d2_const, numeric(1))
    if (any(!is.finite(d2v) | d2v <= 0)) return(NA_real_)
    mean(ranges / d2v)
}

compute_sbar_sd <- function(data, groups) {
    lvls <- unique(groups[!is.na(groups)])
    sds <- numeric(0); sizes <- numeric(0)
    for (g in lvls) {
        gd <- data[!is.na(groups) & groups == g]
        ng <- length(gd)
        if (ng >= 2) {
            sds   <- c(sds, sd(gd))
            sizes <- c(sizes, ng)
        }
    }
    if (length(sds) == 0) return(NA_real_)
    # mean of s_i / c4(n_i): identical to Sbar/c4(n) for equal subgroup sizes,
    # unbiased for unequal sizes
    c4v <- vapply(sizes, c4_const, numeric(1))
    if (any(!is.finite(c4v) | c4v <= 0)) return(NA_real_)
    mean(sds / c4v)
}

compute_mrbar_sd <- function(data) {
    x <- data[!is.na(data)]
    n <- length(x)
    if (n < 2) return(NA_real_)
    mr    <- abs(diff(x))
    mrbar <- mean(mr)
    if (!is.finite(mrbar) || mrbar <= 0) return(NA_real_)   # constant data guard
    mrbar / D2_TABLE["2"]
}

PC_DIST_MAP <- c(NORMAL="Normal", LOGNORMAL="Lognormal", LOGNORMAL3P="Lognormal3P",
                 WEIBULL="Weibull", WEIBULL3P="Weibull3P", GAMMA="Gamma", GAMMA3P="Gamma3P",
                 EXPONENTIAL="Exponential", LOGISTIC="Logistic", LOGLOGISTIC="Loglogistic",
                 SEV="SEV", LEV="LEV", INVGAUSS="InvGaussian", JOHNSONSU="JohnsonSU",
                 JOHNSONSB="JohnsonSB", BETA="Beta")

PC_DIST_CHOICES <- c("AUTO", names(PC_DIST_MAP), "BOXCOX", "EMPIRICAL")

pc_fit_chosen <- function(x, dist) {
    nm <- PC_DIST_MAP[[dist]]
    fits <- da_fit_all(x, include_3p = grepl("3P$", nm))
    if (is.null(fits)) return(NULL)
    hit <- Filter(function(f) identical(f$name, nm), fits)
    if (length(hit)) hit[[1]] else NULL
}

pc_draw <- function(expr, width = 480, height = 480) {
    d <- tempfile(pattern = "pc_chart_"); dir.create(d, showWarnings = FALSE)
    grDevices::png(file.path(d, "page%03d.png"), width = width, height = height)   # one file per page
    dev_id <- grDevices::dev.cur()
    err <- tryCatch({ expr; NULL }, error = function(e) e)
    if (dev_id %in% grDevices::dev.list()) grDevices::dev.off(dev_id)
    # submit whatever was drawn (a chart that failed part-way still shows its pages)
    pages <- sort(list.files(d, pattern = "^page[0-9]+\\.png$", full.names = TRUE))
    for (f in pages) if (file.info(f)$size > 0) spssRGraphics.Submit(f)
    if (!is.null(err)) stop(err)
    invisible(NULL)
}

pc_nonnormal <- function(x, dist, lambda, lsl, usl, K, unbias, groups, sm, alpha) {
    x <- x[is.finite(x)]; n <- length(x)
    p_lo <- pnorm(-K / 2); p_up <- pnorm(K / 2)
    out <- list(ok = FALSE, dist = dist, n = n, msg = NULL, x = x, lsl = lsl, usl = usl, K = K)
    obs_b <- if (!is.null(lsl)) mean(x < lsl) * 1e6 else NA_real_
    obs_a <- if (!is.null(usl)) mean(x > usl) * 1e6 else NA_real_
    out$obs <- c(obs_b, obs_a, sum(c(obs_b, obs_a), na.rm = TRUE))
    iso <- function(q) {
        l <- if (!is.null(lsl)) (q[2] - lsl) / (q[2] - q[1]) else NA_real_
        u <- if (!is.null(usl)) (usl - q[2]) / (q[3] - q[2]) else NA_real_
        list(pp = if (!is.null(lsl) && !is.null(usl)) (usl - lsl) / (q[3] - q[1]) else NA_real_,
             ppl = l, ppu = u, ppk = min(c(l, u), na.rm = TRUE))
    }
    if (dist == "EMPIRICAL") {
        q <- unname(quantile(x, c(p_lo, 0.5, p_up), type = 7))
        if (!(q[3] > q[2] && q[2] > q[1])) { out$msg <- "the sample percentiles are not distinct (too few distinct values)"; return(out) }
        out <- c(out, iso(q)); out$q <- q; out$exp <- out$obs
        out$label <- "Empirical (no distribution assumed)"; out$params <- "—"
        out$ok <- TRUE; return(out)
    }
    if (dist == "BOXCOX") {
        sh <- if (min(x) <= 0) abs(min(x)) + 0.001 else 0
        lam_est <- is.null(lambda) || !is.finite(lambda)
        lam <- if (lam_est) boxcox_lambda(x + sh) else lambda
        t <- bc_transform(x + sh, lam, 0)
        if (any(!is.finite(t)) || sd(t) <= 0) { out$msg <- "the Box-Cox transformation could not be applied"; return(out) }
        tl <- if (!is.null(lsl)) { v <- suppressWarnings(bc_transform(lsl + sh, lam, 0)); if (is.finite(v)) v else -Inf } else NULL
        tu <- if (!is.null(usl)) { v <- suppressWarnings(bc_transform(usl + sh, lam, 0)); if (is.finite(v)) v else NA_real_ } else NULL
        m <- mean(t); so <- sd(t); if (unbias) so <- so / c4_const(length(t))
        sw <- tryCatch(switch(sm,
                  POOLED = if (!is.null(groups)) compute_within_sd(t, groups, unbias) else compute_mrbar_sd(t),
                  RBAR   = if (!is.null(groups)) compute_rbar_sd(t, groups) else compute_mrbar_sd(t),
                  SBAR   = if (!is.null(groups)) compute_sbar_sd(t, groups) else compute_mrbar_sd(t),
                  compute_mrbar_sd(t)), error = function(e) NA_real_)
        if (!is.finite(sw) || sw <= 0) sw <- so
        idx <- function(sg) { l <- if (!is.null(tl)) (m - tl) / ((K / 2) * sg) else NA_real_
                              u <- if (!is.null(tu)) (tu - m) / ((K / 2) * sg) else NA_real_
                              list(p = if (!is.null(tl) && !is.null(tu) && is.finite(tl) && is.finite(tu)) (tu - tl) / (K * sg) else NA_real_,
                                   l = l, u = u, k = min(c(l, u), na.rm = TRUE)) }
        w <- idx(sw); o <- idx(so)
        out$pp <- o$p; out$ppl <- o$l; out$ppu <- o$u; out$ppk <- o$k
        out$cp <- w$p; out$cpl <- w$l; out$cpu <- w$u; out$cpk <- w$k
        eb <- if (!is.null(tl)) pnorm((tl - m) / so) * 1e6 else NA_real_
        ea <- if (!is.null(tu)) (1 - pnorm((tu - m) / so)) * 1e6 else NA_real_
        out$exp <- c(eb, ea, sum(c(eb, ea), na.rm = TRUE))
        bt <- function(z) { v <- if (abs(lam) < 1e-4) exp(z) else (lam * z + 1)^(1 / lam); v - sh }
        out$q <- c(bt(m + qnorm(p_lo) * so), bt(m), bt(m + qnorm(p_up) * so))
        adr <- tryCatch(if (requireNamespace("nortest", quietly = TRUE) && length(t) >= 8) {
                   a <- nortest::ad.test(t); c(a$statistic[[1]], a$p.value) } else {
                   a <- da_ad_stat(pnorm(sort(t), mean(t), sd(t))); c(a, da_ad_pval(a, length(t))) },
                   error = function(e) c(NA_real_, NA_real_))
        out$ad <- adr[1]; out$ad_p <- adr[2]; out$aic <- NA_real_
        out$label <- sprintf("Box-Cox (λ = %.3f%s)", lam, if (lam_est) ", estimated" else ", specified")
        out$params <- sprintf("λ = %.4f; shift = %s; transformed mean = %.4f, SD = %.4f", lam,
                              if (sh > 0) format(signif(sh, 4)) else "none", m, so)
        out$bc <- list(t = t, lam = lam, sh = sh, tl = tl, tu = tu, m = m, so = so, sw = sw)
        out$ok <- TRUE; return(out)
    }
    fi <- tryCatch(pc_fit_chosen(x, dist), error = function(e) NULL)
    if (is.null(fi)) {
        out$msg <- if (dist %in% c("LOGNORMAL","LOGNORMAL3P","WEIBULL","WEIBULL3P","GAMMA","GAMMA3P",
                                   "EXPONENTIAL","LOGLOGISTIC","INVGAUSS") && min(x) <= 0)
                       "this distribution needs all values > 0"
                   else if (dist == "BETA" && (min(x) <= 0 || max(x) >= 1)) "the Beta distribution needs all values strictly between 0 and 1"
                   else if (grepl("3P$", dist) && n < 10) "3-parameter fits need at least 10 observations"
                   else "the maximum-likelihood fit did not converge"
        return(out)
    }
    q <- vapply(c(p_lo, 0.5, p_up), function(pr) da_qval(fi, pr, x), numeric(1))
    if (any(!is.finite(q)) || !(q[3] > q[2] && q[2] > q[1])) { out$msg <- "the fitted percentiles could not be computed"; return(out) }
    out <- c(out, iso(q)); out$q <- q
    eb <- if (!is.null(lsl)) da_pval(fi, lsl) * 1e6 else NA_real_
    ea <- if (!is.null(usl)) (1 - da_pval(fi, usl)) * 1e6 else NA_real_
    out$exp <- c(eb, ea, sum(c(eb, ea), na.rm = TRUE))
    out$ad <- fi$ad; out$ad_p <- fi$ad_p; out$aic <- fi$aic
    out$label <- fi$label; out$params <- da_fmt_params(fi); out$fit <- fi
    out$ok <- TRUE; out
}

pc_nn_density <- function(ni, xs) {
    if (ni$dist == "EMPIRICAL") { d <- density(ni$x); return(approx(d$x, d$y, xs, yleft = 0, yright = 0)$y) }
    if (ni$dist == "BOXCOX") { b <- ni$bc; z <- xs + b$sh; ok <- z > 0; y <- rep(0, length(xs))
        tz <- bc_transform(z[ok], b$lam, 0); y[ok] <- dnorm(tz, b$m, b$so) * z[ok]^(b$lam - 1); return(y) }
    y <- da_density_vals(ni$fit, xs); y[!is.finite(y)] <- 0; y
}

pc_nn_probplot <- function(ni) {
    x <- sort(ni$x); n <- length(x)
    if (n > 400) x <- x[unique(round(seq(1, n, length.out = 400)))]
    pp <- (match(x, sort(ni$x)) - 0.375) / (n + 0.25)
    if (ni$dist == "BOXCOX") { t <- sort(ni$bc$t); tt <- t[unique(round(seq(1, n, length.out = min(n, 400))))]
        return(list(theo = qnorm(((seq_along(tt) - 0.375) / (length(tt) + 0.25)), ni$bc$m, ni$bc$so), obs = tt, scale = "Box-Cox scale")) }
    list(theo = vapply(pp, function(pr) da_qval(ni$fit, pr, ni$x), numeric(1)), obs = x, scale = "original scale")
}

pc_hist <- function(x, lsl = NULL, usl = NULL, nbins = NULL, xlim = NULL, breaks = NULL) {
    x <- x[is.finite(x)]; n <- length(x)
    if (!is.null(breaks)) {                        # given breaks (e.g. shared by several panels)
        brks <- breaks; w <- diff(brks[1:2]); tol <- w * 1e-9
        x <- x[x >= brks[1] - tol & x <= brks[length(brks)] + tol]; n <- max(1L, length(x))
        idx <- findInterval(x, brks, rightmost.closed = TRUE, all.inside = TRUE)
        if (!is.null(usl) && length(usl) && is.finite(usl)) {
            on <- abs(x - usl) <= tol & idx > 1 & abs(brks[idx] - usl) <= tol; idx[on] <- idx[on] - 1L }
        cnt <- tabulate(idx, nbins = length(brks) - 1L)
        return(structure(list(breaks = brks, counts = cnt, density = cnt / (n * diff(brks)),
                              mids = (brks[-1] + brks[-length(brks)]) / 2, xname = "x", equidist = TRUE),
                         class = "histogram"))
    }
    if (is.null(nbins) || !is.finite(nbins)) nbins <- max(8L, min(30L, ceiling(sqrt(n) * 1.5)))
    r <- range(x)
    w <- diff(r) / nbins
    if (!is.finite(w) || w <= 0) w <- max(abs(r[1]) * 0.01, 1e-6)
    lsl <- if (!is.null(lsl) && length(lsl) && is.finite(lsl)) lsl else NULL
    usl <- if (!is.null(usl) && length(usl) && is.finite(usl)) usl else NULL
    if (!is.null(lsl) && !is.null(usl) && usl > lsl) {
        k <- max(1L, round((usl - lsl) / w)); w <- (usl - lsl) / k; anchor <- lsl
    } else anchor <- if (!is.null(lsl)) lsl else if (!is.null(usl)) usl else r[1]
    tol <- w * 1e-9
    lo <- anchor + floor((r[1] - anchor) / w + 1e-9) * w
    hi <- anchor + ceiling((r[2] - anchor) / w - 1e-9) * w
    if (hi <= r[2] + tol) hi <- hi + w
    if (lo > r[1] - tol) lo <- lo - (if (lo > r[1]) w else 0)
    brks <- seq(lo, hi + tol, by = w)
    if (length(brks) < 2) brks <- c(lo, lo + w)
    idx <- findInterval(x, brks, rightmost.closed = TRUE, all.inside = TRUE)
    if (!is.null(usl)) {                           # x == USL belongs to the bin ending at USL
        on <- abs(x - usl) <= tol & idx > 1 & abs(brks[idx] - usl) <= tol
        idx[on] <- idx[on] - 1L
    }
    cnt <- tabulate(idx, nbins = length(brks) - 1L)
    structure(list(breaks = brks, counts = cnt, density = cnt / (n * diff(brks)),
                   mids = (brks[-1] + brks[-length(brks)]) / 2, xname = "x", equidist = TRUE),
              class = "histogram")
}

medcouple <- function(x) {
    x <- sort(x[is.finite(x)])
    if (length(x) > 3000) x <- x[round(seq(1, length(x), length.out = 3000))]
    m <- median(x); lo <- x[x <= m]; hi <- x[x >= m]
    h <- outer(hi, lo, function(a, b) ifelse(a == b, 0, ((a - m) - (m - b)) / (a - b)))
    median(h)
}

boxcox_lambda <- function(data) {
    data <- data[!is.na(data) & data > 0]
    n <- length(data)
    if (n < 4) return(1)
    log_x     <- log(data)
    sum_log_x <- sum(log_x)

    profile_loglik <- function(lam) {
        y <- if (abs(lam) < 1e-8) log_x else (data^lam - 1) / lam
        if (any(!is.finite(y))) return(-Inf)
        sigma2 <- mean((y - mean(y))^2)
        if (!is.finite(sigma2) || sigma2 <= 0) return(-Inf)
        -(n/2) * log(sigma2) + (lam - 1) * sum_log_x
    }

    best_ll <- -Inf; best_lam <- 1
    for (lam in seq(-2, 2, by = 0.01)) {
        ll <- profile_loglik(lam)
        if (is.finite(ll) && ll > best_ll) { best_ll <- ll; best_lam <- lam }
    }
    best_lam
}

boxcox_lambda_ci <- function(data, conf = 0.95) {
    data <- data[!is.na(data) & data > 0]
    n <- length(data)
    if (n < 4) return(c(NA_real_, NA_real_))
    lx <- log(data); slx <- sum(lx)
    grid <- seq(-2, 2, by = 0.01)
    ll <- vapply(grid, function(lam) {
        y <- if (abs(lam) < 1e-8) lx else (data^lam - 1) / lam
        s2 <- mean((y - mean(y))^2)
        if (!is.finite(s2) || s2 <= 0) -Inf else -(n/2) * log(s2) + (lam - 1) * slx
    }, numeric(1))
    ok <- is.finite(ll) & ll >= max(ll[is.finite(ll)]) - qchisq(conf, 1) / 2
    if (!any(ok)) return(c(NA_real_, NA_real_))
    range(grid[ok])
}

gesd_outliers <- function(x, alpha = 0.05, r = NULL) {
    n <- length(x)
    if (is.null(r)) r <- max(1L, min(10L, floor(n / 10)))
    r <- min(r, n - 3L)
    if (r < 1L) return(integer(0))
    idx <- seq_len(n); xs <- x; found <- integer(0); R <- lam <- numeric(r)
    for (i in seq_len(r)) {
        m <- mean(xs); sdv <- sd(xs)
        if (!is.finite(sdv) || sdv <= 0) { r <- i - 1L; break }
        j <- which.max(abs(xs - m))
        R[i] <- abs(xs[j] - m) / sdv
        nn <- n - i + 1
        tq <- qt(1 - alpha / (2 * nn), df = nn - 2)
        lam[i] <- (nn - 1) * tq / sqrt((nn - 2 + tq^2) * nn)
        found <- c(found, idx[j]); xs <- xs[-j]; idx <- idx[-j]
    }
    if (r < 1L) return(integer(0))
    k <- which(R[seq_len(r)] > lam[seq_len(r)])
    if (!length(k)) integer(0) else found[seq_len(max(k))]
}

bc_transform <- function(x, lambda, shift = 0) {
    x <- x + shift
    if (any(x <= 0, na.rm = TRUE)) return(rep(NA_real_, length(x)))
    if (abs(lambda) < 1e-4) log(x) else (x^lambda - 1) / lambda
}

dsev_ <- function(x, mu=0, sigma=1, log=FALSE) {
    z  <- (x - mu) / sigma
    ld <- -log(sigma) + z - exp(z)
    if (log) ld else exp(ld)
}

psev_ <- function(q, mu=0, sigma=1, lower.tail=TRUE) {
    p <- 1 - exp(-exp((q - mu)/sigma))
    if (lower.tail) p else 1 - p
}

dlev_ <- function(x, mu=0, sigma=1, log=FALSE) {
    z  <- -(x - mu) / sigma
    ld <- -log(sigma) + z - exp(z)
    if (log) ld else exp(ld)
}

plev_ <- function(q, mu=0, sigma=1, lower.tail=TRUE) {
    p <- exp(-exp(-(q - mu)/sigma))
    if (lower.tail) p else 1 - p
}

dllogis_ <- function(x, shape=1, scale=1, log=FALSE) {
    x  <- pmax(x, .Machine$double.eps)
    z  <- (x/scale)^shape
    ld <- log(shape) - log(scale) + (shape-1)*log(x/scale) - 2*log1p(z)
    if (log) ld else exp(ld)
}

pllogis_ <- function(q, shape=1, scale=1, lower.tail=TRUE) {
    q <- pmax(q, .Machine$double.eps)
    p <- 1 / (1 + (q/scale)^(-shape))
    if (lower.tail) p else 1 - p
}

dinvgauss_ <- function(x, mean=1, shape=1, log=FALSE) {
    x  <- pmax(x, .Machine$double.eps)
    ld <- 0.5*(log(shape) - log(2*pi) - 3*log(x)) -
          shape*(x - mean)^2 / (2*mean^2*x)
    if (log) ld else exp(ld)
}

pinvgauss_ <- function(q, mean=1, shape=1, lower.tail=TRUE) {
    q  <- pmax(q, .Machine$double.eps)
    t1 <- sqrt(shape/q)*(q/mean - 1)
    t2 <- -sqrt(shape/q)*(q/mean + 1)
    p  <- pnorm(t1) + exp(2*shape/mean + pnorm(t2, log.p=TRUE))
    p  <- pmin(1, pmax(0, p))
    if (lower.tail) p else 1 - p
}

da_ad_stat <- function(Fn_sorted) {
    n  <- length(Fn_sorted)
    Fn <- pmax(1e-12, pmin(1-1e-12, Fn_sorted))
    i  <- seq_len(n)
    -n - sum((2*i - 1)*(log(Fn) + log(1 - rev(Fn)))) / n
}

da_ad_pval <- function(A2, n=Inf) {
    # D'Agostino & Stephens (1986) approximation with the small-sample
    # modification A* = A2(1 + 0.75/n + 2.25/n^2) -- the same formula as
    # nortest::ad.test. The previous lookup table mixed critical values from
    # different cases and mislabelled several of them (e.g. 1.035 is the 1%
    # point, not 2.5%), so p-values and the ranking weight built on them were off.
    if (is.na(A2) || !is.finite(A2) || A2 < 0) return(NA_real_)
    AA <- if (is.finite(n) && n > 0) A2 * (1 + 0.75/n + 2.25/n^2) else A2
    p <- if (AA < 0.2)       1 - exp(-13.436 + 101.14 * AA - 223.73 * AA^2)
         else if (AA < 0.34) 1 - exp(-8.318 + 42.796 * AA - 59.938 * AA^2)
         else if (AA < 0.6)  exp(0.9177 - 4.279 * AA - 1.38 * AA^2)
         else if (AA < 10)   exp(1.2937 - 5.709 * AA + 0.0186 * AA^2)
         else 3.7e-24
    max(0, min(1, p))
}

da_pp_r2 <- function(dat, pfun, params) {
    tryCatch({
        n  <- length(dat); x <- sort(dat); i <- seq_len(n)
        ep <- (i - 0.375)/(n + 0.25)
        tp <- do.call(pfun, c(list(q=x), as.list(params)))
        if (any(!is.finite(tp))) return(NA_real_)
        cor(ep, tp)^2
    }, error=function(e) NA_real_)
}

da_fit_mle <- function(dat, dfun, pfun, start, lower=NULL, upper=NULL) {
    n <- length(dat)
    negll <- function(theta) {
        pars <- setNames(as.list(theta), names(start))
        lv   <- tryCatch(do.call(dfun, c(list(x=dat), pars, list(log=TRUE))),
                         error=function(e) rep(-Inf, n))
        if (any(!is.finite(lv))) return(1e15)
        -sum(lv)
    }
    opt <- tryCatch({
        if (!is.null(lower) || !is.null(upper))
            optim(unlist(start), negll, method="L-BFGS-B",
                  lower=if(is.null(lower)) rep(-Inf,length(start)) else lower,
                  upper=if(is.null(upper)) rep( Inf,length(start)) else upper,
                  control=list(maxit=3000))
        else
            optim(unlist(start), negll, method="Nelder-Mead",
                  control=list(maxit=3000, reltol=1e-10))
    }, error=function(e) NULL)
    # 1.1.0: for strictly positive parameters also search in log-parameter space.
    # Bound-constrained L-BFGS-B in raw units stalls when the parameters differ by
    # orders of magnitude (low-variability data: Weibull shape ~250, Gamma shape
    # ~5e4), which left Weibull stuck at its start value and made Gamma fail. The
    # log-space search is scale-free; the better of the two fits is kept, so a fit
    # is never worse than before.
    if (!is.null(lower) && length(lower) == length(start) && all(is.finite(lower) & lower > 0)) {
        negll_log <- function(t) negll(pmax(exp(t), lower))
        starts <- list(log(pmax(unlist(start), lower)))
        if (!is.null(opt) && all(is.finite(opt$par))) starts <- c(starts, list(log(pmax(opt$par, lower))))
        for (st in starts) {
            o2 <- tryCatch({
                a <- if (length(st) == 1)
                         optim(st, negll_log, method="BFGS", control=list(maxit=1000, reltol=1e-14))
                     else optim(st, negll_log, method="Nelder-Mead", control=list(maxit=6000, reltol=1e-12))
                b <- tryCatch(optim(a$par, negll_log, method="BFGS", control=list(maxit=500, reltol=1e-14)),
                              error=function(e) a)
                if (is.finite(b$value) && b$value <= a$value) b else a
            }, error=function(e) NULL)
            if (!is.null(o2) && is.finite(o2$value) && o2$value < 1e14 &&
                (is.null(opt) || !is.finite(opt$value) || opt$value >= 1e14 || o2$value < opt$value - 1e-9)) {
                opt <- list(par = setNames(pmax(exp(o2$par), lower), names(start)), value = o2$value, convergence = 0L)
            }
        }
    }
    # 1e15 is the penalty returned for infeasible parameters: such a "fit" failed
    if (is.null(opt) || opt$convergence > 1 || !is.finite(opt$value) || opt$value >= 1e14) return(NULL)
    params  <- setNames(opt$par, names(start))
    loglik  <- -opt$value
    if (!is.finite(loglik)) return(NULL)
    k       <- length(params)
    Fn_sort <- tryCatch({
        xs <- sort(dat)
        do.call(pfun, c(list(q=xs), as.list(params)))
    }, error=function(e) NULL)
    ad  <- if (!is.null(Fn_sort) && all(is.finite(Fn_sort)))
               da_ad_stat(Fn_sort) else NA_real_
    adp <- da_ad_pval(ad, n)
    r2  <- da_pp_r2(dat, pfun, params)
    list(params=params, loglik=loglik,
         aic=-2*loglik+2*k, bic=-2*loglik+k*log(n),
         ad=ad, ad_p=adp, pp_r2=r2, nparams=k)
}

da_density_vals <- function(fi, x_seq) {
    tryCatch({
        xs <- if (!is.null(fi$loc3)) x_seq - fi$loc3 else x_seq
        p  <- fi$params
        vals <- switch(fi$name,
            "Normal"      = dnorm(x_seq, p["mean"],     p["sd"]),
            "Logistic"    = dlogis(x_seq, p["location"], p["scale"]),
            "Cauchy"      = dcauchy(x_seq, p["location"],p["scale"]),
            "SEV"         = dsev_(x_seq, p["mu"],       p["sigma"]),
            "LEV"         = dlev_(x_seq, p["mu"],       p["sigma"]),
            "Lognormal"   = , "Lognormal3P" = dlnorm(pmax(xs,1e-15), p["meanlog"],p["sdlog"]),
            "Weibull"     = , "Weibull3P"   = dweibull(pmax(xs,1e-15), p["shape"],p["scale"]),
            "Gamma"       = , "Gamma3P"     = dgamma(pmax(xs,1e-15),  p["shape"],p["rate"]),
            "Exponential" = dexp(pmax(xs,1e-15),    p["rate"]),
            "Loglogistic" = dllogis_(pmax(xs,1e-15),p["shape"],p["scale"]),
            "InvGaussian" = dinvgauss_(pmax(xs,1e-15),p["mean"],p["shape"]),
            "Beta"        = dbeta(pmax(pmin(xs,1-1e-9),1e-9),p["shape1"],p["shape2"]),
            "JohnsonSU"   = djohnson_su_(x_seq, p["gamma"], p["delta"], p["xi"], p["lambda"]),
            "JohnsonSB"   = djohnson_sb_(x_seq, p["gamma"], p["delta"], p["xi"], p["lambda"]),
            rep(NA_real_, length(x_seq))
        )
        vals
    }, error=function(e) rep(NA_real_, length(x_seq)))
}

da_fmt_params <- function(fi) {
    p     <- fi$params
    parts <- mapply(function(nm,val) sprintf("%s=%.4g",nm,val), names(p), p)
    txt   <- paste(parts, collapse=", ")
    if (!is.null(fi$loc3) && is.finite(fi$loc3))
        txt <- paste0("loc=",sprintf("%.4g",fi$loc3),", ",txt)
    txt
}

djohnson_su_ <- function(x, gamma, delta, xi, lambda, log=FALSE) {
    z  <- (x - xi) / lambda
    y  <- gamma + delta * asinh(z)
    ld <- log(delta) - log(abs(lambda)) - 0.5*log(2*pi) -
          0.5*log1p(z^2) - 0.5*y^2
    if (log) ld else exp(ld)
}

pjohnson_su_ <- function(q, gamma, delta, xi, lambda)
    pnorm(gamma + delta * asinh((q - xi) / lambda))

djohnson_sb_ <- function(x, gamma, delta, xi, lambda, log=FALSE) {
    ld <- rep(-Inf, length(x))
    v  <- x > xi & x < xi + lambda
    if (any(v)) {
        xv <- x[v]; zv <- (xv - xi) / (xi + lambda - xv)
        yv <- gamma + delta * log(zv)
        ld[v] <- log(delta) + log(lambda) - log(xi + lambda - xv) -
                 log(xv - xi) - 0.5*log(2*pi) - 0.5*yv^2
    }
    if (log) ld else { r <- exp(ld); r[!is.finite(r)] <- 0; r }
}

pjohnson_sb_ <- function(q, gamma, delta, xi, lambda) {
    p <- rep(0.0, length(q))
    v <- q > xi & q < xi + lambda
    if (any(v))
        p[v] <- pnorm(gamma + delta * log((q[v]-xi)/(xi+lambda-q[v])))
    p[q >= xi + lambda] <- 1
    p
}

da_jb_test <- function(x) {
    n <- length(x)
    if (n < 8) return(list(stat=NA_real_, p=NA_real_))
    mu <- mean(x)
    m2 <- mean((x-mu)^2)          # biased variance  (n denominator)
    m3 <- mean((x-mu)^3)
    m4 <- mean((x-mu)^4)
    s3 <- m3 / m2^1.5             # biased skewness
    s4 <- m4 / m2^2               # biased kurtosis
    jb <- n/6 * (s3^2 + (s4-3)^2/4)
    list(stat=jb, p=pchisq(jb, df=2, lower.tail=FALSE))
}

da_pval <- function(fi, x) {
    p  <- fi$params
    x2 <- if (!is.null(fi$loc3)) x - fi$loc3 else x
    tryCatch(switch(fi$name,
        "Normal"      = pnorm(x,    p["mean"],     p["sd"]),
        "Logistic"    = plogis(x,   p["location"], p["scale"]),
        "Cauchy"      = pcauchy(x,  p["location"], p["scale"]),
        "SEV"         = psev_(x,    p["mu"],       p["sigma"]),
        "LEV"         = plev_(x,    p["mu"],       p["sigma"]),
        "Lognormal"   = , "Lognormal3P" = plnorm(pmax(x2,1e-15), p["meanlog"], p["sdlog"]),
        "Weibull"     = , "Weibull3P"   = pweibull(pmax(x2,1e-15), p["shape"], p["scale"]),
        "Gamma"       = , "Gamma3P"     = pgamma(pmax(x2,1e-15),   p["shape"], p["rate"]),
        "Exponential" = pexp(pmax(x2,1e-15), p["rate"]),
        "Loglogistic" = pllogis_(pmax(x2,1e-15), p["shape"], p["scale"]),
        "InvGaussian" = pinvgauss_(pmax(x2,1e-15), p["mean"], p["shape"]),
        "Beta"        = pbeta(pmax(pmin(x,1-1e-9),1e-9), p["shape1"], p["shape2"]),
        "JohnsonSU"   = pjohnson_su_(x, p["gamma"], p["delta"], p["xi"], p["lambda"]),
        "JohnsonSB"   = pjohnson_sb_(x, p["gamma"], p["delta"], p["xi"], p["lambda"]),
        NA_real_), error=function(e) NA_real_)
}

da_qval <- function(fi, prob, dat) {
    # Numerical CDF inversion via uniroot
    rng <- diff(range(dat, na.rm=TRUE))
    lo  <- min(dat, na.rm=TRUE) - rng * 5
    hi  <- max(dat, na.rm=TRUE) + rng * 5
    tryCatch(uniroot(function(x) da_pval(fi, x) - prob,
                     interval=c(lo, hi), tol=1e-8, extendInt="yes")$root,
             error=function(e) NA_real_)
}

da_fit_all <- function(dat, alpha=0.05, include_3p=TRUE) {
    dat  <- dat[is.finite(dat)]
    n    <- length(dat)
    if (n < 5) return(NULL)
    mn   <- mean(dat); vr <- var(dat); sd_ <- sqrt(max(vr, 1e-30))
    mn_  <- min(dat);  mx_ <- max(dat)
    all_pos <- mn_ > 0
    all_01  <- mn_ > 0 && mx_ < 1

    results <- list()

    add_fit <- function(fi, name, label, loc3=NULL) {
        if (is.null(fi)) return()
        fi$name <- name; fi$label <- label; fi$loc3 <- loc3
        results[[length(results)+1]] <<- fi
    }

    # ── Symmetric / unbounded distributions ──────────────────────────────────
    # 1. Normal
    ll_n <- sum(dnorm(dat, mn, sd_, log=TRUE))
    add_fit(list(params=c(mean=mn, sd=sd_), loglik=ll_n,
                 aic=-2*ll_n+4, bic=-2*ll_n+2*log(n),
                 ad=da_ad_stat(pnorm(sort(dat),mn,sd_)),
                 ad_p=da_ad_pval(da_ad_stat(pnorm(sort(dat),mn,sd_)),n),
                 pp_r2=da_pp_r2(dat,pnorm,c(mean=mn,sd=sd_)),
                 nparams=2L),
            "Normal","Normal")

    # 2. Logistic
    add_fit(da_fit_mle(dat, dlogis, plogis,
                       list(location=mn, scale=sd_*sqrt(3)/pi),
                       lower=c(-Inf,1e-6)),
            "Logistic","Logistic")

    # 3. Cauchy (use median/IQR as robust starting values)
    med_ <- median(dat); iqr_ <- IQR(dat)
    add_fit(da_fit_mle(dat, dcauchy, pcauchy,
                       list(location=med_, scale=max(iqr_/2, 1e-4)),
                       lower=c(-Inf,1e-6)),
            "Cauchy","Cauchy")

    # 4. SEV — Smallest Extreme Value (Gumbel min)
    mu_sev <- mn - 0.5772*sd_*sqrt(6)/pi
    sg_sev <- sd_*sqrt(6)/pi
    add_fit(da_fit_mle(dat, dsev_, psev_,
                       list(mu=mu_sev, sigma=max(sg_sev,1e-4)),
                       lower=c(-Inf,1e-6)),
            "SEV","SEV (Gumbel Min)")

    # 5. LEV — Largest Extreme Value (Gumbel max)
    add_fit(da_fit_mle(dat, dlev_, plev_,
                       list(mu=mn + 0.5772*sd_*sqrt(6)/pi,
                            sigma=max(sg_sev,1e-4)),
                       lower=c(-Inf,1e-6)),
            "LEV","LEV (Gumbel Max)")

    # 6. Johnson SU (unbounded, flexible skewness+kurtosis, 4-param)
    tryCatch(
        add_fit(da_fit_mle(dat, djohnson_su_, pjohnson_su_,
                           list(gamma=0, delta=1, xi=mn, lambda=sd_),
                           lower=c(-Inf, 1e-4, -Inf, 1e-4)),
                "JohnsonSU", "Johnson SU"),
        error=function(e) NULL)

    # 7. Johnson SB (bounded, 4-param — only when n ≥ 10 and range finite)
    if (n >= 10 && is.finite(mn_) && is.finite(mx_)) {
        mg   <- (mx_ - mn_) * 0.15
        xi0  <- mn_ - mg
        lam0 <- (mx_ - mn_) + 2*mg
        tryCatch(
            add_fit(da_fit_mle(dat, djohnson_sb_, pjohnson_sb_,
                               list(gamma=0, delta=1, xi=xi0, lambda=lam0),
                               lower=c(-Inf, 1e-4, -Inf, 1e-4)),
                    "JohnsonSB", "Johnson SB"),
            error=function(e) NULL)
    }

    # ── Strictly-positive distributions ──────────────────────────────────────
    if (all_pos) {
        # 6. Lognormal (2P) — closed-form MLE
        ld <- log(dat); lmn <- mean(ld); lsd <- sqrt(mean((ld - lmn)^2))   # MLE (divisor n), as the 3P profile fit
        ll_ln <- sum(dlnorm(dat,lmn,lsd,log=TRUE))
        add_fit(list(params=c(meanlog=lmn,sdlog=lsd), loglik=ll_ln,
                     aic=-2*ll_ln+4, bic=-2*ll_ln+2*log(n),
                     ad=da_ad_stat(plnorm(sort(dat),lmn,lsd)),
                     ad_p=da_ad_pval(da_ad_stat(plnorm(sort(dat),lmn,lsd)),n),
                     pp_r2=da_pp_r2(dat,plnorm,c(meanlog=lmn,sdlog=lsd)),
                     nparams=2L),
                "Lognormal","Lognormal (2P)")

        # 7. Weibull (2P)
        add_fit(da_fit_mle(dat, dweibull, pweibull,
                           list(shape=min(max(1.2*mn/sd_,0.5),1e5), scale=mn),
                           lower=c(1e-3,1e-6)),
                "Weibull","Weibull (2P)")

        # 8. Gamma (2P) — method-of-moments start
        add_fit(da_fit_mle(dat, dgamma, pgamma,
                           list(shape=max(mn^2/vr,0.5), rate=max(mn/vr,1e-6)),
                           lower=c(1e-3,1e-9)),
                "Gamma","Gamma (2P)")

        # 9. Exponential (1P)
        add_fit(da_fit_mle(dat, dexp, pexp,
                           list(rate=1/mn), lower=c(1e-9)),
                "Exponential","Exponential")

        # 10. Loglogistic (2P)
        add_fit(da_fit_mle(dat, dllogis_, pllogis_,
                           list(shape=max(pi/sd_*mn,0.5), scale=mn),
                           lower=c(1e-3,1e-9)),
                "Loglogistic","Loglogistic (2P)")

        # 11. Inverse Gaussian
        add_fit(da_fit_mle(dat, dinvgauss_, pinvgauss_,
                           list(mean=mn, shape=max(mn^3/vr,1e-3)),
                           lower=c(1e-6,1e-6)),
                "InvGaussian","Inverse Gaussian")
    }

    # ── (0,1)-bounded ─────────────────────────────────────────────────────────
    if (all_01) {
        al <- mn*(mn*(1-mn)/vr - 1); be <- (1-mn)*(mn*(1-mn)/vr - 1)
        if (is.finite(al) && al > 0 && is.finite(be) && be > 0)
            add_fit(da_fit_mle(dat, dbeta, pbeta,
                               list(shape1=al, shape2=be),
                               lower=c(1e-3,1e-3)),
                    "Beta","Beta")
    }

    # ── 3-parameter (threshold) variants ─────────────────────────────────────
    if (include_3p && all_pos && n >= 10) {
        # Threshold: profile-likelihood (lognormal) estimate below the minimum,
        # instead of the fixed 0.95 * min (meaningless for data far from zero,
        # e.g. data near 1000 with SD 1 gave a threshold of 950). The old value is
        # kept whenever it gives the higher likelihood, so the fit never gets worse.
        theta0 <- mn_ * 0.95
        prof_ln <- function(th) {
            d <- dat - th
            if (any(d <= 0)) return(-Inf)
            ld <- log(d); s_ <- sqrt(mean((ld - mean(ld))^2))
            if (!is.finite(s_) || s_ <= 0) return(-Inf)
            sum(dlnorm(d, mean(ld), s_, log=TRUE))
        }
        sc_  <- max(mx_ - mn_, sd_)
        th_o <- tryCatch(optimize(prof_ln, c(mn_ - 10*sc_, mn_ - 1e-3*sc_), maximum=TRUE),
                         error=function(e) NULL)
        theta <- if (!is.null(th_o) && is.finite(th_o$objective) &&
                     th_o$objective > prof_ln(theta0) && th_o$maximum < mn_) th_o$maximum else theta0

        # 13. Weibull (3P)
        d3 <- dat - theta
        # Weibull / Gamma 3P: own profile-likelihood threshold (best of the old
        # 0.95*min, the lognormal threshold and this distribution's own optimum)
        fit3p <- function(dfun, pfun, startf, lower) {
            fit_at <- function(th) {
                d <- dat - th
                if (any(d <= 0)) return(NULL)
                f <- tryCatch(da_fit_mle(d, dfun, pfun, startf(d), lower=lower), error=function(e) NULL)
                if (!is.null(f)) f$th <- th
                f
            }
            ll_at <- function(th) { f <- fit_at(th); if (is.null(f)) -1e300 else f$loglik }
            cands <- list(fit_at(theta0), fit_at(theta))
            o <- tryCatch(optimize(ll_at, c(mn_ - 10*sc_, mn_ - 1e-3*sc_), maximum=TRUE, tol=1e-4*sc_),
                          error=function(e) NULL)
            if (!is.null(o)) cands <- c(cands, list(fit_at(o$maximum)))
            cands <- Filter(Negate(is.null), cands)
            if (!length(cands)) return(NULL)
            cands[[which.max(vapply(cands, function(f) f$loglik, numeric(1)))]]
        }
        fi3 <- fit3p(dweibull, pweibull,
                     function(d) list(shape=min(max(1.2*mean(d)/sd(d),0.5),200), scale=mean(d)),
                     c(1e-3,1e-6))
        if (!is.null(fi3)) {
            fi3$loc3 <- fi3$th; fi3$th <- NULL; fi3$name <- "Weibull3P"
            fi3$label <- "Weibull (3P)"; fi3$nparams <- 3L
            fi3$aic <- fi3$aic + 2; fi3$bic <- fi3$bic + log(n)
            results[[length(results)+1]] <- fi3
        }

        # 14. Lognormal (3P)
        if (all(d3 > 0)) {
            ld3 <- log(d3); lm3 <- mean(ld3); ls3 <- sqrt(mean((ld3 - lm3)^2))   # MLE, as the profile
            ll3 <- sum(dlnorm(d3,lm3,ls3,log=TRUE))
            ad3 <- da_ad_stat(plnorm(sort(d3),lm3,ls3))
            results[[length(results)+1]] <- list(
                name="Lognormal3P", label="Lognormal (3P)",
                params=c(meanlog=lm3, sdlog=ls3), loglik=ll3,
                aic=-2*ll3+6, bic=-2*ll3+3*log(n),
                ad=ad3, ad_p=da_ad_pval(ad3,n),
                pp_r2=da_pp_r2(d3, plnorm, c(meanlog=lm3,sdlog=ls3)),
                nparams=3L, loc3=theta)
        }

        # 15. Gamma (3P)
        fi3g <- fit3p(dgamma, pgamma,
                      function(d) list(shape=max(mean(d)^2/var(d),0.5), rate=max(mean(d)/var(d),1e-6)),
                      c(1e-3,1e-9))
        if (!is.null(fi3g)) {
            fi3g$loc3 <- fi3g$th; fi3g$th <- NULL; fi3g$name <- "Gamma3P"
            fi3g$label <- "Gamma (3P)"; fi3g$nparams <- 3L
            fi3g$aic <- fi3g$aic + 2; fi3g$bic <- fi3g$bic + log(n)
            results[[length(results)+1]] <- fi3g
        }
    }

    if (length(results) == 0) return(NULL)

    # ── Rank by composite score: 60% AD p-value + 40% AIC rank ──────────────
    aic_vals <- sapply(results, function(r) r$aic)
    aic_rnk  <- rank(aic_vals, ties.method="first")
    m        <- length(results)
    for (i in seq_len(m)) {
        adp_i <- results[[i]]$ad_p
        adp_i <- if (is.na(adp_i)) 0 else adp_i
        aic_s <- 1 - (aic_rnk[i]-1)/(m - 1 + 1e-9)
        results[[i]]$score <- 0.60*adp_i + 0.40*aic_s
    }
    results <- results[order(-sapply(results, function(r) r$score))]
    results
}

# ══════════════════════════════════════════════════════════════════════════════
# 1.1.0 quality-control tools: physical bounds, fast refits, tolerance intervals,
# recommendation stability (bootstrap), k-sample pooling test, distribution-based
# control limits, outlier sensitivity, saved probability / normal scores.
# ══════════════════════════════════════════════════════════════════════════════

di_base <- function(name) sub("3P$", "", name)
DI_DFUN <- list(Normal = dnorm, Logistic = dlogis, Cauchy = dcauchy, SEV = dsev_, LEV = dlev_, Lognormal = dlnorm,
                Weibull = dweibull, Gamma = dgamma, Exponential = dexp, Loglogistic = dllogis_, InvGaussian = dinvgauss_,
                Beta = dbeta, JohnsonSU = djohnson_su_, JohnsonSB = djohnson_sb_)
DI_UNBOUNDED <- c("Normal", "Logistic", "Cauchy", "SEV", "LEV", "JohnsonSU", "JohnsonSB")
DI_POSITIVE  <- c("Lognormal", "Weibull", "Gamma", "Exponential", "Loglogistic", "InvGaussian")

# ── physical bounds: positive families are fitted to y = x - L, U - x or (x - L)/(U - L) ──
di_bound_text <- function(b) switch(b$type, lower = sprintf("lower bound %s", format(b$L)),
                                    upper = sprintf("upper bound %s", format(b$U)),
                                    both = sprintf("bounds %s to %s", format(b$L), format(b$U)))
di_y <- function(f, x) { b <- f$bt; if (is.null(b)) x else switch(b$type, lower = x - b$L, upper = b$U - x, both = (x - b$L) / (b$U - b$L)) }
di_xy <- function(f, y) { b <- f$bt; if (is.null(b)) y else switch(b$type, lower = b$L + y, upper = b$U - y, both = b$L + (b$U - b$L) * y) }

# closed-form quantiles on the fit's own scale (threshold included)
di_qy <- function(f, p, dat = NULL) {
    q <- f$params; b <- di_base(f$name); loc <- if (!is.null(f$loc3) && is.finite(f$loc3)) f$loc3 else 0
    v <- tryCatch(switch(b,
        Normal = qnorm(p, q["mean"], q["sd"]), Logistic = qlogis(p, q["location"], q["scale"]),
        Cauchy = qcauchy(p, q["location"], q["scale"]),
        SEV = q["mu"] + q["sigma"] * log(-log(1 - p)), LEV = q["mu"] - q["sigma"] * log(-log(p)),
        Lognormal = qlnorm(p, q["meanlog"], q["sdlog"]), Weibull = qweibull(p, q["shape"], q["scale"]),
        Gamma = qgamma(p, q["shape"], q["rate"]), Exponential = qexp(p, q["rate"]),
        Loglogistic = q["scale"] * (p / (1 - p))^(1 / q["shape"]), Beta = qbeta(p, q["shape1"], q["shape2"]),
        JohnsonSU = q["xi"] + q["lambda"] * sinh((qnorm(p) - q["gamma"]) / q["delta"]),
        JohnsonSB = q["xi"] + q["lambda"] / (1 + exp(-(qnorm(p) - q["gamma"]) / q["delta"])),
        NULL), error = function(e) NULL)
    if (is.null(v) && b == "InvGaussian") v <- di_qinvg(p, q["mean"], q["shape"])
    if (is.null(v)) {   # numerical inversion (fallback)
        d <- if (!is.null(dat)) dat else c(q[1] * 0.1, q[1] * 3)
        v <- vapply(p, function(pp) da_qval(f, pp, d) - loc, numeric(1))
    }
    unname(v) + if (b %in% DI_POSITIVE) loc else 0
}
# inverse Gaussian quantile: vectorized Newton from a moment-matched lognormal start
di_qinvg <- function(p, m, l) {
    m <- unname(m); l <- unname(l); s2 <- log(1 + m / l)
    x <- qlnorm(p, log(m) - s2 / 2, sqrt(s2))
    for (it in 1:60) {
        Fx <- pinvgauss_(x, m, l); fx <- dinvgauss_(x, m, l)
        step <- (Fx - p) / pmax(fx, 1e-300); xn <- x - step
        bad <- !is.finite(xn) | xn <= 0; xn[bad] <- x[bad] / 2
        done <- all(abs(xn - x) <= 1e-12 * pmax(abs(x), 1e-300)); x <- xn
        if (done) break
    }
    # very skewed shapes: finish the few unconverged points by bisection on log(x)
    bad <- which(p > 0 & p < 1 & (!is.finite(x) | abs(pinvgauss_(x, m, l) - p) > 1e-9))
    for (i in bad) x[i] <- tryCatch(exp(uniroot(function(t) pinvgauss_(exp(t), m, l) - p[i], c(log(m) - 30, log(m) + 30),
                                               tol = 1e-12, extendInt = "upX")$root), error = function(e) NA_real_)
    x[p <= 0] <- 0; x[p >= 1] <- Inf
    x
}
# CDF, quantile and density on the data scale for any fit (bounded or not)
di_P <- function(f, x) {
    p <- da_pval(f, di_y(f, x))
    if (!is.null(f$bt) && f$bt$type == "upper") 1 - p else p
}
di_Q <- function(f, p, x = NULL) {
    if (is.null(f$bt)) {
        v <- di_qy(f, p, x)
        bad <- !is.finite(v)
        if (any(bad) && !is.null(x)) v[bad] <- vapply(p[bad], function(pp) da_qval(f, pp, x), numeric(1))
        return(v)
    }
    pp <- if (f$bt$type == "upper") 1 - p else p
    di_xy(f, di_qy(f, pp, if (!is.null(x)) di_y(f, x) else NULL))
}
di_D <- function(f, x) {
    d <- da_density_vals(f, di_y(f, x))
    if (!is.null(f$bt) && f$bt$type == "both") d / (f$bt$U - f$bt$L) else d
}

# ── fast refit with the threshold / bounds held fixed (bootstrap, sensitivity) ──
di_refit <- function(f, x) {
    z <- di_y(f, x); b <- di_base(f$name); p0 <- f$params; nm <- names(p0)
    loc <- if (!is.null(f$loc3) && is.finite(f$loc3)) f$loc3 else 0
    zz <- if (b %in% DI_POSITIVE) z - loc else z
    if (b %in% DI_POSITIVE && any(zz <= 0)) return(NULL)
    if (b == "Beta" && any(zz <= 0 | zz >= 1)) return(NULL)
    n <- length(zz)
    par <- if (b == "Normal") c(mean = mean(zz), sd = sd(zz))
           else if (b == "Lognormal") { lz <- log(zz); c(meanlog = mean(lz), sdlog = sqrt(mean((lz - mean(lz))^2))) }
           else if (b == "Exponential") c(rate = 1 / mean(zz))
           else if (b == "InvGaussian") c(mean = mean(zz), shape = 1 / mean(1 / zz - 1 / mean(zz)))
           else {
               pos <- nm %in% DI_POSPAR | (b == "InvGaussian" & nm == "mean")
               nll <- function(t) {
                   pr <- ifelse(pos, exp(t), t); names(pr) <- nm
                   d <- tryCatch(do.call(DI_DFUN[[b]], c(list(zz), as.list(pr), list(log = TRUE))), error = function(e) NA)
                   if (any(!is.finite(d))) 1e10 else -sum(d)
               }
               t0 <- ifelse(pos, log(pmax(p0, 1e-12)), p0)
               o <- tryCatch(optim(t0, nll, control = list(maxit = 400, reltol = 1e-9)), error = function(e) NULL)
               if (is.null(o) || o$value >= 1e10) return(NULL)
               setNames(ifelse(pos, exp(o$par), o$par), nm)
           }
    f2 <- f; f2$params <- par
    ll <- sum(log(pmax(da_density_vals(f2, z), 1e-300)))
    if (!is.null(f$bt) && f$bt$type == "both") ll <- ll - n * log(f$bt$U - f$bt$L)
    k <- f$nparams %||% length(par)
    f2$loglik <- ll; f2$aic <- -2 * ll + 2 * k; f2$bic <- -2 * ll + k * log(n)
    u <- sort(pmin(pmax(da_pval(f2, z), 1e-12), 1 - 1e-12))
    f2$ad <- da_ad_stat(u); f2$ad_p <- da_ad_pval(f2$ad, n)
    f2
}
# random values from a fit (data scale)
di_rand <- function(f, n) di_Q(f, runif(n))

# ── tolerance intervals ───────────────────────────────────────────────────────
di_np_tol <- function(x, P, g) {
    n <- length(x); xs <- sort(x); best <- 0; cf <- NA_real_
    for (r in seq_len(floor(n / 2))) { c_r <- pbeta(P, n - 2 * r + 1, 2 * r, lower.tail = FALSE); if (c_r >= g) { best <- r; cf <- c_r } else break }
    nmin <- 2; while (nmin < 1e5 && pbeta(P, nmin - 1, 2, lower.tail = FALSE) < g) nmin <- nmin + 1
    if (best == 0) return(list(lo = NA_real_, hi = NA_real_, conf = pbeta(P, n - 1, 2, lower.tail = FALSE), nmin = nmin))
    list(lo = xs[best], hi = xs[n - best + 1], conf = cf, r = best, nmin = nmin)
}
di_tolint <- function(x, rec, tr, fits, P, g, alpha, B = 500) {
    n <- length(x); rows <- list()
    add <- function(m, lo, hi, note) rows[[length(rows) + 1]] <<- data.frame(Method = m, Lower = di_fmt(lo, 5), Upper = di_fmt(hi, 5), Basis = note, stringsAsFactors = FALSE, check.names = FALSE)
    nf <- Filter(function(f) f$name == "Normal", fits)
    k2 <- sqrt((n - 1) * (1 + 1 / n) * qnorm((1 + P) / 2)^2 / qchisq(1 - g, n - 1))
    np <- length(nf) && is.finite(nf[[1]]$ad_p %||% NA) && nf[[1]]$ad_p >= alpha
    add("Normal", mean(x) - k2 * sd(x), mean(x) + k2 * sd(x),
        sprintf("Mean ± %.3f × SD (Howe). %s", k2, if (np) "The normal distribution fits." else "Not valid here: the normal distribution does not fit (AD p < alpha)."))
    if (identical(rec$key, "BOXCOX") && !is.null(tr$boxcox)) {
        sh <- if (min(x) <= 0) abs(min(x)) + 0.001 else 0; lam <- tr$boxcox$lam
        t <- bc_transform(x + sh, lam, 0); lo <- mean(t) - k2 * sd(t); hi <- mean(t) + k2 * sd(t)
        inv <- function(v) (if (abs(lam) < 1e-8) exp(v) else pmax(lam * v + 1, 0)^(1 / lam)) - sh
        add(sprintf("Box-Cox (λ = %.3f)", lam), inv(lo), inv(hi), "Normal tolerance interval on the transformed scale, transformed back.")
    } else if (is.null(rec$fit) && !is.null(rec$key) && grepl("^JOHNSON", rec$key)) {
        add(paste("Johnson", sub("JOHNSON", "", rec$key)), NA, NA, "No interval for a Johnson transformation; use the distribution-free row.")
    } else if (!is.null(rec$fit) && rec$fit$name != "Normal") {
        # bootstrap calibration (Beran): find the tail probability a such that the
        # interval [Q(a), Q(1 - a)] of a refitted distribution contains at least P of
        # the fitted distribution in a fraction g of parametric bootstrap samples
        f <- rec$fit
        fb <- Filter(Negate(is.null), lapply(seq_len(B), function(i) tryCatch(di_refit(f, di_rand(f, n)), error = function(e) NULL)))
        if (length(fb) >= B / 2) {
            cover <- function(a) mean(vapply(fb, function(h) { q <- di_Q(h, c(a, 1 - a)); di_P(f, q[2]) - di_P(f, q[1]) >= P }, TRUE), na.rm = TRUE)
            lo <- 1e-7; hi <- (1 - P) / 2
            short <- FALSE
            if (cover(hi) >= g) a <- hi else if (cover(lo) < g) { a <- lo; short <- TRUE } else {
                for (it in 1:25) { mid <- exp((log(lo) + log(hi)) / 2); if (cover(mid) >= g) lo <- mid else hi <- mid }; a <- lo }
            q <- di_Q(f, c(a, 1 - a), x)
            add(f$label, q[1], q[2], paste0(sprintf("Recommended distribution: fitted %.4g%% and %.4g%% points, calibrated by parametric bootstrap (%d samples).", 100 * a, 100 * (1 - a), length(fb)),
                                            if (short) " The requested confidence could not be reached with this sample size; the interval is the widest considered and its confidence is lower than requested." else ""))
        } else add(f$label, NA, NA, "Recommended distribution: the bootstrap refits failed, so no interval is given.")
    }
    t <- di_np_tol(x, P, g)
    add("Distribution-free", t$lo, t$hi,
        if (is.finite(t$lo)) sprintf("Order statistics %d and %d of %d; achieved confidence %.1f%%. Valid for any continuous distribution.", t$r, n - t$r + 1, n, 100 * t$conf)
        else sprintf("Needs at least %d values for %.0f%% coverage with %.0f%% confidence.", t$nmin, 100 * P, 100 * g))
    do.call(rbind, rows)
}

# ── recommendation stability (non-parametric bootstrap) ───────────────────────
di_stability <- function(fits, x, criterion, alpha, B = 100, rec = NULL, chk = NULL) {
    # Every fitted distribution is refitted on each resample (thresholds and bounds
    # held fixed) and the recommendation rules of di_recommend are applied to the
    # complete set, so on the original data this reproduces the recommendation.
    if (length(fits) < 2) return(NULL)
    nmv <- vapply(fits, `[[`, "", "name"); m <- length(fits)
    usable <- !is.na(DI_PCA_KEY[nmv]) & !vapply(fits, function(f) isTRUE(f$excluded), TRUE)
    pick <- function(rf) {
        ok <- !vapply(rf, is.null, TRUE)
        adp <- vapply(rf, function(f) if (is.null(f) || !is.finite(f$ad_p)) NA_real_ else f$ad_p, numeric(1))
        aic <- vapply(rf, function(f) if (is.null(f)) Inf else f$aic, numeric(1))
        bic <- vapply(rf, function(f) if (is.null(f)) Inf else f$bic, numeric(1))
        pass <- ok & usable & !is.na(adp) & adp >= alpha
        if (!any(pass)) {
            # coarse resolution (and nothing else wrong): best AD fit, as di_recommend
            if (isTRUE(chk$coarse) && !isTRUE(chk$bimodal) && !isTRUE(chk$unstable) && any(ok & usable))
                return(which(ok & usable)[which.max(ifelse(is.na(adp[ok & usable]), -1, adp[ok & usable]))])
            return(0L)                                                  # no distribution fits
        }
        iN <- which(nmv == "Normal" & pass)
        if (length(iN) && aic[iN[1]] - min(aic[ok & usable]) <= 2) return(iN[1])
        a0 <- ifelse(is.na(adp), 0, adp)
        key <- switch(criterion, AD = -ifelse(is.na(adp), -1, adp), AIC = aic, BIC = bic,
                      -(0.6 * a0 + 0.4 * (1 - (rank(aic, ties.method = "first") - 1) / (m - 1 + 1e-9))))
        o <- order(key); w <- o[pass[o]][1]
        two <- DI_TWO_OF[nmv[w]]
        if (!is.na(two)) { j <- which(nmv == two & pass)
            if (length(j) && pchisq(max(2 * (rf[[w]]$loglik - rf[[j[1]]]$loglik), 0), 1, lower.tail = FALSE) >= alpha) w <- j[1] }
        w
    }
    n <- length(x); win <- numeric(m); nonefit <- 0; passc <- numeric(m); done <- 0
    for (b in seq_len(B)) {
        xb <- x[sample.int(n, n, replace = TRUE)]
        rf <- lapply(fits, function(f) tryCatch(di_refit(f, xb), error = function(e) NULL))
        if (sum(!vapply(rf, is.null, TRUE)) < 2) next
        wi <- pick(rf); if (wi == 0L) nonefit <- nonefit + 1 else win[wi] <- win[wi] + 1
        passc <- passc + vapply(rf, function(f) as.numeric(!is.null(f) && is.finite(f$ad_p) && f$ad_p >= alpha), numeric(1))
        done <- done + 1
    }
    if (done < 10) return(NULL)
    reclab <- if (!is.null(rec$fit)) rec$fit$label else NULL
    show <- which(win > 0 | vapply(fits, function(f) identical(f$label, reclab), TRUE))
    show <- show[order(-win[show])]
    lab <- c(vapply(fits[show], `[[`, "", "label"), if (nonefit > 0) "No distribution fits (transformation or empirical)")
    sh  <- c(win[show], if (nonefit > 0) nonefit) / done
    tb <- data.frame(Distribution = lab, `Recommended in % of resamples` = sprintf("%.1f", 100 * sh),
                     `Fits (AD p ≥ alpha) in % of resamples` = c(sprintf("%.1f", 100 * passc[show] / done), if (nonefit > 0) "—"),
                     stringsAsFactors = FALSE, check.names = FALSE)
    o <- order(-sh); tb <- tb[o, , drop = FALSE]; sh <- sh[o]; rownames(tb) <- NULL
    verdict <- if (sh[1] >= 0.7) sprintf("Stable: %s in %.0f%% of resamples.", if (grepl("^No distribution", tb$Distribution[1])) "no distribution fits" else paste(tb$Distribution[1], "is recommended"), 100 * sh[1])
               else sprintf("Uncertain: %s (%.0f%%) and %s (%.0f%%) are close. Prefer the one that makes engineering sense, and check that capability does not depend on the choice.",
                            tb$Distribution[1], 100 * sh[1], if (nrow(tb) > 1) tb$Distribution[2] else "others", if (nrow(tb) > 1) 100 * sh[2] else 100 * (1 - sh[1]))
    list(table = tb, verdict = verdict, B = done, crit = "the same rules as the recommendation")
}

# ── k-sample Anderson-Darling test (Scholz & Stephens 1987, A2akN, ties allowed) ──
di_ad_k_stat <- function(z, g) {
    N <- length(z); zs <- sort(unique(z)); L <- length(zs); m <- match(z, zs)
    lj <- tabulate(m, L); Baj <- cumsum(lj) - lj / 2
    den <- Baj * (N - Baj) - N * lj / 4; ok <- den > 0
    A <- 0
    for (lv in unique(g)) {
        cnt <- tabulate(m[g == lv], L); ni <- sum(cnt); Maj <- cumsum(cnt) - cnt / 2
        A <- A + sum((lj * (N * Maj - ni * Baj)^2 / den)[ok]) / ni
    }
    A * (N - 1) / N^2
}
di_pooltest <- function(x, g, B = 999) {
    keep <- is.finite(x) & !is.na(g); x <- x[keep]; g <- as.character(g[keep])
    if (length(unique(g)) < 2) return(NULL)
    a <- di_ad_k_stat(x, g)
    perm <- vapply(seq_len(B), function(i) di_ad_k_stat(x, sample(g)), numeric(1))
    list(A = a, p = (1 + sum(perm >= a - 1e-12)) / (B + 1), k = length(unique(g)), N = length(x))
}

# ── control limits for individual values ──────────────────────────────────────
di_ctrl <- function(x, rec, tr) {
    n <- length(x); mrbar <- mean(abs(diff(x)))
    nl <- c(mean(x) - 2.66 * mrbar, mean(x), mean(x) + 2.66 * mrbar)
    dl <- NULL; dname <- NULL; Pf <- NULL
    if (!is.null(rec$fit)) {
        f <- rec$fit; dl <- di_Q(f, c(0.00135, 0.5, 0.99865), x); dname <- f$label; Pf <- function(v) di_P(f, v)
    } else if (identical(rec$key, "BOXCOX") && !is.null(tr$boxcox)) {
        sh <- if (min(x) <= 0) abs(min(x)) + 0.001 else 0; lam <- tr$boxcox$lam; t <- bc_transform(x + sh, lam, 0)
        inv <- function(v) (if (abs(lam) < 1e-8) exp(v) else pmax(lam * v + 1, 0)^(1 / lam)) - sh
        dl <- inv(mean(t) + c(-3, 0, 3) * sd(t)); dname <- sprintf("Box-Cox (λ = %.3f)", lam)
        Pf <- function(v) pnorm(bc_transform(pmax(v + sh, 1e-12), lam, 0), mean(t), sd(t))
    }
    out <- function(l) sum(x < l[1] | x > l[3])
    fa <- function(l) if (is.null(Pf)) NA_real_ else Pf(l[1]) + 1 - Pf(l[3])
    rows <- data.frame(Limits = "Normal theory: mean ± 2.66 × MR-bar", LCL = di_fmt(nl[1], 5), `Center (mean)` = di_fmt(nl[2], 5), UCL = di_fmt(nl[3], 5),
                       `Points outside` = as.character(out(nl)), `False-alarm rate per point` = if (is.finite(fa(nl))) sprintf("%.3f%%", 100 * fa(nl)) else "—",
                       stringsAsFactors = FALSE, check.names = FALSE)
    if (!is.null(dl)) rows <- rbind(rows, data.frame(Limits = sprintf("%s: 0.135%% / median / 99.865%%", dname), LCL = di_fmt(dl[1], 5), `Center (mean)` = di_fmt(dl[2], 5),
                       UCL = di_fmt(dl[3], 5), `Points outside` = as.character(out(dl)), `False-alarm rate per point` = "0.270%",
                       stringsAsFactors = FALSE, check.names = FALSE))
    names(rows)[3] <- "Center"
    list(table = rows, nl = nl, dl = dl, dname = dname, fa_normal = fa(nl))
}
di_ichart <- function(x, ct, lab) {
    n <- length(x); yl <- range(c(x, ct$nl, ct$dl), na.rm = TRUE); yl <- yl + c(-1, 1) * diff(yl) * 0.05
    plot(seq_len(n), x, type = "o", pch = 19, cex = 0.5, col = "#2C3E50", lwd = 0.8, ylim = yl, xlab = "Observation (data order)", ylab = "Value",
         main = sprintf("Individuals Chart with Distribution-Based Limits — %s", lab), cex.main = 0.95)
    abline(h = ct$nl[c(1, 3)], col = "#95A5A6", lty = 2, lwd = 1.5); abline(h = ct$nl[2], col = "#95A5A6", lwd = 1)
    if (!is.null(ct$dl)) { abline(h = ct$dl[c(1, 3)], col = "#C0392B", lwd = 2); abline(h = ct$dl[2], col = "#27AE60", lwd = 1.5)
        o <- x < ct$dl[1] | x > ct$dl[3]; points(which(o), x[o], pch = 19, col = "#C0392B", cex = 1) }
    legend("topleft", c("Normal-theory limits", if (!is.null(ct$dl)) c(paste(ct$dname, "limits"), "Median")),
           col = c("#95A5A6", if (!is.null(ct$dl)) c("#C0392B", "#27AE60")), lty = c(2, 1, 1)[seq_len(if (is.null(ct$dl)) 1 else 3)], lwd = 2, bty = "n", cex = 0.75)
}

# ── SPSS expressions for the fitted CDF (used by /SAVE) ───────────────────────
di_num <- function(v) sprintf("%.15g", unname(v))
# split a long SPSS command over several lines (breaks at spaces; continuation lines indented)
di_wrap <- function(cmd, width = 200) {
    out <- character(0)
    while (nchar(cmd) > width) {
        cut <- max(gregexpr(" ", substr(cmd, 1, width), fixed = TRUE)[[1]])
        if (cut < 20) break
        out <- c(out, substr(cmd, 1, cut - 1)); cmd <- paste0("    ", substr(cmd, cut + 1, nchar(cmd)))
    }
    c(out, cmd)
}
di_cdf_expr <- function(f, var) {
    v <- var; b <- f$bt
    if (!is.null(b)) v <- switch(b$type, lower = if (b$L == 0) var else sprintf("(%s - %s)", var, di_num(b$L)), upper = sprintf("(%s - %s)", di_num(b$U), var),
                                 both = sprintf("((%s - %s) / %s)", var, di_num(b$L), di_num(b$U - b$L)))
    q <- f$params; N <- function(k) di_num(q[k]); bs <- di_base(f$name)
    loc <- if (!is.null(f$loc3) && is.finite(f$loc3)) f$loc3 else 0
    w <- if (loc != 0) sprintf("(%s - %s)", v, di_num(loc)) else v
    pw <- sprintf("MAX(%s, 1E-300)", w)
    e <- switch(bs,
        Normal = sprintf("CDF.NORMAL(%s, %s, %s)", v, N("mean"), N("sd")),
        Logistic = sprintf("1 / (1 + EXP(-(%s - %s) / %s))", v, N("location"), N("scale")),
        Cauchy = sprintf("0.5 + ARTAN((%s - %s) / %s) / 3.14159265358979", v, N("location"), N("scale")),
        SEV = sprintf("1 - EXP(-EXP((%s - %s) / %s))", v, N("mu"), N("sigma")),
        LEV = sprintf("EXP(-EXP(-(%s - %s) / %s))", v, N("mu"), N("sigma")),
        Lognormal = sprintf("CDF.NORMAL(LN(%s), %s, %s)", pw, N("meanlog"), N("sdlog")),
        Weibull = sprintf("1 - EXP(-((%s / %s) ** %s))", pw, N("scale"), N("shape")),
        Gamma = sprintf("CDF.CHISQ(2 * %s * %s, 2 * %s)", N("rate"), pw, N("shape")),
        Exponential = sprintf("1 - EXP(-%s * %s)", N("rate"), pw),
        Loglogistic = sprintf("1 / (1 + (%s / %s) ** (-%s))", pw, N("scale"), N("shape")),
        InvGaussian = sprintf("CDF.NORMAL(SQRT(%s / %s) * (%s / %s - 1), 0, 1) + EXP(2 * %s / %s + LN(MAX(CDF.NORMAL(-SQRT(%s / %s) * (%s / %s + 1), 0, 1), 1E-300)))",
                              N("shape"), pw, pw, N("mean"), N("shape"), N("mean"), N("shape"), pw, pw, N("mean")),
        Beta = sprintf("CDF.BETA(MIN(MAX(%s, 1E-12), 1 - 1E-12), %s, %s)", v, N("shape1"), N("shape2")),
        JohnsonSU = sprintf("CDF.NORMAL(%s + %s * LN((%s - %s) / %s + SQRT(((%s - %s) / %s) ** 2 + 1)), 0, 1)",
                            N("gamma"), N("delta"), v, N("xi"), N("lambda"), v, N("xi"), N("lambda")),
        JohnsonSB = sprintf("CDF.NORMAL(%s + %s * LN(MIN(MAX((%s - %s) / %s, 1E-12), 1 - 1E-12) / (1 - MIN(MAX((%s - %s) / %s, 1E-12), 1 - 1E-12))), 0, 1)",
                            N("gamma"), N("delta"), v, N("xi"), N("lambda"), v, N("xi"), N("lambda")),
        NULL)
    if (is.null(e)) return(NULL)
    if (!is.null(b) && b$type == "upper") e <- sprintf("1 - (%s)", e)
    e
}
di_bc_expr <- function(var, x, lam) {
    sh <- if (min(x) <= 0) abs(min(x)) + 0.001 else 0; t <- bc_transform(x + sh, lam, 0)
    tv <- if (abs(lam) < 1e-8) sprintf("LN(%s + %s)", var, di_num(sh)) else sprintf("(((%s + %s) ** %s) - 1) / %s", var, di_num(sh), di_num(lam), di_num(lam))
    sprintf("CDF.NORMAL(%s, %s, %s)", tv, di_num(mean(t)), di_num(sd(t)))
}
# new variable name: <base><suffix> within SPSS's 64-byte limit, never equal to the base
di_newname <- function(v, suffix) {
    b <- v
    while (nchar(paste0(b, suffix), type = "bytes") > 64 || toupper(paste0(b, suffix)) == toupper(v)) b <- substr(b, 1, nchar(b) - 1)
    paste0(b, suffix)
}
di_spss_str <- function(v) if (is.character(v)) paste0("'", gsub("'", "''", v), "'") else sprintf("%.17g", v)

# ══════════════════════════════════════════════════════════════════════════════
# STATS DISTRIBUTION IDENTIFICATION — main procedure
# Which distribution describes the data?  Fits up to 17 distributions by maximum
# likelihood and reports goodness of fit, parameter estimates with intervals,
# percentiles, capability by distribution, normalizing transformations, data
# checks that explain poor fits, and a recommendation with ready-to-run
# STATS PROCESS CAPABILITY syntax.
# ══════════════════════════════════════════════════════════════════════════════

DI_VERSION <- "1.1.0"
DI_TWO_OF  <- c(Weibull3P = "Weibull", Lognormal3P = "Lognormal", Gamma3P = "Gamma")
DI_PCA_KEY <- setNames(names(PC_DIST_MAP), unname(PC_DIST_MAP))     # internal name -> PCA keyword
DI_POSPAR  <- c("sd","scale","sigma","sdlog","shape","rate","delta","lambda","shape1","shape2","mean_ig")

di_fmt <- function(v, d = 4) if (is.null(v) || length(v) != 1 || !is.finite(v)) "—" else formatC(v, d, format = "f")
di_fmtp <- function(p) if (is.null(p) || !is.finite(p)) "—" else if (p < 0.0001) "<0.0001" else sprintf("%.4f", p)

# ── ranking ───────────────────────────────────────────────────────────────────
di_rank <- function(fits, criterion) {
    key <- switch(criterion,
        AD  = -vapply(fits, function(f) if (is.finite(f$ad_p %||% NA)) f$ad_p else -1, numeric(1)),
        AIC = vapply(fits, function(f) f$aic, numeric(1)),
        BIC = vapply(fits, function(f) f$bic, numeric(1)),
        -vapply(fits, function(f) f$score %||% 0, numeric(1)))
    fits[order(key)]
}

# Likelihood-ratio test: does the threshold (3P) improve on the 2-parameter model?
di_lrt <- function(fi, fits) {
    two <- DI_TWO_OF[fi$name]
    if (is.na(two)) return(NA_real_)
    f2 <- Filter(function(f) identical(f$name, unname(two)), fits)
    if (!length(f2)) return(NA_real_)
    lr <- 2 * (fi$loglik - f2[[1]]$loglik)
    pchisq(max(lr, 0), 1, lower.tail = FALSE)
}

# ── parameter estimates with standard errors (observed information) ─────────────
di_param_table <- function(fi, x, conf) {
    x <- di_y(fi, x)
    th <- fi$params; nm <- names(th); has_loc <- !is.null(fi$loc3) && is.finite(fi$loc3)
    if (has_loc) { th <- c(th, threshold = fi$loc3); nm <- names(th) }
    negll <- function(t) {
        f2 <- fi; f2$params <- setNames(t[seq_along(fi$params)], names(fi$params))
        if (has_loc) f2$loc3 <- t[length(t)]
        d <- da_density_vals(f2, x)
        if (any(!is.finite(d)) || any(d <= 0)) return(1e10)
        -sum(log(d))
    }
    se <- rep(NA_real_, length(th))
    H <- tryCatch(optimHess(th, negll, control = list(parscale = pmax(abs(th), 1e-8))), error = function(e) NULL)
    if (!is.null(H) && all(is.finite(H))) {
        V <- tryCatch(solve(H), error = function(e) NULL)
        if (!is.null(V)) { v <- diag(V); se <- ifelse(is.finite(v) & v > 0, sqrt(pmax(v, 0)), NA_real_) }
    }
    z <- qnorm(1 - (1 - conf) / 2)
    pos <- nm %in% DI_POSPAR | (fi$name == "InvGaussian" & nm == "mean") | (fi$name == "Exponential")
    lo <- ifelse(pos & th > 0, th * exp(-z * se / th), th - z * se)
    hi <- ifelse(pos & th > 0, th * exp( z * se / th), th + z * se)
    data.frame(Distribution = fi$label, Parameter = nm, Estimate = sapply(th, di_fmt, d = 6),
               `Std. Error` = sapply(se, di_fmt, d = 6),
               Lower = sapply(lo, di_fmt, d = 6), Upper = sapply(hi, di_fmt, d = 6),
               stringsAsFactors = FALSE, check.names = FALSE, row.names = NULL)
}

# ── capability with one fitted distribution (ISO 22514-4 percentile method) ────
di_cap <- function(fi, x, lsl, usl, K) {
    plo <- pnorm(-K / 2); pup <- pnorm(K / 2)
    q <- di_Q(fi, c(plo, 0.5, pup), x)
    if (any(!is.finite(q)) || !(q[3] > q[2] && q[2] > q[1])) return(NULL)
    ppl <- if (!is.null(lsl)) (q[2] - lsl) / (q[2] - q[1]) else NA_real_
    ppu <- if (!is.null(usl)) (usl - q[2]) / (q[3] - q[2]) else NA_real_
    eb  <- if (!is.null(lsl)) di_P(fi, lsl) * 1e6 else NA_real_
    ea  <- if (!is.null(usl)) (1 - di_P(fi, usl)) * 1e6 else NA_real_
    et  <- sum(c(eb, ea), na.rm = TRUE)
    list(pp = if (!is.null(lsl) && !is.null(usl)) (usl - lsl) / (q[3] - q[1]) else NA_real_,
         ppl = ppl, ppu = ppu, ppk = min(c(ppl, ppu), na.rm = TRUE), q = q,
         eb = eb, ea = ea, et = et, z = if (et > 0 && et < 1e6) -qnorm(et / 1e6) else NA_real_)
}

# ── normalizing transformations ───────────────────────────────────────────────
di_adp <- function(z) {
    z <- z[is.finite(z)]
    if (length(z) < 8) return(NA_real_)
    if (requireNamespace("nortest", quietly = TRUE)) return(tryCatch(nortest::ad.test(z)$p.value, error = function(e) NA_real_))
    a <- da_ad_stat(pnorm(sort(z), mean(z), sd(z))); da_ad_pval(a, length(z))
}
di_transforms <- function(x, fits, alpha) {
    rows <- list(); add <- function(...) rows[[length(rows) + 1]] <<- data.frame(..., stringsAsFactors = FALSE, check.names = FALSE)
    p0 <- di_adp(x)
    add(Transformation = "None (original data)", Parameters = "—", `AD p after` = di_fmtp(p0),
        `Normal after` = if (is.finite(p0)) (if (p0 >= alpha) "Yes" else "No") else "—", `Use in Process Capability` = "DISTRIBUTION=NORMAL")
    if (min(x) > 0) {
        pl <- di_adp(log(x))
        add(Transformation = "Log", Parameters = "ln(x)", `AD p after` = di_fmtp(pl),
            `Normal after` = if (is.finite(pl)) (if (pl >= alpha) "Yes" else "No") else "—", `Use in Process Capability` = "DISTRIBUTION=LOGNORMAL")
    }
    sh <- if (min(x) <= 0) abs(min(x)) + 0.001 else 0
    bc <- tryCatch({
        lam <- boxcox_lambda(x + sh)
        ci  <- tryCatch(boxcox_lambda_ci(x + sh), error = function(e) c(NA, NA))
        pb  <- di_adp(bc_transform(x + sh, lam, 0))
        lr  <- round(lam * 2) / 2
        list(lam = lam, ci = unlist(ci)[1:2], p = pb, lr = lr)
    }, error = function(e) NULL)
    if (!is.null(bc)) {
        add(Transformation = "Box-Cox", Parameters = sprintf("λ = %.3f (CI %s to %s; rounded %.1f)%s", bc$lam,
                di_fmt(bc$ci[1], 2), di_fmt(bc$ci[2], 2), bc$lr, if (sh > 0) sprintf(", shift %.4g", sh) else ""),
            `AD p after` = di_fmtp(bc$p), `Normal after` = if (is.finite(bc$p)) (if (bc$p >= alpha) "Yes" else "No") else "—",
            `Use in Process Capability` = sprintf("DISTRIBUTION=BOXCOX LAMBDA=%.3f", bc$lam))
    }
    best_j <- NULL
    for (f in Filter(function(f) f$name %in% c("JohnsonSU", "JohnsonSB"), fits)) {
        p <- f$params
        z <- tryCatch(if (f$name == "JohnsonSU") p["gamma"] + p["delta"] * asinh((x - p["xi"]) / p["lambda"])
                      else { u <- (x - p["xi"]) / p["lambda"]; p["gamma"] + p["delta"] * log(u / (1 - u)) },
                      error = function(e) rep(NA_real_, length(x)))
        pj <- di_adp(z)
        if (is.finite(pj) && (is.null(best_j) || pj > best_j$p)) best_j <- list(f = f, p = pj)
    }
    if (!is.null(best_j)) {
        add(Transformation = paste0("Johnson (", sub("Johnson", "", best_j$f$name), " family)"), Parameters = da_fmt_params(best_j$f),
            `AD p after` = di_fmtp(best_j$p), `Normal after` = if (best_j$p >= alpha) "Yes" else "No",
            `Use in Process Capability` = paste0("DISTRIBUTION=", toupper(best_j$f$name)))
    }
    list(table = do.call(rbind, rows), boxcox = bc, johnson = best_j, p0 = p0)
}

# ── data checks that explain poor fits ────────────────────────────────────────
di_checks <- function(x, alpha, xord = x) {
    n <- length(x); k <- length(unique(x))
    m3 <- mean((x - mean(x))^3) / sd(x)^3; m4 <- mean((x - mean(x))^4) / sd(x)^4 - 3
    g1 <- if (n > 2) m3 * sqrt(n * (n - 1)) / (n - 2) else NA_real_
    g2 <- if (n > 3) ((n + 1) * m4 + 6) * (n - 1) / ((n - 2) * (n - 3)) else NA_real_
    bcoef <- if (n > 3 && is.finite(g1) && is.finite(g2)) (g1^2 + 1) / (g2 + 3 * (n - 1)^2 / ((n - 2) * (n - 3))) else NA_real_
    # stability, distribution-free (an I chart's 3-sigma rule assumes normal data):
    # runs test about the median (Wald-Wolfowitz) and a shift test between halves
    # (xord: the values in data order, unweighted; differs from x only with weights)
    side <- sign(xord - median(xord)); side <- side[side != 0]
    n1 <- sum(side > 0); n2 <- sum(side < 0); R <- 1 + sum(diff(side) != 0)
    mu <- 1 + 2 * n1 * n2 / (n1 + n2); vr <- 2 * n1 * n2 * (2 * n1 * n2 - n1 - n2) / ((n1 + n2)^2 * (n1 + n2 - 1))
    p_runs <- if (n1 > 1 && n2 > 1 && vr > 0) 2 * pnorm(-abs((R - mu) / sqrt(vr))) else NA_real_
    no <- length(xord); h <- floor(no / 2)
    p_shift <- if (no >= 20) tryCatch(suppressWarnings(wilcox.test(xord[1:h], xord[(h + 1):no])$p.value), error = function(e) NA_real_) else NA_real_
    unstable <- (is.finite(p_runs) && p_runs < 0.01) || (is.finite(p_shift) && p_shift < 0.01)
    out <- tryCatch(length(gesd_outliers(x, alpha)), error = function(e) NA_integer_)
    rows <- list(); add <- function(a, b, c, d) rows[[length(rows) + 1]] <<- c(a, b, c, d)
    add("Sample size", as.character(n), if (n < 30) "Check" else "OK",
        if (n < 30) "Fewer than 30 values: several distributions will fit equally well; tail percentiles are uncertain." else "")
    coarse <- k < 10 || k / n < 0.2
    add("Distinct values (resolution)", sprintf("%d of %d", k, n), if (coarse) "Check" else "OK",
        if (coarse) "Many tied values (coarse measurement resolution): Anderson-Darling tests can reject every distribution. Judge the fit from the probability plots." else "")
    add("Skewness / excess kurtosis", sprintf("%s / %s", di_fmt(g1, 3), di_fmt(g2, 3)),
        if (is.finite(g1) && abs(g1) > 1) "Note" else "OK",
        if (is.finite(g1) && abs(g1) > 1) "Strong skew: consider right-skewed (Weibull, Lognormal, Gamma) or left-skewed (SEV, Weibull with large shape) models." else "")
    add("Bimodality coefficient", di_fmt(bcoef, 3), if (is.finite(bcoef) && bcoef > 0.555) "Check" else "OK",
        if (is.finite(bcoef) && bcoef > 0.555) "Above 0.555: the data may mix two processes (machines, shifts, lots). No single distribution can describe a mixture; split the data with GROUPVAR." else "")
    add("Stability (data order)", sprintf("runs test p = %s; first vs second half p = %s", di_fmtp(p_runs), di_fmtp(p_shift)),
        if (unstable) "Check" else "OK",
        if (unstable) "The values drift, shift or cluster in time order (p < 0.01). A distribution fitted to unstable data describes no single process, and capability results based on it are not reliable. Use a control chart before capability analysis." else "")
    add("Outliers (generalized ESD)", if (is.finite(out)) as.character(out) else "—", if (is.finite(out) && out > 0) "Check" else "OK",
        if (is.finite(out) && out > 0) "Outliers distort the fitted tails; verify them before choosing a distribution." else "")
    tb <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
    names(tb) <- c("Check", "Result", "Status", "Comment")
    list(table = tb, coarse = coarse, bimodal = is.finite(bcoef) && bcoef > 0.555, unstable = unstable)
}

# ── recommendation ───────────────────────────────────────────────────────────
di_recommend <- function(fits, alpha, criterion, tr, checks, n) {
    usable <- Filter(function(f) !is.null(DI_PCA_KEY[f$name]) && !is.na(DI_PCA_KEY[f$name]) && !isTRUE(f$excluded), fits)
    pass <- Filter(function(f) is.finite(f$ad_p %||% NA) && f$ad_p >= alpha, usable)
    why <- ""; rec <- NULL; key <- NULL
    nrm <- Filter(function(f) f$name == "Normal", pass)
    if (length(nrm)) {
        best_aic <- min(vapply(usable, function(f) f$aic, numeric(1)))
        if (nrm[[1]]$aic - best_aic <= 2) {
            rec <- nrm[[1]]; key <- "NORMAL"
            why <- "The normal distribution fits (AD p ≥ alpha) and no other distribution fits clearly better (ΔAIC ≤ 2). Use the standard analysis."
        }
    }
    if (is.null(rec) && length(pass)) {
        rec <- pass[[1]]
        two <- DI_TWO_OF[rec$name]
        if (!is.na(two)) {
            f2 <- Filter(function(f) identical(f$name, unname(two)), pass)
            lp <- di_lrt(rec, fits)
            if (length(f2) && is.finite(lp) && lp >= alpha) {
                rec <- f2[[1]]
                why <- sprintf("Fits adequately (AD p = %s). The 3-parameter version was not significantly better (LRT p = %.3f), so the simpler model is preferred.", di_fmtp(rec$ad_p), lp)
            }
        }
        if (!nzchar(why)) why <- sprintf("Best-ranked distribution (%s) that fits adequately (AD p = %s ≥ %.2f).",
                                         switch(criterion, AD = "highest AD p-value", AIC = "lowest AIC", BIC = "lowest BIC", "60% AD p + 40% AIC rank"),
                                         di_fmtp(rec$ad_p), alpha)
        key <- unname(DI_PCA_KEY[rec$name])
    }
    if (is.null(rec)) {
        if (!is.null(tr$boxcox) && is.finite(tr$boxcox$p) && tr$boxcox$p >= alpha) {
            key <- "BOXCOX"; why <- sprintf("No distribution fits, but the Box-Cox transformation (λ = %.3f) makes the data normal (AD p = %s).", tr$boxcox$lam, di_fmtp(tr$boxcox$p))
        } else if (!is.null(tr$johnson) && tr$johnson$p >= alpha) {
            key <- toupper(tr$johnson$f$name); why <- sprintf("No standard distribution fits, but a Johnson %s transformation makes the data normal (AD p = %s).", sub("Johnson", "", tr$johnson$f$name), di_fmtp(tr$johnson$p))
        } else if (isTRUE(checks$coarse) && length(usable) && !checks$bimodal && !checks$unstable) {
            rec <- usable[order(-vapply(usable, function(f) if (is.finite(f$ad_p %||% NA)) f$ad_p else -1, numeric(1)))][[1]]
            key <- unname(DI_PCA_KEY[rec$name])
            why <- sprintf("No distribution passes the AD test, but the data have few distinct values (coarse resolution), which makes AD reject every distribution. %s has the best AD fit (p = %s); confirm it with the probability plot.", rec$label, di_fmtp(rec$ad_p))
        } else if (n >= 1000) {
            key <- "EMPIRICAL"; why <- "No distribution or transformation fits. With 1000+ values the empirical percentiles can be used directly."
        } else {
            key <- NULL; why <- "No distribution or transformation fits these data. Review the Data Checks table: mixtures, instability, outliers or coarse resolution are the usual causes."
        }
    }
    cautions <- c(if (n < 30) sprintf("with only %d values the fit tests cannot tell similar distributions apart (30 or more recommended, 100 or more for tail percentiles)", n),
                  if (checks$unstable) "the process may be unstable", if (checks$bimodal) "the data may be a mixture",
                  if (checks$coarse) "coarse resolution makes the AD p-values pessimistic",
                  if (any(vapply(fits, function(f) isTRUE(f$excluded), TRUE))) "distributions that put more than 0.135% beyond the physical bound are not recommended")
    alts <- vapply(Filter(function(f) !identical(f$name, rec$name %||% ""), pass)[seq_len(min(3, max(0, length(pass) - 1)))],
                   function(f) sprintf("%s (AD p = %s)", f$label, di_fmtp(f$ad_p)), character(1))
    list(fit = rec, key = key, why = why, alts = alts, cautions = cautions)
}

di_pca_syntax <- function(varname, key, lsl, usl, lam = NULL, fit = NULL) {
    if (is.null(key)) return("—")
    pre <- ""
    if (!is.null(fit$bt)) {
        # positive distribution fitted against a physical bound: Process Capability
        # runs on the shifted / reflected / rescaled variable with the limits mapped
        b <- fit$bt; nv <- di_newname(varname, "_b"); m <- function(v) di_num(di_y(fit, v))
        if (b$type == "lower" && b$L == 0) nv <- varname
        pre <- if (identical(nv, varname)) "" else sprintf("COMPUTE %s = %s. ", nv, switch(b$type, lower = sprintf("%s - %s", varname, di_num(b$L)), upper = sprintf("%s - %s", di_num(b$U), varname),
                                                      both = sprintf("(%s - %s) / %s", varname, di_num(b$L), di_num(b$U - b$L))))
        lo <- if (b$type == "upper") usl else lsl; hi <- if (b$type == "upper") lsl else usl
        lsl <- if (!is.null(lo)) as.numeric(m(lo)) else NULL; usl <- if (!is.null(hi)) as.numeric(m(hi)) else NULL
        if (!is.null(lsl) && lsl <= 0) lsl <- NULL
        if (b$type == "both" && !is.null(usl) && usl >= 1) usl <- NULL
        varname <- nv; key <- toupper(sub(" .*", "", unname(DI_PCA_KEY[fit$name])))
    }
    crit <- paste(c(if (!is.null(lsl)) paste0("LSL=", di_num(lsl)), if (!is.null(usl)) paste0("USL=", di_num(usl))), collapse = " ")
    paste0(pre, "STATS PROCESS CAPABILITY VARIABLE=", varname,
           if (nzchar(crit)) paste0(" /CRITERIA ", crit) else " /CRITERIA LSL=<lsl> USL=<usl>",
           if (key == "NORMAL") "." else paste0(" /DATADIST DISTRIBUTION=", key, if (key == "BOXCOX" && !is.null(lam)) sprintf(" LAMBDA=%.3f", lam) else "", "."))
}

# ── Viewer charts ─────────────────────────────────────────────────────────────
di_probplot_panel <- function(fi, x, alpha) {
    xs <- sort(x); n <- length(xs); pp <- (seq_len(n) - 0.375) / (n + 0.25)
    if (n > 400) { k <- unique(round(seq(1, n, length.out = 400))); xs <- xs[k]; pp <- pp[k] }
    th <- di_Q(fi, pp, x); ok <- is.finite(th)
    col <- if (is.finite(fi$ad_p %||% NA) && fi$ad_p >= alpha) "#1E8449" else "#C0392B"
    plot(th[ok], xs[ok], pch = 19, cex = 0.55, col = "#2980B9", xlab = "Fitted quantile", ylab = "Observed",
         main = fi$label, cex.main = 0.95)
    abline(0, 1, col = "#E74C3C", lwd = 1.8)
    mtext(sprintf("AD p = %s", di_fmtp(fi$ad_p)), side = 3, line = 0.1, cex = 0.7, col = col)
    grid(col = "grey92", lty = 1, lwd = 0.5)
}

# ── HTML report ───────────────────────────────────────────────────────────────
di_plt <- function(fig, id, height = 420) {
    js <- tryCatch(jsonlite::toJSON(fig, auto_unbox = TRUE, null = "null", na = "null", digits = 10), error = function(e) NULL)
    if (is.null(js)) return("")
    sprintf('<div id="%s" style="width:100%%;height:%dpx"></div><script>Plotly.newPlot("%s", %s.data, %s.layout, {responsive:true, displaylogo:false, editable:true, edits:{shapePosition:false}, toImageButtonOptions:{format:"png", scale:2, filename:"%s"}});</script>',
            id, height, id, js, js, id)
}
di_hist_fig <- function(x, fits, lsl, usl, title) {
    h <- pc_hist(x, lsl, usl); xs <- seq(min(c(x, lsl, usl)) - diff(range(x)) * 0.15, max(c(x, lsl, usl)) + diff(range(x)) * 0.15, length.out = 300)
    pal <- c("#2C3E50", "#E67E22", "#8E44AD", "#16A085")
    tr <- list(list(type = "bar", x = h$mids, y = h$density, width = diff(h$breaks[1:2]) * 0.97, name = "Data",
                    marker = list(color = "#AED6F1", line = list(color = "white", width = 0.5))))
    for (i in seq_along(fits)) { y <- di_D(fits[[i]], xs); y[!is.finite(y)] <- NA
        tr[[length(tr) + 1]] <- list(type = "scatter", mode = "lines", x = xs, y = y, name = fits[[i]]$label,
                                     line = list(color = pal[(i - 1) %% 4 + 1], width = 2.5)) }
    shp <- Filter(Negate(is.null), list(
        if (!is.null(lsl)) list(type = "line", x0 = lsl, x1 = lsl, y0 = 0, y1 = 1, yref = "paper", line = list(color = "#E74C3C", width = 2, dash = "dash")),
        if (!is.null(usl)) list(type = "line", x0 = usl, x1 = usl, y0 = 0, y1 = 1, yref = "paper", line = list(color = "#E74C3C", width = 2, dash = "dash"))))
    list(data = tr, layout = list(title = list(text = title), xaxis = list(title = "Value"), yaxis = list(title = "Density"),
                                  shapes = shp, bargap = 0, legend = list(orientation = "h", y = -0.2), paper_bgcolor = "white", plot_bgcolor = "#FAFAFA"))
}
di_prob_fig <- function(fits, x, alpha) {
    n <- length(fits); nc <- 2; nr <- ceiling(n / nc); tr <- list(); ann <- list(); lay <- list()
    xs <- sort(x); m <- length(xs); pp <- (seq_len(m) - 0.375) / (m + 0.25)
    if (m > 300) { k <- unique(round(seq(1, m, length.out = 300))); xs <- xs[k]; pp <- pp[k] }
    for (i in seq_len(n)) {
        fi <- fits[[i]]; th <- di_Q(fi, pp, x); ok <- is.finite(th)
        ax <- if (i == 1) "" else as.character(i)
        r <- (i - 1) %/% nc; cc <- (i - 1) %% nc
        dx <- c(cc / nc + 0.04, (cc + 1) / nc - 0.04); dy <- c(1 - (r + 1) / nr + 0.07, 1 - r / nr - 0.07)
        lay[[paste0("xaxis", ax)]] <- list(domain = dx, anchor = paste0("y", ax), title = list(text = "Fitted quantile", font = list(size = 10)))
        lay[[paste0("yaxis", ax)]] <- list(domain = dy, anchor = paste0("x", ax), title = list(text = "Observed", font = list(size = 10)))
        tr[[length(tr) + 1]] <- list(type = "scatter", mode = "markers", x = th[ok], y = xs[ok], xaxis = paste0("x", ax), yaxis = paste0("y", ax),
                                     marker = list(color = "#2980B9", size = 4), showlegend = FALSE, name = fi$label)
        rg <- range(c(th[ok], xs[ok]))
        tr[[length(tr) + 1]] <- list(type = "scatter", mode = "lines", x = rg, y = rg, xaxis = paste0("x", ax), yaxis = paste0("y", ax),
                                     line = list(color = "#E74C3C", width = 1.5), showlegend = FALSE, hoverinfo = "skip")
        col <- if (is.finite(fi$ad_p %||% NA) && fi$ad_p >= alpha) "#1E8449" else "#C0392B"
        ann[[length(ann) + 1]] <- list(text = sprintf("<b>%s</b>  <span style='color:%s'>AD p = %s</span>", fi$label, col, di_fmtp(fi$ad_p)),
                                       x = mean(dx), y = dy[2] + 0.01, xref = "paper", yref = "paper", xanchor = "center", yanchor = "bottom", showarrow = FALSE)
    }
    lay$annotations <- ann; lay$paper_bgcolor <- "white"; lay$plot_bgcolor <- "#FAFAFA"; lay$margin <- list(t = 60)
    lay$title <- list(text = "Probability Plots (points on the line = good fit)")
    list(data = tr, layout = lay)
}
di_board_fig <- function(fits, alpha) {
    lab <- rev(vapply(fits, `[[`, "", "label")); p <- rev(vapply(fits, function(f) if (is.finite(f$ad_p %||% NA)) f$ad_p else 0, numeric(1)))
    w <- rev(vapply(fits, function(f) f$akaike_w %||% 0, numeric(1)))
    list(data = list(
        list(type = "bar", orientation = "h", y = lab, x = p, name = "AD p-value", marker = list(color = ifelse(p >= alpha, "#27AE60", "#E74C3C"))),
        list(type = "bar", orientation = "h", y = lab, x = w, name = "Akaike weight", xaxis = "x2", marker = list(color = "#5D6D7E"))),
        layout = list(title = list(text = "Fit Leaderboard"), barmode = "group",
                      xaxis = list(title = "Anderson-Darling p-value", domain = c(0, 0.48)),
                      xaxis2 = list(title = "Akaike weight (probability of being the best model)", domain = c(0.52, 1), range = c(0, 1)),
                      yaxis = list(automargin = TRUE, title = list(text = "Distribution")), shapes = list(list(type = "line", x0 = alpha, x1 = alpha, y0 = 0, y1 = 1, yref = "paper", line = list(color = "#C0392B", dash = "dash"))),
                      legend = list(orientation = "h", y = -0.15), paper_bgcolor = "white", plot_bgcolor = "#FAFAFA"))
}
di_tbl_html <- function(df) {
    if (is.null(df) || !nrow(df)) return("")
    h <- paste0("<tr>", paste0("<th>", htmltools_escape(names(df)), "</th>", collapse = ""), "</tr>")
    b <- paste0(apply(df, 1, function(r) paste0("<tr>", paste0("<td>", htmltools_escape(r), "</td>", collapse = ""), "</tr>")), collapse = "")
    paste0("<table>", h, b, "</table>")
}
htmltools_escape <- function(s) { s <- gsub("&", "&amp;", as.character(s), fixed = TRUE); s <- gsub("<", "&lt;", s, fixed = TRUE); gsub(">", "&gt;", s, fixed = TRUE) }

di_outdir <- function() {
    home <- Sys.getenv(if (.Platform$OS.type == "windows") "USERPROFILE" else "HOME")
    for (d in c(file.path(home, "Documents", "SPSS_Distribution_Reports"), file.path(tempdir(), "SPSS_Distribution_Reports"))) {
        if (dir.exists(d) || tryCatch(dir.create(d, recursive = TRUE, showWarnings = FALSE), error = function(e) FALSE)) return(d)
    }
    tempdir()
}
di_open <- function(f) tryCatch({
    if (.Platform$OS.type == "windows")
        system2(file.path(Sys.getenv("SystemRoot", "C:\\Windows"), "explorer.exe"), shQuote(normalizePath(f, winslash = "\\"), type = "cmd"),
                wait = FALSE, stdout = FALSE, stderr = FALSE, invisible = TRUE)
    else if (Sys.info()[["sysname"]] == "Darwin") system2("/usr/bin/open", shQuote(f), wait = FALSE, stdout = FALSE, stderr = FALSE)
    else system2("xdg-open", shQuote(f), wait = FALSE, stdout = FALSE, stderr = FALSE)
}, error = function(e) NULL)

# Fit, rank and check one set of values (also used for the outlier sensitivity refit)
di_fit_unit <- function(x, alpha, include3p, dsel, criterion, bounds, xord = x) {
    bt <- bounds
    if (!is.null(bt)) {
        out <- (if (!is.null(bt$L)) sum(x <= bt$L) else 0) + (if (!is.null(bt$U)) sum(x >= bt$U) else 0)
        if (out > 0) return(list(err = sprintf("%d value(s) lie at or beyond the physical bound, so the bound was ignored. A bound must lie outside the data.", out), retry = TRUE))
    }
    fits <- tryCatch(da_fit_all(x, alpha = alpha, include_3p = isTRUE(include3p) && is.null(bt)), error = function(e) NULL)
    if (!is.null(bt) && length(fits)) {
        n <- length(x); fy <- tryCatch(da_fit_all(di_y(list(bt = bt), x), alpha = alpha, include_3p = FALSE), error = function(e) NULL)
        fits <- Filter(function(f) f$name %in% DI_UNBOUNDED, fits)
        for (i in seq_along(fits)) {
            pb <- (if (!is.null(bt$L)) di_P(fits[[i]], bt$L) else 0) + (if (!is.null(bt$U)) 1 - di_P(fits[[i]], bt$U) else 0)
            fits[[i]]$beyond <- pb; fits[[i]]$excluded <- is.finite(pb) && pb > 0.00135
        }
        for (f in Filter(function(f) f$name %in% c(DI_POSITIVE, if (bt$type == "both") "Beta"), fy %||% list())) {
            f$bt <- bt; f$label <- sprintf("%s [%s]", f$label, di_bound_text(bt)); f$beyond <- 0
            if (bt$type == "both") { adj <- n * log(bt$U - bt$L); f$loglik <- f$loglik - adj; f$aic <- f$aic + 2 * adj; f$bic <- f$bic + 2 * adj }
            fits[[length(fits) + 1]] <- f
        }
        # composite score over the merged set (same rule as the shared library)
        rk <- rank(vapply(fits, function(f) f$aic, numeric(1)), ties.method = "first"); m <- length(fits)
        for (i in seq_len(m)) fits[[i]]$score <- 0.6 * (if (is.finite(fits[[i]]$ad_p %||% NA)) fits[[i]]$ad_p else 0) + 0.4 * (1 - (rk[i] - 1) / (m - 1 + 1e-9))
    }
    if (!"ALL" %in% dsel && length(fits)) {
        want <- c(unname(PC_DIST_MAP[intersect(dsel, names(PC_DIST_MAP))]), if ("CAUCHY" %in% dsel) "Cauchy")
        fits <- Filter(function(f) f$name %in% want, fits)
    }
    if (!length(fits)) return(list(err = "none of the requested distributions could be fitted."))
    fits <- di_rank(fits, criterion)
    aics <- vapply(fits, function(f) f$aic, numeric(1)); d_aic <- aics - min(aics)
    w <- exp(-d_aic / 2); w <- w / sum(w)
    for (i in seq_along(fits)) { fits[[i]]$d_aic <- d_aic[i]; fits[[i]]$akaike_w <- w[i]; fits[[i]]$lrt <- di_lrt(fits[[i]], fits) }
    chk <- di_checks(x, alpha, xord)
    tr  <- di_transforms(x, fits, alpha)
    rec <- di_recommend(fits, alpha, criterion, tr, chk, length(x))
    list(fits = fits, chk = chk, tr = tr, rec = rec)
}
di_rec_label <- function(rec) if (!is.null(rec$fit)) rec$fit$label else rec$key %||% "None"
di_rec_cdf <- function(rec, tr, x, var) {
    if (!is.null(rec$fit)) return(di_cdf_expr(rec$fit, var))
    if (identical(rec$key, "BOXCOX") && !is.null(tr$boxcox)) return(di_bc_expr(var, x, tr$boxcox$lam))
    if (!is.null(rec$key) && grepl("^JOHNSON", rec$key) && !is.null(tr$johnson)) return(di_cdf_expr(tr$johnson$f, var))
    NULL
}

# ══════════════════════════════════════════════════════════════════════════════
# main
# ══════════════════════════════════════════════════════════════════════════════
distident <- function(variables = NULL, groupvar = NULL,
                      distributions = list("ALL"), include3p = TRUE, alpha = 0.05, criterion = "COMPOSITE",
                      lowerbound = NULL, upperbound = NULL,
                      lsl = NULL, usl = NULL, sigma = 6,
                      rankings = TRUE, params = TRUE, conf = 0.95, percentiles = TRUE,
                      pctlist = list(0.135, 1, 5, 10, 50, 90, 95, 99, 99.865),
                      capcompare = TRUE, transforms = TRUE, checks = TRUE, recommend = TRUE,
                      tolint = TRUE, coverage = 0.99, ticonf = 0.95, stability = TRUE, nboot = 100,
                      pooltest = TRUE, ctrllimits = TRUE, outliersens = TRUE,
                      saveprob = FALSE, savenscore = FALSE,
                      probplots = TRUE, histogram = TRUE, html = TRUE, openreport = TRUE, ntop = 4) {
    setuplocalization("STATS_DISTRIBUTION_IDENTIFICATION")
    procname <- "Distribution Identification"
    warns <- Warn(procname = procname, omsid = "STATSDISTID")
    # out-of-range options fall back to their defaults, with a warning
    opt <- function(v, nm, ok, dflt) { v <- suppressWarnings(as.numeric(v))
        if (length(v) != 1 || !is.finite(v) || !ok(v)) { warns$warn(sprintf("%s = %s is out of range; %s is used.", nm, paste(v, collapse = " "), format(dflt))); dflt } else v }
    alpha    <- opt(alpha, "ALPHA", function(v) v > 0 && v < 0.5, 0.05)
    conf     <- opt(conf, "CONFIDENCE", function(v) v > 0.5 && v < 1, 0.95)
    sigma    <- opt(sigma, "SIGMA", function(v) v > 0, 6)
    coverage <- opt(coverage, "COVERAGE", function(v) v > 0.5 && v < 1, 0.99)
    ticonf   <- opt(ticonf, "TICONF", function(v) v > 0.5 && v < 1, 0.95)
    nboot    <- as.integer(opt(nboot, "NBOOT", function(v) v >= 20 && v <= 2000, 100))
    ntop     <- as.integer(opt(ntop, "TOP", function(v) v >= 1 && v <= 8, 4))
    criterion <- toupper(as.character(criterion)); if (!criterion %in% c("COMPOSITE", "AD", "AIC", "BIC")) criterion <- "COMPOSITE"
    num1 <- function(v) if (!is.null(v) && length(v) && is.finite(as.numeric(v))) as.numeric(v) else NULL
    lsl <- num1(lsl); usl <- num1(usl); lb <- num1(lowerbound); ub <- num1(upperbound)
    if (!is.null(lsl) && !is.null(usl) && lsl >= usl) warns$warn("LSL must be less than USL.", dostop = TRUE)
    if (!is.null(lb) && !is.null(ub) && lb >= ub) warns$warn("LOWERBOUND must be less than UPPERBOUND.", dostop = TRUE)
    bounds <- if (!is.null(lb) && !is.null(ub)) list(type = "both", L = lb, U = ub) else if (!is.null(lb)) list(type = "lower", L = lb) else if (!is.null(ub)) list(type = "upper", U = ub) else NULL
    dsel <- toupper(unlist(distributions)); dsel <- sub("^DIST_", "", dsel)
    known <- c(names(PC_DIST_MAP), "CAUCHY")
    if (!"ALL" %in% dsel) {
        bad <- setdiff(dsel, known)
        if (length(bad)) warns$warn(sprintf("Unknown distribution(s): %s. Valid: ALL, %s.", paste(bad, collapse = ", "), paste(known, collapse = ", ")), dostop = TRUE)
    }
    pct <- suppressWarnings(as.numeric(unlist(pctlist))); pct <- sort(unique(pct[is.finite(pct) & pct > 0 & pct < 100]))
    vars <- unlist(variables); if (!length(vars)) warns$warn("No variables were specified.", dostop = TRUE)

    # ── Load packages (same guard as STATS PROCESS CAPABILITY: CRAN, then the
    # organisation mirror, then a clear message). Both are pure-R packages.
    .spss_ensure_packages(c("nortest", "jsonlite"))

    tryCatch(spsspkg.Submit("EXECUTE."), error = function(e) NULL)
    # Split File and Weight are honoured: split groups are analysed separately,
    # a weight variable is used as a frequency weight (values rounded)
    splitv <- tryCatch(unlist(spssdata.GetSplitVariableNames()), error = function(e) NULL)
    wtv <- tryCatch({ w <- spssdictionary.GetWeightVariable(); if (is.null(w) || !nzchar(w)) NULL else w }, error = function(e) NULL)
    gname <- if (!is.null(groupvar) && length(unlist(groupvar))) unlist(groupvar)[1] else NULL
    byv <- unique(c(splitv, gname))
    allv <- unique(c(vars, byv, wtv))
    dta <- spssdata.GetDataFromSPSS(allv, missingValueToNA = TRUE, factorMode = "none")
    blab <- if (length(byv)) tryCatch(spssdata.GetDataFromSPSS(byv, missingValueToNA = TRUE, factorMode = "labels"), error = function(e) dta[byv]) else NULL
    wt <- if (!is.null(wtv)) suppressWarnings(as.numeric(dta[[wtv]])) else NULL

    StartProcedure(procname, "STATSDISTID")
    reports <- list(); summary_rows <- list(); saves <- list(); pool_rows <- list()
    on.exit(tryCatch(warns$display(inproc = TRUE), error = function(e) NULL), add = FALSE)
    if (!is.null(wtv)) warns$warn(sprintf("Cases are weighted by %s: weights are used as frequency weights (rounded to whole numbers) for the fits; the stability check and the control limits use the unweighted values in data order.", wtv))

    # analysis units: variable × split group × group
    combos <- if (length(byv)) unique(dta[byv]) else data.frame(.all = 1)
    if (length(byv)) {
        combos <- combos[stats::complete.cases(combos), , drop = FALSE]
        combos <- combos[do.call(order, unname(as.list(combos))), , drop = FALSE]
        if (nrow(combos) > 50) { warns$warn(sprintf("%d split/group combinations; only the first 50 are analysed.", nrow(combos))); combos <- combos[1:50, , drop = FALSE] }
    }
    units <- list()
    for (v in vars) {
        x <- suppressWarnings(as.numeric(dta[[v]]))
        for (ci in seq_len(nrow(combos))) {
            sel <- rep(TRUE, length(x)); cond <- list(); lbl <- character(0)
            for (bv in byv) {
                cv <- combos[[bv]][ci]; sel <- sel & !is.na(dta[[bv]]) & dta[[bv]] == cv; cond[[bv]] <- cv
                lv <- as.character(blab[[bv]][which(!is.na(dta[[bv]]) & dta[[bv]] == cv)[1]])
                lbl <- c(lbl, sprintf("%s = %s", bv, if (length(lv) && !is.na(lv)) lv else as.character(cv)))
            }
            xx <- x[sel]; ww <- if (!is.null(wt)) wt[sel] else NULL; xord <- xx[is.finite(xx)]
            if (!is.null(ww)) { ok <- is.finite(xx) & is.finite(ww) & ww > 0; xord <- xx[ok]; xx <- rep(xx[ok], pmax(round(ww[ok]), 0)) }
            units[[length(units) + 1]] <- list(var = v, lab = if (length(lbl)) sprintf("%s [%s]", v, paste(lbl, collapse = ", ")) else v,
                                               x = xx, xord = xord, cond = cond, glab = if (!is.null(gname)) as.character(combos[[gname]][ci]) else NULL,
                                               skey = if (length(splitv)) paste(vapply(splitv, function(s) as.character(combos[[s]][ci]), ""), collapse = "|") else "")
        }
    }

    for (u in units) {
        x <- u$x[is.finite(u$x)]; lab <- u$lab; xord <- u$xord
        if (length(x) < 8) { warns$warn(sprintf("%s: fewer than 8 valid values; skipped.", lab)); next }
        if (sd(x) <= 0) { warns$warn(sprintf("%s: all values are identical; no distribution can be fitted.", lab)); next }
        fu <- di_fit_unit(x, alpha, include3p, dsel, criterion, bounds, xord)
        ubounds <- bounds
        if (isTRUE(fu$retry)) { warns$warn(sprintf("%s: %s", lab, fu$err)); ubounds <- NULL; fu <- di_fit_unit(x, alpha, include3p, dsel, criterion, NULL, xord) }
        if (!is.null(fu$err)) { warns$warn(sprintf("%s: %s", lab, fu$err)); next }
        fits <- fu$fits; chk <- fu$chk; tr <- fu$tr; rec <- fu$rec
        # spec limits at or beyond a physical bound have no lower/upper capability
        ulsl <- lsl; uusl <- usl; capnote <- ""
        if (!is.null(ubounds$L) && !is.null(lsl) && lsl <= ubounds$L) { ulsl <- NULL; capnote <- "LSL is at or below the physical lower bound, so only the upper side is assessed. " }
        if (!is.null(ubounds$U) && !is.null(usl) && usl >= ubounds$U) { uusl <- NULL; capnote <- paste0(capnote, "USL is at or above the physical upper bound, so only the lower side is assessed. ") }

        # 1. Recommendation (first: the answer to "which distribution?")
        syn <- di_pca_syntax(u$var, rec$key, ulsl, uusl, if (identical(rec$key, "BOXCOX")) tr$boxcox$lam else NULL, rec$fit)
        if (isTRUE(recommend)) {
            spsspivottable.Display(data.frame(
                Item  = c("Recommended", "Reason", "Alternatives", "Cautions", "Process Capability syntax"),
                Value = c(di_rec_label(rec), rec$why, if (length(rec$alts)) paste(rec$alts, collapse = "; ") else "—",
                          if (length(rec$cautions)) paste0("Check the Data Checks table: ", paste(rec$cautions, collapse = "; "), ".") else "—", syn),
                stringsAsFactors = FALSE),
                title = sprintf("Recommendation — %s", lab), templateName = "DISTIDREC", outline = "Recommendation",
                caption = paste0("Copy the syntax to run the capability analysis with this distribution. Choose a distribution that fits AND makes engineering sense for how the data are generated.",
                                 if (!is.null(rec$fit$bt)) " The distribution was fitted against the physical bound, so Process Capability runs on the shifted variable created by the COMPUTE; Ppk is unchanged by the shift." else ""))
        }
        # 2. Data checks
        if (isTRUE(checks))
            spsspivottable.Display(chk$table, title = sprintf("Data Checks — %s", lab), templateName = "DISTIDCHK",
                                   caption = "Conditions that make every distribution fit poorly or mislead.")
        # 3. Goodness of fit
        if (isTRUE(rankings)) {
            gt <- data.frame(Rank = seq_along(fits), Distribution = vapply(fits, `[[`, "", "label"),
                Parameters = vapply(fits, function(f) as.character(f$nparams), ""),
                `AD` = vapply(fits, function(f) di_fmt(f$ad, 4), ""), `AD p` = vapply(fits, function(f) di_fmtp(f$ad_p), ""),
                `Fit` = vapply(fits, function(f) if (!is.finite(f$ad_p %||% NA)) "—" else if (f$ad_p >= alpha) "Pass" else "Fail", ""),
                `LRT p (3P vs 2P)` = vapply(fits, function(f) di_fmtp(f$lrt), ""),
                AIC = vapply(fits, function(f) di_fmt(f$aic, 2), ""), `ΔAIC` = vapply(fits, function(f) di_fmt(f$d_aic, 2), ""),
                `Akaike weight` = vapply(fits, function(f) di_fmt(f$akaike_w, 3), ""),
                BIC = vapply(fits, function(f) di_fmt(f$bic, 2), ""),
                `In Process Capability` = vapply(fits, function(f) { k <- DI_PCA_KEY[f$name]; if (is.na(k)) "—" else if (!is.null(f$bt)) paste(k, "(on the shifted variable)") else k }, ""),
                stringsAsFactors = FALSE, check.names = FALSE)
            if (!is.null(ubounds)) gt$`Beyond bound %` <- vapply(fits, function(f) if (is.finite(f$beyond %||% NA)) sprintf("%.4f", 100 * f$beyond) else "—", "")
            spsspivottable.Display(gt, title = sprintf("Goodness of Fit — %s", lab), templateName = "DISTIDGOF", outline = "Goodness of Fit",
                caption = paste0(sprintf("Ranked by %s. n = %d, alpha = %.2f. ", switch(criterion, AD = "AD p-value", AIC = "AIC", BIC = "BIC", "composite score (60%% AD p-value + 40%% AIC rank)"), length(x), alpha),
                                 "AD p: D'Agostino-Stephens approximation. LRT p < alpha: the threshold of the 3-parameter model is needed. ",
                                 "Akaike weight: relative likelihood that the model is the best of those fitted.",
                                 if (!is.null(ubounds)) sprintf(" Physical %s: positive distributions are fitted to the distance from the bound; distributions that put more than 0.135%% beyond the bound are not recommended.", di_bound_text(ubounds)) else ""))
        }
        # 4. Parameter estimates
        if (isTRUE(params)) {
            pt <- do.call(rbind, lapply(fits, function(f) tryCatch(di_param_table(f, x, conf), error = function(e) NULL)))
            if (!is.null(pt) && nrow(pt))
                spsspivottable.Display(pt, title = sprintf("Parameter Estimates — %s", lab), templateName = "DISTIDPAR",
                    caption = paste0(sprintf("Maximum likelihood. %.0f%% Wald intervals from the observed information (log scale for positive parameters). 3-parameter thresholds come from profile likelihood; their intervals are approximate.", conf * 100),
                                     if (!is.null(ubounds)) " Bounded fits: parameters refer to the distance from the bound." else ""))
        }
        # 5. Percentiles
        if (isTRUE(percentiles) && length(pct)) {
            topf <- fits[seq_len(min(length(fits), max(ntop, 1)))]
            pm <- sapply(topf, function(f) vapply(di_Q(f, pct / 100, x), di_fmt, "", d = 5))
            if (is.null(dim(pm))) pm <- matrix(pm, nrow = length(pct))
            colnames(pm) <- vapply(topf, `[[`, "", "label")
            pdf <- data.frame(Percent = formatC(pct, format = "fg", digits = 5), Empirical = sapply(quantile(x, pct / 100, type = 6), di_fmt, d = 5),
                              pm, stringsAsFactors = FALSE, check.names = FALSE)
            spsspivottable.Display(pdf, title = sprintf("Percentiles — %s", lab), templateName = "DISTIDPCT",
                caption = "The value below which the given percentage of the process output falls, for the top-ranked distributions and from the data (type 6). 0.135% and 99.865% are the ±3σ-equivalent process limits used for Pp/Ppk; agreement with the empirical column in the tails shows how far the fitted tails can be trusted.")
        }
        # 6. Capability by distribution
        caps <- NULL
        if (isTRUE(capcompare) && (!is.null(ulsl) || !is.null(uusl))) {
            obs <- (sum(if (!is.null(lsl)) x < lsl else FALSE) + sum(if (!is.null(usl)) x > usl else FALSE)) / length(x) * 1e6
            caps <- lapply(fits, function(f) tryCatch(di_cap(f, x, ulsl, uusl, sigma), error = function(e) NULL))
            keep <- !vapply(caps, is.null, TRUE)
            if (any(keep)) {
                ct <- data.frame(Distribution = vapply(fits[keep], `[[`, "", "label"),
                    Fit = vapply(fits[keep], function(f) if (!is.finite(f$ad_p %||% NA)) "—" else if (f$ad_p >= alpha) "Pass" else "Fail", ""),
                    Pp = vapply(caps[keep], function(c) di_fmt(c$pp), ""), PPL = vapply(caps[keep], function(c) di_fmt(c$ppl), ""),
                    PPU = vapply(caps[keep], function(c) di_fmt(c$ppu), ""), Ppk = vapply(caps[keep], function(c) di_fmt(c$ppk), ""),
                    `Expected PPM` = vapply(caps[keep], function(c) di_fmt(c$et, 1), ""), `Z-Bench` = vapply(caps[keep], function(c) di_fmt(c$z, 3), ""),
                    stringsAsFactors = FALSE, check.names = FALSE)
                pk <- vapply(seq_along(fits)[keep], function(i) if (is.finite(fits[[i]]$ad_p %||% NA) && fits[[i]]$ad_p >= alpha) caps[[i]]$ppk else NA_real_, 0)
                sens <- if (sum(is.finite(pk)) >= 2) sprintf("Across the %d distributions that fit, Ppk ranges from %.3f to %.3f: %s", sum(is.finite(pk)), min(pk, na.rm = TRUE), max(pk, na.rm = TRUE),
                                                            if (diff(range(pk, na.rm = TRUE)) > 0.1 * max(abs(pk), na.rm = TRUE)) "the choice of distribution matters for these data." else "the conclusion does not depend much on the choice.") else ""
                spsspivottable.Display(ct, title = sprintf("Capability by Distribution — %s", lab), templateName = "DISTIDCAP",
                    caption = paste0(sprintf("Overall capability for each fitted distribution, ISO 22514-4 percentile method, %g-sigma. Observed PPM = %.1f. ", sigma, obs), capnote, sens))
            }
        }
        # 7. Transformations
        if (isTRUE(transforms))
            spsspivottable.Display(tr$table, title = sprintf("Normalizing Transformations — %s", lab), templateName = "DISTIDTRN",
                caption = "Anderson-Darling normality test after each transformation. The last column is the matching option in STATS PROCESS CAPABILITY.")

        # 8. Tolerance intervals
        ti <- NULL
        if (isTRUE(tolint)) {
            ti <- tryCatch(di_tolint(x, rec, tr, fits, coverage, ticonf, alpha), error = function(e) { warns$warn(sprintf("%s: tolerance intervals failed: %s", lab, conditionMessage(e))); NULL })
            if (!is.null(ti))
                spsspivottable.Display(ti, title = sprintf("Tolerance Intervals — %s", lab), templateName = "DISTIDTOL", outline = "Tolerance Intervals",
                    caption = sprintf("Two-sided intervals expected to contain at least %g%% of the process output, with %g%% confidence. Use the row for the distribution that fits; the distribution-free interval needs no distribution but more data.", 100 * coverage, 100 * ticonf))
        }
        # 9. Recommendation stability
        st <- NULL
        if (isTRUE(stability)) {
            st <- tryCatch(di_stability(fits, x, criterion, alpha, nboot, rec, chk), error = function(e) { warns$warn(sprintf("%s: stability check failed: %s", lab, conditionMessage(e))); NULL })
            if (!is.null(st))
                spsspivottable.Display(st$table, title = sprintf("Recommendation Stability — %s", lab), templateName = "DISTIDSTB", outline = "Recommendation Stability",
                    caption = sprintf("%s Based on %d bootstrap resamples of the data, refitting the leading distributions (thresholds and bounds held fixed) and choosing with %s. A stable choice is one the data clearly support; an uncertain one means the data cannot separate the leading distributions.", st$verdict, st$B, st$crit))
        }
        # 10. Control limits for individual values
        ctl <- NULL
        if (isTRUE(ctrllimits)) {
            ctl <- tryCatch(di_ctrl(xord, rec, tr), error = function(e) NULL)
            if (!is.null(ctl)) {
                spsspivottable.Display(ctl$table, title = sprintf("Control Limits for Individual Values — %s", lab), templateName = "DISTIDCTL", outline = "Control Limits",
                    caption = paste0("Normal-theory I-chart limits assume normal data. With skewed data they give too many or too few signals; the distribution-based limits keep the intended 0.27% false-alarm rate. ",
                                     if (is.finite(ctl$fa_normal)) sprintf("Under the fitted distribution the normal-theory limits would signal %.2f%% of in-control points. ", 100 * ctl$fa_normal) else "",
                                     "Limits are estimated from the data in their current order."))
                tryCatch(pc_draw({ di_ichart(xord, ctl, lab); invisible(NULL) }, width = 720, height = 360),
                                           error = function(e) warns$warn(sprintf("Control chart error (%s): %s", lab, conditionMessage(e))))
            }
        }
        # 11. Outlier sensitivity
        osens <- NULL
        if (isTRUE(outliersens)) {
            oi <- tryCatch(gesd_outliers(x, alpha), error = function(e) integer(0))
            if (length(oi) && length(x) - length(oi) >= 8) {
                xo <- x[-oi]; fo <- di_fit_unit(xo, alpha, include3p, dsel, criterion, ubounds, if (is.null(wtv)) xo else xord)
                if (is.null(fo$err)) {
                    row <- function(lb, xx, r) {
                        cp <- if (!is.null(r$fit) && (!is.null(ulsl) || !is.null(uusl))) tryCatch(di_cap(r$fit, xx, ulsl, uusl, sigma), error = function(e) NULL) else NULL
                        q <- if (!is.null(r$fit)) di_Q(r$fit, c(0.00135, 0.99865), xx) else c(NA, NA)
                        data.frame(Data = lb, N = length(xx), Recommended = di_rec_label(r), `AD p` = if (!is.null(r$fit)) di_fmtp(r$fit$ad_p) else "—",
                                   `0.135% point` = di_fmt(q[1], 5), `99.865% point` = di_fmt(q[2], 5), Ppk = if (!is.null(cp)) di_fmt(cp$ppk) else "—",
                                   stringsAsFactors = FALSE, check.names = FALSE)
                    }
                    osens <- rbind(row("All values", x, rec), row(sprintf("Without %d outlier(s)", length(oi)), xo, fo$rec))
                    same <- identical(di_rec_label(rec), di_rec_label(fo$rec))
                    spsspivottable.Display(osens, title = sprintf("Outlier Sensitivity — %s", lab), templateName = "DISTIDOUT", outline = "Outlier Sensitivity",
                        caption = paste0(sprintf("Outliers (generalized ESD, alpha = %.2f): %s. The ESD test assumes normal data, so with skewed data it can flag ordinary tail values; this table shows how much the conclusions depend on them. ", alpha, paste(format(sort(x[oi]), digits = 6), collapse = ", ")),
                                         if (same) "The recommendation does not change without them; compare the tail points and Ppk to see how much they matter."
                                         else "The recommendation changes without them: verify the outliers (measurement error or a real but rare event?) before choosing the distribution."))
                }
            }
        }

        # Viewer charts
        topn <- fits[seq_len(min(length(fits), ntop))]
        if (isTRUE(probplots)) tryCatch(pc_draw({
            nr <- ceiling(length(topn) / 2); op <- par(mfrow = c(nr, min(2, length(topn))), mar = c(4, 4, 3, 1), oma = c(0, 0, 2.5, 0)); on.exit(par(op), add = TRUE)
            for (f in topn) di_probplot_panel(f, x, alpha)
            mtext(sprintf("Probability Plots — %s", lab), outer = TRUE, cex = 1.05, font = 2)
            invisible(NULL) }, width = 640, height = 320 * ceiling(length(topn) / 2)),
            error = function(e) warns$warn(sprintf("Probability plot error (%s): %s", lab, conditionMessage(e))))
        if (isTRUE(histogram)) tryCatch(pc_draw({
            h <- pc_hist(x, lsl, usl); xs <- seq(min(h$breaks, lsl, usl), max(h$breaks, lsl, usl), length.out = 400)
            ys <- lapply(topn[seq_len(min(3, length(topn)))], function(f) { y <- di_D(f, xs); y[!is.finite(y)] <- 0; y })
            plot(h, freq = FALSE, col = "#AED6F1", border = "white", main = sprintf("Histogram with Fitted Distributions — %s", lab),
                 xlab = "Value", ylim = c(0, max(c(h$density, unlist(ys))) * 1.1), xlim = range(xs), cex.main = 0.95)
            pal <- c("#2C3E50", "#E67E22", "#8E44AD")
            for (i in seq_along(ys)) lines(xs, ys[[i]], col = pal[i], lwd = 2.2)
            if (!is.null(lsl)) abline(v = lsl, col = "#E74C3C", lty = 2, lwd = 2); if (!is.null(usl)) abline(v = usl, col = "#E74C3C", lty = 2, lwd = 2)
            legend("topright", vapply(topn[seq_along(ys)], `[[`, "", "label"), col = pal[seq_along(ys)], lwd = 2, bty = "n", cex = 0.8)
            invisible(NULL) }), error = function(e) warns$warn(sprintf("Histogram error (%s): %s", lab, conditionMessage(e))))

        summary_rows[[length(summary_rows) + 1]] <- data.frame(Data = lab, N = length(x),
            Recommended = di_rec_label(rec),
            `AD p` = if (!is.null(rec$fit)) di_fmtp(rec$fit$ad_p) else "—",
            `Normal AD p` = { nf <- Filter(function(f) f$name == "Normal", fits)
                              di_fmtp(if (length(nf)) nf[[1]]$ad_p else da_ad_pval(da_ad_stat(pnorm(sort(x), mean(x), sd(x))), length(x))) },
            Stability = if (!is.null(st)) sub(":.*", "", st$verdict) else "—",
            Checks = if (any(chk$table$Status == "Check")) paste(chk$table$Check[chk$table$Status == "Check"], collapse = "; ") else "OK",
            stringsAsFactors = FALSE, check.names = FALSE)
        if (isTRUE(saveprob) || isTRUE(savenscore)) {
            e <- tryCatch(di_rec_cdf(rec, tr, x, u$var), error = function(e) NULL)
            if (is.null(e)) warns$warn(sprintf("%s: no fitted distribution, so nothing is saved for these cases.", lab))
            else saves[[length(saves) + 1]] <- list(var = u$var, cond = u$cond, expr = e, dist = di_rec_label(rec))
        }
        reports[[length(reports) + 1]] <- list(lab = lab, var = u$var, x = x, xord = xord, fits = fits, chk = chk, tr = tr, rec = rec, caps = caps, syn = syn,
                                               ti = ti, st = st, ctl = ctl, osens = osens, glab = u$glab, skey = u$skey)
    }

    # 12. Can the groups be pooled? (k-sample Anderson-Darling, per variable and split group)
    if (isTRUE(pooltest) && !is.null(gname) && length(reports)) {
        keys <- unique(vapply(reports, function(r) paste(r$var, r$skey, sep = "\r"), ""))
        for (k in keys) {
            rs <- Filter(function(r) identical(paste(r$var, r$skey, sep = "\r"), k), reports)
            if (length(rs) < 2) next
            pt <- tryCatch(di_pooltest(unlist(lapply(rs, `[[`, "x")), rep(vapply(rs, function(r) r$glab %||% "", ""), vapply(rs, function(r) length(r$x), 1L))), error = function(e) NULL)
            if (is.null(pt)) next
            recs <- unique(vapply(rs, function(r) di_rec_label(r$rec), ""))
            pool_rows[[length(pool_rows) + 1]] <- data.frame(Variable = rs[[1]]$var, Split = if (nzchar(rs[[1]]$skey)) rs[[1]]$skey else "—", Groups = pt$k, N = pt$N,
                `A² (k-sample AD)` = di_fmt(pt$A, 3), `p (permutation)` = di_fmtp(pt$p),
                `Recommended per group` = paste(recs, collapse = "; "),
                Conclusion = if (pt$p >= alpha) "No evidence the groups differ: they may be pooled (if each is stable)." else "The groups differ: analyse them separately.",
                stringsAsFactors = FALSE, check.names = FALSE)
        }
        if (length(pool_rows)) {
            pdf_ <- do.call(rbind, pool_rows); if (all(pdf_$Split == "—")) pdf_$Split <- NULL
            spsspivottable.Display(pdf_, title = sprintf("Can the Groups Be Pooled? (%s)", gname), templateName = "DISTIDPOOL", outline = "Pooling Test",
                caption = sprintf("k-sample Anderson-Darling test (Scholz and Stephens) that all groups come from the same distribution, with a 999-permutation p-value; alpha = %.2f. It compares the whole distribution (centre, spread and shape), not just the means.", alpha))
        }
    }

    if (length(summary_rows) > 1)
        spsspivottable.Display(do.call(rbind, summary_rows), title = "Summary — Recommended Distributions", templateName = "DISTIDSUM",
                               caption = "One row per variable (and group). Different recommendations across groups suggest the groups behave as different processes.")

    # HTML report
    # plotly is only needed for an offline copy of plotly.js; without it the report
    # loads plotly.js from the internet, so a failed install is not fatal
    if (isTRUE(html) && length(reports) && !requireNamespace("plotly", quietly = TRUE))
        tryCatch(.spss_ensure_packages(c("plotly")), error = function(e)
            warns$warn("The plotly R package could not be installed, so the HTML report loads its chart library from the internet (cdn.plot.ly). On a computer without internet access the report charts stay blank."))
    if (isTRUE(html) && length(reports) && requireNamespace("jsonlite", quietly = TRUE)) {
        f <- tryCatch({
            body <- paste(vapply(seq_along(reports), function(i) {
                r <- reports[[i]]; topn <- r$fits[seq_len(min(length(r$fits), ntop))]
                gof <- data.frame(Rank = seq_along(r$fits), Distribution = vapply(r$fits, `[[`, "", "label"), `AD p` = vapply(r$fits, function(f) di_fmtp(f$ad_p), ""),
                                  AIC = vapply(r$fits, function(f) di_fmt(f$aic, 2), ""), `Akaike weight` = vapply(r$fits, function(f) di_fmt(f$akaike_w, 3), ""),
                                  check.names = FALSE, stringsAsFactors = FALSE)
                paste0('<h2>', htmltools_escape(r$lab), '</h2>',
                       '<div class="rec"><b>Recommended: ', htmltools_escape(di_rec_label(r$rec)), '</b><br>',
                       htmltools_escape(r$rec$why), if (!is.null(r$st)) paste0('<br><i>', htmltools_escape(r$st$verdict), '</i>') else "",
                       '<br><code>', htmltools_escape(r$syn), '</code></div>',
                       '<div class="card">', di_plt(di_hist_fig(r$x, topn[seq_len(min(3, length(topn)))], lsl, usl, paste("Histogram with fitted distributions —", r$lab)), paste0("h", i), 420), '</div>',
                       '<div class="card">', di_plt(di_prob_fig(topn, r$x, alpha), paste0("p", i), 300 * ceiling(length(topn) / 2)), '</div>',
                       '<div class="card">', di_plt(di_board_fig(r$fits, alpha), paste0("b", i), max(300, 120 + 34 * length(r$fits))), '</div>',
                       if (!is.null(r$ctl)) paste0('<div class="card">', di_plt(di_ctrl_fig(r$xord, r$ctl, r$lab), paste0("c", i), 380), di_tbl_html(r$ctl$table), '</div>') else "",
                       '<div class="card"><h3>Goodness of fit</h3>', di_tbl_html(gof), '<h3>Data checks</h3>', di_tbl_html(r$chk$table),
                       if (!is.null(r$ti)) paste0('<h3>Tolerance intervals (', 100 * coverage, '% coverage, ', 100 * ticonf, '% confidence)</h3>', di_tbl_html(r$ti)) else "",
                       if (!is.null(r$st)) paste0('<h3>Recommendation stability</h3>', di_tbl_html(r$st$table)) else "",
                       if (!is.null(r$osens)) paste0('<h3>Outlier sensitivity</h3>', di_tbl_html(r$osens)) else "",
                       '<h3>Normalizing transformations</h3>', di_tbl_html(r$tr$table), '</div>')
            }, ""), collapse = "\n")
            if (length(pool_rows)) body <- paste0(body, '<h2>Can the groups be pooled?</h2><div class="card">', di_tbl_html(do.call(rbind, pool_rows)), '</div>')
            page <- paste0('<!DOCTYPE html><html><head><meta charset="utf-8"><title>Distribution Identification</title>', .pc_plotly_js(),
                '<style>body{font-family:Segoe UI,Arial,sans-serif;background:#F4F6F8;color:#2C3E50;margin:0}header{background:#2C3E50;color:white;padding:14px 22px}',
                '.wrap{max-width:1200px;margin:auto;padding:12px 20px}.card{background:white;border-radius:6px;padding:12px;margin:12px 0;box-shadow:0 1px 3px rgba(0,0,0,.1)}',
                '.rec{background:#E8F6EF;border-left:5px solid #27AE60;padding:12px;border-radius:4px;margin:10px 0}code{background:#fff;padding:4px 6px;display:inline-block;margin-top:6px}',
                'table{border-collapse:collapse;width:100%;font-size:12px}th{background:#34495E;color:white;padding:5px}td{padding:4px 6px;border-bottom:1px solid #ECF0F1;text-align:center}p.tip{font-size:12px;color:#666}</style></head><body>',
                '<header><b>Distribution Identification</b> &nbsp;|&nbsp; ', format(Sys.time(), "%Y-%m-%d %H:%M"), '</header><div class="wrap">',
                '<p class="tip">Charts are interactive: zoom, hover, click a title or axis label to edit it, and download with the camera icon. The capability analysis itself is done with STATS PROCESS CAPABILITY using the recommended syntax.</p>',
                body, '<p class="tip">Generated by STATS DISTRIBUTION IDENTIFICATION ', DI_VERSION, '</p></div></body></html>')
            fn <- file.path(di_outdir(), sprintf("Distribution_Identification_%s.html", format(Sys.time(), "%Y%m%d_%H%M%S")))
            con <- file(fn, open = "wb"); writeBin(charToRaw(enc2utf8(page)), con); close(con); fn
        }, error = function(e) { warns$warn(sprintf("The HTML report could not be created: %s", conditionMessage(e))); NULL })
        if (!is.null(f)) {
            spsspivottable.Display(data.frame(Item = "HTML report", Value = f, stringsAsFactors = FALSE), title = "HTML Report",
                                   caption = if (isTRUE(openreport)) "Opened in the default browser." else "Open the file in a browser.")
            if (isTRUE(openreport)) assign(".di_open_after", f, envir = globalenv())
        }
    }
    on.exit(NULL)
    warns$display(inproc = TRUE)

    # 13. Save fitted probability / normal score to the active dataset (after the
    # procedure has ended: data cannot be changed while output is open)
    if (length(saves)) di_save(saves, saveprob, savenscore, byv)

    if (exists(".di_open_after", envir = globalenv())) { f <- get(".di_open_after", envir = globalenv()); rm(".di_open_after", envir = globalenv()); di_open(f) }
    invisible(reports)
}

di_ctrl_fig <- function(x, ct, lab) {
    n <- length(x); idx <- seq_len(n)
    hl <- function(y, col, dash, nm) list(type = "scatter", mode = "lines", x = c(1, n), y = c(y, y), name = nm, line = list(color = col, dash = dash, width = 2), hoverinfo = "name+y")
    tr <- list(list(type = "scatter", mode = "lines+markers", x = idx, y = x, name = "Data", line = list(color = "#2C3E50", width = 1), marker = list(size = 4)),
               hl(ct$nl[1], "#95A5A6", "dash", "Normal-theory LCL"), hl(ct$nl[3], "#95A5A6", "dash", "Normal-theory UCL"))
    if (!is.null(ct$dl)) tr <- c(tr, list(hl(ct$dl[1], "#C0392B", "solid", paste(ct$dname, "LCL")), hl(ct$dl[2], "#27AE60", "solid", "Median"),
                                          hl(ct$dl[3], "#C0392B", "solid", paste(ct$dname, "UCL"))))
    list(data = tr, layout = list(title = list(text = paste("Individuals chart with distribution-based limits —", lab)), xaxis = list(title = "Observation (data order)"),
                                  yaxis = list(title = "Value"), legend = list(orientation = "h", y = -0.2), paper_bgcolor = "white", plot_bgcolor = "#FAFAFA"))
}

di_save <- function(saves, saveprob, savenscore, byv) {
    lines <- character(0); labs <- character(0); made <- list()
    for (v in unique(vapply(saves, `[[`, "", "var"))) {
        ss <- Filter(function(s) identical(s$var, v), saves)
        pn <- di_newname(v, "_prob"); nn <- di_newname(v, "_nscore")
        if (isTRUE(saveprob)) lines <- c(lines, sprintf("COMPUTE %s = $SYSMIS.", pn))
        if (isTRUE(savenscore)) lines <- c(lines, sprintf("COMPUTE %s = $SYSMIS.", nn))
        for (s in ss) {
            cond <- if (length(s$cond)) paste(vapply(names(s$cond), function(b) sprintf("%s = %s", b, di_spss_str(s$cond[[b]])), ""), collapse = " AND ") else NULL
            if (!is.null(cond)) lines <- c(lines, sprintf("DO IF (%s).", cond))
            pe <- sprintf("MIN(MAX(%s, 1E-12), 1 - 1E-12)", s$expr)
            if (isTRUE(saveprob)) lines <- c(lines, di_wrap(sprintf("COMPUTE %s = %s.", pn, s$expr)))
            if (isTRUE(savenscore)) lines <- c(lines, di_wrap(sprintf("COMPUTE %s = IDF.NORMAL(%s, 0, 1).", nn, pe)))
            if (!is.null(cond)) lines <- c(lines, "END IF.")
        }
        dl <- if (length(ss) == 1) ss[[1]]$dist else "fitted per group"
        if (isTRUE(saveprob)) { labs <- c(labs, sprintf("%s '%s'", pn, substr(sprintf("Fitted cumulative probability of %s (%s)", v, dl), 1, 250))); made[[length(made) + 1]] <- c(pn, v, "Probability under the fitted distribution (0 to 1)") }
        if (isTRUE(savenscore)) { labs <- c(labs, sprintf("%s '%s'", nn, substr(sprintf("Normal score of %s (%s)", v, dl), 1, 250))); made[[length(made) + 1]] <- c(nn, v, "Normal score: standard normal if the fitted distribution is right") }
    }
    lines <- c(lines, "EXECUTE.", "VARIABLE LABELS", paste0("    ", labs, c(rep(" /", length(labs) - 1), ".")))
    ok <- tryCatch({ spsspkg.Submit(lines); TRUE }, error = function(e) conditionMessage(e))
    tryCatch({
        StartProcedure("Distribution Identification", "STATSDISTID")
        if (isTRUE(ok)) {
            spsspivottable.Display(data.frame(Variable = vapply(made, `[`, "", 1), Source = vapply(made, `[`, "", 2), Contents = vapply(made, `[`, "", 3), stringsAsFactors = FALSE),
                title = "Saved Variables", templateName = "DISTIDSAVE",
                caption = "Computed from the recommended distribution with the transformation syntax below, which you can rerun on new data. Use the normal score in normal-theory tools such as Control Charts.")
            spsspivottable.Display(data.frame(Syntax = lines, stringsAsFactors = FALSE), title = "Syntax Used to Save the Variables", templateName = "DISTIDSAVESYN")
        } else spsspivottable.Display(data.frame(Message = c("The variables could not be saved.", ok), stringsAsFactors = FALSE), title = "Saved Variables")
        spsspkg.EndProcedure()
    }, error = function(e) NULL)
}

Run <- function(args) {
    # Force C numeric locale so the JSON in the HTML report always uses "." as the
    # decimal separator (comma-decimal locales otherwise break the charts)
    old_lc_num <- tryCatch(Sys.getlocale("LC_NUMERIC"), error=function(e) "C")
    suppressWarnings(Sys.setlocale("LC_NUMERIC", "C"))
    on.exit(tryCatch(suppressWarnings(Sys.setlocale("LC_NUMERIC", old_lc_num)), error=function(e) NULL), add=TRUE)

    cmdname <- args[[1]]
    args    <- args[[2]]
    oobj <- spsspkg.Syntax(templ = list(
        spsspkg.Template("VARIABLES",     subc = "",       ktype = "existingvarlist", var = "variables", islist = TRUE),
        spsspkg.Template("GROUPVAR",      subc = "",       ktype = "existingvarlist", var = "groupvar", islist = TRUE),
        spsspkg.Template("DISTRIBUTIONS", subc = "FIT",    ktype = "str",   var = "distributions", islist = TRUE),
        spsspkg.Template("INCLUDE3P",     subc = "FIT",    ktype = "bool",  var = "include3p"),
        spsspkg.Template("ALPHA",         subc = "FIT",    ktype = "float", var = "alpha"),
        spsspkg.Template("CRITERION",     subc = "FIT",    ktype = "str",   var = "criterion", vallist = list("composite", "ad", "aic", "bic")),
        spsspkg.Template("LSL",           subc = "SPECS",  ktype = "float", var = "lsl"),
        spsspkg.Template("USL",           subc = "SPECS",  ktype = "float", var = "usl"),
        spsspkg.Template("SIGMA",         subc = "SPECS",  ktype = "float", var = "sigma"),
        spsspkg.Template("RANKINGS",      subc = "OUTPUT", ktype = "bool",  var = "rankings"),
        spsspkg.Template("PARAMETERS",    subc = "OUTPUT", ktype = "bool",  var = "params"),
        spsspkg.Template("CONFIDENCE",    subc = "OUTPUT", ktype = "float", var = "conf"),
        spsspkg.Template("PERCENTILES",   subc = "OUTPUT", ktype = "bool",  var = "percentiles"),
        spsspkg.Template("PCTLIST",       subc = "OUTPUT", ktype = "float", var = "pctlist", islist = TRUE),
        spsspkg.Template("CAPABILITY",    subc = "OUTPUT", ktype = "bool",  var = "capcompare"),
        spsspkg.Template("TRANSFORMS",    subc = "OUTPUT", ktype = "bool",  var = "transforms"),
        spsspkg.Template("CHECKS",        subc = "OUTPUT", ktype = "bool",  var = "checks"),
        spsspkg.Template("RECOMMEND",     subc = "OUTPUT", ktype = "bool",  var = "recommend"),
        spsspkg.Template("TOP",           subc = "OUTPUT", ktype = "int",   var = "ntop"),
        spsspkg.Template("PROBPLOTS",     subc = "CHARTS", ktype = "bool",  var = "probplots"),
        spsspkg.Template("HISTOGRAM",     subc = "CHARTS", ktype = "bool",  var = "histogram"),
        spsspkg.Template("HTML",          subc = "CHARTS", ktype = "bool",  var = "html"),
        spsspkg.Template("OPENREPORT",    subc = "CHARTS", ktype = "bool",  var = "openreport"),
        spsspkg.Template("LOWERBOUND",    subc = "FIT",    ktype = "float", var = "lowerbound"),
        spsspkg.Template("UPPERBOUND",    subc = "FIT",    ktype = "float", var = "upperbound"),
        spsspkg.Template("TOLINT",        subc = "QC",     ktype = "bool",  var = "tolint"),
        spsspkg.Template("COVERAGE",      subc = "QC",     ktype = "float", var = "coverage"),
        spsspkg.Template("TICONF",        subc = "QC",     ktype = "float", var = "ticonf"),
        spsspkg.Template("STABILITY",     subc = "QC",     ktype = "bool",  var = "stability"),
        spsspkg.Template("NBOOT",         subc = "QC",     ktype = "int",   var = "nboot"),
        spsspkg.Template("POOLTEST",      subc = "QC",     ktype = "bool",  var = "pooltest"),
        spsspkg.Template("CTRLLIMITS",    subc = "QC",     ktype = "bool",  var = "ctrllimits"),
        spsspkg.Template("OUTLIERS",      subc = "QC",     ktype = "bool",  var = "outliersens"),
        spsspkg.Template("PROB",          subc = "SAVE",   ktype = "bool",  var = "saveprob"),
        spsspkg.Template("NSCORE",        subc = "SAVE",   ktype = "bool",  var = "savenscore")
    ))
    if ("HELP" %in% attr(args, "names")) {
        cat("STATS DISTRIBUTION IDENTIFICATION: help is in the dialog (Analyze > Quality Control > Distribution Identification, Help button)",
            "and in STATS_DISTRIBUTION_IDENTIFICATION.htm in the extension folder (SHOW EXTPATHS lists the folders).\n")
    } else if (inherits(oobj, "spssError")) {
        print(oobj)
    } else {
        # numerical warnings from the optimisers (NaNs produced, ...) are not
        # meaningful to users; the procedure reports its own problems in Warnings
        suppressWarnings(spsspkg.processcmd(oobj, args, "distident"))
    }
}
