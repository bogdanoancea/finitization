# tools/winlibs.R
# Download prebuilt Windows (UCRT) libs for GMP, CLN, GiNaC into ./windows/ucrt64/
# Executed automatically by Windows builders (e.g., r-universe). CRAN ignores it
# because you set OS_type: unix in DESCRIPTION.

if (.Platform$OS.type == "windows") {

    message("tools/winlibs.R: fetching Windows libraries for finitization …")

    # ---- Configuration --------------------------------------------------------
    # Prefer environment override in CI; otherwise use your GitHub release URL.
    base_url <- Sys.getenv(
        "FINITIZATION_WINLIBS_BASE",
        "https://github.com/bogdanoancea/finitization-winlibs/releases/download/v1.0.0/"  # <— placeholder, replace!
    )

    # Architecture / toolchain tag: use UCRT for R >= 4.2
    plat <- Sys.getenv("FINITIZATION_WIN_ARCH", unset = "ucrt64")
    bundle_name <- sprintf("finitization-ucrt64-gmp+cln+ginac-static.zip", plat)
    bundle_url  <- sprintf("%s/%s", sub("/+$", "", base_url), bundle_name)

    dest_dir <- normalizePath(file.path("windows", plat), mustWork = FALSE)
    pc_dir   <- file.path(dest_dir, "lib", "pkgconfig")
    zipfile  <- file.path(tempdir(), bundle_name)

    # Optional: SHA256 for integrity (set in CI via env var)
    # sha256_expected <- Sys.getenv("FINITIZATION_WINLIBS_SHA256", unset = "")
    sha256_expected <- as.character("6ce8cccf03db78285cef30a66ff697033d65bd6405e8cfd725bbc9a757079a98")
    # ---- Helpers --------------------------------------------------------------
    download_file <- function(url, dst) {
        dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
        utils::download.file(url, dst, mode = "wb", quiet = TRUE)
    }

    sha256 <- function(path) {
        # Use tools::md5sum alternative if openssl missing; prefer openssl if present
        if (requireNamespace("openssl", quietly = TRUE)) {
            as.character(openssl::sha256(file(path)))
        } else {
            # Fallback: no sha256 available; return empty string
            ""
        }
    }

    write_pc_if_missing <- function(name, prefix, libdir, includedir, version, libs) {
        dir.create(pc_dir, recursive = TRUE, showWarnings = FALSE)
        pc_path <- file.path(pc_dir, paste0(name, ".pc"))
        if (!file.exists(pc_path)) {
            cat(sprintf(
                "prefix=%s\nexec_prefix=${prefix}\nlibdir=%s\nincludedir=%s\n\nName: %s\nDescription: %s\nVersion: %s\nLibs: %s\nCflags: -I${includedir}\n",
                normalizePath(prefix, winslash = "/", mustWork = FALSE),
                normalizePath(libdir, winslash = "/", mustWork = FALSE),
                normalizePath(includedir, winslash = "/", mustWork = FALSE),
                name, paste("Vendored", toupper(name)), version, libs
            ), file = pc_path)
        }
    }

    # ---- Download & unpack ----------------------------------------------------
    message("  • downloading bundle: ", bundle_url)
    download_file(bundle_url, zipfile)

    if (nzchar(sha256_expected)) {
        got <- sha256(zipfile)
        got <- as.character(unname(got)[1])       # drop names & ensure length-1 scalar
        got <- tolower(trimws(got))
        sha256_expected <- tolower(trimws(sha256_expected))

        if (!nzchar(got) || got != sha256_expected) {
            stop("tools/winlibs.R: SHA256 mismatch for ", basename(zipfile),
                 "\n  expected: ", sha256_expected, "\n  got:      ", got)
        }
    }

    message("  • unpacking to: ", dest_dir)
    utils::unzip(zipfile, exdir = ".", junkpaths = FALSE)

    # ---- Ensure .pc files exist (generate if bundle omitted them) -------------
    # This helps configure.win find the libs via pkg-config when we add this path.
    incdir <- file.path(dest_dir, "include")
    libdir <- file.path(dest_dir, "lib")
    if (!dir.exists(pc_dir)) dir.create(pc_dir, recursive = TRUE, showWarnings = FALSE)

    # Create minimal .pc files if missing
    write_pc_if_missing(
        name = "gmp",
        prefix = dest_dir,
        libdir = libdir,
        includedir = file.path(incdir, "gmp"),
        version = "6.3.0",
        libs = "-L${libdir} -lgmp"
    )
    write_pc_if_missing(
        name = "cln",
        prefix = dest_dir,
        libdir = libdir,
        includedir = file.path(incdir, "cln"),
        version = "1.3.7",
        libs = "-L${libdir} -lcln -lgmp"
    )
    write_pc_if_missing(
        name = "ginac",
        prefix = dest_dir,
        libdir = libdir,
        includedir = file.path(incdir, "ginac"),
        version = "1.8.9",
        libs = "-L${libdir} -lginac -lcln -lgmp"
    )

    message("tools/winlibs.R: done. Installed headers/libs under ", dest_dir)
    message("  pkgconfig: ", normalizePath(pc_dir, winslash = "/"))

    # Friendly hint for your configure.win (not executed here):
    # - In configure.win, prepend PKG_CONFIG_PATH with 'windows/ucrt64/lib/pkgconfig'
    #   if it exists, before probing system pkg-config locations.
}
