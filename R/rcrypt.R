rcrypt_decrypt <- function (input, output = NULL, passphrase = NULL, verbosity = 1)
{
    if (missing(input)) {
        stop("Check the input argument, it seems to be missing. There's nothing to decrypt.")
    }
    if (!file.exists(input)) {
        stop("Check the input argument, the file name doesn't exist. There's nothing to decrypt.")
    }
    if (is.null(output)) {
        output <- gsub(".gpg|.asc", "", input)
    }
    if (file.exists(output)) {
        stop("Check the output argument, the file name is already in use! The decrypted file may already exist, or you need to specify a new output file name.")
    }
    if (.Platform$OS.type == "unix") {
        tty <- "--no-tty"
    }
    else {
        tty <- NULL
    }
    if (!(verbosity %in% c(0, 1, 2, 3)) || length(verbosity %in%
        c(0, 1, 2, 3)) == 0) {
        stop("Check the verbosity argument. You've used an invalid value.")
    }
    verbosity <- switch(as.character(verbosity), `0` = "--quiet",
        `1` = NULL, `2` = "--verbose", `3` = "--verbose --verbose")
    if (is.null(passphrase)) {
        command <- "gpg"
        system2.args <- c("--output", output, "--decrypt", verbosity,
            input)
    }
    else {
        command <- "echo"
        system2.args <- c(paste(passphrase, "|", sep = ""), "gpg",
            "--passphrase-fd 0", "--batch", tty, "--output",
            output, "--decrypt", verbosity, input)
    }
    if (.Platform$OS.type == "unix") {
        what <- "system2"
        args <- list(command = command, args = system2.args)
    }
    else {
        what <- "shell"
        args <- list(cmd = paste(command, paste(system2.args,
            collapse = " "), collapse = " "))
    }
    do.call(what = what, args = args)
}

rcrypt_encrypt <- function (input, output = NULL, passphrase = NULL, compress = "ZLIB",
    cipher = "AES256", armor = FALSE, mdc = TRUE, s2k.mode = 3,
    s2k.digest = "SHA512", s2k.count = 65011712, verbosity = 1)
{
    if (missing(input)) {
        stop("Check the input argument, it seems to be missing. There's nothing to encrypt.")
    }
    if (is.null(output)) {
        if (armor) {
            output <- paste(input, ".asc", sep = "")
        }
        else {
            output <- paste(input, ".gpg", sep = "")
        }
    }
    if (file.exists(output)) {
        stop("Check the output argument, the file name is already in use! The encrypted file may already exist, or you need to specify a new output file name.")
    }
    if (.Platform$OS.type == "unix") {
        tty <- "--no-tty"
    }
    else {
        tty <- NULL
    }
    if (armor == FALSE) {
        armor <- NULL
    }
    else {
        armor <- "--armor"
    }
    if (mdc == TRUE) {
        mdc <- "--force-mdc"
    }
    else {
        mdc <- NULL
    }
    if (!(verbosity %in% c(0, 1, 2, 3)) || length(verbosity %in%
        c(0, 1, 2, 3)) == 0) {
        stop("Check the verbosity argument. You've used an invalid value.")
    }
    verbosity <- switch(as.character(verbosity), `0` = "--quiet",
        `1` = NULL, `2` = "--verbose", `3` = "--verbose --verbose")
    if (is.null(passphrase)) {
        command <- "gpg"
        system2.args <- c("--output", output, "--symmetric",
            armor, mdc, paste("--compress-algo", compress), paste("--cipher-algo",
                cipher), paste("--s2k-mode", s2k.mode), paste("--s2k-digest-algo",
                s2k.digest), paste("--s2k-count", s2k.count),
            verbosity, input)
    }
    else {
        command <- "echo"
        system2.args <- c(paste(passphrase, "|", sep = ""), "gpg",
            "--passphrase-fd 0", "--batch", tty, "--output",
            output, "--symmetric", armor, mdc, paste("--compress-algo",
                compress), paste("--cipher-algo", cipher), paste("--s2k-mode",
                s2k.mode), paste("--s2k-digest-algo", s2k.digest),
            paste("--s2k-count", s2k.count), verbosity, input)
    }
    if (.Platform$OS.type == "unix") {
        what <- "system2"
        args <- list(command = command, args = system2.args)
    }
    else {
        what <- "shell"
        args <- list(cmd = paste(command, paste(system2.args,
            collapse = " "), collapse = " "))
    }
    do.call(what = what, args = args)
}
