getgitrev <- function(path) {
  gitdir <- paste("--git-dir=", path, sep="")
  rev = system2("git", args=c(gitdir, "rev-parse", "--short", "HEAD"), stdout=TRUE)
  return(rev)
}

switchgitrev <- function(path, rev) {
  gitdir <- paste("--git-dir=", path, sep="")
  res = system2("git", args=c(gitdir, "checkout", "-f", rev)) == 0
  return(res)
}

gethgrev <- function(path) {
  rev = system2("hg", args=c("-R", paste(path), "id", "-i"), stdout=TRUE)
  return(rev)
}

rmfile <- function(path) {
  res = system2("rm", "-f", path)
  return(res)
}
