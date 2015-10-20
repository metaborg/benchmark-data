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

switchgraalrev <- function(graalpath, mxpath, rev) {
  res = system2("hg", args=c("-R", paste(mxpath), "pull")) == 0
  res = res && system2("hg", args=c("-R", paste(mxpath), "update")) == 0
  res = res && system2("mx", args=c("-p", paste(graalpath), "spull")) == 0
  res = res && system2("hg", args=c("-R", paste(graalpath), "update", "-r", rev)) == 0
  res = res && system2("mx", args=c("-p", paste(graalpath), "sforceimports")) == 0

  quitonfail(res, paste("Graal switching failed"))
}

quitonfail <- function(exit, msg="Command failed") {
  if(exit != 0) {
    print(paste(msg))
    quit("no", status=exit)
  }
}

rmfile <- function(path) {
  res = system2("rm", path)
  return(res)
}
