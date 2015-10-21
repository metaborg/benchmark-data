getgitrev <- function(path) {
  rev = system2("git", args=c("-C", paste(path), "rev-parse", "--short", "HEAD"), stdout=TRUE)
  return(rev)
}

switchgitrev <- function(path, rev) {
  res = system2("git", args=c("-C", paste(path), "fetch")) == 0
  res = res && system2("git", args=c("-C", paste(path), "checkout", "-f", paste(rev))) == 0
  quitonfail(ifelse(res, 0, 1), "Switching Git revision failed")
}

gethgrev <- function(path) {
  rev = system2("hg", args=c("-R", paste(path), "id", "-i"), stdout=TRUE)
  return(rev)
}

switchhgrev <- function(path, rev) {
  res = system2("hg", args=c("-R", paste(path), "pull")) == 0
  res = res && system2("hg", args=c("-R", paste(path), "update", "-r", paste(rev))) == 0

  quitonfail(ifelse(res, 0, 1), "Switching Hg revision failed")
}

switchgraalrev <- function(graalpath, mxpath, rev) {
  res = system2("hg", args=c("-R", paste(mxpath), "pull")) == 0
  res = res && system2("hg", args=c("-R", paste(mxpath), "update")) == 0
  res = res && system2("mx", args=c("-p", paste(graalpath), "spull")) == 0
  switchhgrev(graalpath, rev)
  res = res && system2("mx", args=c("-p", paste(graalpath), "sforceimports")) == 0
  quitonfail(ifelse(res, 0, 1), paste("Graal switching failed"))
}

quitonfail <- function(exitstatus, msg="Command failed") {
  if(exitstatus != 0) {
    print(paste(msg))
    # quit("no", status=exitstatus)
  }
}

rmfile <- function(path) {
  res = system2("rm", args=c("-rf", path))
  return(res)
}
