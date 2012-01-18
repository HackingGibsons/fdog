This is the Dark Work.

## Ubuntu Setup
Constructed during debugging why the hell the test suite
was failing on the Jenkins machine and passing **everywhere else**.

The base machine is `Ubuntu 10.04 x64` the tests were performed on
ami `ami-35de095c` **WARNING: This is an instance store AMI**

`bash
# Everything we need from apt to build everything below
sudo aptitude install build-essential sqlite3 libsqlite3-dev uuid-dev \
                      git-core autoconf

# Build everything in a subdir to be nice
mkdir src && cd src

# Fetch ZeroMQ source
wget http://download.zeromq.org/zeromq-2.1.11.tar.gz

# Fetch the fdog source. -b is temporary for now.
git clone git@github.com:vitrue/fdog.git -b afdog-supervised-mongrel2-die-harder-again

# Fetch the current deploy-blessed mongrel2 tree
git clone git://github.com/sshirokov/mongrel2.git -b deploy

# Fetch the sbcl-1.0.54 linux binary and the 1.0.55 source
# The pre-build x64 1.0.55 binary is not compatibile with the
# glibc version in Ubuntu 10.04 x64
wget -O sbcl-1.0.54-x64-linux-binary.tar.bz2 http://bit.ly/sbcl-1-0-54-x64-linux-binary
wget -O sbcl-1.0.55-source.tar.bz2 http://bit.ly/sbcl-1-0-55-src
`


