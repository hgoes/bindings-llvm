set -o errexit -o verbose

case $TRAVIS_OS_NAME in
    "osx")
	ARCH='x86_64-apple-darwin10'
	END='gz'
	;;
    "linux")
	ARCH='x86_64-linux'
	END='bz2'
	;;
esac

URL="http://releases.llvm.org/${LLVM_VERSION}/clang+llvm-${LLVM_VERSION}-${ARCH}.tar.${END}"

if test -f "${HOME}/.local/bin/llvm-config"
then
    echo "llvm already installed"
else
    curl --location ${URL} > llvm.tar.${END}
    tar -xf llvm.tar.${END} --strip-components 1 --one-top-level=${HOME}/.local
    llvm-config --version
    rm llvm.tar.${END}
fi

