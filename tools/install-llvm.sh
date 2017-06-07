set -o errexit -o verbose

case $TRAVIS_OS_NAME in
    "osx")
	case $LLVM_VERSION in
	    2.8)
		FILE='2.8/clang+llvm-2.8-x86_64-apple-darwin10.tar.gz'
		;;
	    2.9)
		FILE='2.9/clang+llvm-2.9-x86_64-apple-darwin10.tar.gz'
		;;
	esac
	;;
    "linux")
	# case $LLVM_VERSION in
	#     2.8)
	# 	FILE='2.8/clang+llvm-2.8-x86_64-linux.tar.bz2'
	# 	;;
	#     2.9)
	# 	FILE='2.9/clang+llvm-2.9-x86_64-linux.tar.bz2'
	# 	;;
	#     3.0)
	# 	FILE='3.0/clang+llvm-3.0-x86_64-linux-Ubuntu-11_10.tar.gz'
	# 	;;
	#     3.1)
	# 	FILE='3.1/clang+llvm-3.1-x86_64-linux-ubuntu_12.04.tar.gz'
	# 	;;
	#     3.2)
	# 	FILE='3.2/clang+llvm-3.2-x86_64-linux-ubuntu-12.04.tar.gz'
	# 	;;
	#     3.3)
	# 	FILE='3.3/clang+llvm-3.3-amd64-Ubuntu-12.04.2.tar.gz'
	# 	;;
	#     3.4)
	# 	FILE='3.4.2/clang+llvm-3.4.2-x86_64-linux-gnu-ubuntu-14.04.xz'
	# 	;;
	#     3.5)
	# 	FILE='3.5.2/clang+llvm-3.5.2-x86_64-linux-gnu-ubuntu-14.04.tar.xz'
	# 	;;
	#     3.6)
	# 	FILE='3.6.2/clang+llvm-3.6.2-x86_64-linux-gnu-ubuntu-15.04.tar.xz'
	# 	;;
	#     3.7)
	# 	FILE='3.7.1/clang+llvm-3.7.1-x86_64-linux-gnu-ubuntu-15.10.tar.xz'
	# 	;;
	#     3.8)
	# 	FILE='3.8.1/clang+llvm-3.8.1-x86_64-linux-gnu-ubuntu-16.04.tar.xz'
	# 	;;
	#     3.9)
	# 	FILE='3.9.1/clang+llvm-3.9.1-x86_64-linux-gnu-ubuntu-16.04.tar.xz'
	# 	;;
	# esac
	;;
esac

END="${FILE##*.}"
URL="http://releases.llvm.org/${FILE}"

if test -f "${HOME}/.local/bin/llvm-config"
then
    echo "llvm already installed"
else
    if [ -z $FILE ]
    then
	ln -s $(which llvm-config-${LLVM_VERSION}) ${HOME}/.local/bin/llvm-config
    else
	curl --location ${URL} > llvm.tar.${END}
	tar -xf llvm.tar.${END} --strip-components 1 -C ${HOME}/.local
	llvm-config --version
	rm llvm.tar.${END}
    fi
fi

