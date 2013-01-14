#!/bin/sh
#
# before executing this script:
#
# - check that lxc template lxc-cross-debian
#   is installed in /usr/share/lxc/templates/
# - binfmt interpreter has been configured
#   using qemu-binfmt-conf.sh
#   (on debian, using --debian and update-binfmts --import)
#

QEMU_PATH=$(dirname $(dirname $(readlink -f $0)))

TARGET_LIST="i386 m68k alpha sparc mips ppc raspberrypi"
check_target() {
    CHECK_BIN=check_default
    case "$1" in
    i386)
        MIRROR=http://ftp.debian.org/debian
        SUITE=stable
        QEMU_TARGET=i386
        DEBIAN_TARGET=i386
        INCLUDE="openssh-server"
        ;;
    m68k)
        MIRROR=http://archive.debian.org/debian
        SUITE=etch-m68k
        DEB_SIGN=55BE302B
        CONFIGURE_PARAMS=--m68k-default-cpu=m68040
        QEMU_TARGET=m68k
        DEBIAN_TARGET=m68k
        CHECK_BIN=check_m68k
        INCLUDE="openssh-server"
        ;;
    alpha)
        MIRROR=http://archive.debian.org/debian
        SUITE=lenny
        QEMU_TARGET=alpha
        DEBIAN_TARGET=alpha
        INCLUDE="openssh-server"
        ;;
    sparc)
        MIRROR=http://ftp.debian.org/debian
        SUITE=stable
        QEMU_TARGET=sparc32plus
        DEBIAN_TARGET=sparc
        INCLUDE="openssh-server"
        ;;
    mips)
        MIRROR=http://ftp.debian.org/debian
        SUITE=stable
        QEMU_TARGET=mips
        DEBIAN_TARGET=mips
        INCLUDE="openssh-server"
        ;;
    ppc|powerpc)
        MIRROR=http://ftp.debian.org/debian
        SUITE=stable
        QEMU_TARGET=ppc
        DEBIAN_TARGET=powerpc
        INCLUDE="openssh-server"
        ;;
    raspberrypi)
        MIRROR=http://archive.raspbian.org/raspbian
        SUITE=wheezy
        DEB_SIGN=""
        DEBIAN_TARGET=armhf
        QEMU_TARGET=arm
        INCLUDE="openssh-server"
        ;;
    *)
        echo "ERROR: unknown target $1" 1>&2
        exit 1
        ;;
    esac
    CONTAINER_NAME=virt${1}-${SUITE}
    QEMU_BIN=${QEMU_PATH}/build-${QEMU_TARGET}/${QEMU_TARGET}-linux-user/qemu-${QEMU_TARGET}
}

check_m68k() {
        if ${QEMU_BIN} -cpu help | grep -q ">m68040"
        then
                echo "Found an existing qemu-m68k, use it !" 1>&2
                return 0
        fi
        echo "Found an existing qemu-m68k, but with no m68040 emulation" 1>&2
        echo "Please, remove it" 1>&2
        echo "m68040 emulation is available from " 1>&2
        echo "git clone git://gitorious.org/qemu-m68k/qemu-m68k.git"
        exit 1
}

check_default() {
        test -e ${QEMU_BIN}
}

create_qemu() {
        if [ -e "${QEMU_BIN}" ]
        then
                if ${CHECK_BIN}
                then
                        return 0
                fi
        fi
        echo "cd ${QEMU_PATH} && \
        mkdir build-${QEMU_TARGET} && \
        cd build-${QEMU_TARGET} && \
        ../configure --static ${CONFIGURE_PARAMS} \
                     --target-list=${QEMU_TARGET}-linux-user && \
        make" | sudo -i -u ${SUDO_USER}
        if ! ${CHECK_BIN}
        then
                echo "ERROR: generated qemu binary is invalid !" 1>&2
                exit 1
        fi
}

create_root() {
    SCRIPT_FLAGS=""
    if [ "$DEB_SIGN" != "" ] ; then
        SCRIPTFLAGS="--deb-sign ${DEB_SIGN}"
    fi
    lxc-create ${LXCFLAGS} -n ${CONTAINER_NAME} -t cross-debian -- \
               --arch=${DEBIAN_TARGET} --interpreter-path=${QEMU_BIN} \
               --suite=${SUITE} --mirror=${MIRROR} --include="${INCLUDE}" \
               ${SCRIPTFLAGS}
}

TARGET="$1"

if [ "$TARGET" = "" ]
then
        echo "ERROR: you must provide a target." 1>&2
        echo "       Available targets are:" 1>&2
        echo "       $TARGET_LIST"
        exit 1
fi

check_target $TARGET

shift
LXCFLAGS="$@"

echo "DEBIAN_TARGET : ${DEBIAN_TARGET}"
echo "QEMU_TARGET   : ${QEMU_TARGET}"
echo "CONTAINER_NAME: ${CONTAINER_NAME}"
echo "QEMU_PATH     : ${QEMU_PATH}"
echo "MIRROR        : ${MIRROR}"
echo "SUITE         : ${SUITE}"
echo "INCLUDE       : ${INCLUDE}"

if [ "$USER" != "root" ]
then
        echo "You need to be root to run this command" 2>&1
        exit 1
fi

create_qemu
create_root
