#!/bin/sh
# enable automatic i386/ARM/M68K/MIPS/SPARC/PPC/s390 program execution by the kernel

qemu_target_list="i386 i486 alpha arm sparc32plus ppc m68k mips mipsel \
                  mipsn32 mipsn32el mips64 mips64el sh4 sh4eb s390x aarch64"

i386_magic='\x7fELF\x01\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x03\x00'
i386_mask='\xff\xff\xff\xff\xff\xfe\xfe\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff\xff'
i386_family=i386

i486_magic='\x7fELF\x01\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x06\x00'
i486_mask='\xff\xff\xff\xff\xff\xfe\xfe\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff\xff'
i486_family=i386

alpha_magic='\x7fELF\x02\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x26\x90'
alpha_mask='\xff\xff\xff\xff\xff\xfe\xfe\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff\xff'
alpha_family=alpha

arm_magic='\x7fELF\x01\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x28\x00'
arm_mask='\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff\xff'
arm_family=arm

armeb_magic='\x7fELF\x01\x02\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x28'
armeb_mask='\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff'
armeb_family=arm

sparc_magic='\x7fELF\x01\x02\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x02'
sparc_mask='\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff'
sparc_family=sparc

sparc32plus_magic='\x7fELF\x01\x02\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x12'
sparc32plus_mask='\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff'
sparc32plus_family=sparc

ppc_magic='\x7fELF\x01\x02\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x14'
ppc_mask='\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff'
ppc_family=ppc

m68k_magic='\x7fELF\x01\x02\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x04'
m68k_mask='\xff\xff\xff\xff\xff\xff\xfe\xfe\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff'
m68k_family=m68k

# FIXME: We could use the other endianness on a MIPS host.

mips_magic='\x7fELF\x01\x02\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x08'
mips_mask='\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff'
mips_family=mips

mipsel_magic='\x7fELF\x01\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x08\x00'
mipsel_mask='\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff\xff'
mipsel_family=mips

mipsn32_magic='\x7fELF\x01\x02\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x08'
mipsn32_mask='\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff'
mipsn32_family=mips

mipsn32el_magic='\x7fELF\x01\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x08\x00'
mipsn32el_mask='\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff\xff'
mipsn32el_family=mips

mips64_magic='\x7fELF\x02\x02\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x08'
mips64_mask='\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff'
mips64_family=mips

mips64el_magic='\x7fELF\x02\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x08\x00'
mips64el_mask='\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff\xff'
mips64el_family=mips

sh4_magic='\x7fELF\x01\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x2a\x00'
sh4_mask='\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff\xff'
sh4_family=sh4

sh4eb_magic='\x7fELF\x01\x02\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x2a'
sh4eb_mask='\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff'
sh4eb_family=sh4

s390x_magic='\x7fELF\x02\x02\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x16'
s390x_mask='\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff'
s390x_family=s390x

aarch64_magic='\x7fELF\x02\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\xb7\x00'
aarch64_mask='\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff\xff'
aarch64_family=aarch64

qemu_get_family() {
    cpu=$(uname -m)
    case "$cpu" in
    i386|i486|i586|i686|i86pc|BePC|x86_64)
        echo "i386"
        ;;
    mips*)
        echo "mips"
        ;;
    "Power Macintosh"|ppc64)
        echo "ppc"
        ;;
    armv[4-9]*)
        echo "arm"
        ;;
    sparc*)
        echo "sparc"
        ;;
    *)
        echo "$cpu"
        ;;
    esac
}

usage() {
    cat <<!EOF
Usage: qemu-binfmt-conf.sh [--qemu-path PATH][--debian [--exportdir PATH]]
                           [--help][--credential yes|no]

       Configure binfmt_misc to use qemu interpreter

       --help:       display this usage
       --qemu-path:  set path to qemu interpreter ($QEMU_PATH)
       --debian:     don't write into /proc,
                     instead generate update-binfmts templates
       --exportdir:  define where to write update-binfmts templates
                     ($EXPORTDIR)
       --credential: if yes, credential an security tokens are 
                     calculated according to the binary to interpret

    To import templates with update-binfmts, use :

        sudo update-binftms --importdir $EXPORTDIR --import qemu-CPU

    To remove interpreter, use :

        sudo update-binftms --package qemu --remove qemu-CPU $QEMU_PATH/qemu-CPU

    where CPU is one of:

!EOF
    echo -n "    "
    for CPU in $qemu_target_list ;
    do
        echo -n "$CPU "
    done
    echo
}

qemu_check_bintfmt_misc() {
    # load the binfmt_misc module
    if [ ! -d /proc/sys/fs/binfmt_misc ]; then
      if ! /sbin/modprobe binfmt_misc ; then
          exit 1
      fi
    fi
    if [ ! -f /proc/sys/fs/binfmt_misc/register ]; then
      if ! mount binfmt_misc -t binfmt_misc /proc/sys/fs/binfmt_misc ; then
          exit 1
      fi
    fi
    
    if [ ! -w /proc/sys/fs/binfmt_misc/register ] ; then
        echo "ERROR: cannot write to /proc/sys/fs/binfmt_misc/register" 1>&2
        exit 1
    fi
}

installed_dpkg() {
    dpkg --status "$1" > /dev/null 2>&1
}

qemu_check_debian() {
    if [ ! -e /etc/debian_version ] ; then
        echo "ERROR: this script works only on Debian based distro" 1>&2
        exit 1
    fi
    if ! installed_dpkg binfmt-support ; then
        echo "ERROR: package binfmt-support is needed !" 1>&2
        exit 1
    fi
}

qemu_register_interpreter() {
    echo "Setting $qemu as binfmt interpreter for $cpu"
    echo ":qemu-$cpu:M::$magic:$mask:$qemu:$FLAGS" > /proc/sys/fs/binfmt_misc/register
}

qemu_generate_packages() {
    cat > "$EXPORTDIR/qemu-$cpu" <<!EOF
package qemu-$cpu
interpreter $qemu
magic $magic
mask $mask
!EOF
    if [ "$FLAGS" = "OC" ] ; then
        echo "credentials yes" >> "$EXPORTDIR/qemu-$cpu"
    fi
}

qemu_set_binfmts() {
    # probe cpu type
    host_family=$(qemu_get_family)
    
    # register the interpreter for each cpu except for the native one
    
    for cpu in ${qemu_target_list} ; do
        magic=$(eval echo \$${cpu}_magic)
        mask=$(eval echo \$${cpu}_mask)
        family=$(eval echo \$${cpu}_family)
    
        if [ "$magic" = "" -o "$mask" = "" -o "$family" = "" ] ; then
            echo "INTERNAL ERROR: unknown cpu $cpu" 1>&2
            continue
        fi
    
        qemu="$QEMU_PATH/qemu-$cpu"
        if [ "$cpu" = "i486" ] ; then
            qemu="$QEMU_PATH/qemu-i386"
        fi
    
        if [ "$host_family" != "$family" ] ; then
            $BINFMT_SET
        fi
    done
}

BINFMT_SET=qemu_register_interpreter
QEMU_PATH=/usr/local/bin
EXPORTDIR="/usr/share/binfmts"
CHECK=qemu_check_bintfmt_misc
FLAGS=""

options=$(getopt -o dQ:e:hc: -l debian,qemu-path:,exportdir:,help,credential: -- "$@")
eval set -- "$options"

while true ; do
    case "$1" in
    -d|--debian)
        CHECK=qemu_check_debian
        BINFMT_SET=qemu_generate_packages
        ;;
    -Q|--qemu-path)
        shift
        QEMU_PATH="$1"
        ;;
    -e|--exportdir)
        shift
        EXPORTDIR="$1"
        if [ ! -d "$EXPORTDIR" ] ; then
            echo "ERROR: $EXPORTDIR doesn't exist" 1>&2
            exit 1
        fi
        ;;
    -h|--help)
        usage
        exit 1
        ;;
    -c|--credential)
        shift
        if [ "$1" = "yes" ] ; then
            FLAGS="OC"
        else
            FLAGS=""
        fi
        ;;
    *)
        break
        ;;
    esac
    shift
done

$CHECK
qemu_set_binfmts
