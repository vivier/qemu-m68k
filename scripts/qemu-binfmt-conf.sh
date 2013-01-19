#!/bin/sh
# enable automatic i386/ARM/M68K/MIPS/SPARC/PPC/s390 program execution by the kernel

MAGIC=$(dirname $(readlink -f "$0"))/binfmts-magic

if [ ! -e "$MAGIC" ] ; then
    echo "ERROR: file $MAGIC is missing !" 1>&2
    exit 1
fi

. "$MAGIC"

QEMU_PATH="$1"
if [ "$QEMU_PATH" = "" ] ; then
    QEMU_PATH=/usr/local/bin
fi

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
        echo "Setting $qemu as binfmt interpreter for $cpu"
        echo ":$cpu:M::$magic:$mask:$qemu:" > /proc/sys/fs/binfmt_misc/register
    fi
done
