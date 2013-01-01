#ifndef QEMU_TYPES_H
#define QEMU_TYPES_H
#include "cpu.h"

#ifdef TARGET_ABI32
typedef uint32_t abi_ulong;
typedef int32_t abi_long;
#define TARGET_ABI_FMT_lx "%08x"
#define TARGET_ABI_FMT_ld "%d"
#define TARGET_ABI_FMT_lu "%u"
#define TARGET_ABI_BITS 32

static inline abi_ulong tswapal(abi_ulong v)
{
    return tswap32(v);
}

static inline abi_ulong abi_ntohl(abi_ulong v)
{
#if defined(HOST_BIG_ENDIAN)
    return v;
#else
    return bswap_32(v);
#endif
}

#else
typedef target_ulong abi_ulong;
typedef target_long abi_long;
#define TARGET_ABI_FMT_lx TARGET_FMT_lx
#define TARGET_ABI_FMT_ld TARGET_FMT_ld
#define TARGET_ABI_FMT_lu TARGET_FMT_lu
#define TARGET_ABI_BITS TARGET_LONG_BITS
/* for consistency, define ABI32 too */
#if TARGET_ABI_BITS == 32
#define TARGET_ABI32 1
#endif

static inline abi_ulong tswapal(abi_ulong v)
{
    return tswapl(v);
}

static inline abi_ulong abi_ntohl(abi_ulong v)
{
#if defined(HOST_BIG_ENDIAN)
    return v;
#else
#if TARGET_LONG_SIZE == 4
    return bswap_32(v);
#else
    return bswap_64(v);
#endif
#endif
}

#endif
#endif
