#if defined(READ_BITS)
#define PALETTE(i, r, g ,b)                        \
    do {                                           \
        r =  s->color_palette[i * 3];              \
        g =  s->color_palette[i * 3 + 1];          \
        b =  s->color_palette[i * 3 + 2];          \
    } while (0)
 
#if READ_BITS == 1
#define READ_PIXEL(from, x, r, g ,b)               \
    do {                                           \
        int bit = x & 7;                           \
        int idx = (*from >> (7 - bit)) & 1;        \
        r = g = b  = ((1 - idx) << 7);             \
        from += (bit == 7);                        \
    } while (0)
#elif READ_BITS == 2
#define READ_PIXEL(from, x, r, g ,b)               \
    do {                                           \
        int bit = (x & 3);                         \
        int idx = (*from >> ((3 - bit) << 1)) & 3; \
        PALETTE(idx, r, g, b);                     \
        from += (bit == 3);                        \
    } while (0)
#elif READ_BITS == 4
#define READ_PIXEL(from, x, r, g ,b)               \
    do {                                           \
        int bit = x & 1;                           \
        int idx = (*from >> ((1 - bit) << 2)) & 15; \
        PALETTE(idx, r, g, b);                     \
        from += (bit == 1);                        \
    } while (0)
#elif READ_BITS == 8
#define READ_PIXEL(from, x, r, g ,b)               \
    do {                                           \
        PALETTE(*from, r, g, b);                   \
        from++;                                    \
    } while (0)
#elif READ_BITS == 16
#define READ_PIXEL(from, x, r, g ,b)               \
    do {                                           \
        uint16_t pixel;                            \
        pixel = (from[0] << 8) | from[1];          \
        r = ((pixel >> 10)& 0x1f) << 3;            \
        g = ((pixel >> 5)& 0x1f) << 3;             \
        b = (pixel & 0x1f) << 3;                   \
        from += 2;                                 \
    } while (0)
#elif READ_BITS == 24
#define READ_PIXEL(from, x, r, g ,b)               \
    do {                                           \
        r = *from++;                               \
        g = *from++;                               \
        b = *from++;                               \
    } while (0)
#else
#error unknown bit depth
#endif

#if WRITE_BITS == 8
#define WRITE_PIXEL(to, r, g, b)                   \
    do {                                           \
        *to = rgb_to_pixel8(r, g, b);              \
        to += 1;                                   \
    } while (0)
#elif WRITE_BITS == 15
#define WRITE_PIXEL(to, r, g, b)                   \
    do {                                           \
        *(uint16_t *)to = rgb_to_pixel15(r, g, b); \
        to += 2;                                   \
    } while (0)
#elif WRITE_BITS == 16
#define WRITE_PIXEL(to, r, g, b)                   \
    do {                                           \
        *(uint16_t *)to = rgb_to_pixel16(r, g, b); \
        to += 2;                                   \
    } while (0)
#elif WRITE_BITS == 24
#define WRITE_PIXEL(to, r, g, b)                   \
    do {                                           \
        uint32_t tmp = rgb_to_pixel24(r, g, b);    \
        *(to++) =         tmp & 0xff;              \
        *(to++) =  (tmp >> 8) & 0xff;              \
        *(to++) = (tmp >> 16) & 0xff;              \
    } while (0)
#elif WRITE_BITS == 32
#define WRITE_PIXEL(to, r, g, b)                   \
    do {                                           \
        *(uint32_t *)to = rgb_to_pixel32(r, g, b); \
        to += 4;                                   \
    } while (0)
#else
#error unknown bit depth
#endif

static void glue(glue(glue(draw_line, READ_BITS), _), WRITE_BITS)
                              (MacfbState *s, uint8_t *to, uint8_t *from, int width)
{
    uint8_t r, g, b;
    int x;
    for (x = 0; x < width; x++) {
        READ_PIXEL(from, x, r, g, b);
        WRITE_PIXEL(to, r, g, b);
    }
}
#undef READ_BITS
#undef READ_PIXEL
#undef WRITE_PIXEL

#elif defined(WRITE_BITS)

#undef MACFB_RECLEVEL
#define MACFB_RECLEVEL 2
#define READ_BITS 1
#include "macfb-template.h"
#define READ_BITS 2
#include "macfb-template.h"
#define READ_BITS 4
#include "macfb-template.h"
#define READ_BITS 8
#include "macfb-template.h"
#define READ_BITS 16 
#include "macfb-template.h"
#define READ_BITS 24 
#include "macfb-template.h"
#undef WRITE_BITS

#else

#define WRITE_BITS 8
#include "macfb-template.h"

#define WRITE_BITS 16
#include "macfb-template.h"

#define WRITE_BITS 24
#include "macfb-template.h"

#define WRITE_BITS 32
#include "macfb-template.h"

typedef void (*macfb_draw_line_func_t)(MacfbState *, uint8_t *, uint8_t *, int);

static macfb_draw_line_func_t macfb_draw_line[24][32] = {
    [0] = { [7] = draw_line1_8, [15] = draw_line1_16,
            [23] = draw_line1_24, [31] = draw_line1_32 },
    [1] = { [7] = draw_line2_8, [15] = draw_line2_16,
            [23] = draw_line2_24, [31] = draw_line2_32 },
    [3] = { [7] = draw_line4_8, [15] = draw_line4_16,
            [23] = draw_line4_24, [31] = draw_line4_32 },
    [7] = { [7] = draw_line8_8, [15] = draw_line8_16,
            [23] = draw_line8_24, [31] = draw_line8_32 },
    [15] = { [7] = draw_line16_8, [15] = draw_line16_16,
             [23] = draw_line16_24, [31] = draw_line16_32 },
    [23] = { [7] = draw_line24_8, [15] = draw_line24_16,
             [23] = draw_line24_24, [31] = draw_line24_32 },
};
#endif
