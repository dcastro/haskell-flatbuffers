/*
 * See https://github.com/haskell/text/blob/9fac5db9b048b7d68fa2fb68513ba86c791b3630/cbits/cbits.c for details.
 */

#include <string.h>
#include <stdint.h>
#include <stdio.h>

uint32_t
_hs_text_length_utf8(const uint16_t *src, size_t srcoff,
		     size_t srclen)
{
  const uint16_t *srcend;

  int32_t counter = 0;

  src += srcoff;
  srcend = src + srclen;

 ascii:
#if defined(__x86_64__)
  while (srcend - src >= 4) {
    uint64_t w = *((uint64_t *) src);

    if (w & 0xFF80FF80FF80FF80ULL) {
      if (!(w & 0x000000000000FF80ULL)) {
	++counter;
	src++;
	if (!(w & 0x00000000FF800000ULL)) {
	  ++counter;
	  src++;
	  if (!(w & 0x0000FF8000000000ULL)) {
	    ++counter;
	    src++;
	  }
	}
      }
      break;
    }
    counter += 4;
    src += 4;
  }
#endif

#if defined(__i386__)
  while (srcend - src >= 2) {
    uint32_t w = *((uint32_t *) src);

    if (w & 0xFF80FF80)
      break;
    counter += 2;
    src += 2;
  }
#endif

  while (src < srcend) {
    uint16_t w = *src++;

    if (w <= 0x7F) {
      ++counter;
      /* An ASCII byte is likely to begin a run of ASCII bytes.
	 Falling back into the fast path really helps performance. */
      goto ascii;
    }
    else if (w <= 0x7FF) {
      counter += 2;
    }
    else if (w < 0xD800 || w > 0xDBFF) {
      counter += 3;
    } else {
      uint32_t c = ((((uint32_t) w) - 0xD800) << 10) +
	(((uint32_t) *src++) - 0xDC00) + 0x10000;
      counter += 4;
    }
  }

  return counter;
}