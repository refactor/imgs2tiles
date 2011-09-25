#include "nif_logger.h"

#ifdef GDAL_NIF_DEBUG

void DEBUG(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    va_end(ap);
}

#endif
