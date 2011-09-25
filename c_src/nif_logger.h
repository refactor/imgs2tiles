#ifndef NIF_LOGGER_H
#define NIF_LOGGER_H

#ifdef GDAL_NIF_DEBUG
    #include <stdio.h>
    #include <stdarg.h>
    extern void DEBUG(const char *fmt, ...);

    #define OPEN_LOGER() {}
    #define CLOSE_LOGER() {}
#else
    #define DEBUG(msg, ...) {}
    #define OPEN_LOGER() {}
    #define CLOSE_LOGER() {}
#endif

#endif
