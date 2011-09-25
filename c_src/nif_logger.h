#ifndef NIF_LOGGER_H
#define NIF_LOGGER_H

#ifdef DEBUG
#include <stdio.h>
    extern FILE* logger;
    #define LOG(msg, ...) (fprintf(logger, "[Ln:%.4d - %s] " msg "\n", __LINE__, __FUNCTION__, ##__VA_ARGS__), fflush(logger))
    #define OPEN_LOGER() (logger =  fopen(__FILE__ ".log", "a"))
    #define CLOSE_LOGER() ( fclose(logger) )
#else
    #define LOG(msg, ...)
    #define OPEN_LOGER() 
    #define CLOSE_LOGER()
#endif

#endif
