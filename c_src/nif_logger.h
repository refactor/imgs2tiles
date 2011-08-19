#ifdef DEBUG
    static FILE* logger;
    #define LOG(msg, ...) (fprintf(logger, msg, __VA_ARGS__), fflush(logger))
    #define OPEN_LOGER() (logger =  fopen("gdal_nifs.log", "a"))
    #define CLOSE_LOGER() ( fclose(logger) )
#else
    #define LOG(msg, ...)
    #define OPEN_LOGER() 
    #define CLOSE_LOGER()
#endif

