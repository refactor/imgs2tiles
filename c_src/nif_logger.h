#ifdef DEBUG
    static FILE* logger;
    #define LOG(msg, ...) (fprintf(logger, "[%s %s - Ln:%d] " msg "\n", __DATE__, __TIME__, __LINE__, __VA_ARGS__), fflush(logger))
    #define OPEN_LOGER() (logger =  fopen(__FILE__ ".log", "a"))
    #define CLOSE_LOGER() ( fclose(logger) )
#else
    #define LOG(msg, ...)
    #define OPEN_LOGER() 
    #define CLOSE_LOGER()
#endif

