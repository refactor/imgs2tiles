#ifdef DEBUG
#include <time.h>
static char* current_time() 
{
    time_t now = time(NULL);
    return ctime(&now);
}

    static FILE* logger;
    #define LOG(msg, ...) (fprintf(logger, "[%s - Ln:%d] " msg "\n", current_time(), __LINE__, __VA_ARGS__), fflush(logger))
    #define OPEN_LOGER() (logger =  fopen(__FILE__ ".log", "a"))
    #define CLOSE_LOGER() ( fclose(logger) )
#else
    #define LOG(msg, ...)
    #define OPEN_LOGER() 
    #define CLOSE_LOGER()
#endif

