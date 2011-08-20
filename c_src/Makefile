objects = gdal_nifs.so
beams = $(objects:%.so=%.beam)

# example: make DFLAG=DEBUG
ifdef DFLAG
MYFLAG = -D$(DFLAG)
endif

ifndef ERL_HOME
	ERL_HOME = /usr/local/lib/erlang
endif

myos := $(shell uname -s)

ifeq ($(myos),Linux)
CFLAGS = -shared
else
CFLAGS = -bundle -undefined suppress -flat_namespace
endif

all: $(objects) $(beams)

$(beams): %.beam: %.erl
	erlc $(MYFLAG) $<

$(objects): %.so: %.c
	gcc -std=c99 -fPIC $(CFLAGS) -o $@ $< -I$(ERL_HOME)/usr/include/ -lgdal $(MYFLAG)

clean: 
	-rm $(objects)
	-rm $(beams)
	-rm *.log

echo:
	echo $(myos)
	echo $(CFLAGS)

