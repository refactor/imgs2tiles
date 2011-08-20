objects = gdal_nifs.so
beams = $(objects:%.so=%.beam)

# example: make DFLAG=DEBUG
ifdef DFLAG
MYFLAG = -D$(DFLAG)
endif

all: $(objects) $(beams)

$(beams): %.beam: %.erl
	erlc $(MYFLAG) $<

$(objects): %.so: %.c
	gcc -std=c99 -fPIC -bundle -undefined suppress -flat_namespace -o $@ $< -I/usr/local/lib/erlang/usr/include/ -lgdal $(MYFLAG)

clean: 
	-rm $(objects)
	-rm $(beams)
	-rm *.log
