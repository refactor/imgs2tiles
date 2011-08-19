objects = gdal_nifs.so
beams = $(objects:%.so=%.beam)

all: $(objects)

$(objects): %.so: %.c
	gcc -std=c99 -fPIC -bundle -undefined suppress -flat_namespace -o $@ $< -I/usr/local/lib/erlang/usr/include/ -lgdal

clean: 
	-rm $(objects)
	-rm $(beams)
