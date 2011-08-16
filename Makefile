objects = gdal_test gdal_transformer spatial_ref_test

all: $(objects)

$(objects): %: %.c
	gcc -o $@ $< -lgdal -std=c99

clean-all: clean clean-tmp

clean:
	-rm $(objects)
	-rm a.out

clean-tmp:
	-rm *~
	-rm .*.swp
