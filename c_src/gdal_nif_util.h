#ifndef GDAL_NIF_UTIL_H
#define GDAL_NIF_UTIL_H

#include "gdal.h"
#include "gdalwarper.h"
#include "cpl_error.h"
#include "cpl_conv.h"

#include "nif_logger.h"

void fill_pband_list(int n, int* band_list);

CPLErr write_data_and_alpha_to_raster(GDALDatasetH dsquery, 
                                      int xoffset, int yoffset, int xsize, int ysize, 
                                      GByte* data, GByte* alpha, 
                                      int dataBandsCount, int tilebands);

CPLErr write_quadtree_tile_to_raster(GDALDatasetH dsquery, 
                                     int xoffset, int yoffset, int xsize, int ysize, 
                                     GDALDatasetH dstile);

CPLErr scale_query_to_tile(GDALDatasetH dsquery, GDALDatasetH dstile, const char* options_resampling);

#endif
