#include "gdal_nif_util.h"

static GDALResampleAlg parse_resampling(const char* option_resampling) 
{
    if (option_resampling != NULL) {
        if (strcmp("near", option_resampling) == 0) {
            return GRA_NearestNeighbour;
        }
        else if (strcmp("bilinear", option_resampling) == 0) {
            return GRA_Bilinear;
        }
        else if (strcmp("cubic", option_resampling) == 0) {
            return GRA_Cubic;
        }
        else if (strcmp("cubicspline", option_resampling) == 0) {
            return GRA_CubicSpline;
        }
        else if (strcmp("lanczos", option_resampling) == 0) {
            return GRA_Lanczos;
        }
        else { 
            // for 'average' or 'antialias'
            return GRA_NearestNeighbour;
        } 
    }
    else {
        return GRA_NearestNeighbour;
    }
}

void fill_pband_list(int n, int band_list[n])
{
    for (int i = 0; i < n; ++i) {
        band_list[i] = i + 1;
    }
}

CPLErr write_quadtree_tile_to_raster(GDALDatasetH dsquery, 
                                     int xoffset, int yoffset, int xsize, int ysize, 
                                     GDALDatasetH dstile)
{
    int tilexsize = GDALGetRasterXSize(dstile);
    int tileysize = GDALGetRasterYSize(dstile);
    int tilebands = GDALGetRasterCount(dstile);
    DEBUG("quadtree tile WriteRaster: xsize: %d, ysize: %d, bands: %d", tilexsize, tileysize, tilebands);
    
    int band_list[tilebands];
    fill_pband_list(tilebands, band_list);

    GByte *data = CPLCalloc(xsize * ysize * tilebands, sizeof(*data));
    CPLErr eErr = GDALDatasetRasterIO(dstile, GF_Read, 0, 0, tilexsize, tileysize, 
                                      data, xsize, ysize, GDT_Byte, 
                                      tilebands, band_list,
                                      0, 0, 0);
    if (eErr != CE_None) {
        DEBUG("ReadRaster failed: %s", CPLGetLastErrorMsg());
        CPLFree(data);
        return eErr;
    }
    eErr = GDALDatasetRasterIO(dsquery, GF_Write,
                                      xoffset, yoffset, xsize, ysize, data, 
                                      xsize, ysize, GDT_Byte, 
                                      tilebands, band_list,
                                      0, 0, 0);
    CPLFree(data);
    return eErr;
}

CPLErr write_data_and_alpha_to_raster(GDALDatasetH dsquery, 
                                      int xoffset, int yoffset, int xsize, int ysize, 
                                      GByte* data, GByte* alpha, 
                                      int dataBandsCount, int tilebands) 
{
    DEBUG("data and alpha WriteRaster");

    int band_list[dataBandsCount];
    fill_pband_list(dataBandsCount, band_list);

    CPLErrorReset();
    CPLErr eErr = GDALDatasetRasterIO(dsquery, GF_Write,
                                      xoffset, yoffset, xsize, ysize, data, 
                                      xsize, ysize, GDT_Byte, 
                                      dataBandsCount, band_list,
                                      0, 0, 0);
    if (eErr == CE_Failure) {
        DEBUG("data WriteRaster(xoffset: %d, yoffset: %d, xsize: %d, ysize: %d, data: %p) for dsquery failed: %s", 
                xoffset, yoffset, xsize, ysize, data,
                CPLGetLastErrorMsg());

        return eErr;
    }

    int pBandList[] = {tilebands};
    eErr = GDALDatasetRasterIO(dsquery, GF_Write,
                               xoffset, yoffset, xsize, ysize, alpha, 
                               xsize, ysize, GDT_Byte, 
                               1, pBandList, 
                               0, 0, 0);
    if (eErr == CE_Failure) {
        DEBUG("alpha WriteRaster dsquery failed: %s", CPLGetLastErrorMsg());
    }
    return eErr;
}

CPLErr scale_query_to_tile(GDALDatasetH dsquery, GDALDatasetH dstile, const char* options_resampling)
{
    DEBUG("Scales down query dataset to the tile dataset");

    int querysize = GDALGetRasterXSize(dsquery);
    int tilesize = GDALGetRasterXSize(dstile);
    int tilebands = GDALGetRasterCount(dstile);
    DEBUG(" is calling ... querysize: %d, tilesize: %d, tilebands: %d", querysize, tilesize, tilebands);

    if (options_resampling && strcmp("average", options_resampling) == 0) {
        DEBUG("sample average");
        for (int i = 1; i < tilebands + 1; ++i) {
            CPLErrorReset();

            GDALRasterBandH overviewBand = GDALGetRasterBand(dstile, i);
            CPLErr eErr = GDALRegenerateOverviews(GDALGetRasterBand(dsquery, i), 1, &overviewBand, "average", NULL, NULL);
            if (eErr != CE_None) {
                DEBUG("GDALRegenerateOverviews failed on (band: %d), error: %s", i, CPLGetLastErrorMsg());
                return eErr;
            }
        }
    }
    else if (options_resampling && strcmp("antialias", options_resampling) == 0) {
        // TODO
        DEBUG("Scaling by PIL (Python Imaging Library) - improved Lanczos");
    }
    else {
        DEBUG("other sampling algorithms");
        // Other algorithms are implemented by gdal.ReprojectImage().
        GDALSetGeoTransform(dsquery, 
                (double []){0.0, tilesize / ((double)querysize), 0.0, 0.0, 0.0, tilesize / ((double)querysize)});
        GDALSetGeoTransform(dstile, 
                (double []){0.0, 1.0, 0.0, 0.0, 0.0, 1.0});

        CPLErrorReset();
        CPLErr eErr = GDALReprojectImage(dsquery, NULL, 
                                         dstile, NULL, 
                                         parse_resampling(options_resampling), 
                                         0.0, 0.0, NULL, NULL, NULL); 
        if (eErr != CE_None) {
            DEBUG("GDALReprojectImage failed, error %s", CPLGetLastErrorMsg());
            return eErr;
        }
    }

    return CE_None;
}

