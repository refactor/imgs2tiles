// -------------------------------------------------------------------
// Purpose:  Convert a raster into TMS (Tile Map Service) tiles in a directory or 
//              something else as fast as possible.
//           - support of global tiles (Spherical Mercator) for compatibility
//               with interactive web maps such as Google Maps
// 
// this is a clone implementent of gdal2tiles.py, but use elang do some 
// parallel work for fast speed 
// 
// gdal2tiles.py is the work of Klokan Petr Pridal, klokan at klokan dot cz
//      Web:      http://www.klokan.cz/projects/gdal2tiles/
// 
// -------------------------------------------------------------------
//  Copyright (c) 2011
//
//   Permission is hereby granted, free of charge, to any person obtaining a
//   copy of this software and associated documentation files (the "Software"),
//   to deal in the Software without restriction, including without limitation
//   the rights to use, copy, modify, merge, publish, distribute, sublicense,
//   and/or sell copies of the Software, and to permit persons to whom the
//   Software is furnished to do so, subject to the following conditions:
//   
//   The above copyright notice and this permission notice shall be included
//   in all copies or substantial portions of the Software.
//   
//   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
//   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
//   THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
//   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
//   DEALINGS IN THE SOFTWARE.
// -------------------------------------------------------------------
#include "erl_nif.h"

#include "gdalwarper.h"
#include "ogr_srs_api.h"
#include "gdal.h"
#include "cpl_conv.h"
#include "cpl_string.h"
#include "cpl_error.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#include "nif_logger.h"

static ErlNifResourceType* gdal_img_RESOURCE;
static ErlNifResourceType* gdal_tile_RESOURCE;

typedef struct
{
    int len;
    double values[];
} nodata_values;

typedef struct
{
    GDALDatasetH in_ds;     // the original dataset
    GDALDatasetH out_ds;    // the VRT dataset which warped in_ds for tile projection

    GDALRasterBandH alphaBand;

    int querysize;
    int tilesize;
    int dataBandsCount;
    int tilebands;

    nodata_values* inNodata;
    
    const char* options_resampling;

    OGRSpatialReferenceH in_srs;
    const char* in_srs_wkt;

    OGRSpatialReferenceH out_srs;
    const char* out_srs_wkt;
} gdal_img_handle;

typedef struct
{
    GDALDriverH  hOutDriver;
    GDALDriverH  hMemDriver;
    const char* tiledriver;
} gdal_priv_data;

typedef struct
{
    int xoffset;
    int yoffset;
    int xsize;
    int ysize;
} bandregion;

typedef struct
{
    GDALDatasetH dstile;
    bandregion w;
    
    int querysize;
    int tilesize;
    
    int dataBandsCount;
    int tilebands;
    
    const char* options_resampling;
    char* tilefilename;

    GByte* data;
    GByte* alpha;
} gdal_tile_handle;

// Atoms (initialized in on_load)
static ERL_NIF_TERM ATOM_ALLOCATION_ERROR;
static ERL_NIF_TERM ATOM_ERROR;
static ERL_NIF_TERM ATOM_NOT_FOUND;
static ERL_NIF_TERM ATOM_NOT_OPEN;
static ERL_NIF_TERM ATOM_FALSE;
static ERL_NIF_TERM ATOM_TRUE;
static ERL_NIF_TERM ATOM_OK;

// Prototypes
static ERL_NIF_TERM gdal_nifs_open_img(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM gdal_nifs_close_img(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM gdal_nifs_calc_nodatavalue(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM gdal_nifs_calc_srs(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM gdal_nifs_warp_dataset(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM gdal_nifs_calc_data_bandscount(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) ;
static ERL_NIF_TERM gdal_nifs_copyout_tile(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM gdal_nifs_build_tile(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM gdal_nifs_save_tile(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM gdal_nifs_get_meta(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static void destroy_img_handle(gdal_img_handle* handle);
static void gdal_nifs_img_resource_cleanup(ErlNifEnv* env, void* arg);


static ErlNifFunc nif_funcs[] = 
{
    {"open_img", 1, gdal_nifs_open_img},
    {"close_img", 1, gdal_nifs_close_img},

    {"calc_nodatavalue", 1, gdal_nifs_calc_nodatavalue},
    {"calc_srs", 1, gdal_nifs_calc_srs},
    {"warp_dataset", 1, gdal_nifs_warp_dataset},
    {"calc_data_bandscount", 1, gdal_nifs_calc_data_bandscount},

    {"copyout_tile", 4, gdal_nifs_copyout_tile},
    {"build_tile", 1, gdal_nifs_build_tile},
    {"save_tile", 1, gdal_nifs_save_tile},

    {"get_meta", 1, gdal_nifs_get_meta}
};


static ERL_NIF_TERM gdal_nifs_open_img(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    char name[4096];
    size_t name_sz;
    if (enif_get_string(env, argv[0], name, sizeof(name), ERL_NIF_LATIN1)) {
        name_sz = strlen(name);

        GDALDatasetH in_ds = GDALOpen(name, GA_ReadOnly);
        if (in_ds != NULL) {
            gdal_img_handle* handle = enif_alloc_resource(
                                                    gdal_img_RESOURCE, 
                                                    sizeof(gdal_img_handle));
            memset(handle, '\0', sizeof(*handle));
            handle->options_resampling = "average";
            handle->querysize = 256 * 4;
            handle->tilesize = 256;

            handle->in_ds = in_ds;

            int rasterCount = GDALGetRasterCount(in_ds);
            if (rasterCount == 0) {
                destroy_img_handle(handle);

                const char* msg = "Input file '%s' has no raster band";
                char errstr[name_sz + strlen(msg) + 1];
                sprintf(errstr, msg, name);
                return enif_make_tuple2(env, 
                                        ATOM_ERROR, 
                                        enif_make_string(env, errstr, ERL_NIF_LATIN1));
            }

            GDALRasterBandH hBand = GDALGetRasterBand(in_ds, 1);
            if (GDALGetRasterColorTable(hBand) != NULL) {
                destroy_img_handle(handle);

                const char* msg = "Please convert this file to RGB/RGBA and run gdal2tiles on the result.\n" 
                    "From paletted file you can create RGBA file (temp.vrt) by:\n"
                    "gdal_translate -of vrt -expand rgba %s temp.vrt\n"
                    "then run this program: gdal2tiles temp.vrt"
                ;
                char errstr[name_sz + strlen(msg) + 1];
                sprintf(errstr, msg, name);
                
                return enif_make_tuple2(env, 
                                        ATOM_ERROR, 
                                        enif_make_string(env, errstr, ERL_NIF_LATIN1));
            }

            double padfTransform[6];
            double errTransform[6] = {0.0, 1.0, 0.0, 0.0, 0.0, 1.0};
            GDALGetGeoTransform(in_ds, padfTransform);
            if (0 == memcmp(padfTransform, errTransform, sizeof(errTransform))
                     && GDALGetGCPCount(in_ds) == 0) {
                destroy_img_handle(handle);
                return enif_make_tuple2(env, 
                                        ATOM_ERROR,
                                        enif_make_string(env, 
                                            "There is no georeference - "
                                            "neither affine transformation (worldfile) nor GCPs",
                                            ERL_NIF_LATIN1));
            }

            ERL_NIF_TERM res = enif_make_resource(env, handle);
            enif_release_resource(handle);

            return enif_make_tuple2(env, ATOM_OK, res);
        }
        else {
            const char* msg = "It is not possible to open the input file '%s'.";
            char errstr[name_sz + strlen(msg) + 1];
            sprintf(errstr, msg, name);
            return enif_make_tuple2(env, 
                                    ATOM_ERROR, 
                                    enif_make_string(env, errstr, ERL_NIF_LATIN1));
        }
    }
    else {
        return enif_make_badarg(env);
    }
}

static ERL_NIF_TERM gdal_nifs_calc_nodatavalue(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    LOG("gdal_nifs_calc_nodatavalue is calling");
    gdal_img_handle* handle;
    if (enif_get_resource(env, argv[0], gdal_img_RESOURCE, (void**)&handle)) {
        // Get NODATA value
        double nodata[3];
        int count = 0;
        int rasterCount = GDALGetRasterCount(handle->in_ds);
        for (int i = 1; i <= rasterCount; ++i) {
            GDALRasterBandH hBand = GDALGetRasterBand(handle->in_ds, i);
            int success;
            double nodataValue = GDALGetRasterNoDataValue(hBand, &success);
            if (success) {
                nodata[ count++ ] = nodataValue;
            }
        }

        nodata_values *ndv = NULL;
        if (count > 0) {
            ndv = malloc(sizeof(*ndv) + count * sizeof(double));
            ndv->len = count;
            memcpy(ndv->values, nodata, count * sizeof(double));
        }
        handle->inNodata = ndv;

        LOG("NODATA: count=%d", count);
        return ATOM_OK;
    }
    else {
        return enif_make_badarg(env);
    }
}

static ERL_NIF_TERM gdal_nifs_warp_dataset(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    LOG("gdal_nifs_warp_dataset is calling");
    gdal_img_handle* handle;
    if (enif_get_resource(env, argv[0], gdal_img_RESOURCE, (void**)&handle)) {
        GDALDatasetH out_ds = GDALAutoCreateWarpedVRT(handle->in_ds, 
                                                      handle->in_srs_wkt, 
                                                      handle->out_srs_wkt, 
                                                      GRA_NearestNeighbour, 
                                                      0.0, 
                                                      NULL);
        handle->out_ds = out_ds;

        double padfTransform[6];
        GDALGetGeoTransform(out_ds, padfTransform);
        if (padfTransform[2] != 0.0 && padfTransform[4] != 0.0) {
            destroy_img_handle(handle);
            return enif_make_tuple2(env, 
                                    ATOM_ERROR,
                                    enif_make_string(env, 
                                        "Georeference of the raster contains rotation or skew. "
                                        "Such raster is not supported. "
                                        "Please use gdalwarp first",
                                        ERL_NIF_LATIN1));
        }

        return enif_make_tuple2(env,
                                ATOM_OK,
                                enif_make_tuple6(env, 
                                    enif_make_double(env, padfTransform[0]),               // OriginX 
                                    enif_make_double(env, padfTransform[3]),               // OriginY
                                    enif_make_double(env, padfTransform[1]),               // PixelXSize
                                    enif_make_double(env, padfTransform[5]),               // PixelYSize
                                    enif_make_int(env, GDALGetRasterXSize(handle->out_ds)), // RasterXSize
                                    enif_make_int(env, GDALGetRasterYSize(handle->out_ds))) // RasterYSize
                );
    }
    else {
        return enif_make_badarg(env);
    }
}

static ERL_NIF_TERM gdal_nifs_calc_srs(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    LOG("gdal_nifs_calc_srs is calling");
    gdal_img_handle* handle;
    if (enif_get_resource(env, argv[0], gdal_img_RESOURCE, (void**)&handle)) {
        const char* in_srs_wkt = GDALGetProjectionRef(handle->in_ds);
        if (in_srs_wkt == NULL && GDALGetGCPCount(handle->in_ds) != 0) {
            in_srs_wkt = GDALGetGCPProjection(handle->in_ds);
        }
        handle->in_srs_wkt = in_srs_wkt;

        OGRSpatialReferenceH in_srs = OSRNewSpatialReference(NULL);
        if (in_srs_wkt != NULL) {
            char* wkt = (char*)in_srs_wkt;
            OSRImportFromWkt(in_srs, &wkt);
        }
        handle->in_srs = in_srs;

        OGRSpatialReferenceH out_srs = OSRNewSpatialReference(NULL);
        OSRImportFromEPSG(out_srs, 900913);
        char* out_srs_wkt;
        OSRExportToWkt(out_srs, &out_srs_wkt);
        handle->out_srs = out_srs;
        handle->out_srs_wkt = out_srs_wkt;

        return ATOM_OK;
    }
    else {
        return enif_make_badarg(env);
    }
}

static ERL_NIF_TERM gdal_nifs_calc_data_bandscount(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) 
{
    LOG(" is calling");
    gdal_img_handle* handle;

    if (enif_get_resource(env, argv[0], gdal_img_RESOURCE, (void**)&handle)) {
        unsigned int dataBandsCount;
        handle->alphaBand = GDALGetMaskBand(GDALGetRasterBand(handle->out_ds, 1));
        int rasterCount = GDALGetRasterCount(handle->out_ds);
        if (GDALGetMaskFlags(handle->alphaBand) & GMF_ALPHA || rasterCount == 4 || rasterCount == 2) {
            dataBandsCount = rasterCount - 1;
        }
        else {
            dataBandsCount = rasterCount;
        }

        handle->dataBandsCount = dataBandsCount;
        handle->tilebands = dataBandsCount + 1;

        return enif_make_uint(env, dataBandsCount);
    }
    else {
        return enif_make_badarg(env);
    }
}

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

static CPLErr scale_query_to_tile(GDALDatasetH dsquery, GDALDatasetH dstile, 
                        const char* const tilefilename, const char* options_resampling)
{
    LOG("Scales down query dataset to the tile dataset");

    int querysize = GDALGetRasterXSize(dsquery);
    int tilesize = GDALGetRasterXSize(dstile);
    int tilebands = GDALGetRasterCount(dstile);
    LOG(" is calling ... tilefilename = %s, querysize: %d, tilesize: %d, tilebands: %d", 
            tilefilename, querysize, tilesize, tilebands);

    if (options_resampling && strcmp("average", options_resampling) == 0) {
        LOG("sample average");
        for (int i = 1; i < tilebands + 1; ++i) {
            CPLErrorReset();

            GDALRasterBandH overviewBand = GDALGetRasterBand(dstile, i);
            CPLErr eErr = GDALRegenerateOverviews(GDALGetRasterBand(dsquery, i), 1, &overviewBand, "average", NULL, NULL);
            if (eErr != CE_None) {
                LOG("GDALRegenerateOverviews failed on %s(band: %d), error: %s", tilefilename, i, CPLGetLastErrorMsg());
                return eErr;
            }
        }
    }
    else if (options_resampling && strcmp("antialias", options_resampling) == 0) {
        // TODO
        LOG("Scaling by PIL (Python Imaging Library) - improved Lanczos");
    }
    else {
        LOG("other sampling algorithms");
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
            LOG("GDALReprojectImage failed on %s, error %s", tilefilename, CPLGetLastErrorMsg());
            return eErr;
        }
    }

    return CE_None;
}

static void fill_pband_list(int n, int band_list[n])
{
    for (int i = 0; i < n; ++i) {
        band_list[i] = i + 1;
    }
}

static int get_bandregion_from(ErlNifEnv* env, const ERL_NIF_TERM *pterm, bandregion* pbr) 
{
    int rarity;
    const ERL_NIF_TERM* r;
    int res = enif_get_tuple(env, *pterm, &rarity, &r);
    if (res) {
        enif_get_int(env, r[0], &(pbr->xoffset));
        enif_get_int(env, r[1], &(pbr->yoffset));
        enif_get_int(env, r[2], &(pbr->xsize));
        enif_get_int(env, r[3], &(pbr->ysize));
        //LOG("rx=%d, ry=%d, rxsize=%d, rysize=%d", rx, ry, rxsize, rysize);
    }
    return res;
}

// free temp data & alpha binary
static void free_temp_data(gdal_tile_handle* hTile)
{
    if (hTile && hTile->data != NULL) {
        CPLFree(hTile->data);
        hTile->data = NULL;
    }
    if (hTile && hTile->alpha != NULL) {
        CPLFree(hTile->alpha);
        hTile->alpha = NULL;
    }
}

static void free_tile(gdal_tile_handle* hTile) 
{
    LOG("free and close resource");
    if (hTile && hTile->dstile != NULL) {
        GDALClose(hTile->dstile);
        hTile->dstile = NULL;
    }
    
    free_temp_data(hTile);

    if (hTile && hTile->tilefilename != NULL) {
        free(hTile->tilefilename);
        hTile->tilefilename = NULL;
    }
}

static ERL_NIF_TERM gdal_nifs_build_tile(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    gdal_tile_handle* hTile;
    if (!enif_get_resource(env, argv[0], gdal_tile_RESOURCE, (void**)&hTile)) {
        return enif_make_badarg(env);
    }
    
    gdal_priv_data* priv_data = (gdal_priv_data*) enif_priv_data(env);   
    GDALDriverH  hMemDriver = priv_data->hMemDriver;

    GDALDatasetH dstile = hTile->dstile;
    GByte* data = hTile->data;
    GByte* alpha = hTile->alpha;
    const char* const tilefilename = hTile->tilefilename;
    bandregion w = hTile->w;

    CPLErr eErr = CE_None;
    if (hTile->tilesize == hTile->querysize) {
        LOG("tilesize(%d) == querysize(%d)", hTile->tilesize, hTile->querysize);
        //GDALDataType ntype = GDALGetRasterDataType( GDALGetRasterBand( dstile, GDALGetRasterCount(dstile) - 1 ) );
        // Use the ReadRaster result directly in tiles ('nearest neighbour' query)
        CPLErrorReset();
        eErr = GDALDatasetRasterIO(dstile, GF_Write,
                                   w.xoffset, w.yoffset, w.xsize, w.ysize, data, 
                                   w.xsize, w.ysize, GDT_Byte, 0, NULL, 
                                   0, 0, 0);
        if (eErr == CE_Failure) {
            LOG("data WriteRaster failed: %s", CPLGetLastErrorMsg());
            return enif_make_badarg(env);
        }

        CPLErrorReset();
        int pBandList[] = {hTile->tilebands};
        eErr = GDALDatasetRasterIO(dstile, GF_Write,
                                   w.xoffset, w.yoffset, w.xsize, w.ysize, alpha, 
                                   w.xsize, w.ysize, GDT_Byte, 0, pBandList, 
                                   0, 0, 0);
        if (eErr == CE_Failure) {
            LOG("alpha WriteRaster failed: %s", CPLGetLastErrorMsg());
            return enif_make_badarg(env);
        }

        // Note: For source drivers based on WaveLet compression (JPEG2000, ECW, MrSID)
        // the ReadRaster function returns high-quality raster (not ugly nearest neighbour)
        // TODO: Use directly 'near' for WaveLet files
    }
    else {
        LOG("tilesize(%d) != querysize(%d)", hTile->tilesize, hTile->querysize);

        CPLErrorReset();
        // Big ReadRaster query in memory scaled to the tilesize - all but 'near' algo
        LOG("create dsquery MEM dataset");
        GDALDatasetH dsquery = GDALCreate(hMemDriver, "", 
                                          hTile->querysize, hTile->querysize, hTile->tilebands, 
                                          GDT_Byte, NULL);
        if (eErr == CE_Failure) {
            LOG("failed to create dsquery MEM dataset");
            return enif_make_badarg(env);
        }

        CPLErrorReset();

        int band_list[hTile->dataBandsCount];
        fill_pband_list(hTile->dataBandsCount, band_list);

        eErr = GDALDatasetRasterIO(dsquery, GF_Write,
                                   w.xoffset, w.yoffset, w.xsize, w.ysize, data, 
                                   w.xsize, w.ysize, GDT_Byte, 
                                   hTile->dataBandsCount, band_list,
                                   0, 0, 0);
        if (eErr == CE_Failure) {
            LOG("data WriteRaster(wx: %d, wy: %d, wxsize: %d, wysize: %d, data: %p) for dsquery failed: %s", 
                    w.xoffset, w.yoffset, w.xsize, w.ysize, data,
                    CPLGetLastErrorMsg());

            GDALClose(dsquery);
            return enif_make_badarg(env);
        }

        CPLErrorReset();
        int pBandList[] = {hTile->tilebands};
        eErr = GDALDatasetRasterIO(dsquery, GF_Write,
                                   w.xoffset, w.yoffset, w.xsize, w.ysize, alpha, 
                                   w.xsize, w.ysize, GDT_Byte, 
                                   1, pBandList, 
                                   0, 0, 0);
        if (eErr == CE_Failure) {
            LOG("alpha WriteRaster dsquery failed: %s", CPLGetLastErrorMsg());

            GDALClose(dsquery);
            return enif_make_badarg(env);
        }

        CPLErrorReset();
        eErr = scale_query_to_tile(dsquery, dstile, tilefilename, hTile->options_resampling);
        GDALClose(dsquery);

        if (eErr == CE_Failure) {
            LOG("scale_query_to_tile failed: %s", CPLGetLastErrorMsg());
            return enif_make_badarg(env);
        }
    }

    free_temp_data(hTile);

    return ATOM_OK;
}

static ERL_NIF_TERM gdal_nifs_save_tile(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    LOG("Write a copy of tile to png/jpg");

    gdal_tile_handle* ti;
    if (!enif_get_resource(env, argv[0], gdal_tile_RESOURCE, (void**)&ti)) {
        return enif_make_badarg(env);
    }
    
    gdal_priv_data* priv_data = (gdal_priv_data*) enif_priv_data(env);   
    GDALDriverH  hOutDriver = priv_data->hOutDriver;

    if ( ! ti->options_resampling || (strcmp("antialias", ti->options_resampling) != 0) ) {
        GDALDatasetH tileDataset = GDALCreateCopy(hOutDriver,
                                                  ti->tilefilename, ti->dstile, 
                                                  FALSE, NULL, NULL, NULL);
        GDALClose(tileDataset);
    }

    return ATOM_OK;
}

static ERL_NIF_TERM gdal_nifs_copyout_tile(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    LOG("is calling, arity = %d", argc);

    gdal_priv_data* priv_data = (gdal_priv_data*) enif_priv_data(env);   
    GDALDriverH  hMemDriver = priv_data->hMemDriver;

    GDALDatasetH ds = NULL;
    gdal_img_handle* hImg = NULL;
    if (enif_get_resource(env, argv[0], gdal_img_RESOURCE, (void**)&hImg)) {
        ds = hImg->out_ds;
    }
    else {
        return enif_make_badarg(env);
    }
        
    bandregion r, w;
    if (!get_bandregion_from(env, argv + 1, &r) || !get_bandregion_from(env, argv + 2, &w)) {
        return enif_make_badarg(env);
    }

    char* const tilefilename = calloc(256, sizeof(char));
    enif_get_string(env, argv[3], tilefilename, 256, ERL_NIF_LATIN1);
    LOG("tilefilename: %s", tilefilename);

    gdal_tile_handle* hTile = enif_alloc_resource(gdal_tile_RESOURCE, sizeof(*hTile));
    *hTile = (gdal_tile_handle) {
        .dstile = NULL,
        .data = NULL,
        .alpha = NULL,
        .tilefilename = tilefilename,
        .w = w,
        .querysize = hImg->querysize,
        .tilesize = hImg->tilesize,
        .dataBandsCount = hImg->dataBandsCount,
        .tilebands = hImg->tilebands,
        .options_resampling = hImg->options_resampling
    };

    ERL_NIF_TERM res = enif_make_resource(env, hTile);
    enif_release_resource(hTile);  // hTile resource now only owned by "Erlang"

    LOG("Tile dataset in memory");
    CPLErrorReset();
    hTile->dstile = GDALCreate(hMemDriver, "",
                               hImg->tilesize, hImg->tilesize, hImg->tilebands, 
                               GDT_Byte, NULL);

    hTile->data = (GByte*)CPLCalloc(w.xsize * w.ysize * hImg->dataBandsCount, sizeof(GByte));

    int panBandMap[hImg->dataBandsCount];
    fill_pband_list(hImg->dataBandsCount, panBandMap);

    CPLErrorReset();
    CPLErr eErr = GDALDatasetRasterIO(ds, GF_Read, 
                                      r.xoffset, r.yoffset, r.xsize, r.ysize, hTile->data, 
                                      w.xsize, w.ysize, GDT_Byte, hImg->dataBandsCount, panBandMap, 
                                      0, 0, 0);
    if (eErr == CE_Failure) {
//        free_tile(hTile);

        char buf[128] = "DatasetRasterIO read failed: ";
        const char* errmsg = CPLGetLastErrorMsg();
        strncat(buf, errmsg, strlen(errmsg));
        return enif_make_tuple2(env, ATOM_ERROR, enif_make_string(env, buf, ERL_NIF_LATIN1));
    }
    LOG("ds.ReadRaster");

    hTile->alpha = (GByte*)CPLCalloc(w.xsize * w.ysize, sizeof(GByte));

    CPLErrorReset();
    eErr = GDALRasterIO(hImg->alphaBand, GF_Read, 
                        r.xoffset, r.yoffset, r.xsize, r.ysize, hTile->alpha, w.xsize, w.ysize, 
                        GDT_Byte, 0, 0);

    if (eErr == CE_Failure) {
//        free_tile(hTile);

        char buf[128] = "DatasetRasterIO read failed: ";
        const char* errmsg = CPLGetLastErrorMsg();
        strncat(buf, errmsg, strlen(errmsg));
        return enif_make_tuple2(env, ATOM_ERROR, enif_make_string(env, buf, ERL_NIF_LATIN1));
    }
    LOG("self.alphaband.ReadRaster");

    // build_tile(hImg, hTile, hOutDriver, hMemDriver);

    return enif_make_tuple2(env, ATOM_OK, res);
}

static ERL_NIF_TERM gdal_nifs_get_meta(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    LOG("gdal_nifs_get_meta is calling");
    gdal_img_handle* handle;

    if (enif_get_resource(env, argv[0], gdal_img_RESOURCE, (void**)&handle)) {
        GDALDatasetH in_ds = handle->in_ds;
        if (in_ds != NULL) {
            GDALDriverH hDriver = GDALGetDatasetDriver(in_ds);

            ERL_NIF_TERM terms[8];
            int idx = 0;

            terms[idx++] = enif_make_tuple2(env,
                                            enif_make_atom(env, "description"),
                                            enif_make_string(env, GDALGetDescription(in_ds), ERL_NIF_LATIN1));

            char buf[256];
            sprintf(buf, "%s/%s", GDALGetDriverShortName(hDriver), GDALGetDriverLongName(hDriver));
            terms[idx++] = enif_make_tuple2(env, 
                                            enif_make_atom(env, "driver"),
                                            enif_make_string(env, buf, ERL_NIF_LATIN1));

            terms[idx++] = enif_make_tuple2(env, 
                                            enif_make_atom(env, "rasterSize"), 
                                            enif_make_tuple2(env, 
                                                enif_make_int(env, GDALGetRasterXSize(in_ds)), 
                                                enif_make_int(env, GDALGetRasterYSize(in_ds))));

            terms[idx++] = enif_make_tuple2(env, 
                                            enif_make_atom(env, "rasterCount"),
                                                enif_make_int(env, GDALGetRasterCount(in_ds)));

            double adfGeoTransform[6];
            if( GDALGetGeoTransform( in_ds, adfGeoTransform ) == CE_None ) {
                terms[idx++] = enif_make_tuple2(env,
                                                enif_make_atom(env, "origin"),
                                                enif_make_tuple2(env,
                                                    enif_make_double(env, adfGeoTransform[0]), 
                                                    enif_make_double(env, adfGeoTransform[3])));

                terms[idx++] = enif_make_tuple2(env,
                                                enif_make_atom(env, "pixelSize"), 
                                                enif_make_tuple2(env, 
                                                    enif_make_double(env, adfGeoTransform[1]), 
                                                    enif_make_double(env, adfGeoTransform[5])));
            }

            if (GDALGetProjectionRef(in_ds) != NULL) {
                terms[idx++] = enif_make_tuple2(env,
                                                enif_make_atom(env, "projection"), 
                                                enif_make_string(env, 
                                                    GDALGetProjectionRef(in_ds), ERL_NIF_LATIN1));
            }

            char** fileList = GDALGetFileList(in_ds);
            if (fileList != NULL) {
                ERL_NIF_TERM fileTerms[16];
                int fileIdx = 0;
                char** files = fileList;

                LOG("start.... count=%d", CSLCount(fileList));
                do {
                    LOG("file: %p -> %s", files, *files);
                    fileTerms[ fileIdx++ ] = enif_make_string(env, *files, ERL_NIF_LATIN1);
                } while(*(++files)) ;
                CSLDestroy(fileList);

                terms[idx++] = enif_make_tuple2(env,
                                                enif_make_atom(env, "fileList"),
                                                enif_make_list_from_array(env, fileTerms, fileIdx));
            }

            return enif_make_list_from_array(env, terms, idx);
        }
        else {
            return ATOM_NOT_OPEN;
        }
    }
    else {
        return enif_make_badarg(env);
    }
}

static ERL_NIF_TERM gdal_nifs_close_img(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    gdal_img_handle* handle = NULL;

    if (enif_get_resource(env, argv[0], gdal_img_RESOURCE, (void**)&handle)) {
        gdal_nifs_img_resource_cleanup(env, handle);
        return ATOM_OK;
    }
    else {
        return enif_make_badarg(env);
    }
}

static void free_img(gdal_img_handle* handle) {
    LOG("close resource for %p", handle);
    if (!handle) {
        return;
    }

    if (handle->in_srs != NULL) {
        LOG("destringing in_srs: %p", handle->in_srs);
        OSRDestroySpatialReference(handle->in_srs);
        handle->in_srs = NULL;
    }
    
    if (handle->out_srs != NULL) {
        LOG("destringing out_srs: %p", handle->out_srs);
        OSRDestroySpatialReference(handle->out_srs);
        handle->out_srs = NULL;
    }

    if (handle->in_ds != NULL) {
        LOG("close in_ds: %p", handle->in_ds);
        GDALClose(handle->in_ds);
        handle->in_ds = NULL;
    }

    if (handle->out_ds != NULL) {
        LOG("close out_ds: %p", handle->out_ds);
        GDALClose(handle->out_ds);
        handle->out_ds = NULL;
    }

    if (handle->inNodata != NULL) {
        LOG("free inNodata: %p", handle->inNodata);
        free(handle->inNodata);
        handle->inNodata = NULL;
    }

    LOG("resource closed ");
}

static void destroy_img_handle(gdal_img_handle* handle) {
    free_img(handle);
    enif_release_resource(handle);
}

static void gdal_nifs_img_resource_cleanup(ErlNifEnv* env, void* arg)
{
    LOG("datasets resource cleaning for %p", arg);
    gdal_img_handle* handle = (gdal_img_handle*)arg;
    free_img(handle);
}

static void gdal_nifs_tile_resource_cleanup(ErlNifEnv* env, void* arg)
{
    LOG("Tile resource cleaning for %p", arg);
    gdal_tile_handle* ti = (gdal_tile_handle*)arg;
    free_tile(ti);
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    OPEN_LOGER();

    gdal_img_RESOURCE = enif_open_resource_type(env, NULL, "gdal_img_resource",
                                                &gdal_nifs_img_resource_cleanup,
                                                ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER,
                                                NULL);

    gdal_tile_RESOURCE = enif_open_resource_type(env, NULL, "gdal_tile_resource",
                                                 &gdal_nifs_tile_resource_cleanup,
                                                 ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER,
                                                 NULL);

    // Register all known configured GDAL drivers
    GDALAllRegister();

    gdal_priv_data* priv = enif_alloc(sizeof(*priv));
    priv->tiledriver = "PNG";
    priv->hOutDriver = GDALGetDriverByName(priv->tiledriver);
    priv->hMemDriver = GDALGetDriverByName("MEM");
    *priv_data = priv;

    if (priv->hOutDriver == NULL) {
        LOG("The '%s' driver was not found, is it available in this GDAL build?", priv->tiledriver);
    }
    if (priv->hMemDriver == NULL) {
        LOG("The 'MEM' driver was not found, is it available in this GDAL build?");
    }

    // Initialize atoms that we use throughout the NIF.
    ATOM_ALLOCATION_ERROR = enif_make_atom(env, "allocation_error");
    ATOM_ERROR = enif_make_atom(env, "error");
    ATOM_FALSE = enif_make_atom(env, "false");
    ATOM_TRUE = enif_make_atom(env, "true");
    ATOM_OK = enif_make_atom(env, "ok");
    ATOM_NOT_OPEN = enif_make_atom(env, "not_open");
    ATOM_NOT_FOUND = enif_make_atom(env, "not_found");

    return 0;
}

static void on_unload(ErlNifEnv* env, void* priv_data)
{
    CLOSE_LOGER();
}

ERL_NIF_INIT(gdal_nifs, nif_funcs, &on_load, NULL, NULL, &on_unload);
