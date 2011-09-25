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

#include "gdal_nif_util.h"
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
static ERL_NIF_TERM gdal_nifs_calc_data_bandscount(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM gdal_nifs_copyout_tile(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM gdal_nifs_generate_overview_tile(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
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

    {"copyout_tile", 3, gdal_nifs_copyout_tile},
    {"build_tile", 1, gdal_nifs_build_tile},
    {"save_tile", 2, gdal_nifs_save_tile},
    {"generate_overview_tile", 4, gdal_nifs_generate_overview_tile},

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
    DEBUG("gdal_nifs_calc_nodatavalue is calling\r\n");
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

        DEBUG("NODATA: count=%d\r\n", count);
        return ATOM_OK;
    }
    else {
        return enif_make_badarg(env);
    }
}

static ERL_NIF_TERM gdal_nifs_warp_dataset(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    DEBUG("gdal_nifs_warp_dataset is calling\r\n");
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
    DEBUG("gdal_nifs_calc_srs is calling\r\n");
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
    DEBUG(" is calling\r\n");
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
        //DEBUG("rx=%d, ry=%d, rxsize=%d, rysize=%d\r\n", rx, ry, rxsize, rysize);
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
    DEBUG("free and close resource\r\n");
    if (hTile && hTile->dstile != NULL) {
        GDALClose(hTile->dstile);
        hTile->dstile = NULL;
    }
    
    free_temp_data(hTile);
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
    bandregion w = hTile->w;

    CPLErr eErr = CE_None;
    if (hTile->tilesize == hTile->querysize) {
        DEBUG("tilesize(%d) == querysize(%d)\r\n", hTile->tilesize, hTile->querysize);
        //GDALDataType ntype = GDALGetRasterDataType( GDALGetRasterBand( dstile, GDALGetRasterCount(dstile) - 1 ) );
        // Use the ReadRaster result directly in tiles ('nearest neighbour' query)
        int xoffset = w.xoffset;
        int yoffset = w.yoffset;
        int xsize = w.xsize;
        int ysize = w.ysize;
        int dataBandsCount = hTile->dataBandsCount;
        int tilebands = hTile->tilebands;
        eErr = write_data_and_alpha_to_raster(dstile, xoffset, yoffset, xsize, ysize, data, alpha, dataBandsCount, tilebands);
        if (eErr == CE_Failure) {
            DEBUG("data or alpha WriteRaster failed: %s\r\n", CPLGetLastErrorMsg());
            return enif_make_badarg(env);
        }

        // Note: For source drivers based on WaveLet compression (JPEG2000, ECW, MrSID)
        // the ReadRaster function returns high-quality raster (not ugly nearest neighbour)
        // TODO: Use directly 'near' for WaveLet files
    }
    else {
        DEBUG("tilesize(%d) != querysize(%d)\r\n", hTile->tilesize, hTile->querysize);

        CPLErrorReset();
        // Big ReadRaster query in memory scaled to the tilesize - all but 'near' algo
        DEBUG("create dsquery MEM dataset\r\n");
        GDALDatasetH dsquery = GDALCreate(hMemDriver, "", 
                                          hTile->querysize, hTile->querysize, hTile->tilebands, 
                                          GDT_Byte, NULL);
        if (eErr == CE_Failure) {
            DEBUG("failed to create dsquery MEM dataset\r\n");
            return enif_make_badarg(env);
        }

        int xoffset = w.xoffset;
        int yoffset = w.yoffset;
        int xsize = w.xsize;
        int ysize = w.ysize;
        int dataBandsCount = hTile->dataBandsCount;
        int tilebands = hTile->tilebands;
        eErr = write_data_and_alpha_to_raster(dsquery, xoffset, yoffset, xsize, ysize, data, alpha, dataBandsCount, tilebands);
        if (eErr == CE_Failure) {
            DEBUG("data or alpha WriteRaster(wx: %d, wy: %d, wxsize: %d, wysize: %d, data: %p) for dsquery failed: %s\r\n", 
                    w.xoffset, w.yoffset, w.xsize, w.ysize, data,
                    CPLGetLastErrorMsg());

            GDALClose(dsquery);
            return enif_make_badarg(env);
        }

        CPLErrorReset();
        eErr = scale_query_to_tile(dsquery, dstile, hTile->options_resampling);
        GDALClose(dsquery);

        if (eErr == CE_Failure) {
            DEBUG("scale_query_to_tile failed: %s\r\n", CPLGetLastErrorMsg());
            return enif_make_badarg(env);
        }
    }

    free_temp_data(hTile);  //DO NOT free, it wiil be reused when generate overview tile.

    return ATOM_OK;
}

static ERL_NIF_TERM gdal_nifs_generate_overview_tile(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    gdal_tile_handle* hTiles[4];
    if (!enif_get_resource(env, argv[0], gdal_tile_RESOURCE, (void**)&hTiles[0])) {
        return enif_make_badarg(env);
    }
    
    if (!enif_get_resource(env, argv[1], gdal_tile_RESOURCE, (void**)&hTiles[1])) {
        return enif_make_badarg(env);
    }
    
    if (!enif_get_resource(env, argv[2], gdal_tile_RESOURCE, (void**)&hTiles[2])) {
        return enif_make_badarg(env);
    }
    
    if (!enif_get_resource(env, argv[3], gdal_tile_RESOURCE, (void**)&hTiles[3])) {
        return enif_make_badarg(env);
    }
    
    const int tilesize = 256; //hTiles[0]->tilesize;
    const int tilebands = hTiles[0]->tilebands;
    const int querysize = 512; //hTiles[0]->querysize;
    const int dataBandsCount = hTiles[0]->dataBandsCount;
    const char* const options_resampling = hTiles[0]->options_resampling;
    DEBUG("tilesize: %d, tilebands: %d, querysize: %d, dataBandsCount: %d\r\n", tilesize, tilebands, querysize, dataBandsCount);

    gdal_priv_data* priv_data = (gdal_priv_data*) enif_priv_data(env);   
    GDALDriverH  hMemDriver = priv_data->hMemDriver;
    
    
    CPLErr eErr = CE_None;
    CPLErrorReset();
    // Big ReadRaster query in memory scaled to the tilesize - all but 'near' algo
    GDALDatasetH dsquery = GDALCreate(hMemDriver, "", 
                                      2*tilesize, 2*tilesize, tilebands,
                                      GDT_Byte, NULL);
    if (eErr == CE_Failure) {
        DEBUG("failed to create dsquery MEM dataset\r\n");
        return enif_make_badarg(env);
    }

    int tileposx = 0, tileposy = 0;
    for (int i = 0; i < 4; ++i) {
        tileposx = (0x01 & i) * tilesize;
        tileposy = ((0x02 & i) >> 1) * tilesize;
        DEBUG("tiles[%d].x: %d, y: %d\r\n", i, tileposx, tileposy);
        if (hTiles[i]->dstile != NULL) {
            eErr = write_quadtree_tile_to_raster(dsquery, tileposx, tileposy, tilesize, tilesize, 
                                                 hTiles[i]->dstile);
/*
            eErr = write_data_and_alpha_to_raster(dsquery, tileposx, tileposy, tilesize, tilesize, 
                                                  hTiles[i]->data, hTiles[i]->alpha,
                                                  hTiles[i]->dataBandsCount, hTiles[i]->tilebands);
// */
            if (eErr == CE_Failure) {
                DEBUG("failed to create dsquery MEM dataset\r\n");
                return enif_make_badarg(env);
            }
        }
        else {
            DEBUG("hTiles[%d].dstile SHOULD NOT be NULL\r\n", i);
            return enif_make_badarg(env);
        }
    }

    GDALDatasetH dstile = GDALCreate(hMemDriver, "", 
                                     tilesize, tilesize, tilebands,
                                     GDT_Byte, NULL);
    eErr = scale_query_to_tile(dsquery, dstile, hTiles[0]->options_resampling);
    if (eErr == CE_Failure) {
        DEBUG("scale_query_to_tile failed: %s\r\n", CPLGetLastErrorMsg());
        return enif_make_badarg(env);
    }

    gdal_tile_handle* hTile = enif_alloc_resource(gdal_tile_RESOURCE, sizeof(*hTile));
    *hTile = (gdal_tile_handle) {
        .dstile = dstile,
        .data = NULL,
        .alpha = NULL,
 //       .w = w,
        .querysize = querysize,
        .tilesize = tilesize,
        .dataBandsCount = dataBandsCount,
        .tilebands = tilebands,
        .options_resampling = options_resampling
    };

    ERL_NIF_TERM res = enif_make_resource(env, hTile);
    enif_release_resource(hTile);  // hTile resource now only owned by "Erlang"

    return enif_make_tuple2(env, ATOM_OK, res);
}

static ERL_NIF_TERM gdal_nifs_save_tile(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    DEBUG("Write a copy of tile to png/jpg\r\n");

    gdal_tile_handle* ti;
    if (!enif_get_resource(env, argv[0], gdal_tile_RESOURCE, (void**)&ti)) {
        return enif_make_badarg(env);
    }
    
    char tilefilename[256] = "";
    if (enif_get_string(env, argv[1], tilefilename, 256, ERL_NIF_LATIN1) <= 0) {
        return enif_make_badarg(env);
    }

    gdal_priv_data* priv_data = (gdal_priv_data*) enif_priv_data(env);   
    GDALDriverH  hOutDriver = priv_data->hOutDriver;

    if ( ! ti->options_resampling || (strcmp("antialias", ti->options_resampling) != 0) ) {
        GDALDatasetH tileDataset = GDALCreateCopy(hOutDriver,
                                                  tilefilename, ti->dstile, 
                                                  FALSE, NULL, NULL, NULL);
        GDALClose(tileDataset);
    }

    return ATOM_OK;
}

static ERL_NIF_TERM gdal_nifs_copyout_tile(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    DEBUG("is calling, arity = %d\r\n", argc);

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

    gdal_tile_handle* hTile = enif_alloc_resource(gdal_tile_RESOURCE, sizeof(*hTile));
    *hTile = (gdal_tile_handle) {
        .dstile = NULL,
        .data = NULL,
        .alpha = NULL,
        .w = w,
        .querysize = hImg->querysize,
        .tilesize = hImg->tilesize,
        .dataBandsCount = hImg->dataBandsCount,
        .tilebands = hImg->tilebands,
        .options_resampling = hImg->options_resampling
    };

    ERL_NIF_TERM res = enif_make_resource(env, hTile);
    enif_release_resource(hTile);  // hTile resource now only owned by "Erlang"

    DEBUG("Tile dataset in memory\r\n");
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
    DEBUG("ds.ReadRaster\r\n");

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
    DEBUG("self.alphaband.ReadRaster\r\n");

    // build_tile(hImg, hTile, hOutDriver, hMemDriver);

    return enif_make_tuple2(env, ATOM_OK, res);
}

static ERL_NIF_TERM gdal_nifs_get_meta(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    DEBUG("gdal_nifs_get_meta is calling\r\n");
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

                DEBUG("start.... count=%d\r\n", CSLCount(fileList));
                do {
                    DEBUG("file: %p -> %s\r\n", files, *files);
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
    DEBUG("close resource for %p\r\n", handle);
    if (!handle) {
        return;
    }

    if (handle->in_srs != NULL) {
        DEBUG("destringing in_srs: %p\r\n", handle->in_srs);
        OSRDestroySpatialReference(handle->in_srs);
        handle->in_srs = NULL;
    }
    
    if (handle->out_srs != NULL) {
        DEBUG("destringing out_srs: %p\r\n", handle->out_srs);
        OSRDestroySpatialReference(handle->out_srs);
        handle->out_srs = NULL;
    }

    if (handle->in_ds != NULL) {
        DEBUG("close in_ds: %p\r\n", handle->in_ds);
        GDALClose(handle->in_ds);
        handle->in_ds = NULL;
    }

    if (handle->out_ds != NULL) {
        DEBUG("close out_ds: %p\r\n", handle->out_ds);
        GDALClose(handle->out_ds);
        handle->out_ds = NULL;
    }

    if (handle->inNodata != NULL) {
        DEBUG("free inNodata: %p\r\n", handle->inNodata);
        free(handle->inNodata);
        handle->inNodata = NULL;
    }

    DEBUG("resource closed \r\n");
}

static void destroy_img_handle(gdal_img_handle* handle) {
    free_img(handle);
    enif_release_resource(handle);
}

static void gdal_nifs_img_resource_cleanup(ErlNifEnv* env, void* arg)
{
    DEBUG("datasets resource cleaning for %p\r\n", arg);
    gdal_img_handle* handle = (gdal_img_handle*)arg;
    free_img(handle);
}

static void gdal_nifs_tile_resource_cleanup(ErlNifEnv* env, void* arg)
{
    DEBUG("Tile resource cleaning for %p\r\n", arg);
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
        DEBUG("The '%s' driver was not found, is it available in this GDAL build?\r\n", priv->tiledriver);
    }
    if (priv->hMemDriver == NULL) {
        DEBUG("The 'MEM' driver was not found, is it available in this GDAL build?\r\n");
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
