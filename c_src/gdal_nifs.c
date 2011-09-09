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

static ErlNifResourceType* gdal_datasets_RESOURCE;

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
    int querysize, tilesize, dataBandsCount, tilebands;

    const char* options_resampling;

    nodata_values* inNodata;
    
    OGRSpatialReferenceH in_srs;
    const char* in_srs_wkt;

    OGRSpatialReferenceH out_srs;
    const char* out_srs_wkt;
} gdal_dataset_handle;

typedef struct
{
    GDALDriverH  hOutDriver;
    GDALDriverH  hMemDriver;
    const char* tiledriver;
} gdal_priv_data;

// Atoms (initialized in on_load)
static ERL_NIF_TERM ATOM_ALLOCATION_ERROR;
static ERL_NIF_TERM ATOM_ERROR;
static ERL_NIF_TERM ATOM_NOT_FOUND;
static ERL_NIF_TERM ATOM_NOT_OPEN;
static ERL_NIF_TERM ATOM_FALSE;
static ERL_NIF_TERM ATOM_TRUE;
static ERL_NIF_TERM ATOM_OK;

static void destroy_handle(gdal_dataset_handle* handle);

static ERL_NIF_TERM gdal_nif_open_img(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    char name[4096];
    size_t name_sz;
    if (enif_get_string(env, argv[0], name, sizeof(name), ERL_NIF_LATIN1)) {
        name_sz = strlen(name);

        GDALDatasetH in_ds = GDALOpen(name, GA_ReadOnly);
        if (in_ds != NULL) {
            gdal_dataset_handle* handle = enif_alloc_resource(
                                                    gdal_datasets_RESOURCE, 
                                                    sizeof(gdal_dataset_handle));
            memset(handle, '\0', sizeof(*handle));
            handle->options_resampling = "average";
            handle->querysize = 256 * 4;
            handle->tilesize = 256;

            handle->in_ds = in_ds;

            int rasterCount = GDALGetRasterCount(in_ds);
            if (rasterCount == 0) {
                destroy_handle(handle);

                const char* msg = "Input file '%s' has no raster band";
                char errstr[name_sz + strlen(msg) + 1];
                sprintf(errstr, msg, name);
                return enif_make_tuple2(env, 
                                        ATOM_ERROR, 
                                        enif_make_string(env, errstr, ERL_NIF_LATIN1));
            }

            GDALRasterBandH hBand = GDALGetRasterBand(in_ds, 1);
            if (GDALGetRasterColorTable(hBand) != NULL) {
                destroy_handle(handle);

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
                destroy_handle(handle);
                return enif_make_tuple2(env, 
                                        ATOM_ERROR,
                                        enif_make_string(env, 
                                            "There is no georeference - neither affine transformation (worldfile) nor GCPs",
                                            ERL_NIF_LATIN1));
            }

            ERL_NIF_TERM result = enif_make_resource(env, handle);
            enif_release_resource(handle);

            return enif_make_tuple2(env, ATOM_OK, result);
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

static ERL_NIF_TERM gdal_nif_calc_nodatavalue(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    LOG("gdal_nif_calc_nodatavalue is calling");
    gdal_dataset_handle* handle;
    if (enif_get_resource(env, argv[0], gdal_datasets_RESOURCE, (void**)&handle)) {
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

static ERL_NIF_TERM gdal_nif_warp_dataset(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    LOG("gdal_nif_warp_dataset is calling");
    gdal_dataset_handle* handle;
    if (enif_get_resource(env, argv[0], gdal_datasets_RESOURCE, (void**)&handle)) {
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
            destroy_handle(handle);
            return enif_make_tuple2(env, 
                                    ATOM_ERROR,
                                    enif_make_string(env, 
                                        "Georeference of the raster contains rotation or skew. Such raster is not supported. Please use gdalwarp first",
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

static ERL_NIF_TERM gdal_nif_calc_srs(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    LOG("gdal_nif_calc_srs is calling");
    gdal_dataset_handle* handle;
    if (enif_get_resource(env, argv[0], gdal_datasets_RESOURCE, (void**)&handle)) {
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

static ERL_NIF_TERM gdal_nif_calc_data_bandscount(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) 
{
    LOG("gdal_nif_calc_data_bandscount is calling");
    gdal_dataset_handle* handle;

    if (enif_get_resource(env, argv[0], gdal_datasets_RESOURCE, (void**)&handle)) {
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

static CPLErr scale_query_to_tile(gdal_dataset_handle* handle, GDALDatasetH dsquery, GDALDatasetH dstile, const char* tilefilename)
{
    LOG("Scales down query dataset to the tile dataset");

    int querysize = GDALGetRasterXSize(dsquery);
    int tilesize = GDALGetRasterXSize(dstile);
    int tilebands = GDALGetRasterCount(dstile);
    LOG("scale_query_to_tile is calling ... tilefilename = %s, querysize: %d, tilesize: %d, tilebands: %d", 
            tilefilename, querysize, tilesize, tilebands);

    if (handle->options_resampling && strcmp("average", handle->options_resampling) == 0) {
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
    else if (handle->options_resampling && strcmp("antialias", handle->options_resampling) == 0) {
        // TODO
        LOG("Scaling by PIL (Python Imaging Library) - improved Lanczos");
    }
    else {
        LOG("other sampling algorithms");
        // Other algorithms are implemented by gdal.ReprojectImage().
        GDALSetGeoTransform(dsquery, (double []){0.0, tilesize / ((double)querysize), 0.0, 0.0, 0.0, tilesize / ((double)querysize)});
        GDALSetGeoTransform(dstile, (double []){0.0, 1.0, 0.0, 0.0, 0.0, 1.0});

        CPLErrorReset();
        CPLErr eErr = GDALReprojectImage(dsquery, NULL, dstile, NULL, parse_resampling(handle->options_resampling), 0.0, 0.0, NULL, NULL, NULL); 
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

static ERL_NIF_TERM gdal_nif_cut_tile(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    LOG("gdal_nif_calc_cut_tile is calling, arity = %d", argc);

    // int querysize = 256 * 4, tilesize = 256, dataBandsCount, tilebands;

    gdal_priv_data* priv_data = (gdal_priv_data*) enif_priv_data(env);   
    GDALDriverH  hOutDriver = priv_data->hOutDriver;
    GDALDriverH  hMemDriver = priv_data->hMemDriver;

    GDALDatasetH ds = NULL;
    gdal_dataset_handle* handle = NULL;
    if (enif_get_resource(env, argv[0], gdal_datasets_RESOURCE, (void**)&handle)) {
        ds = handle->out_ds;
    }
    else {
        return enif_make_badarg(env);
    }
        
    int rx, ry, rxsize, rysize;
    int rarity;
    const ERL_NIF_TERM* r;
    if (enif_get_tuple(env, argv[1], &rarity, &r)) {
        enif_get_int(env, r[0], &rx);
        enif_get_int(env, r[1], &ry);
        enif_get_int(env, r[2], &rxsize);
        enif_get_int(env, r[3], &rysize);
        LOG("rx=%d, ry=%d, rxsize=%d, rysize=%d", rx, ry, rxsize, rysize);
    }
    else {
        return enif_make_badarg(env);
    }

    int wx, wy, wxsize, wysize;
    int warity;
    const ERL_NIF_TERM* w;
    if (enif_get_tuple(env, argv[2], &warity, &w)) {
        enif_get_int(env, w[0], &wx);
        enif_get_int(env, w[1], &wy);
        enif_get_int(env, w[2], &wxsize);
        enif_get_int(env, w[3], &wysize);
        LOG("wx=%d, wy=%d, wxsize=%d, wysize=%d", wx, wy, wxsize, wysize);
    }
    else {
        return enif_make_badarg(env);
    }

    char tilefilename[256];
    enif_get_string(env, argv[3], tilefilename, 256, ERL_NIF_LATIN1);
    LOG("tilefilename: %s", tilefilename);

/* 
    int querysize, tilesize, dataBoundsCount, tilebands;
    int sarity;
    const ERL_NIF_TERM* s;
    if (enif_get_tuple(env, argv[3], &sarity, &s)) {
        enif_get_int(env, s[0], &querysize);
        enif_get_int(env, s[1], &tilesize);
    }
// */        
    LOG("Tile dataset in memory");
    CPLErrorReset();
    GDALDatasetH dstile = GDALCreate(hMemDriver, 
                                     "", handle->tilesize, handle->tilesize, handle->tilebands, 
                                     GDT_Byte, NULL);

    //GByte data[wxsize * wysize * (handle->dataBandsCount)];
    GByte* data = (GByte*)CPLCalloc(wxsize * wysize * handle->dataBandsCount, sizeof(GByte));

    int panBandMap[handle->dataBandsCount];
    fill_pband_list(handle->dataBandsCount, panBandMap);
    LOG("panBandMap[%zu] = {%d, %d, %d}", sizeof(panBandMap)/sizeof(int), panBandMap[0], panBandMap[1], panBandMap[2]);

    CPLErrorReset();
    CPLErr eErr = GDALDatasetRasterIO(ds, GF_Read, 
                                      rx, ry, rxsize, rysize, data, 
                                      wxsize, wysize, GDT_Byte, handle->dataBandsCount, panBandMap, 
                                      0, 0, 0);
    if (eErr == CE_Failure) {
        LOG("DatasetRasterIO read failed: %s", CPLGetLastErrorMsg());
    }
    LOG("ds.ReadRaster");

    //GByte alpha[wxsize * wysize * 1];
    GByte* alpha = (GByte*)CPLCalloc(wxsize * wysize, sizeof(GByte));

    CPLErrorReset();
    eErr = GDALRasterIO(handle->alphaBand, GF_Read, 
                        rx, ry, rxsize, rysize, alpha, wxsize, wysize, 
                        GDT_Byte, 0, 0);

    if (eErr == CE_Failure) {
        LOG("RasterIO read failed: %s", CPLGetLastErrorMsg());
    }
    LOG("self.alphaband.ReadRaster");

    if (handle->tilesize == handle->querysize) {
        LOG("tilesize(%d) == querysize(%d)", handle->tilesize, handle->querysize);
        //GDALDataType ntype = GDALGetRasterDataType( GDALGetRasterBand( dstile, GDALGetRasterCount(dstile) - 1 ) );
        // Use the ReadRaster result directly in tiles ('nearest neighbour' query)
        CPLErrorReset();
        eErr = GDALDatasetRasterIO(dstile, GF_Write,
                            wx, wy, wxsize, wysize, data, 
                            wxsize, wysize, GDT_Byte, 0, NULL, 
                            0, 0, 0);
        if (eErr == CE_Failure) {
            LOG("data WriteRaster failed: %s", CPLGetLastErrorMsg());
        }

        CPLErrorReset();
        int pBandList[] = {handle->tilebands};
        eErr = GDALDatasetRasterIO(dstile, GF_Write,
                                   wx, wy, wxsize, wysize, alpha, 
                                   wxsize, wysize, GDT_Byte, 0, pBandList, 
                                   0, 0, 0);
        if (eErr == CE_Failure) {
            LOG("alpha WriteRaster failed: %s", CPLGetLastErrorMsg());
        }

        // Note: For source drivers based on WaveLet compression (JPEG2000, ECW, MrSID)
        // the ReadRaster function returns high-quality raster (not ugly nearest neighbour)
        // TODO: Use directly 'near' for WaveLet files
    }
    else {
        LOG("tilesize(%d) != querysize(%d)", handle->tilesize, handle->querysize);

        CPLErrorReset();
        // Big ReadRaster query in memory scaled to the tilesize - all but 'near' algo
        GDALDatasetH dsquery = GDALCreate(hMemDriver, 
                                          "", handle->querysize, handle->querysize, handle->tilebands, 
                                          GDT_Byte, NULL);
        LOG("create dsquery MEM dataset");

        CPLErrorReset();

        int band_list[handle->dataBandsCount];
        fill_pband_list(handle->dataBandsCount, band_list);

        eErr = GDALDatasetRasterIO(dsquery, GF_Write,
                            wx, wy, wxsize, wysize, data, 
                            wxsize, wysize, GDT_Byte, 
                            handle->dataBandsCount, band_list,
                            0, 0, 0);
        if (eErr == CE_Failure) {
            LOG("data WriteRaster dsquery failed: %s", CPLGetLastErrorMsg());
            LOG("dsquery.WriteRaster(wx: %d, wy: %d, wxsize: %d, wysize: %d, data: %p, ", wx, wy, wxsize, wysize, data);
        }

        CPLErrorReset();
        int pBandList[] = {handle->tilebands};
        eErr = GDALDatasetRasterIO(dsquery, GF_Write,
                                   wx, wy, wxsize, wysize, alpha, 
                                   wxsize, wysize, GDT_Byte, 
                                   1, pBandList, 
                                   0, 0, 0);
        if (eErr == CE_Failure) {
            LOG("alpha WriteRaster dsquery failed: %s", CPLGetLastErrorMsg());
        }

        scale_query_to_tile(handle, dsquery, dstile, tilefilename);

        GDALClose(dsquery);
    }

    if ( ! handle->options_resampling || (strcmp("antialias", handle->options_resampling) != 0) ) {
        LOG("Write a copy of tile to png/jpg");
        GDALDatasetH tileDataset = GDALCreateCopy(hOutDriver,
                                                  tilefilename, dstile, 
                                                  FALSE, NULL, NULL, NULL);
        GDALClose(tileDataset);
    }

    CPLFree(data);
    data = NULL;
    CPLFree(alpha);
    data = NULL;

    if (handle->options_resampling && strcmp("antialias", handle->options_resampling) != 0) {
        LOG("Write a copy of tile to png/jpg");
    }
    GDALClose(dstile);
    dstile = NULL;

    return ATOM_OK;
}

static ERL_NIF_TERM gdal_nif_get_meta(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    LOG("gdal_nif_get_meta is calling");
    gdal_dataset_handle* handle;

    if (enif_get_resource(env, argv[0], gdal_datasets_RESOURCE, (void**)&handle)) {
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

static ERL_NIF_TERM gdal_nif_close(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    gdal_dataset_handle* handle = NULL;
    if (enif_get_resource(env, argv[0], gdal_datasets_RESOURCE, (void**)&handle)) {
        if (handle != NULL) {
            destroy_handle(handle);
        }
        return ATOM_OK;
    }
    else {
        return enif_make_badarg(env);
    }
}

static void close_resource(gdal_dataset_handle* handle) {
    LOG("close resource for %p", handle);
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

static void destroy_handle(gdal_dataset_handle* handle) {
    close_resource(handle);
    enif_release_resource(handle);
}

static ErlNifFunc nif_funcs[] = 
{
    {"open_img", 1, gdal_nif_open_img},
    {"calc_nodatavalue", 1, gdal_nif_calc_nodatavalue},
    {"calc_srs", 1, gdal_nif_calc_srs},
    {"warp_dataset", 1, gdal_nif_warp_dataset},
    {"calc_data_bandscount", 1, gdal_nif_calc_data_bandscount},
    {"cut_tile", 4, gdal_nif_cut_tile},
    {"get_meta", 1, gdal_nif_get_meta},

    {"close", 1, gdal_nif_close}
};

static void gdal_nifs_resource_cleanup(ErlNifEnv* env, void* arg)
{
    LOG("resource cleaning for %p", arg);
    gdal_dataset_handle* handle = (gdal_dataset_handle*)arg;
    close_resource(handle);

}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    OPEN_LOGER();

    // Register all known configured GDAL drivers
    GDALAllRegister();

    gdal_datasets_RESOURCE = enif_open_resource_type(env, NULL, "gdal_datasets_resource",
                                                     &gdal_nifs_resource_cleanup,
                                                     ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER,
                                                     0);

    gdal_priv_data* priv = enif_alloc(sizeof(gdal_priv_data));
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
