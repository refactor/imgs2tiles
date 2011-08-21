#include "erl_nif.h"

#include "gdal.h"
#include "cpl_conv.h"
#include "cpl_string.h"

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
    GDALDatasetH inDataset;
    nodata_values* inNodata;
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
static ERL_NIF_TERM ATOM_NOT_OPEN;
static ERL_NIF_TERM ATOM_FALSE;
static ERL_NIF_TERM ATOM_TRUE;
static ERL_NIF_TERM ATOM_OK;

ERL_NIF_TERM gdal_nif_open(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    char name[4096];
    size_t name_sz;
    if (enif_get_string(env, argv[0], name, sizeof(name), ERL_NIF_LATIN1)) {
        name_sz = strlen(name);

        GDALDatasetH inDataset = GDALOpen(name, GA_ReadOnly);
        if (inDataset != NULL) {
            int rasterCount = GDALGetRasterCount(inDataset);
            if (rasterCount == 0) {
                const char* msg = "Input file '%s' has no raster band";
                char errstr[name_sz + strlen(msg) + 1];
                sprintf(errstr, msg, name);
                return enif_make_tuple2(env, ATOM_ERROR, 
                        enif_make_string(env, errstr, ERL_NIF_LATIN1));
            }

            GDALRasterBandH hBand = GDALGetRasterBand(inDataset, 1);
            if (GDALGetRasterColorTable(hBand) != NULL) {
                const char* msg = "Please convert this file to RGB/RGBA and run gdal2tiles on the result.\n" 
                    "From paletted file you can create RGBA file (temp.vrt) by:\n"
                    "gdal_translate -of vrt -expand rgba %s temp.vrt\n"
                    "then run this program: gdal2tiles temp.vrt"
                    ;
                char errstr[name_sz + strlen(msg) + 1];
                sprintf(errstr, msg, name);
                
                return enif_make_tuple2(env, ATOM_ERROR, 
                        enif_make_string(env, errstr, ERL_NIF_LATIN1));
            }

            gdal_dataset_handle* handle = enif_alloc_resource(
                                                    gdal_datasets_RESOURCE, 
                                                    sizeof(gdal_dataset_handle));
            memset(handle, '\0', sizeof(*handle));
            handle->inDataset = inDataset;

            // Get NODATA value
            double nodata[3];
            int count = 0;
            for (int i = 1; i <= rasterCount; ++i) {
                GDALRasterBandH hBand = GDALGetRasterBand(inDataset, i);
                int success;
                double nodataValue = GDALGetRasterNoDataValue(hBand, &success);
                if (success) {
                    nodata[ count++ ] = nodataValue;
                }
            }
            if (count > 0) {
                nodata_values *ndv = malloc(sizeof(*ndv) + count * sizeof(double));
                ndv->len = count;
                memcpy(ndv->values, nodata, count * sizeof(double));
                handle->inNodata = ndv;
            }
            LOG("NODATA: count=%d", count);

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

ERL_NIF_TERM gdal_nif_close(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    gdal_dataset_handle* handle;

    if (enif_get_resource(env, argv[0], gdal_datasets_RESOURCE, (void**)&handle)) {
        GDALDatasetH inDataset = handle->inDataset;
        if (inDataset != NULL) {
            GDALClose(inDataset);
            handle->inDataset = NULL;
            if (handle->inNodata != NULL) {
                free(handle->inDataset);
            }
            return ATOM_OK;
        }
        else {
            return enif_make_tuple2(env, 
                        ATOM_ERROR, 
                        enif_make_string(env, "close error", ERL_NIF_LATIN1));
        }
    }
    else {
        return enif_make_badarg(env);
    }
}

ERL_NIF_TERM gdal_nif_get_meta(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    gdal_dataset_handle* handle;

    if (enif_get_resource(env, argv[0], gdal_datasets_RESOURCE, (void**)&handle)) {
        GDALDatasetH inDataset = handle->inDataset;
        if (inDataset != NULL) {
            GDALDriverH hDriver = GDALGetDatasetDriver(inDataset);

            ERL_NIF_TERM terms[8];
            int idx = 0;

            char buf[256];
            sprintf(buf, "%s/%s", GDALGetDriverShortName(hDriver), GDALGetDriverLongName(hDriver));
            terms[idx++] = enif_make_tuple2(env, enif_make_atom(env, "driver"),
                                    enif_make_string(env, buf, ERL_NIF_LATIN1));

            terms[idx++] = enif_make_tuple2(env, enif_make_atom(env, "rasterSize"), 
                                enif_make_tuple2(env, enif_make_int(env, GDALGetRasterXSize(inDataset)), 
                                                    enif_make_int(env, GDALGetRasterYSize(inDataset))));

            terms[idx++] = enif_make_tuple2(env, enif_make_atom(env, "rasterCount"),
                                                        enif_make_int(env, GDALGetRasterCount(inDataset)));

            double adfGeoTransform[6];
            if( GDALGetGeoTransform( inDataset, adfGeoTransform ) == CE_None ) {
                terms[idx++] = enif_make_tuple2(env,enif_make_atom(env, "origin"),
                                                    enif_make_tuple2(env,
                                                        enif_make_double(env, adfGeoTransform[0]), 
                                                        enif_make_double(env, adfGeoTransform[3])));

                terms[idx++] = enif_make_tuple2(env,enif_make_atom(env, "pixelSize"), 
                                                    enif_make_tuple2(env, 
                                                        enif_make_double(env, adfGeoTransform[1]), 
                                                        enif_make_double(env, adfGeoTransform[5])));
            }

            if (GDALGetProjectionRef(inDataset) != NULL) {
                terms[idx++] = enif_make_tuple2(env,enif_make_atom(env, "projection"), 
                                                    enif_make_string(env, 
                                                        GDALGetProjectionRef(inDataset), ERL_NIF_LATIN1));
            }

            char** fileList = GDALGetFileList(inDataset);
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

                terms[idx++] = enif_make_tuple2(env,enif_make_atom(env, "fileList"),
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

static ErlNifFunc nif_funcs[] = 
{
    {"open", 1, gdal_nif_open},
    {"get_meta", 1, gdal_nif_get_meta},
    {"close", 1, gdal_nif_close}
};

static void gdal_nifs_resource_cleanup(ErlNifEnv* env, void* arg)
{
    CLOSE_LOGER();
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
        LOG("The 'MEM' driver was not found, is it available in this GDAL build?", "");
    }

    // Initialize atoms that we use throughout the NIF.
    ATOM_ALLOCATION_ERROR = enif_make_atom(env, "allocation_error");
    ATOM_ERROR = enif_make_atom(env, "error");
    ATOM_FALSE = enif_make_atom(env, "false");
    ATOM_TRUE = enif_make_atom(env, "true");
    ATOM_OK = enif_make_atom(env, "ok");
    ATOM_NOT_OPEN = enif_make_atom(env, "not_open");

    return 0;
}

ERL_NIF_INIT(gdal_nifs, nif_funcs, &on_load, NULL, NULL, NULL);
