#include "gdalwarper.h"
#include "ogr_srs_api.h"
//#include "ogr_spatialref.h"

int main(int argc, char *argv[]) 
{
    if (argc < 2) {
        printf( "no input file\n");
        exit(0);
    }


    GDALAllRegister();

    GDALDatasetH hSrcDS = GDALOpen(argv[1], GA_ReadOnly);
    CPLAssert(hSrcDS != NULL);

    GDALDataType eDT = GDALGetRasterDataType(GDALGetRasterBand(hSrcDS, 1));

    const char *pszSrcWKT = GDALGetProjectionRef(hSrcDS);
    CPLAssert(pszSrcWKT != NULL && strlen(pszSrcWKT) > 0);

    OGRSpatialReferenceH hSRS = OSRNewSpatialReference(NULL);
    OSRImportFromEPSG(hSRS, 3857);
/*
    OSRSetUTM(hSRS, 17, TRUE);
    OSRSetWellKnownGeogCS(hSRS, "WGS84");
//*/

    char *pszDstWKT = NULL;
    OSRExportToWkt(hSRS, &pszDstWKT);

    void*  hTransformArg = 
                GDALCreateGenImgProjTransformer( hSrcDS, pszSrcWKT, NULL, pszDstWKT, 
                        FALSE, 0, 1);
    CPLAssert( hTransformArg != NULL );

    double adfDstGeoTransform[6];
    int nPixels=0, nLines=0;
    CPLErr eErr =  GDALSuggestedWarpOutput( hSrcDS, 
                                            GDALGenImgProjTransform, hTransformArg, 
                                            adfDstGeoTransform, &nPixels, &nLines );
    CPLAssert( eErr == CE_None );

    GDALDestroyGenImgProjTransformer( hTransformArg );
    
    GDALDriverH hDriver = GDALGetDriverByName("GTiff");
    CPLAssert(hDriver != NULL);

    GDALDatasetH hDstDS = GDALCreate( hDriver, "out.tif", nPixels, nLines, 
                                     GDALGetRasterCount(hSrcDS), eDT, NULL );
    CPLAssert( hDstDS != NULL );
    GDALClose(hDstDS);
    GDALClose(hSrcDS);

    return 0;
}

