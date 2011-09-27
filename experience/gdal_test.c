#include "gdal.h"
#include "cpl_conv.h"
#include "cpl_string.h"

#include "ogr_srs_api.h"
#include "gdalwarper.h"

static void printMeta(GDALDriverH hDataset);
static void printBand(GDALRasterBandH hBand);

int main(int argc, char* argv[]) 
{
    printf("sizeof(long): %zu, sizeof(*void): %zu\n\n", sizeof(long), sizeof(void*));

    if (argc < 2) {
        printf( "no input file\n");
        exit(0);
    }

    GDALAllRegister();

    GDALDatasetH hDataset = GDALOpen(argv[1], GA_ReadOnly);
    if (hDataset == NULL) {
        printf("no gdal support file: %s\n", argv[1]);
        exit(0);
    }
    printMeta(hDataset);

    GDALRasterBandH hBand = GDALGetRasterBand(hDataset, 1);
    printBand(hBand);

    const char* in_srs_wkt = GDALGetProjectionRef(hDataset);
    OGRSpatialReferenceH out_srs = OSRNewSpatialReference(NULL);
    OSRImportFromEPSG(out_srs, 900913);
    char* out_srs_wkt;
    OSRExportToWkt(out_srs, &out_srs_wkt);
    GDALDatasetH out_ds = GDALAutoCreateWarpedVRT(hDataset,
                                                  in_srs_wkt,
                                                  out_srs_wkt,
                                                  GRA_NearestNeighbour,
                                                  0.0,
                                                  NULL);


    GDALClose(hDataset);
    //GDALClose(out_ds);

    return 0;
}

static void printBand(GDALRasterBandH hBand)
{
    int nBlockXSize, nBlockYSize;
    int bGotMin, bGotMax;
    double adfMinMax[2];
    GDALGetBlockSize(hBand, &nBlockXSize, &nBlockYSize);
    printf("Block=%dx%d Type=%s, ColorInterp=%s\n", nBlockXSize, nBlockYSize,
            GDALGetDataTypeName(GDALGetRasterDataType(hBand)), GDALGetColorInterpretationName(GDALGetRasterColorInterpretation(hBand)));

    adfMinMax[0] = GDALGetRasterMinimum( hBand, &bGotMin );
    adfMinMax[1] = GDALGetRasterMaximum( hBand, &bGotMax );
    if( ! (bGotMin && bGotMax) )
        GDALComputeRasterMinMax( hBand, TRUE, adfMinMax );
    printf( "Min=%.3fd, Max=%.3f\n", adfMinMax[0], adfMinMax[1] );

    if( GDALGetOverviewCount(hBand) > 0 )
        printf( "Band has %d overviews.\n", GDALGetOverviewCount(hBand));

    if( GDALGetRasterColorTable( hBand ) != NULL )
        printf( "Band has a color table with %d entries.\n", 
                GDALGetColorEntryCount( GDALGetRasterColorTable( hBand ) ) );
}

static 
void printMeta(GDALDriverH hDataset)
{
    GDALDriverH hDriver = GDALGetDatasetDriver(hDataset);
    printf("Driver CreationOptions: %s\n", GDALGetDriverCreationOptionList(hDriver));
    printf("Driver: %s/%s\n", GDALGetDriverShortName(hDriver), GDALGetDriverLongName(hDriver));
    printf("Size is %dx%dx%d\n", GDALGetRasterXSize(hDataset), GDALGetRasterYSize(hDataset), GDALGetRasterCount(hDataset));
    if (GDALGetProjectionRef(hDataset) != NULL) {
        printf("Projection is '%s'\n", GDALGetProjectionRef(hDataset));
    }

    const char* descs[6] = {
        "pixel leftest corner",
        "pixel width",
        "",
        "pixel uppest corner",
        "",
        "pixel height"
    };
    double        adfGeoTransform[6];
    if( GDALGetGeoTransform( hDataset, adfGeoTransform ) == CE_None ) {
         printf( "Origin = (%.6f,%.6f)\n",  adfGeoTransform[0], adfGeoTransform[3] ); 
         printf( "Pixel Size = (%.6f,%.6f)\n",  adfGeoTransform[1], adfGeoTransform[5] );
        for (int i = 0; i < 6; ++i) {
            printf("padf transform[%d]: %f\t%s\n", i, adfGeoTransform[i], descs[i]);
        }
    }

    printf("GCP projection: %s\n", GDALGetGCPProjection(hDataset));

    CPLErrorReset();
    const GDAL_GCP* gcp = GDALGetGCPs(hDataset);

    if (gcp != NULL) {
 //       fprintf( stderr, "GDALGetGCPs failed - %d  %d\n%s\n", CPLGetLastErrorType(),
 //                 CPLGetLastErrorNo(), CPLGetLastErrorMsg() );
        printf("GCP pszId: %s, pszInfo: %s\n", 
                gcp->pszId, gcp->pszInfo);
        printf("\t dfGCPPixel: %f, dfGCPLine: %f, dfGCPX: %f, dfGCPY: %f, dfGCPZ: %f\n",
                gcp->dfGCPPixel, gcp->dfGCPLine, gcp->dfGCPX, gcp->dfGCPY, gcp->dfGCPZ);
    }

    char **papszMetadata = GDALGetMetadata(hDriver, NULL);
    if (CSLFetchBoolean(papszMetadata, GDAL_DCAP_CREATE, FALSE))
        printf("Driver %s supports Create() method.\n", GDALGetDriverLongName(hDriver));
    if (CSLFetchBoolean(papszMetadata, GDAL_DCAP_CREATECOPY, FALSE))
        printf("Driver %s supports CreateCopy() method.\n", GDALGetDriverLongName(hDriver));

}
