#include "gdal.h"
#include "cpl_conv.h"

static void printMeta(GDALDriverH hDataset);

int main(int argc, char* argv[]) 
{
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

    return 0;
}

static 
void printMeta(GDALDriverH hDataset)
{
    GDALDriverH hDriver = GDALGetDatasetDriver(hDataset);
    printf("Driver: %s/%s\n", GDALGetDriverShortName(hDriver), GDALGetDriverLongName(hDriver));
    printf("Size is %dx%dx%d\n", GDALGetRasterXSize(hDataset), GDALGetRasterYSize(hDataset), GDALGetRasterCount(hDataset));
    if (GDALGetProjectionRef(hDataset) != NULL) {
        printf("Projection is '%s'\n", GDALGetProjectionRef(hDataset));
    }

    double        adfGeoTransform[6];
    if( GDALGetGeoTransform( hDataset, adfGeoTransform ) == CE_None ) {
         printf( "Origin = (%.6f,%.6f)\n",  adfGeoTransform[0], adfGeoTransform[3] ); 
         printf( "Pixel Size = (%.6f,%.6f)\n",  adfGeoTransform[1], adfGeoTransform[5] );
    }

}
