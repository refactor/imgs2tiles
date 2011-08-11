#include "ogr_srs_api.h"


int main()
{
    OGRSpatialReferenceH hSRS = OSRNewSpatialReference(NULL);
    OSRImportFromEPSG(hSRS, 3857);


    char *pszWKT = NULL;

    OSRExportToWkt(hSRS, &pszWKT);

    printf("%s\n", pszWKT);
    OGRFree(pszWKT);

    return 0;
}

