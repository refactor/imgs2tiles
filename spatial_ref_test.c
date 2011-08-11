#include "ogr_srs_api.h"


int main()
{
    OGRSpatialReferenceH hSRS = OSRNewSpatialReference(NULL);
    //OSRImportFromEPSG(hSRS, 3857);
    OSRSetProjCS(hSRS, "UTM 17(WGS84) in northern hemisphere.");
    OSRSetWellKnownGeogCS(hSRS, "WGS84");
    OSRSetUTM(hSRS, 17, TRUE);


    char *pszWKT = NULL;

    OSRExportToWkt(hSRS, &pszWKT);
    printf("%s\n", pszWKT);
    printf("isGeographic: %d\n", OSRIsGeographic(hSRS));
    printf("isProjected: %d\n", OSRIsProjected(hSRS));
    OGRFree(pszWKT);

    OSRDestroySpatialReference(hSRS);

    return 0;
}

