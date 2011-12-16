%% {LeftTopX, LeftTopY, RightBottomX, RightBottomY} = Bound
-type bound() :: {LeftTopX::float(), LeftTopY::float(), RightBottomX::float(), RightBottom::float()}.

-type enclosure() :: {MinX::float(), MinY::float(), MaxX::float(), MaxY::float()}.

%% {OriginX, OriginY, PixelSizeX, PixelSizeY, RasterXSize, RasterYSize},
-type rasterinfo() :: {float(), float(), float(), float(), non_neg_integer(), non_neg_integer()}.

%% XOffset: the pixel offset to the top left corner of the region of the band to be accessed
%% YOffset: The line offset to the top left corner of the region of the band to be accessed. 
%% XSize: The width of the region of the band to be accessed in pixels.
%% YSize: The height of the region of the band to be accessed in lines
-type bandregion() :: {XOffset::non_neg_integer(), YOffset::non_neg_integer(), XSize::non_neg_integer(), YSize::non_neg_integer()}.

-define(TILE_SIZE, 256).

%% QuerySize: How big should be query window be for scaling down
%% Later on reset according the chosen resampling algorightm
-type sizeinfo() :: {QuerySize::non_neg_integer(), TileSize::non_neg_integer()}.

-type img()  :: reference().
-type tile() :: reference().

-record(imghandler, { filename :: string(), 
                      img :: img(),
                      rasterinfo :: rasterinfo(),
                      sizeinfo :: sizeinfo()}).

