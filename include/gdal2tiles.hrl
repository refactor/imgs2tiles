%% QuerySize: How big should be query window be for scaling down
%% Later on reset according the chosen resampling algorightm
-include_lib("geo_utils/include/mercator_tiles.hrl").

-type sizeinfo() :: {QuerySize::non_neg_integer(), TileSize::non_neg_integer()}.

-type img()  :: reference().
-type tile() :: reference().
-type tile_rawdata() :: reference().

-record(imghandler, { filename :: string(), 
                      img :: img(),
                      rasterinfo :: rasterinfo(),
                      sizeinfo :: sizeinfo()}).

