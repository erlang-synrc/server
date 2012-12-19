-record(mhits,
        {
         word    :: term(),
         ip      :: term(),
         count   :: integer(),
         date    :: {integer(), integer(), integer()} %% {Year, Month, Day}
        }).
