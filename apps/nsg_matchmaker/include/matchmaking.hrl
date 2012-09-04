-record(request, {
          ref    :: any(), %% request referense
          game   :: 'GameId()', %% game atom
          pid    :: pid(), %% session pid
          monref :: any(), %% monitor of session
          data   :: any()  %% data used by ?LIB
         }).
