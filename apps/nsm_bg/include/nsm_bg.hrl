%%----------------------------------------------------------------------
%% @author Vladimir Baranov <baranoff.vladimir@gmail.com>
%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%% @doc
%%    Notice application definitions file
%% @end
%%--------------------------------------------------------------------

-type route() :: [term()]. %% list representation of the route. For example
                           %% ['X', "Y", 1] will be transformed to <<"X.Y.1">>

%% proplists:get_value shorter
-define(gv, proplists:get_value).

-define(DEAD_LETTER_EXCHANGE, <<"nsm_bg.dead_letter_exchange.fanout">>).
-define(BOOTSTRAP_WORKER_QUEUE, <<"nsm_bg.worker.bootstrap2">>).
-define(REANIMATOR_QUEUE_NAME(Node),list_to_binary(lists:concat(["nsm_bg.reanimator.", Node]))).

%% this name is user to register in gproc and to create/consume queue.
%% for queue name it should be converted to form of
%% <<"feed.worker.Type.Name">>
-define(FEED_WORKER_NAME(Type, Name), [feed, worker, Type, Name]).
