%%----------------------------------------------------------------------
%% @author Serge Polkovnikov <serge.polkovnikov@gmail.com>
%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%% @doc
%% Gifts related object manipulation API.
%% @end
%% Created: Sep 10, 2012
%%----------------------------------------------------------------------
-module(nsm_gifts_objects).

%%
%% Include files
%%

-include("common.hrl").

%%
%% Exported Functions
%%
-export([
         gift_add_cats/2,
         gift_del_cats/2
        ]).

%%
%% API Functions
%%

%% @spec gift_add_cats(GiftRec, Categories) -> NewGiftRec
%% @doc
%% Types:
%%     GiftRec = NewGiftRec = #gift{}
%%     Categories = [Category]
%%       Catrgory = integer()
%% Links the gift with specified categories.
%% @end

gift_add_cats(#gift{categories = CurCats} = GiftRec, Categories) ->
    NewCats = lists:usort(Categories ++ CurCats),
    GiftRec#gift{categories = NewCats}.

%% @spec gift_del_cats(GiftRec, Categories) -> NewGiftRec
%% @doc
%% Types:
%%     GiftRec = NewGiftRec = #gift{}
%%     Categories = [Category]
%%       Catrgory = integer()
%% Unlink the gift from specified categories.
%% @end

gift_del_cats(#gift{categories = CurCats} = GiftRec, Categories) ->
    NewCats = CurCats -- Categories,
    GiftRec#gift{categories = NewCats}.


%%
%% Local Functions
%%

