-module(comment).

-include("feed.hrl").

-export([add/3, add/4, select_by_entry_id/1]).

add(EId, User, Value) ->
    add(EId, User, Value, []).

add(EId, User, Value, Medias) ->
    CId = zealot_db:next_id("comment"),
    R = #comment{
      id = {CId, EId},
      comment_id = CId,
      entry_id = EId,
      content = Value,
      author_id = User,
      media = Medias,
      create_time = now()
     },
    case zealot_db:put(R) of
        ok ->
            {ok, R};
        Other ->
            Other
    end.

select_by_entry_id(EntryId) ->
    zealot_db:comments_by_entry(EntryId).
