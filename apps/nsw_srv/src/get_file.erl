%% -*- mode: nitrogen -*-
-module(get_file).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_srv/include/feed.hrl").
-include_lib("nsm_srv/include/user.hrl").
-include_lib("alog/include/alog.hrl").
-include("setup.hrl").


%PUBLIC BETA maybe we should have a module for this stuff?
replace(String, Dirt, Icecream) ->
    Pos = string:str(String, Dirt),
    case Pos of
        0 ->
            String;
        _ ->
            string:left(String, Pos-1) ++ Icecream ++ replace(string:right(String, length(String) - length(Dirt) - Pos + 1), Dirt, Icecream)
    end.


main() ->
    EntryId = wf:q(fid),    % this is definetly wrong
    CommentId = list_to_integer(wf:q(cid)),
    MediaId = wf:q(mid),


    FeedOwner = wf:state(feed_owner),
    case FeedOwner of
        undefined ->
            {ok, UserInfo} = rpc:call(?APPSERVER_NODE, nsm_users, get_user, [wf:user()]),
            FeedId = UserInfo#user.feed;
        {UserOrGroup, Info} ->
            case UserOrGroup of
                user ->
                    FeedId = Info#user.feed;
                group ->
                    FeedId = Info#group.feed
            end
     end,

    RealEntryId = replace(EntryId, "_", "-"),   % I can't express an amount of bricks I shited seing how riak rejects perfectly good key for no fucking reason

    Media = case CommentId of
		0 -> 
             {ok, Entry} = rpc:call(?APPSERVER_NODE, nsm_db, get, [entry, {RealEntryId, FeedId}]),
		     [Media0|_] = [M || M <- Entry#entry.media, M#media.id =:= MediaId],
		     Media0;
		_ -> {ok, Comment} = rpc:call(?APPSERVER_NODE,nsm_db,comment_by_id,[{CommentId, FeedId}]),   %PUBLIC BETA we don't have these for now
		     [Media1] = [M || M <- Comment#comment.media, M#media.id =:= MediaId],
		     Media1
	    end,

    ?PRINT(Media),

    {attachment, Type} = Media#media.type,
    MediaPath = lists:concat([code:priv_dir(nsw_srv), "/static", Media#media.url]),
    {ok, Bin} = file:read_file(MediaPath),

    wf:header('Content-Disposition', "attachment; filename=\"" ++ Media#media.title ++ "\""),
    wf:content_type(Type),
    Bin.
