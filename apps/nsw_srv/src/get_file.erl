%% -*- mode: nitrogen -*-
-module(get_file).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_db/include/feed.hrl").
-include_lib("nsm_db/include/user.hrl").
-include_lib("nsx_config/include/log.hrl").
-include("setup.hrl").


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

    RealEntryId = ling:replace(EntryId, "_", "-"), 
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

    wf:header(<<"Content-Disposition">>, "attachment; filename=\"" ++ Media#media.title ++ "\""),
    wf:content_type(Type),
    Bin.
