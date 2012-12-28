-module(attachment_storage).
-author('Yura Zhloba <yzh44yzh@gmail.com>').

-export([place_avatar/4, next_version_for_avatar/1]).
-export([place_feed_raw_file/3, place_feed_thumb/4, place_feed_converted/4]).
-export([create_dir/1, hash_from_file/1]).
-export([test/0]).

-define(ROOT, code:priv_dir(nsp_srv)++"/static/files/").
-include_lib("nitrogen_core/include/wf.hrl").

-include("loger.hrl").

%% @doc
%% define what is the new version for avatar for current user
%% @end
-spec next_version_for_avatar(string()) -> integer().
next_version_for_avatar(UserId) ->
    Dir = lists:concat(["users/user_", UserId, "/avatar/"]),
    ok = create_dir(Dir),
    get_version(1, Dir).


%% @doc
%% Place Avatar (image file) to storage with path
%% /users/user_id/avatar/<version>-<size>.<ext>
%% @end
-spec place_avatar(string(), string(), integer(), atom()) -> {ok, string()} | {error, atom()}.
place_avatar(UserId, Filename, Version, Size) ->
    Dir = lists:concat(["users/user_", UserId, "/avatar/"]),
    ok = create_dir(Dir),
    NewFile = lists:concat([Dir, integer_to_list(Version), "-", atom_to_list(Size), filename:extension(Filename)]),
    case file:copy(Filename, ?ROOT ++ NewFile) of
	{ok, _} -> {ok, NewFile};
	{error, Reason} ->
	    ?ERROR("error:~p file:~p newfile:~p~n", [Reason, Filename, NewFile]),
	    {error, Reason}
    end.


%% @doc
%% Place file (raw) which is attached to feed entry or feed comment to storage with path:
%% /media/user_id/entry_id/raw/<hash>.<ext>
%% @end
-spec place_feed_raw_file(string(), string(), string()) -> {ok, string(), string()} | {error, atom()}.
place_feed_raw_file(UserId, EntryId, Filename) ->
    ?PRINT({"PFRF", UserId, EntryId, Filename}),
    Dir = lists:concat(["media/user_", UserId, "/", EntryId, "/raw/"]),
    ok = create_dir(Dir),
    Hash = hash_from_file(Filename),
    NewFile = lists:concat([Dir, Hash, filename:extension(Filename)]),
    case file:copy(Filename, ?ROOT ++ NewFile) of
	{ok, _} -> {ok, NewFile, Hash};
	{error, Reason} ->
	    ?ERROR("error:~p file:~p newfile:~p~n", [Reason, Filename, NewFile]),
	    {error, Reason}
    end.

%% @doc
%% Place file (thumbnail) which is attached to feed entry or feed comment to storage with path:
%% /media/user_id/entry_id/thumbnail/<hash>.<size>.<ext>
%% @end
-spec place_feed_thumb(string(), string(), string(), atom()) -> {ok, string()} | {error, atom()}.
place_feed_thumb(UserId, EntryId, Filename, Size) ->
    Dir = lists:concat(["media/user_", UserId, "/", EntryId, "/thumbnail/"]),
    ok = create_dir(Dir),
    NewFile = lists:concat([Dir, hash_from_file(Filename), ".", atom_to_list(Size), filename:extension(Filename)]),
    case file:copy(Filename, ?ROOT ++ NewFile) of
	{ok, _} -> {ok, NewFile};
	{error, Reason} ->
	    ?ERROR("error:~p file:~p newfile:~p~n", [Reason, Filename, NewFile]),
	    {error, Reason}
    end.


%% @doc
%% Place file (converted) which is attached to feed entry or feed comment to storage with path:
%% /media/user_id/entry_id/converted/<original-hash>.<ext>
%% @end
-spec place_feed_converted(string(), string(), string(), string()) -> {ok, string()} | {error, atom()}.
place_feed_converted(UserId, EntryId, Filename, OrigHash) ->
    Dir = lists:concat(["media/user_", UserId, "/", EntryId, "/converted/"]),
    ok = create_dir(Dir),
    NewFile = lists:concat([Dir, OrigHash, filename:extension(Filename)]),
    case file:copy(Filename, ?ROOT ++ NewFile) of
	{ok, _} -> {ok, NewFile};
	{error, Reason} ->
	    ?ERROR("error:~p file:~p newfile:~p~n", [Reason, Filename, NewFile]),
	    {error, Reason}
    end.


%% internal functions

create_dir(Dir) ->
    ?PRINT({"Create DIR", Dir, "in", ?ROOT}),
    case filelib:ensure_dir(?ROOT ++ Dir) of
	ok -> ok;
	{error,
	    Reason} -> ?ERROR("error:~p dir:~p~n", [Reason, Dir]),
	    {error, Reason}
    end.


get_version(Version, Dir) ->
    Filename = lists:concat([Dir, integer_to_list(Version), "-*"]),
    case filelib:wildcard(?ROOT ++ Filename) of
	[] -> Version;
	_Files -> get_version(Version + 1, Dir)
    end.

hash_from_file(Filename) ->
    {ok, Content} = file:read_file(Filename),
    <<Hash:160/integer>> = crypto:sha(Content),
    lists:flatten(io_lib:format("~40.16.0b", [Hash])).


%% test

test() ->
    OutDir = ".././priv/test",
    Version1 = next_version_for_avatar("1"),
    io:format("next version for user 1 ~p~n", [Version1]),
    place_avatar("1", OutDir ++ "/test1.jpg", Version1, big),
    place_avatar("1", OutDir ++ "/test1.jpg", Version1, raw),

    Version2 = next_version_for_avatar("2"),
    io:format("next version for user 2 ~p~n", [Version2]),
    place_avatar("2", OutDir ++ "/test2.png", Version2, big),
    place_avatar("2", OutDir ++ "/test2.png", Version2, raw),

    %% attachment_storage:place_feed_raw_file("1", "55", "/home/yura/tmp/01.jpg").

    attachment_storage:place_feed_raw_file("1", "55", OutDir ++ "/test1.jpg"),
    attachment_storage:place_feed_thumb("1", "55", OutDir ++ "/test2.png", big),
    attachment_storage:place_feed_converted("1", "55", OutDir ++ "/test3.gif", "orighash").
