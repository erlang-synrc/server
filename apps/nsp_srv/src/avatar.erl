-module(avatar).
-author('Yura Zhloba <yzh44yzh@gmail.com>').

-include_lib("nsm_db/include/user.hrl").

-include("loger.hrl").
-include("setup.hrl").

-export([get_avatar/2, get_avatar_by_username/2, process_uploaded_avatar/3]).

-spec get_avatar_by_username(string(), atom()) -> string().
get_avatar_by_username(UserName, Size) ->
    case nsm_users:get_user({username, UserName}) of
	{ok, User} ->
	    get_avatar(User, Size);
	_ -> get_avatar(no_avatar, Size)
    end.

-spec get_avatar(record(user) | record(avatar), atom()) -> string().
get_avatar(#user{avatar = Avatar}, Size) ->
    get_avatar(Avatar, Size);
get_avatar(Avatar, Size) ->
    case Avatar of
	#avatar{big = Big} when Size =:= big -> Big;
	#avatar{small = Small} when Size =:= small -> Small;
	#avatar{tiny = Tiny} when Size =:= tiny -> Tiny;
	_ -> case Size of
		 big -> ?STATIC_ADDRESS ++ "/images/no_avatar_big.jpg";
		 small -> ?STATIC_ADDRESS ++ "/images/no_avatar_small.jpg";
		 tiny -> ?STATIC_ADDRESS ++ "/images/no_avatar_tiny.jpg"
	     end
    end.

-spec process_uploaded_avatar(string(), string(), string()) -> {ok, tuple()} | {error, string()}.
process_uploaded_avatar(UserId, OrigFile, LocalFile) ->
    FileSize = filelib:file_size(LocalFile),
    ?DBG("Uploaded file: ~p ~p ~n", [OrigFile, FileSize]),
    IsImage = image_processing:is_image(LocalFile),
    Limit = 2 * 1024 * 1024,
    Res = if
	      IsImage =:= false -> {error, "uploaded file is not an image"};
	      FileSize > Limit -> {error, "image should be less than " ++ integer_to_list(Limit) ++ " bytes"};
	      true -> {ok, save_avatar(UserId, OrigFile, LocalFile)}
	  end,
    file:delete(LocalFile),
    Res.

save_avatar(UserId, OrigFile, LocalFile) ->
    Dir = filename:dirname(LocalFile),
    Name = filename:basename(LocalFile),
    Ext = filename:extension(OrigFile),

    Avatar = Dir ++ "/" ++ Name ++ Ext,
    Thumbnail = Dir ++ "/thumb_" ++ Name ++ Ext,
    Version = attachment_storage:next_version_for_avatar(UserId),

    file:copy(LocalFile, Avatar),
    {ok, _} = attachment_storage:place_avatar(UserId, Avatar, Version, raw),
    ok = image_processing:make_thumb(Avatar, 150, 150, Thumbnail),
    {ok, Big} = attachment_storage:place_avatar(UserId, Thumbnail, Version, big),
    ok = image_processing:make_thumb(Avatar, 70, 70, Thumbnail),
    {ok, Small} = attachment_storage:place_avatar(UserId, Thumbnail, Version, small),
    ok = image_processing:make_thumb(Avatar, 30, 30, Thumbnail),
    {ok, Tiny} = attachment_storage:place_avatar(UserId, Thumbnail, Version, tiny),

    file:delete(Avatar),
    file:delete(Thumbnail),

    #avatar{big = "/files/" ++ Big, small = "/files/" ++ Small, tiny = "/files/" ++ Tiny}.




