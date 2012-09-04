%%----------------------------------------------------------------------
%% @author Yura Zhloba <yzh44yzh@gmail.com>
%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%% @doc
%% This module keeps functions for working with feed attachments
%% @end
%%----------------------------------------------------------------------


-module(feed_attachment).

-export([process_uploaded_file/4,
         get_default_thumb/1]).

-include_lib("nsm_srv/include/attachment.hrl").
-include_lib("nitrogen_core/include/wf.hrl").
-include("setup.hrl").
-include("common.hrl").
-define(ROOT_DIR, "/files/").
-define(ROOT_DIR_DEFAULT, "/images/").

-spec process_uploaded_file(string(), string(), string(), string()) -> {ok, record(attachment)} | {error, string()}.
process_uploaded_file(UserId, FeedId, OrigFile, LocalFile) ->
    case check_type_file(LocalFile) of
        true ->
            {ok, Type} = mime_type:identify(LocalFile),
            Size = filelib:file_size(LocalFile),
            {ok, Limit}    = rpc:call(?APPSERVER_NODE, zealot_db, get, [config, "storage/upload_limit",     30 * 1024 * 1024]),
            {ok, NumLimit} = rpc:call(?APPSERVER_NODE, zealot_db, get, [config, "storage/upload_num_limit", 15]),
            NumUploads = get_num_uploads(UserId),

            Res = if
                      Size > Limit ->
                          {error, ?_TS("File should be less than $size$ bytes", [{size, Limit}])};
                      NumUploads >= NumLimit ->
                          {error, ?_TS("You can't upload more than $number$ files per day", [{number, NumLimit}])};
                      true ->
                          case Type of
                              "image/jpeg" -> save_image(UserId, FeedId, OrigFile, LocalFile);
                              "image/png" -> save_image(UserId, FeedId, OrigFile, LocalFile);
                              "image/gif" -> save_image(UserId, FeedId, OrigFile, LocalFile);
                              "application/pdf" -> save_pdf(UserId, FeedId, OrigFile, LocalFile);
                              "audio/mpeg" -> save_audio(UserId, FeedId, OrigFile, LocalFile);
                              "video/x-msvideo" -> save_video(UserId, FeedId, OrigFile, LocalFile);
                              "video/x-flv" -> save_video(UserId, FeedId, OrigFile, LocalFile);
                              "video/mp4" -> save_video(UserId, FeedId, OrigFile, LocalFile);
                              _OtherMime -> save_other(UserId, FeedId, OrigFile, LocalFile)
                          end
                  end,

            %%file:delete(LocalFile),
            case Res of
                {error, Reason} -> {error, Reason};
                {ok, Att0} ->
                    increment_num_uploads(UserId),
                    Att = Att0#attachment{type = Type},
                    %% It was decided at the moment does not show other icons - @Peter
                    %% Att1 =
                    %%     case Att#attachment.thumb of
                    %%         undefined ->
                    %%             Att#attachment{thumb = ?ROOT_DIR_DEFAULT++get_default_thumb(Type)};
                    %%         _ -> Att
                    %%     end,
                    {ok, Att}
            end;
        false ->
            {error, "This file type is not supported"}
    end.


check_type_file(File) ->
    {ok, FileType} = mime_type:identify(File),
    ?PRINT({"FEED ATTACHMENT: CTF:", FileType}),
    Default = [%% images
               "image/jpeg",
               "image/png",
               "image/gif",
               "image/svg+xml",
               "image/bmp",

               %% audio/video
               "audio/mpeg",
               "video/x-msvideo",
               "video/x-flv",
               "video/mp4",
               "audio/ogg",
               "audio/x-flac",

               %% text
               "text/plain",
               "text/xml"

               %% Portable documents
               "application/postscript", %% ps
               "application/pdf",

               %% microsoft
               "application/rtf",
               "application/vnd.openxmlformats", %% xlsx, xlsm, docx, docm, dotx, dotm
               %% %% General
               "application/vnd.ms-office"

               %% %% Excel
               "application/vnd.ms-excel",   %% xls, xlw, xlt, xltx, xltm, xlsb
               "application/x-pocket-excel", %% pxl
               %% %% Word
               "application/msword",        %% doc
               "application/x-pocket-word", %% psw
               %% %% Powerpoint
               "application/mspowerpoint",               %% ppt, pps, pot
               "application/vnd.ms-powerpoint.template", %% pptx, pptm, potx, potm

               %% OpenOffice.org
               %% %% Writer
               "application/vnd.oasis.opendocument.text",            %% odt
               "application/vnd.oasis.opendocument.formula-template" %% ott,
               "application/vnd.oasis.opendocument.text-master",     %% odm
               %% %% Calc
               "application/vnd.oasis.opendocument.spreadsheet" %% ods
               "application/octet-stream",                      %% ots,
               %% %% Draw
               "application/vnd.oasis.opendocument.graphics",          %% odg,
               "application/vnd.oasis.opendocument.graphics-template", %% otg,
               %% %% Impress
               "application/vnd.oasis.opendocument.presentation",          %% odp
               "application/vnd.oasis.opendocument.presentation-template", %% otp
               %% %% Math
               "application/vnd.oasis.opendocument.formula", %% odf
               %% %% Chart
               "application/vnd.oasis.opendocument.chart",   %% odc
               %% %% Database
               "application/octet-stream", %% odb

               %% eBook
               "application/x-mobipocket", %% mobi
               "application/octet-stream", %% azw, opf, epub, mobi
               "image/x-djvu"              %% djvu
               ],

    {ok, SupportedFileType} = rpc:call(?APPSERVER_NODE,zealot_db,get,[config,"webs/upload/supported_type", Default]),
    %?PRINT({"FEED ATTACHMENT: SUPPORTED FILES:", SupportedFileType}),
    lists:member(FileType, SupportedFileType).



save_image(UserId, FeedId, OrigFile, LocalFile) ->
    Dir = filename:dirname(LocalFile),
    Name = filename:basename(LocalFile),
    Ext = filename:extension(OrigFile),
    OriginName = filename:basename(OrigFile),

    Image = Dir ++ "/" ++ Name ++ Ext,
    Thumbnail = Dir ++ "/thumb_" ++ Name ++ Ext,
    file:copy(LocalFile, Image),

    ok = image_processing:make_thumb(LocalFile, 130, 130, Thumbnail),

    {ok, Raw, Hash} = attachment_storage:place_feed_raw_file(UserId, FeedId, Image),
    {ok, Thumb} = attachment_storage:place_feed_thumb(UserId, FeedId, Thumbnail, small),

    {ok, #attachment{id = Hash,
		     name = OriginName,
                     file = ?ROOT_DIR++Raw,
                     thumb = ?ROOT_DIR++Thumb,
                     owner = UserId,
                     create_data = erlang:now()}}.

save_pdf(UserId, FeedId, OrigFile, LocalFile) ->
    Dir = filename:dirname(LocalFile),
    Name = filename:basename(LocalFile),
    Ext = filename:extension(OrigFile),
    OriginName = filename:basename(OrigFile),

    Pdf = Dir ++ "/" ++ Name ++ Ext,
    Thumbnail = Dir ++ "/" ++ Name ++ ".jpg",
    file:copy(LocalFile, Pdf),

    %%ok = create_pdf_thumb(Pdf, 70, 70, Thumbnail),
    ok = image_processing:make_thumb(LocalFile, 70, 70, Thumbnail),
    {ok, Raw, Hash} = attachment_storage:place_feed_raw_file(UserId, FeedId, Pdf),
    {ok, Thumb} = attachment_storage:place_feed_thumb(UserId, FeedId, Thumbnail, small),

    {ok, #attachment{id = Hash,
		     name = OriginName,
                     file = ?ROOT_DIR++Raw,
                     thumb = ?ROOT_DIR++Thumb,
                     owner = UserId,
                     create_data = erlang:now()}}.

save_audio(UserId, FeedId, OrigFile, LocalFile) ->
    Dir = filename:dirname(LocalFile),
    Name = filename:basename(LocalFile),
    Ext = filename:extension(OrigFile),
    OriginName = filename:basename(OrigFile),

    Audio = Dir ++ "/" ++ Name ++ Ext,
    file:copy(LocalFile, Audio),
    {ok, Raw, Hash} = attachment_storage:place_feed_raw_file(UserId, FeedId, Audio),

    {ok, #attachment{id = Hash,
		     name = OriginName,
                     file = ?ROOT_DIR++Raw,
                     owner = UserId,
                     create_data = erlang:now()}}.

save_video(UserId, FeedId, OrigFile, LocalFile) ->
    Dir = filename:dirname(LocalFile),
    Name = filename:basename(LocalFile),
    Ext = filename:extension(OrigFile),
    OriginName = filename:basename(OrigFile),

    Video = Dir ++ "/" ++ Name ++ Ext,
    file:copy(LocalFile, Video),
    {ok, Raw, Hash} = attachment_storage:place_feed_raw_file(UserId, FeedId, Video),

    {ok, #attachment{id = Hash,
		     name = OriginName,
                     file = ?ROOT_DIR++Raw,
                     owner = UserId,
                     create_data = erlang:now()}}.

save_other(UserId, FeedId, OrigFile, LocalFile) ->
    Dir = filename:dirname(LocalFile),
    Name = filename:basename(LocalFile),
    Ext = filename:extension(OrigFile),
    OriginName = filename:basename(OrigFile),

    Video = Dir ++ "/" ++ Name ++ Ext,
    file:copy(LocalFile, Video),
    {ok, Raw, Hash} = attachment_storage:place_feed_raw_file(UserId, FeedId, Video),

    {ok, #attachment{id = Hash,
		     name = OriginName,
                     file = ?ROOT_DIR++Raw,
                     owner = UserId,
                     create_data = erlang:now()}}.

-spec get_default_thumb(string()) -> string().
get_default_thumb(Type) ->
    case Type of
        "image/jpeg" ->       "default/image.png";
        "image/png" ->        "default/image.png";
        "image/gif" ->        "default/image.png";
        "application/pdf" ->  "default/pdf.png";
        "audio/mpeg" ->       "default/audio.png";
        "video/x-msvideo" ->  "default/video.png";
        "video/x-flv" ->  "default/video.png";
        "video/mp4" ->        "default/video.png";
        _ ->                  "default/other.png"

    end.

get_num_uploads(UserId) ->
    get_num_uploads(UserId, now_date()).

get_num_uploads(UserId, Date) ->
    case rpc:call(?APPSERVER_NODE,zealot_db,get,[uploads, {UserId, Date}]) of
	{error, _E} -> 0;
	{ok, #uploads{counter = Counter}} -> Counter
    end.

increment_num_uploads(UserId) ->
    increment_num_uploads(UserId, now_date()).

increment_num_uploads(UserId, Date) ->
    Counter = get_num_uploads(UserId),
    rpc:call(?APPSERVER_NODE,zealot_db,put,[#uploads{key={UserId, Date}, counter=Counter + 1}]).


now_date() ->
    {{Y, M, D}, _} = calendar:local_time(),
    integer_to_list(Y) ++ "-" ++ integer_to_list(M) ++ "-" ++ integer_to_list(D).
