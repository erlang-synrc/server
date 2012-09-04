%% -*- mode: nitrogen -*-
-module (element_view_media).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_srv/include/feed.hrl").
-include_lib("alog/include/alog.hrl").
-include("records.hrl").
-include("gettext.hrl").

%% Move the following line to records.hrl:

reflect() -> record_info(fields, view_media).


%PUBLIC BETA this is not the right way to resize videos, but it will do for now
split(String, Separator) ->
    Pos = string:str(String, Separator),
    case Pos of
        0 ->
            [String];
        _ ->
            [string:left(String, Pos-1)] ++ split(string:right(String, length(String) - length(Separator) - Pos + 1), Separator)
    end.

trim_to_first([], _) ->
    [];
trim_to_first([StringH| StringT], First) ->
    case StringH == hd(First) of
        true ->
            [StringH| StringT];
        false ->
            trim_to_first(StringT, First)
    end.



render_element(ViewMedia = #view_media{media = Media,
                                       anchor = Anchor,
                                       target = Target}) ->
    Fid = ViewMedia#view_media.fid,
    Cid = ViewMedia#view_media.cid,
    ViewVideoPostbackInfo = wf_event:serialize_event_context({view_video, Anchor,
                                                              Media#media.html, Target},
                                                             Anchor, undefined, ?MODULE),

    MediaPanelId = wf:temp_id(),
    case Media#media.type of
        {link, video}                   -> link_video(ViewMedia, ViewVideoPostbackInfo, Fid, Cid);
        {link, photo}                   -> link_photo(ViewMedia, Fid, Cid);
        {attachment, "image/jpeg"}      -> images_media(ViewMedia, MediaPanelId, Fid, Cid);
        {attachment, "image/png"}       -> images_media(ViewMedia, MediaPanelId, Fid, Cid);
        {attachment, "image/gif"}       -> images_media(ViewMedia, MediaPanelId, Fid, Cid);
        {attachment, "application/pdf"} -> images_media(ViewMedia, MediaPanelId, Fid, Cid);
        {attachment, "audio/mpeg"}      -> audio_media(ViewMedia, MediaPanelId, Fid, Cid);
        {attachment, "video/x-msvideo"} -> video_media(ViewMedia, MediaPanelId, Fid, Cid);
        {attachment, "video/x-flv"} -> video_media(ViewMedia, MediaPanelId, Fid, Cid);
        {attachment, "video/mp4"}       -> video_media(ViewMedia, MediaPanelId, Fid, Cid);
        {attachment, _}                 -> binary_media(ViewMedia, MediaPanelId, Fid, Cid)
    end.


link_video(#view_media{media=Media}=_VM, ViewVideoPostbackInfo, _Fid, _Cid) ->
    WidthLR = split(binary_to_list(Media#media.html), "width=\""),
    WidthR = trim_to_first(hd(tl(WidthLR)), "\""),
    MediaHTMLWidth = hd(WidthLR) ++ "width=\"400" ++ WidthR,
    HeightLR = split(MediaHTMLWidth, "height=\""),
    HeightR = trim_to_first(hd(tl(HeightLR)), "\""),
    MediaHTMLWidthHeight = hd(HeightLR) ++ "height=\"300"++HeightR, % "class=\"linked-video-" ++_Fid++"\" " ++ I want to make it friendfeed like

    #panel{        
        body=[
            MediaHTMLWidthHeight
        ]
    }.


link_photo(#view_media{media=Media}=_VM, _Fid, _Cid) ->
    #panel{body=#image{id=thumbImage,
                       image=Media#media.url,
                       class="view_media_thumb",
                       actions=#event{type=click,actions=#script{script=webutils:new_tab_js(Media#media.url)}}
        }
    }.



images_media(#view_media{media=Media}=_VM, MediaPanelId, Fid, Cid) ->
    {attachment, MediaType} = Media#media.type,
    case MediaType of
        "application/pdf" ->
            #panel{id=MediaPanelId, body=
                #link{url=lists:concat([?_U("/get-file"),"/fid/",Fid,"/cid/",Cid,"/mid/",Media#media.id]),
                    body=[
                        #image{id=thumbImage,
                            image=Media#media.thumbnail_url,
                            class="view_media_thumb"}
                    ]
                }
            };
        _ ->
            #panel{id=MediaPanelId, body=
                #lightbox_link{url=lists:concat([?_U("/get-file"),"/fid/",Fid,"/cid/",Cid,"/mid/",Media#media.id]),
                    gallery = "gallery",
                    body=[
                        #image{id=thumbImage,
                            image=Media#media.thumbnail_url,
                            class="view_media_thumb"}
                    ]
                }
            }
    end.


audio_media(#view_media{media=Media}=VM, MPI, Fid, Cid) ->
    case VM#view_media.only_thumb of
        true ->
            TempId = wf:temp_id(),
            [
            #panel{style="width:100%;clear:both;", body=[
	             wf:f("<div id=\"~s\"></div>", [TempId]),
	             wf:f("<script type=\"text/javascript\">"
		          "AudioPlayer.embed(\"~s\", {soundFile: \"~ts\", transparentpagebg:\"yes\"});"
		          "</script>", [TempId, Media#media.url]),
                     view_attachment(Media, MPI, Fid, Cid)
                ]}
    	    ];

        _ ->
	    TempId = wf:temp_id(),
            [
            #panel{style="width:100%;clear:both;", body=[
	     wf:f("<div id=\"~s\"></div>", [TempId]),
	     wf:f("<script type=\"text/javascript\">"
		  "AudioPlayer.embed(\"~s\", {soundFile: \"~ts\", transparentpagebg:\"yes\"});"
		  "</script>", [TempId, Media#media.url]),
             view_attachment(Media, MPI, Fid, Cid)
            ]}
	    ]
    end.

video_media(#view_media{media=Media}=_VM, MPI, Fid, Cid) ->
    URL = Media#media.url,
    {_, Type} = Media#media.type,
    [
        wf:f("
            <video id='videojs_"++Fid++"' class='video-js vjs-default-skin' controls
              preload='auto' data-setup='{}'>
              <source src='"++URL++"' type='"++Type++"'>
            </video><br>
        "),
        view_attachment(Media, MPI, Fid, Cid)
    ].

binary_media(#view_media{media=Media}=_VM, MPI, Fid, Cid) ->
    view_attachment(Media, MPI, Fid, Cid).

view_attachment(Media, _MPI, Fid, Cid) ->
    #panel{class="view_media_other_attachment", body=[
        #link{text=Media#media.title,
              actions=#event{type=click,
                             actions=#script{
                               script=webutils:new_tab_js(?_U("/get-file") ++
                                                              "/fid/" ++ Fid ++
                                                                  "/cid/" ++ integer_to_list(Cid) ++
                                                                  "/mid/" ++ Media#media.id)}
                }}
    ]}.

event({view_video, Anchor, Html, Target}) ->
    HideVideoPostbackInfo = wf_event:serialize_event_context({hide_video, Target}, Anchor, undefined, ?MODULE),
    Body = [#link{text="Close",
                  actions=#event{type=click,
                                 actions=#script{script=wf:f("Nitrogen.$queue_event(null, '~s', '')", [HideVideoPostbackInfo])}
                                }},
            Html],
    wf:update(Target, Body);

event({hide_video, Target}) ->
    wf:update(Target, "").

