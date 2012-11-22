-module (element_view_comment).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_db/include/user.hrl").
-include_lib("nsm_db/include/feed.hrl").
-include("records.hrl").
-include("setup.hrl").

reflect() -> record_info(fields, view_comment).

render_element(_R = #view_comment{comment=C}) ->
    Author = C#comment.author_id,

    {D,H} = calendar:now_to_local_time(C#comment.create_time),
    %Date = io_lib:fwrite("~4..0b/~2..0b/~2..0b", tuple_to_list(D)),
    %Hour = io_lib:fwrite("~2..0b:~2..0b:~2..0b", tuple_to_list(H)),
    %Time = io_lib:fwrite("~s  ~s", [Hour, Date]),

    Time = site_utils:feed_time_tuple({D,H}),

    %ViewMediaPanelId = wf:temp_id(),

    {ok, User} = nsm_users:get_user(Author),
    Avatar = #image{image = avatar:get_avatar(User, tiny), style="width:30px;height:30px", class = 
        case nsm_accounts:user_paid(Author) of
            true -> "paid_user_avatar";
            _ -> ""
        end
    },
    {CId, {EId, FId}} = C#comment.id,
    Id = lists:concat([CId,"_", EId, "_", FId]),
    [
    #listitem{id = Id, body=[
        #panel{class="img", body=#link{url=site_utils:user_link(Author), body=Avatar}},
        #panel{class="descr", body=[
            io_lib:format("<p class=\"small\"><a href=\"#\">~s</a> <span class=\"marked\">~s</span></p>",
                [Author, Time]),
            #p{body=C#comment.content},
            render_comment_media(C)
        ]}
    ]}
%    #panel{style="float:right;margin-right:16px;margin-top;2px;background-color:#f6f6f6;width:617px;min-height:60px", body=[
%	#panel{style="float:left;margin-left:10px;margin-top:15px;width:30px;height:30px;background-color:#FFFFFF;border:1px solid #DDDDDD", body=[
%	    #link{body=Avatar, url=site_utils:user_link(Author)}
%	]},
%	#panel{style="padding-left:50", body=[
%	    #panel{class="blue_font", body=Author, style="padding-left:50px; padding-top:10px;padding-right:10px"},
%	    #panel{body=C#comment.content, style="padding-left:50px"},
%	    %#panel{style="padding-left:50px", body=[#view_media{media=M, target=ViewMediaPanelId, fid = Fid, cid = Cid} ||  M <- C#comment.media ]},
%	    render_comment_media(C),
%	    #panel{body=Time, style="padding-left:50px"}
%	    ]}
%    ]},
%    #panel{style="float:left;height:3px;width:500px;", body=[]}
    ].


render_comment_media(C) ->
    ViewMediaPanelId = wf:temp_id(),
    {Cid, Fid} = C#comment.id,
    case length(C#comment.media) of
	I when is_integer(I),I>0 ->
%	?PRINT({"NavBody", I}),
	  NavBody = if
	    I*2 > 4 -> [#panel{body=[], class="scroller_prev"},#panel{body=[], class="scroller_next"}];
	    true    -> []
	  end,
	  #panel{class="scroller_container", body=[
	     #panel{style="float:left;width:34px;min-height:30px", body=NavBody},
	     #panel{class="scroller", body=[
		#panel{class="modal-content", body=[
		    [#view_media{media=M, target=ViewMediaPanelId, fid = Fid, cid = Cid} ||  M <- C#comment.media]
		]}
	     ]}
	    %#panel{body=[], class="scroller_next"}
	  ]}
	;_-> #panel{body=[]}
    end.

%    [#panel{class="comment",
%            body=[#panel{class="avatar_space avatar",
%                         body=#link{body=Avatar,
%                                    url=site_utils:user_link(Author)}},
%
%                  #panel{class="comment_space",
%                         body=[#panel{class="comment_body",
%                                      body=[#link{text=Author, class="comment_author",
%                                                  url=site_utils:user_link(Author)},
%                                            #span{text=C#comment.content, class="comment_text"}]},
%			       #grid_6 {body=[
%					      #panel{body=[
%							    #view_media{media=M,
%									target=ViewMediaPanelId,
%									fid = Fid, cid = Cid} ||
%							      M <- C#comment.media ]}
%					     ]},
%                               #panel{class="comment_time", body=#label{text=Time}}]}
%                 ]}].
