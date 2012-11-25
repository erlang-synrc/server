%% -*- mode: nitrogen -*-
-module(webutils).

-compile(export_all).


-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_db/include/user.hrl").
-include_lib("nsm_db/include/config.hrl").
-include_lib("nsm_db/include/tournaments.hrl").
-include_lib("nsm_db/include/feed.hrl").
-include_lib("nsm_db/include/table.hrl").
-include_lib("nsm_db/include/accounts.hrl").
-include("elements/records.hrl").
-include("setup.hrl").
-include("common.hrl").

-define(TOOLTIP_TIMEOUT, "1500").

-define(GROUPS_ON_DASHBOARD, 10).

redirect_to_ssl(Page) ->
    Req = wf_context:request_bridge(),
    Port = Req:peer_port(),
    Host = hd(ling:split(proplists:get_value(host, wf_context:headers()), ":")),
    ?INFO("Req ~p Port ~p Host ~p",[Req,Port,Host]),
    case Port of
        443 -> ok;
        _ -> wf:redirect(["https://",Host,"/",Page])
    end.

redirect_to_tcp(Page) ->
    Req = wf_context:request_bridge(),
    Port = Req:peer_port(),
    Host = hd(ling:split(proplists:get_value(host, wf_context:headers()), ":")),
    ?INFO("Req ~p Port ~p Host ~p",[Req,Port,Host]),
    case Port of
        80 -> no_redirect;
        8000 -> no_redirect;
        _ ->
            case Host == "kakaranet.com" of
                 true ->  wf:redirect(["http://",Host,"/",Page]);
                 false ->  wf:redirect(["http://",Host,":8000/",Page])
            end 
    end.

main() -> [].

user_info() ->
    User = wf:session(user_info),
    case User of
        undefined ->
            erlang:throw(not_logged_in);
        _ ->
            User
    end.

user_info(Field) ->
    user_info(user_info(), Field).
user_info(User, Field) ->
    RecordInfo = record_info(fields, user),
    [user | RecordValue] = tuple_to_list(User),
    UserInfo = lists:zip(RecordInfo, RecordValue),
    proplists:get_value(Field, UserInfo).


title(_Site) ->
    User = wf:user(),
    case User /= undefined of
        true ->
            io_lib:fwrite("Kakaranet - ~s", [site_utils:username_upper(User)]);
        false ->
            "Kakaranet"
    end.

-spec new_window_js(string()) -> string().
new_window_js(Url) ->
    new_window_js(Url, "").

-spec new_window_js(string(), atom() | string()) -> string().
new_window_js(Url, Name) ->
    lists:concat(["window.open('",
                  Url,
                  "', '", Name, "', 'height=570, width=770, location=no, "
                  "menubar=no, resizable=no, scrollbars=no, status=no, toolbar=no');"]).

new_tab_js(Url) when is_binary(Url)->
    new_tab_js(binary_to_list(Url));
new_tab_js(Url) when is_list(Url)->
    lists:concat(["window.open('",
                  Url,
                  "');"]).

header_box() ->
    #template { file=code:priv_dir(nsw_srv)++"/templates/header.html"}.

header_body() -> [account_menu(), menu_links()].

account_menu() ->
    Element = case wf:user() /= undefined of 
	true ->
	    case R = nsm_users:get_user(wf:user()) of 
		{error,notfound} -> event(logout);
		_UserFound -> 
		    {ok, User} = R,
		    Submenus = #list{body=[#listitem{body=#link{url=Url,text=Text}}
		    || {Url,Text}  <- [{?_U("/profile"), ?_T("My Profile")},
				      {?_U("/profile/account"), ?_T("My Account")},
				      {?_U("/profile/gifts"), ?_T("My Gifts")},
				      {?_U("/profile/stats"), ?_T("Stats")},
				      {?_U("/profile/invite"), ?_T("Invite Buddy")}]
					++ case nsm_acl:check_access(User, {feature, admin}) of
					    allow -> [{?_U("/kakaadmin"), ?_T("Admin")}];
					    _ -> []
					end ]},
		    {ok,#user_info{username=Username,avatar_url = AvatarUrl}} = nsm_auth:get_user_info(webutils:user_info(username)),
		    {ok, Quota}  = nsm_accounts:balance(Username, ?CURRENCY_QUOTA),
		    {ok, Kakush} = nsm_accounts:balance(Username, ?CURRENCY_KAKUSH),
		    #list{class="user-menu", body=[
			#listitem{body=[
			    #link{url="#", body=[ 
				#image{image=AvatarUrl, id=header_user_avatar, style="width:23px;height:23px", alt="#"},
				#label{class="username", text=Username}]},
			    Submenus
			]},
			#listitem{class=quota, body=[ #link{text=lists:concat([?_T("Quota"), " : ",Quota])}]},
			#listitem{class=kakus, body=[ #link{text=lists:concat([?_T("Kakush")," : ",Kakush])}]},
			case wf:session(logged_with_fb) of
			    true -> #listitem{body=fb_utils:logout_btn()};
			    _ -> #listitem{body=#link{text=?_T("Logout"), postback=logout}}
			end
		    ]}
	    end;
	_UserLoggedIn ->
	    [#list{class="user-menu", body=[
		#listitem{body=fb_utils:login_btn()},
		#listitem{body=#link{class=login, text=?_T("Login"), url=[?HTTP_ADDRESS,?_U("/login")]}},
		#listitem{body=#link{class=signup, text=?_T("Signup"), postback=register}}
	    ]}]
	end,
	#panel{class="top", body= #panel{class="ar", body=[#panel{class="box", body=Element}]}}.

menu_links() ->
	["<nav>",
	 #list{body=[
		#listitem{body=#link{text=?_T("Home"), url=?_U("/"),
                                     title=?_T("You can play games here"), id="mainmenumainpage"}},
		#listitem{body=#link{text=?_T("My Page"), url=?_U("/dashboard"),
                                     title=?_T("You can share information with others"), id="mainmenumypage"}},
		#listitem{body=#link{text=?_T("Rules"), url=?_U("/rules-okey"),
                                     title=?_T("Read the rules of our games"), id="mainmenurules"}},
		#listitem{body=#link{text=?_T("Gifts"), url=?_U("/gifts"),
                                     title=?_T("Have no idea, what it is about"), id="mainmenugifts"}},
		#listitem{body=#link{text=?_T("Tournaments"), url=?_U("/tournaments"),
                                     title=?_T("You can join tournaments and show them all"), id="mainmenutournaments"}},
		#listitem{body=#link{text=?_T("Groups"), url=?_U("/groups"),
                                     title=?_T("You can manage your groups settings here"), id="mainmenugroups"}}
	 ]},
	 "</nav>
      <script>
      (function(){
          var C = {text:false};
          var P = {my:'top right', at:'bottom left'};
          var S = {delay: "++?TOOLTIP_TIMEOUT++"};
          objs('mainmenumainpage').qtip({content: C, position: P, show: S} );
          objs('mainmenumypage').qtip({content: C, position: P, show: S});
          objs('mainmenugifts').qtip({content: C, position: P, show: S});
          objs('mainmenutournaments').qtip({content: C, position: P, show: S});
          objs('mainmenugroups').qtip({content: C, position: P, show: S});
      })();
      </script>"
	 ].

language() ->
    case wf_context:page_module() of
	join_game -> "";
	_ -> language_selector()
    end.

language_selector() ->
    SL = site_utils:detect_language(),
    case SL of
        "en" -> Text= "Turkish", Img = "flag-tr.png";
        "tr" -> Text= "English", Img = "flag-uk.png"
    end,
    Image = #image { image="/images/"++Img, style="width:14px;height:10px;", alt=" " },
    #link{class="lang", body=[Image, Text], postback={change_language, SL}}.

logo() ->
    #link{url="/", class="fn org url", text=?_T("KakaraNet - Public Beta")}.

lightboxes() ->
    login:lightboxes().

user_avatar(small) ->
    ok;
user_avatar(medium) ->
    ok;
user_avatar(big) ->
    ok.

footer_box() ->
    case wf_context:page_module() of
        view_table ->
            [];
        _ ->
            LinkList = [
%                        #listitem{body=[?_T("About Us")] },
                #listitem{body=#link{url=?_U("/gifts"), text=?_T("Gifts")}},
%                        #listitem{body=?_T("Pointing System")},

                        #listitem{body=#link{url=?_U("/terms"), text=?_T("Terms of Service")}},
                        #listitem{body=#link{url=?_U("/privacy"), text=?_T("Privacy Policy")}},
                        #listitem{body=wf_tags:emit_tag(a, ?_T("Help & Support"),
                                    [{href, "http://kakaranet.uservoice.com/"},{target,"_blank"}])},
                        #listitem{body=#link{url=?_U("/contact"), text=?_T("Contact")}},
                        #listitem{body=[?_T("2011 &copy; Kakaranet. All rights reserved."),"<br/>",
                                    ?_T("Kakaranet is registered trademark of Paynet Inc."),"<br/>"]},
                        #listitem{body=[#checkbox { id=replay_guiders, text=?_T("Replay Guiders"), postback=replay_guiders_changed,
                                    checked=(wf:cookie("replayguiders")=="yes") }]}
                       ],

            ["<footer>",
             #list{class="navbar", body = LinkList},
             language(),
             "</footer>"
            ]
    end.


event({error, Msg}) ->
    wf:wire(#alert{text=Msg});

event(login) ->
    login(login,password,login_hintbox);
event(register)->
    wf:session(fb_registration, undefined),
    wf:redirect([?HTTP_ADDRESS,?_T("/login/register")]);
event(logout) ->
    %wf:clear_session(),
    wf:logout(),
    wf:redirect("/");
event({change_language,SL}) ->
    NewLang = case SL of
		"en" -> "tr";
		"tr" -> "en"
	      end,
    site_utils:reset_language(),
    wf:session(lang, NewLang),
    wf:cookie("lang", site_utils:detect_language(), "/", 100*24*60), %% 100 days
    Request = wf_context:request_bridge(),
    URI = Request:uri(),
    wf:redirect(uri_translator:translate(URI, SL, NewLang));
event({birthday_changed}) ->
    element_register:event({birthday_changed});
event({more_entries, Module, PageAmount, LastId}) ->
    Entries = Module:on_more_entries(LastId, PageAmount),
    ?PRINT({length(Entries), PageAmount}),
    Rendered = [ #view_entry{entry = E} || E <- Entries],
    wf:insert_bottom(feed, Rendered),
    wf:update(more_button_holder, more_button(Module, Entries, PageAmount));
event(replay_guiders_changed) ->
    case wf:q(replay_guiders) of
        "on" -> 
            wf:cookie("replayguiders", "yes");
        _ ->
            wf:cookie("replayguiders", "no")
    end;

event(Other) ->
    login:event(Other).

%% API: list_to_options/1,2,3
list_to_options(List) ->
    list_to_options(List, undefined).

list_to_options(List, Selected) ->
    list_to_options(List, Selected, "-").
list_to_options(List, Selected, FirstItemText) ->
    R = [
        case El of
            {Selected, Text} ->
                #option{value=Selected, text=Text, selected=true};
            {Val, Text} ->
                #option{value=Val, text=Text};
            Selected ->
                #option{value=El, text=El, selected=true};
            _ ->
                #option{value=El, text=El}
        end || El <- List ],

    case FirstItemText of
        undefined ->
            R;
        _->
            [#option{text=FirstItemText, value=undefined} | R]
    end.

%% API: create_option_with_number/1,2,3
create_option_with_number({From, To}) ->
    create_option_with_number({From, To}, undefined).

create_option_with_number({From, To}, Selected) ->
    create_option_with_number({From, To}, Selected, "-").

create_option_with_number({From, To}, Selected, FirstItemText) when is_integer(Selected) ->
    Selected1 = wf:to_list(Selected),
    create_option_with_number({From, To}, Selected1, FirstItemText);

create_option_with_number({From, To}, Selected, FirstItemText) when From < To->
    L = [ wf:to_list(V) || V <- lists:seq(From, To) ],
    list_to_options(L, Selected, FirstItemText);

create_option_with_number({From, To}, Selected, FirstItemText) when From > To->
    L = [ wf:to_list(V) || V <- lists:seq(From, To, -1) ],
    list_to_options(L, Selected, FirstItemText).



city_list() ->
    ["İstanbul",
     "Ankara",
     "İzmir",
     "Adana",
     "Mersin",
     "Adıyaman",
     "Afyonkarahisar",
     "Ağrı",
     "Amasya",
     "Antalya",
     "Artvin",
     "Aydın",
     "Balıkesir",
     "Bilecik",
     "Bingöl",
     "Bitlis",
     "Bolu",
     "Burdur",
     "Bursa",
     "Çanakkale",
     "Çankırı",
     "Çorum",
     "Denizli",
     "Diyarbakır",
     "Edirne",
     "Elazığ",
     "Erzincan",
     "Erzurum",
     "Eskişehir",
     "Gaziantep",
     "Giresun",
     "Gümüşhane",
     "Hakkâri",
     "Hatay",
     "Isparta",
     "Kars",
     "Kastamonu",
     "Kayseri",
     "Kırklareli",
     "Kırşehir",
     "Kocaeli",
     "Konya",
     "Kütahya",
     "Malatya",
     "Manisa",
     "Kahramanmaraş",
     "Mardin",
     "Muğla",
     "Muş",
     "Nevşehir",
     "Niğde",
     "Ordu",
     "Rize",
     "Sakarya",
     "Samsun",
     "Siirt",
     "Sinop",
     "Sivas",
     "Tekirdağ",
     "Tokat",
     "Trabzon",
     "Tunceli",
     "Şanlıurfa",
     "Uşak",
     "Van",
     "Yozgat",
     "Zonguldak",
     "Aksaray",
     "Bayburt",
     "Karaman",
     "Kırıkkale",
     "Batman",
     "Şırnak",
     "Bartın",
     "Ardahan",
     "Iğdır",
     "Yalova",
     "Kilis",
     "Karabük",
     "Osmaniye"
     ].


custom_info_box(Title, Items) ->
    #panel{class="info_box", body=[
        #h4{text=Title},
        #list{body=Items}
    ]}.


view_feed_entries(Module, PageAmount, Entries) ->
    Rendered = [ #view_entry{entry = E} || E <- Entries ],
    [
        #panel{id = feed, body=[Rendered]},
        #panel{id = more_button_holder, body=more_button(Module, Entries, PageAmount)}
    ].

view_comments_entry(EId) ->
    Comments = comment:select_by_entry_id(EId),
    [ #view_comment{comment = C} || C <- Comments ].


-spec lightbox_panel_template(atom()|string(), any()) -> record(panel).
lightbox_panel_template(LightboxId, PanelBody) ->
    lightbox_panel_template(LightboxId, PanelBody, undefined).

lightbox_panel_template(LightboxId, PanelBody, undefined) ->
    DefaultAction = site_utils:js_close_on_click(wf:to_list(LightboxId)),
    lightbox_panel_template(LightboxId, PanelBody, DefaultAction);
lightbox_panel_template(LightboxId, PanelBody, CloseActions) ->
    Class = case LightboxId of splash_lightbox -> "popup-2"; _ -> "popup-2 popup-3" end,
    case LightboxId of 
        gift_lightbox ->
            #panel{class = Class, 
                body = [
                    #panel{class = in, style = "max-height:600px;", body = #panel{class = frame, style = "max-height:600px;", body = PanelBody}},
		            #link{class = "btn-close", text = "close", postback = CloseActions} % ! this is not an action
                ]
            };
        _ ->
            #panel{class = Class, 
                body = [
                    #panel{class = in, body = #panel{class = frame, body = PanelBody}},
		            #link{class = "btn-close", text = "close", actions = CloseActions}
                ]
            }
    end.

-spec table_info(proplist()) -> [record(p)].
table_info(Table) ->
    UsersList = site_utils:join([ site_utils:linkify_name(Name, normal) || Name <- Table#game_table.users ], ", "),
    InviteUsersList = [],
    AgeLimit = case site_utils:as_str(Table#game_table.age_limit) of
        "undefined" -> 
            undefined;
        [From|[To|[]]] -> 
            integer_to_list(From) ++ "–" ++ integer_to_list(To)
    end,
    [
        #p{body=[?_T("Name"), ": ", site_utils:as_str(Table#game_table.name)],
        show_if=site_utils:show_if(Table#game_table.name)},
        #p{body=[?_T("Owner"), ": ", site_utils:as_str(Table#game_table.owner)],
            show_if=site_utils:show_if(Table#game_table.owner)},
        #p{body=[?_T("Players"), ": ", UsersList],
            show_if=site_utils:show_if(UsersList)},
        #p{body=[?_T("Game mode"), ": ", site_utils:as_str(Table#game_table.game_mode)],
            show_if=site_utils:show_if(Table#game_table.game_mode)},
        #p{body=[?_T("Game speed"), ": ", site_utils:as_str(Table#game_table.game_speed)],
            show_if=site_utils:show_if(Table#game_table.game_speed)},
%        #p{body=[?_T("Groups"), ": ", site_utils:as_str_list(Table#game_table.groups)],
%           show_if=site_utils:show_if(Table#game_table.groups)},
        #p{body=[?_T("Gender limit"), ": ", site_utils:as_str(Table#game_table.gender_limit)],
            show_if=site_utils:show_if(Table#game_table.gender_limit)},
        #p{body=[?_T("Sets"), ": ", site_utils:as_str(Table#game_table.sets)],
            show_if=site_utils:show_if(Table#game_table.sets)},
        #p{body=[?_T("Friends only"), ": ", site_utils:as_str(Table#game_table.friends_only)],
            show_if=site_utils:show_if(Table#game_table.friends_only)},
        #p{body=[?_T("Invite users"), ": ", InviteUsersList],
            show_if=site_utils:show_if(InviteUsersList)},
  %      #p{body=[?_T("Paid"), ": ", site_utils:as_str(Table#game_table.paid)],
  %          show_if=site_utils:show_if(Table#game_table.paid)},
   %     #p{body=[?_T("Location"), ": ", site_utils:as_str(Table#game_table.location)],
    %        show_if=site_utils:show_if(Table#game_table.location)},
        #p{body=[?_T("Age limit"), ": ", AgeLimit], show_if = (AgeLimit =/= undefined) }
    ].

serialize_event(Fun, Anchor, Module) ->
    Event = wf_event:serialize_event_context(Fun, Anchor, undefined, Module),
    #event{type=click, actions=#script{script=wf:f("Nitrogen.$queue_event(null, '~s', '')", [Event])}}.

login(UserField, PassField, MsgBox)->
    wf:wire(MsgBox, #hide{effect=blink, speed=300}),
    User = wf:q(UserField),
    Password = wf:q(PassField),

    case nsm_auth:login([{username, User},{password, Password}]) of
        {ok, User} ->
            login:login_user(User);
        {error, user_not_found} ->
            display_error(MsgBox, "User not found");
        {error, not_verified} ->
            login:login_user(User);
        {error, banned} ->
            display_error(MsgBox, "Account is banned.");
        {error, unknown} ->
            display_error(MsgBox, "Your account is damaged. Contact with administrator.");
        {error, incorrect_password} ->
            display_error(MsgBox, "Bad password")
    end.

-spec save_facebook_id(string(), string(), string()) -> ok.
save_facebook_id(UserName, FBID, undefined) ->
    %% if token unedfined log and skip
    ?WARNING("facebook token unedfined. User=~p, FBId=~p", [UserName, FBID]),
    ok;
save_facebook_id(UserName, FBID, FbToken) ->
    ?PRINT(FbToken),
    case fb_utils:get_user_info(FbToken) of
        {ok, {struct, Response}} ->
            case wf:to_list(proplists:get_value(<<"id">>, Response)) of
                FBID ->
                    %% access token belongs to same user
                    case nsm_users:get_user(UserName) of
                        {ok, User} ->
                            case User#user.facebook_id of
                                undefined ->
                                    NewUser = User#user{facebook_id = list_to_binary(FBID)},
                                    nsx_msg:notify(["system", "put"], NewUser);
                                FBID ->
                                    nothing_to_save;
                                Id ->
                                    ?ERROR("User already have facebook account linked to user"
                                           " User=~p, FBID=~p, ID=~p",
                                           [UserName, FBID, Id])
                            end;
                        Error ->
                            ?ERROR("User not found. User=~p, FBID=~p, Error=~p",
                                   [UserName, FBID, Error])
                    end,
                    ok;
                Error ->
                    ?ERROR("wrong UserId. FBID=~p, Error=~p", [FBID, Error])
            end;
        Error ->
            ?ERROR("Unexpected error: ~p", [Error])
    end,
    ok.

display_error(MsgBox, Message)->
    wf:update(MsgBox,?_T(Message)),
    wf:wire(MsgBox, #show{effect=slide, speed=300}).

user_count(GameH) ->
  GameCounts = rpc:call(?GAMESRVR_NODE,game_manager,counter,[
    case GameH of
      tavla -> game_tavla;
      okey -> game_okey; _ -> GameH
    end
  ]),
  integer_to_list(GameCounts).

get_members(GId) ->
    Nav = [
        #span_b{class="links", body=[
          #link{text=?_T("All members"), url=?_U("/view/members")++"/id/"++GId}
        ]}
    ],
    get_metalist(GId, ?_T("MEMBERS"), nsm_groups, list_group_members, ?_T("Group have no members"), Nav).

get_friends() ->
    User = webutils:user_info(),
    get_friends(User).

get_friends(User) ->
    Msg = case User#user.username == wf:user() of
        true ->
            #panel{class="mark-cell", body=[
                #br{},
                #br{},
                #br{},
                #br{},
                #br{},
                "<p><strong>",
                ?_T("Make new friends on kakaranet."),
                "</strong></p>",
                #link{url="/view/all-users", class="btn", text=?_T("Find someone!")}
            ]};
        false ->
            ?_T("User is not subscribed to anyone")
    end,
    Nav = [
        case User#user.username == wf:user() of
            true ->
                #span_b{class="links", body=[
                    #link{text=?_T("All your friends"), url="/friends", id="friendslink",
                    title=?_T("You can unsubscribe or write someone private message via this list")}
                ]};
            false ->
                #span_b{class="links", body=[
                    #link{text=?_T("All friends of ") ++ User#user.username, url="/friends/of/"++User#user.username,
                    id="friendslink", title=?_T("You can unsubscribe or write someone private message via this list")}
                ]}
        end,
        #span_b{class="links", body=[
            #link{text=?_T("All the people on kakaranet"), url="/view/all-users", id="alluserslink",
            title=?_T("You can unsubscribe or write someone private message via this list")}
        ]}
    ],
    [
        "<span id='guidersfriends'>",
        get_metalist(User, ?_T("FRIENDS"), nsm_users, list_subscr_for_metalist, Msg, Nav),
        "</span>"
    ].

get_metalist(Id, Title, Module, List, EmptyMsg, Nav) ->
    Friends = case Module:List(Id) of
        [] ->
            [EmptyMsg];
        Sub ->
            Sub2 = lists:sublist(Sub, 10),
            case Sub2 of
                [] -> [];
                _ ->    
                    Sub3 = case is_list(hd(Sub2)) of    % just ids, no names
                        true -> [{UId, undefined} || UId <- lists:sort(Sub2)];
                        _ -> Sub2
                    end,
                    [
                        begin
                            case WhoName of
                                undefined -> RealName = nsm_users:user_realname(Who);   % because name is changable
                                [Name, 32, LastName] ->
                                    if   
                                        LastName == "", Name == "" ->
                                            RealName = Who;
                                        LastName == "undefined", Name == "undefined" ->
                                            RealName = Who;
                                        LastName == "undefined" ->
                                            RealName = Name;
                                        Name == "undefined" ->
                                            RealName = LastName;
                                        true ->
                                            RealName = Name ++ [" "] ++ LastName
                                    end;
                                "" -> RealName = Who;
                                Name_Surname -> RealName = Name_Surname
                            end,
                            #listitem{body=[
                                #image{image=get_user_avatar(Who), style=
                                    case nsm_accounts:user_paid(Who) of
                                        true -> "border:3px solid #ffb03b; padding:0px;";
                                        _ -> ""
                                    end
                                },
                                #link{text=RealName, url=site_utils:user_link(Who)}
                            ]}
                        end
                    || {Who, WhoName} <- Sub3 ]
            end
    end,
    [
        #panel{class="box", body=[
            #h3{text=Title}, #list{class="list-photo", body=[ Friends ]},
            Nav
        ]}
    ].

get_user_avatar(UserUid) ->
    case nsm_db:get(user, UserUid) of
        {ok, #user{avatar={_,_,_,Avatar}}} -> Avatar
        ;_-> "/images/no_avatar_tiny.jpg"
    end.

get_user_avatar(UserUid, "tiny") ->
    case nsm_db:get(user, UserUid) of
        {ok, #user{avatar={_,_,_,Avatar}}} -> Avatar
        ;_-> "/images/no_avatar_tiny.jpg"
    end;
get_user_avatar(UserUid, "small") ->
    case nsm_db:get(user, UserUid) of
        {ok, #user{avatar={_,_,Avatar,_}}} -> Avatar
        ;_-> "/images/no_avatar_small.jpg"
    end;
get_user_avatar(UserUid, _) ->
    case nsm_db:get(user, UserUid) of
        {ok, #user{avatar={_,Avatar,_,_}}} -> Avatar
        ;_-> "/images/no_avatar_big.jpg"
    end.

get_group_avatar(GId) -> get_group_avatar(GId, "tiny").
get_group_avatar(GId, Variant) -> %PUBLIC BETA - we don't have that option for now
    case GId of
        "kakaranet" ->
            "/images/default_avatars/kakaranet_avatar_"++Variant++".png";
        "yeniler" ->
            "/images/default_avatars/yeniler_avatar_"++Variant++".png";
        _ ->
            "/images/no_avatar_"++Variant++".jpg"
    end.


get_groups() ->
    User = webutils:user_info(),
    get_groups(User).

%% user - #user{username}
get_groups(User) ->
    Groups = case nsm_groups:list_groups_per_user(User#user.username) of
        [] ->
            case User#user.username == wf:user() of
                true ->
                    ?_T("You are currently not in any group");
                false ->
                    ?_T("$user$ is currently not in any group")
            end;
        Gs ->
            UC_GId = lists:sublist(
                lists:reverse(
                    lists:sort([{nsm_groups:group_members_count(GId), GId} || GId <- Gs])
                ), 
            ?GROUPS_ON_DASHBOARD),
            lists:flatten([
                begin
                    {ok, Group} = nsm_groups:get_group(GId),
                    GName = Group#group.name,
                    #listitem{body=[
                        #link{body=[GName], url=site_utils:group_link(GId)},
                        #span{style="padding-left:4px;", text="(" ++ integer_to_list(UC) ++ ")"}
                    ]}
                end
                || {UC, GId} <- UC_GId
            ])
    end,
    [
        "<span id='guidersgroups'>",
        #panel{class="box", body=[
            #h3{text=?_T("GROUPS")},
            #list{class="list-photo list-photo-in", body=[ Groups ]},
            case User#user.username == wf:user() of
                true ->
                    [
                        #span_b{class="links", body=[
                            #link{text=?_T("List of all your groups"), url="/groups/of/"++wf:user(), id="groupslink",
                                  title=?_T("You can unsubscribe a group from this list")}
                        ]},
                        #span_b{class="links", body=[
                            #link{text=?_T("List of all groups on kakaranet"), url="/groups", id="allgroupslink",
                                  title=?_T("You can subscribe to any group from this list")}
                        ]}
                    ];
                false->
                    ""  % here should be all users groups link
            end
        ]},
        "</span>"
    ].


get_tournaments() ->
    User = webutils:user_info(),
    Tournaments = case tournaments:user_tournaments(User) of
        [] ->
            ?_T("You are currently not in any group");
        Gs ->
            lists:flatten([
                #listitem{ body = [
                     #link {
                         body = [ GName ++ " ", #span{text="(" ++ integer_to_list(UserCount) ++ ")"}],
                         url = lists:concat(["/view-tournament/id/", GName])
                     }
                ]}
                || {#tournament{name = GName},UserCount} <- Gs
            ])
    end,
    [
        #panel{class="box", body=[
            #h3{text=?_T("TOURNAMENTS")},
            #list{class="list-photo list-photo-in", body=[ Tournaments ]},
            #span_b{class="links", body=[
                #link{text=?_T("Browse"), url="/tournaments"},
                " / ",
                #link{text=?_T("Edit tournaments list"), url="/tournaments"}
            ]}
        ]}
    ].

get_misc_links() ->
    [
        #link{text=?_T("My accont"), url="/account"},
        case nsm_acl:check_access(wf:user_info(), {feature, admin}) of
            allow ->
                [#br{}, #link{text="Admin panel", url="/kakaadmin"}];
            _ ->
                []
        end
    ].

get_ribbon_menu() ->
    CheckedUser = case {wf:q("user"),wf:user()} of
        {CU,CU} -> undefined;
        _       -> wf:q("user")
    end,
    IsSubscribedUser = case CheckedUser of
        undefined -> undefined;
        _         -> feed:is_subscribed_user(wf:user(), CheckedUser)
    end,
    User = case CheckedUser of
        undefined -> webutils:user_info();
        _         -> {ok, Usr} = nsm_users:get_user(CheckedUser), Usr
    end,
    {SubscribersCount, FriendsCount, CommentsCount, LikesCount} =
    case CheckedUser of
        undefined -> {0,0,0,0};
        _         -> {
            feed:user_subscription_count(CheckedUser),
            feed:user_friends_count(CheckedUser),
            feed:user_comments_count(CheckedUser),
            feed:user_likes_count(CheckedUser)
        }
    end,
    NewDirectMessages=false,    %PUBLIC BETA 
    BlockedUsers = nsm_users:get_blocked_users(wf:user()),
    BlockUnblock = case CheckedUser of
        undefined -> [];
        _ -> case lists:member(CheckedUser, BlockedUsers) of
                true ->
                    #panel{id="blockunblock", class="center",
                        body=#link{text=?_T("Unblock this user"),url="javascript:void(0)",
                            postback={unblock, CheckedUser}
                        }
                    }
                ;_   ->
                    #panel{id="blockunblock", class="center",
                        body=#link{text=?_T("Block this user"), url="javascript:void(0)",
                            postback={block, CheckedUser}
                        }
                    }
             end
    end,
    MenuTail = case {CheckedUser, IsSubscribedUser} of
        {undefined, undefined} ->
            #list{class="add-nav", body=[
                #listitem{body=[
                    "<span id='guidersmyfeed'>",
                    #link{url="/dashboard",
                        text=?_T("My Feed"),
                        title=?_T("All your own and your friends posts. And your favourite groups' posts too."),
                        id="myfeedlink"
                    },
                    "</span>"
                ]},
                #listitem{body=[
                    "<span id='guidersdirectmessages'>",
                    #link{url="/dashboard/filter/direct",
                        title=?_T("Your private correspondence, no one other can read"),
                        id="directmessageslink",
                        body=[
                            #span{text=?_T("Direct messages")},
                            #image{image="/images/ico-002.gif", style="width:22px;height:22px", show_if=NewDirectMessages}
                        ]
                    },
                    "</span>"
                ]}
%PHASE1                #listitem{body=[#link{url="/dashboard/filter/my_discussions", text=?_T("My Discussions")}]},
%PHASE1                #listitem{body=[#link{url="#", text=?_T("Best of day")}]}
            ]};
        {_, true}  -> [
                #link{text=?_T("Unsubscribe"), url="#", class="btn-abone btn-abone-2", postback={unsubscribe, CheckedUser}},
                BlockUnblock,
                #list{class="list-6", body=[
                    #listitem{body=#link{url="javascript:void(0)",
                        postback={direct_message_to, CheckedUser},
                        body=[#image{image="/images/ico-04.gif", style="width:27px;height:34px"}, ?_T("Send direct message")]}},
                        #listitem{body=#link{url="#", body=[
                            #image{image="/images/ico-05.gif", style="width:27px;height:34px"}, ?_T("Notification options")
                        ]}
                    }
                ]},
                #list{class="list-5", body=[
                    #listitem{body=
                        #link{url="/friends/t/subscribtion",
                            text=integer_to_list(SubscribersCount) ++ " " ++ ?_T("subscription")
                        }
                    },
                    #listitem{body=
                        #link{url="/friends/t/subscribers",
                            text=integer_to_list(FriendsCount) ++ " " ++ ?_T("subscribers")
                        }
                    },
                    #listitem{body=
                        #link{url="/dashboard/filter/comments/user/" ++ wf:to_list(CheckedUser),
                            text=integer_to_list(CommentsCount) ++ " " ++ ?_T("comments")
                        }
                    },
                    #listitem{body=
                        #link{url="/dashboard/filter/like/user/" ++ wf:to_list(CheckedUser),
                            text=integer_to_list(LikesCount) ++ " " ++ ?_T("likes")
                        }
                    }
                ]}
            ];
        {_, false} -> [
                #link{text=?_T("Subscribe"), url="#", class="btn-abone", postback={subscribe, CheckedUser}},
                BlockUnblock,
                #list{class="list-5", body=[
                    #listitem{body=
                        #link{url="/friends/t/subscribtion",
                            text=integer_to_list(SubscribersCount) ++ " " ++ ?_T("subscription")
                        }
                    },
                    #listitem{body=
                        #link{url="/friends/t/subscribers",
                            text=integer_to_list(FriendsCount) ++ " " ++ ?_T("subscribers")
                        }
                    },
                    #listitem{body=
                        #link{url="/dashboard/filter/comments/user/" ++ wf:to_list(CheckedUser),
                            text=integer_to_list(CommentsCount) ++ " " ++ ?_T("comments")
                        }
                    },
                    #listitem{body=
                        #link{url="/dashboard/filter/like/user/" ++ wf:to_list(CheckedUser),
                            text=integer_to_list(LikesCount) ++ " " ++ ?_T("likes")
                        }
                    }
                ]}
            ];
        _-> ""
    end,
    StatusBlock = case {CheckedUser, IsSubscribedUser} of
        {undefined, undefined} ->
            #panel{class="form-002", body=[
                #form{body=[
                    #dropdown{class="cs-2", id="user_status",
                        postback={set_user_status},
                        value=nsm_users:get_user_game_status(User#user.username),
                        options=[
                            #option{text=?_T("Online"),         value="online"}
%PHASE1                        #option{text=?_T("Offline"),        value="offline"},
%PHASE1                        #option{text=?_T("Busy"),           value="busy"},
%PHASE1                        #option{text=?_T("Free for game"),  value="free_for_game"},
%PHASE1                        #option{text=?_T("Invisible"),      value="invisible"}
                        ]
                    }
                ]}
            ], id="statuschanger"}
        ;_->
            {Status,ClassStatus} = case nsm_users:get_user_game_status(CheckedUser) of
                "offline"       -> {"Offline","stat-offline"};
                "busy"          -> {"Busy", "stat-busy"};
                "free_for_game" -> {"Free for game", "stat-ffg"};
                "invisible"     -> {"Invisible", "stat-invisible"};
                _               -> {"Online","stat-online"}
            end,
            io_lib:format("<span class=\"stat-info\"><span class=\"~s\">~s</span></span>", [ClassStatus, Status])
    end,
    wf:wire(wf:f("TestStOpt[0]='~s';",[wf_event:serialize_event_context({set_user_status, "online"}, undefined, undefined, dashboard)])),
    wf:wire(wf:f("TestStOpt[1]='~s';",[wf_event:serialize_event_context({set_user_status, "offline"}, undefined, undefined, dashboard)])),
    wf:wire(wf:f("TestStOpt[2]='~s';",[wf_event:serialize_event_context({set_user_status, "busy"}, undefined, undefined, dashboard)])),
    wf:wire(wf:f("TestStOpt[3]='~s';",[wf_event:serialize_event_context({set_user_status, "free_for_game"}, undefined, undefined, dashboard)])),
    wf:wire(wf:f("TestStOpt[4]='~s';",[wf_event:serialize_event_context({set_user_status, "invisible"}, undefined, undefined, dashboard)])),
    [
        #panel{class="top-box", body=[
            case nsm_accounts:user_paid(element(2, User) ) of
                true -> 
                    #panel{class="paid_user_avatar_photo", body=[#image{image=get_user_avatar(element(2, User) ,"small"), style="height:48px; width:48px;"}]};                   
                _ -> 
                    #panel{class="photo", body=[#image{image=get_user_avatar(element(2, User) ,"small"), style="height:48px; width:48px;"}]}
            end, 
            #panel{class="holder", body=[
                io_lib:format("<strong class=\"title\">~s</strong>", [?_T("Status")]),
                StatusBlock,
                #list{class="list-ico", body=[
                    #listitem{body=#image{image="/images/ico-001.png", style="width:12px; height:17px"}},
                    #listitem{body=#image{image="/images/ico-001.png", style="width:12px; height:17px"}},
                    #listitem{body=#image{image="/images/ico-001.png", style="width:12px; height:17px"}},
                    #listitem{body=#image{image="/images/ico-001.png", style="width:12px; height:17px"}},
                    #listitem{body=#image{image="/images/ico-001.png", style="width:12px; height:17px"}},
                    #listitem{body=#image{image="/images/ico-001.png", style="width:12px; height:17px"}}
                ]}
            ]}
        ]},
        #h3{class="title-box", text=wf:to_list(User#user.name) ++ " " ++ wf:to_list(User#user.surname)},
        #panel{class="block", body=[
            #panel{class="gallery-game", body=[
                #panel{class="slider-container", body=[
                 #panel{class="slider-content", body=[
                  #panel{class="slider", body=[
                    #list{body=[
                        #listitem{body=#image{image="/images/img-013.gif", style="width:53px;height:26px"}},
                        #listitem{body=io_lib:format("<span>~s</span><strong>~s</strong>",[?_T("Num"), integer_to_list(12)])},
                        #listitem{body=io_lib:format("<span>~s</span><strong>~s</strong>",[?_T("Points"), integer_to_list(2133)])}
                    ]}
                  ]},
                  #panel{class="slider", body=[
                    #list{body=[
                        #listitem{body=#image{image="/images/img-013.gif", style="width:53px;height:26px"}},
                        #listitem{body=io_lib:format("<span>~s</span><strong>~s</strong>",[?_T("Num"), integer_to_list(1)])},
                        #listitem{body=io_lib:format("<span>~s</span><strong>~s</strong>",[?_T("Points"), integer_to_list(32324)])}
                    ]}
                  ]},
                  #panel{class="slider", body=[
                    #list{body=[
                        #listitem{body=#image{image="/images/img-013.gif", style="width:53px;height:26px"}},
                        #listitem{body=io_lib:format("<span>~s</span><strong>~s</strong>",[?_T("Num"), integer_to_list(2)])},
                        #listitem{body=io_lib:format("<span>~s</span><strong>~s</strong>",[?_T("Points"), integer_to_list(223445)])}
                    ]}
                  ]},
                  #panel{class="slider", body=[
                    #list{body=[
                        #listitem{body=#image{image="/images/img-013.gif", style="width:53px;height:26px"}},
                        #listitem{body=io_lib:format("<span>~s</span><strong>~s</strong>",[?_T("Num"), integer_to_list(3)])},
                        #listitem{body=io_lib:format("<span>~s</span><strong>~s</strong>",[?_T("Points"), integer_to_list(456464)])}
                    ]}
                  ]},
                  #panel{class="slider", body=[
                    #list{body=[
                        #listitem{body=#image{image="/images/img-013.gif", style="width:53px;height:26px"}},
                        #listitem{body=io_lib:format("<span>~s</span><strong>~s</strong>",[?_T("Num"), integer_to_list(4)])},
                        #listitem{body=io_lib:format("<span>~s</span><strong>~s</strong>",[?_T("Points"), integer_to_list(3453)])}
                    ]}
                  ]},
                  #panel{class="slider", body=[
                    #list{body=[
                        #listitem{body=#image{image="/images/img-013.gif", style="width:53px;height:26px"}},
                        #listitem{body=io_lib:format("<span>~s</span><strong>~s</strong>",[?_T("Num"), integer_to_list(5)])},
                        #listitem{body=io_lib:format("<span>~s</span><strong>~s</strong>",[?_T("Points"), integer_to_list(56757)])}
                    ]}
                  ]}
                 ]}
                ]},
                #link{class="prev-link"},
                #link{class="next-link"}
            ]},
            MenuTail
        ]},
        statistics_block(User)
    ].

statistics_block(Info) ->
    try % this fails if user reloads page fast enough after deleting an entry
        EntriesCount  = feed:get_entries_count(Info#user.username),
        CommentsCount = feed:get_comments_count(Info#user.username),
        Subscriptions = nsm_users:list_subscr(Info#user.username),
        Subscribers   = nsm_users:list_subscr_me(Info#user.username),
        LikesCount    = feed:get_user_likes_count(Info#user.username),

        #panel{class="box statistics-box-text", body=[
            "<span id='guidersstatistics'>",
            #h3{text=?_T("STATISTICS")},
            #span{text=?_T("Subscriptions")++": "++integer_to_list(length(Subscriptions))},
            #br{},
            #span{text=?_T("Subscribers")++": "++integer_to_list(length(Subscribers))},
            #br{},
            #span{text=?_T("Entries")++": "++integer_to_list(EntriesCount)},
            #br{},
            #span{text=?_T("Comments")++": "++integer_to_list(CommentsCount)},
            #br{},
            #span{text=?_T("Likes")++": "++integer_to_list(LikesCount)},
            "</span>"
        ]}
    catch
        _:_ ->
            ""  % well, if it is impossible to gather statistic right now - there will be no statistics block
    end.

affiliates_if_any(User) ->
    case nsm_affiliates:is_existing_affiliate(User#user.username) of
        true ->
            affiliates(User);
        false -> []
    end.

affiliates(User) ->
    Nav = case User#user.username == wf:user() of
        true ->
            #span_b{class="links", body=[
                #link{text=?_T("All your affiliate followers"), url="/affiliates"}
            ]};
        false ->
            #span_b{class="links", body=[
                #link{text=?_T("All affiliate followers of") ++ User#user.username, url="/affiliates/of/"++User#user.username}
            ]}
    end,
    AffiliateList = nsm_affiliates:get_followers(User#user.username),
    case AffiliateList of
        [] ->
            [];
        AL ->
            ShortList = ling:part(AL, 1, 10),
            #panel{class="box", body=[
                #h3{text=?_T("AFFILIATE")},
                #list{class="list-photo", body=[
                    #listitem{body=[
                        #image{image=get_user_avatar(Who), style="width:32px,height:33px"},
                        #link{text=Who, url=site_utils:user_link(Who)}
                    ]}
                    || {Who, _N} <- ShortList]},
                Nav
            ]}
    end.

get_hemen_nav() ->
    get_hemen_nav(dashboard).
get_hemen_nav(Page) ->
    ListClass = case Page of dashboard -> "list-top-photo"; tournament -> "list-top-photo"; _ -> "" end,
    #list{class="hemen-nav "++ListClass, body=[
    begin
        Link = case Page of tournament -> #link{postback=Postback}; _ -> #link{url=Url} end,
        #listitem{class=Class, body=[Link#link{body=[
	    #panel{class="box",body=[
		#panel{class="img", body=[#image{image=Img1, style="width:160px;height:88px"}]},
		#panel{class="img img-2", body=[#image{image=Img2, style="width:160px;height:88px"}]}
	    ]},
	    case Page of
                dashboard -> "";
                tournament -> "";
		_ -> #span{class="descr", text=?_T("Let's Play!")}
	    end,
	    #panel{class="stat", body=[#span{text=?_T("Very soon...")}]},
	    "<em><img class=\"png\" src=\""++Img3++"\" alt=\"\" width=\"77\" height=\"77\" ></em>"
	]}]}
    end || {Postback, Url, Class, Img1, Img2, Img3} <-
	[
	{{game, okey}, ?_U("/matchmaker/okey"), "mkh_active", "/images/img-007.jpg", "/images/img-008.jpg", "/images/text-okey.png"},
	{{game, tavla}, ?_U("/matchmaker/tavla"), "mkh_active", "/images/img-005.jpg", "/images/img-006.jpg", "/images/text-tavla.png"},
	{undefined, "#", "", "/images/img-003.jpg", "/images/img-004.jpg", "/images/text-king.png"},
	{undefined, "#", "", "/images/img-001.jpg", "/images/img-002.jpg", "/images/text-batak.png"},
	{undefined, "#", "", "/images/img-009.jpg", "/images/img-010.jpg", "/images/text-sorbi.png"}
	]
    ]}.

page_module() ->
    wf:to_list(wf_context:page_module()).

print_head() ->
    Scripts = get_head(),
    [
        case Type of
	        script -> "\t<script src=\""++Element++"\" type=\"text/javascript\" charset=\"utf-8\"></script>\n";
        	raw -> "\t"++Element++"\n";
        	Other -> ?PRINT({"Error", Other})
        end || {Type, Element} <- Scripts
    ].

get_head() ->
    case wf:state(scripts) of
    	S when is_list(S) -> S;
	    _ -> []
    end.

add_script(Src) ->
    add_to_head({script, Src}).

add_raw(Data) ->
    add_to_head({raw, Data}).

add_to_head({Type, Element}) ->
    Scripts = get_head(),
    NewScripts = [{Type, Element}|Scripts],
    wf:state(scripts, NewScripts).

more_button(Module, Entries, Pageamount) ->
    case length(Entries) < Pageamount of
        true ->
            [];
        _ ->
            Last = lists:last(Entries),
            #button{class="btn-submit", text = ?_T("More"),
                postback = {more_entries, Module, Pageamount, Last#entry.id}
            }
    end.

post_user_system_message(Description) ->
    User = wf:user(),
    Destinations = [{User, user}],
    ID = utils:uuid_ex(),
    Route = [feed, user, User, entry, ID, add_system],
    nsx_msg:notify(Route, [User, Destinations, Description, []]),
    ID.

unpost_user_system_message(ID) ->   % this hasn't been tested yet
    User = wf:user(),
    Destinations = [{User, user}],
    Route = [feed, user, User, entry, ID, delete_system],
    nsx_msg:notify(Route, [User, Destinations, "", []]).

guiders_ok(Cookie) ->
%    true.
  case wf:cookie("replayguiders") of
    "yes" -> true;
    _ -> 
      {_, NowSecs, _} = erlang:now(),
      UserSecsOrUndefined = (webutils:user_info())#user.register_date,
      case UserSecsOrUndefined of
        {_, Number ,_} -> UserSecs = Number;
        _ -> UserSecs = NowSecs
      end,
      case NowSecs-UserSecs<86400 of % first 24 hours only (for these, who disable or loose cookies)
        false -> false;
        true -> 
          case wf:cookie(Cookie ++ wf:user()) of
            "yes" -> false;
            _ ->
              wf:cookie(Cookie ++ wf:user(), "yes", "/", 24*60),
              true
          end
      end
  end.

js_for_main_authorized_game_stats_menu() ->
    webutils:add_script("/nitrogen/js/jquery.autosize-min.js"),
    webutils:add_script("/nitrogen/js/jquery.scrollTo-1.4.2-min.js"),
    webutils:add_script("/nitrogen/js/jquery.serialScroll-1.2.2-min.js"),
    webutils:add_raw("
    <link href='/nitrogen/video-js/video-js.css' rel='stylesheet'>
    <script src='/nitrogen/video-js/video.js'></script>

    <link href='/nitrogen/guiders-js/guiders-1.2.8.css' rel='stylesheet'>
    <script src='/nitrogen/guiders-js/guiders-1.2.8.js'></script>

    <script src=\"/nitrogen/js/form.js\" type=\"text/javascript\"></script>
    <script src=\"/nitrogen/swfobject.js\" type=\"text/javascript\" charset=\"utf-8\"></script>
    <script src=\"/nitrogen/audio-player/audio-player.js\" type=\"text/javascript\" charset=\"utf-8\"></script>
    <script type=\"text/javascript\">

        AudioPlayer.setup(\"/nitrogen/audio-player/player.swf\", {width: 290});

        function upd_scrollers(){
            $('.scroller').serialScroll({
                cycle:true,
                items:'img',
                start:0,
                duration:500,
                force:true,
                stop:true,
                lock:false,
                exclude:1,
                event:'click'
            });
            $('.scroller_container').each(function(i){
            $(this).find(\".scroller_prev\").bind('click',{mel:i},
                function(event){$('.scroller').each(function(j){
                    if(j==event.data.mel){$(this).trigger('prev')}})
                });
            $(this).find(\".scroller_next\").bind('click',{mel:i},
                function(event){$('.scroller').each(function(j){
                    if(j==event.data.mel){$(this).trigger('next')}})
                });
            });
        }

        function game_slider(){
            $('.gallery-game').serialScroll({
                target:'.slider-container',
                cycle:true,
                items:'div.slider',
                start:0,
                auto:2000,
                duration:500,
                force:true,
                stop:true,
                lock:false,
                event:'click',
                prev:'.prev-link',
                next:'.next-link'
            });
        }

        function clear_inner_textarea(){
            var VRR = $('.inner_textaera');
            VRR.val('');
        }

        function getOffset( el ) {
            var _x = 0;
            var _y = 0;
            while( el && !isNaN( el.offsetLeft ) && !isNaN( el.offsetTop ) ) {
                _x += el.offsetLeft - el.scrollLeft;
                _y += el.offsetTop - el.scrollTop;
                el = el.offsetParent;
            }
            return { top: _y, left: _x };
        }

        function clear_tauto(){
            clear_inner_textarea();
            var Ss = $('.wfid_to_kaka_user').size();

            var ACBY = getOffset( document.getElementById('atocompletetextbox') ).top - 225;
            var ACBX = getOffset( document.getElementById('atocompletetextbox') ).left - 35;

            $('.ui-autocomplete').css('left', ACBX + 'px');
            $('.ui-autocomplete').css('top', ACBY + 'px');

            if((Ss % 8)==0 && Ss > 1){
                $('.wfid_form001toRow').css('height', (parseInt(Ss/8)+1)*30 + 'px' );
                $('.row .inner-row').css('height', ((parseInt(Ss/8)+1)*32+8) + 'px' );
            }else if(Ss < 8){
                $('.wfid_form001toRow').css('height', '30px' );
                $('.row .inner-row').css('height', '30px' );
            }
            if(Ss == 0){
                $('.wfid_form001toRow').css('display', 'none');
            }
        }

        function remove_all_tos(){
            $('.wfid_form001toRow').css('display', 'none');
            $('.wfid_flashm').children().remove();
        }

        function set_focus_to_search(){
            $('.wfid_add_entry_textbox').focus();
        }

        function upd_parent_to_float(BoxId){
            $('.wfid_' + BoxId).css('float', 'left');
            $('.wfid_' + BoxId).css('clear', '');
        }

        var TestStOpt = new Array();
        var NitrogenDropDownPostBackOptions = {values_array:'TestStOpt', select_number:3};
        var MyFeedEvent = '';
        function add_myfeed_to(){

            var ACBY = getOffset( document.getElementById('atocompletetextbox') ).top - 225;
            var ACBX = getOffset( document.getElementById('atocompletetextbox') ).left - 35;

            var Ss = $('.wfid_to_kaka_user').size();
            if(Ss == 0){
                Nitrogen.$queue_event(null, MyFeedEvent, '');
            }
            $('.ui-autocomplete').css('left', ACBX + 'px');
            $('.ui-autocomplete').css('top', ACBY + 'px');
        }

        function delete_flash_to(Ele){
            var E1 = $(Nitrogen.$anchor_path);
            var E2 = $(Nitrogen.$anchor_path).parent();
            E2.remove();
        }

        function qtip_all_links(){
            //%PHASE1 This came from qTip demo page to make default tooltips look better
            $('#content a[href][title]').qtip({
                content: {
                    text: false // Use each elements title attribute
                },
                hide: {
	                event: 'click'
                },
                show: { delay: "++?TOOLTIP_TIMEOUT++" }
            });
        }

        $(document).ready(function(){
            upd_scrollers();
            game_slider();

            //%PHASE1
            qtip_all_links();

            objs('sendentry').qtip({    // share button

                // these properties are actually working
                content: '" ++ ?_T("Click here to post your entry to the feed. You can still remove it anytime") ++ "',
                show: { delay: "++?TOOLTIP_TIMEOUT++" },
                position: {
	                my: 'top right',
	                at: 'bottom left'
                }
            });

            objs('statuschanger').qtip({    // status combobox
                show: { delay: "++?TOOLTIP_TIMEOUT++" },
                content: '" ++ ?_T("Change your status to show what are you up to") ++ "'
            });

            objs('friendslink').qtip({content:{text:false}, show: { delay: "++?TOOLTIP_TIMEOUT++" }});
            objs('groupslink').qtip({content:{text:false}, show: { delay: "++?TOOLTIP_TIMEOUT++" }});

            objs('directmessageslink').qtip({content:{text:false}, show: { delay: "++?TOOLTIP_TIMEOUT++" }});
            objs('myfeedlink').qtip({content:{text:false}, show: { delay: "++?TOOLTIP_TIMEOUT++" }});

            objs('leavegrouplink').qtip({content:{text:false}, show: { delay: "++?TOOLTIP_TIMEOUT++" }});

            objs('add_entry_textbox').autosize();

        });
	</script>").
