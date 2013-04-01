-module(webutils).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_db/include/user.hrl").
-include_lib("nsm_db/include/config.hrl").
-include_lib("nsm_db/include/tournaments.hrl").
-include_lib("nsm_db/include/feed.hrl").
-include_lib("nsm_db/include/table.hrl").
-include_lib("nsm_db/include/accounts.hrl").
-include_lib("nsm_db/include/scoring.hrl").
-include("elements/records.hrl").
-include("setup.hrl").
-include("common.hrl").

-define(TOOLTIP_TIMEOUT, "1500").

main() -> [].

user_info() -> User = wf:session(user_info).
user_info(Field) -> user_info(user_info(), Field).
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

header()->
  [#panel{class=header, body=[
    #panel{class=headerblue, body=[]},
    #panel{class="block", body=[
      #span{class="logo vcard", body=logo()},
      account_menu(),
      menu_links()
    ]}
  ]},
  lightboxes()].

header_box() -> 
   #template { file=code:priv_dir(nsp_srv)++"/templates/header.html"}.

header_body() -> [account_menu(), menu_links()].

account_menu() ->
  Element = case wf:user() /= undefined of
    true ->
      case R = nsm_users:get_user(wf:user()) of 
        {error,notfound} -> event(logout);
        _UserFound ->
          {ok, User} = R,
          Submenus = #list{body=[
            #listitem{body=#link{url=Url,text=Text}} || {Url,Text}  <-
              [ {?_U("/profile"), ?_T("My Profile")},
                {?_U("/profile/account"), ?_T("My Account")},
                {?_U("/profile/gifts"), ?_T("My Gifts")},
                {?_U("/profile/stats"), ?_T("Stats")},
                {?_U("/profile/invite"), ?_T("Invite Buddy")}
              ] ++  case nsm_acl:check_access(User, {feature, admin}) of
                      allow -> [{?_U("/kakaadmin"), ?_T("Admin")}];
                      _ -> []
                    end]},
          {ok,#user_info{username=Username,avatar_url = AvatarUrl}} = nsm_auth:get_user_info(webutils:user_info(username)),
          {ok, Quota}  = nsm_accounts:balance(Username, ?CURRENCY_QUOTA),
          {ok, Kakush} = nsm_accounts:balance(Username, ?CURRENCY_KAKUSH),
          LogoutLink = case wf:session(is_facebook) of
            true -> [];
            _ -> #listitem{body=[#link{text=?_T("Logout"), postback=logout}]}
          end,
          #list{class="user-menu", body=[
            #listitem{body=[
              #link{url="#", body=[ 
                #image{image=AvatarUrl, id=header_user_avatar, style="width:23px;height:23px", alt="#"},
                #label{class="username", text=Username}]},
                Submenus
            ]},
            #listitem{class=quota, body=[ #link{text=lists:concat([?_T("Quota"), " : ",Quota])}]},
            #listitem{class=kakus, body=[ #link{text=lists:concat([?_T("Kakush")," : ",Kakush])}]},
            #span{id=logout_btn, body=LogoutLink}
          ]}
      end;
    _UserLoggedIn ->
        [#list{class="user-menu", body=[
          %#listitem{body=fb_utils:login_btn()},
          #listitem{body=#link{class=login, text=?_T("Login"), url=?_U("/login")}},
          #listitem{body=#link{class=signup, text=?_T("Signup"), postback=register}}
        ]}]
  end,
  #panel{class="top", body= #panel{class="ar", body=[#panel{class="box", body=Element}]}}.

menu_links() ->
  ["<nav>",
  #list{body=[
%    #listitem{body=#link{text=?_T("Home"), url=?_U("/"), id="mainmenumainpage"}},
    #listitem{show_if=wf:user()=/=undefined, body=#link{text=?_T("Wall"), url=?_U("/wall"),
      title=?_T("You can share information with others"), id="mainmenumypage"}},
    #listitem{body=#link{text=?_T("Matchmaker"), url=?_U("/matchmaker/okey"),
      title=?_T("Set your game criteria and face your opponent"), id="mainmenumatchmaker"}},
    #listitem{body=#link{text=?_T("Rules"), url=?_U("/rules-okey"),
      title=?_T("Read the rules of our games"), id="mainmenurules"}},
    #listitem{body=#link{text=?_T("Gifts"), url=?_U("/gifts"),
      title=?_T("You can see all the prizes here"), id="mainmenugifts"}},
    #listitem{body=#link{text=?_T("Tournaments"), url=?_U("/tournaments"),
      title=?_T("You can join tournaments and show them all"), id="mainmenutournaments"}},
    #listitem{body=#link{text=?_T("Groups"), url=?_U("/groups"),
      title=?_T("You can manage your groups settings here"), id="mainmenugroups"}}
  ]},
  "</nav>"
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

footer() ->
  case wf_context:page_module() of
    view_table -> [];
    _ ->
      LinkList = [
%       #listitem{body=[?_T("About Us")] },
        #listitem{body=#link{url=?_U("/gifts"), text=?_T("Gifts")}},
%       #listitem{body=?_T("Pointing System")},
        #listitem{body=#link{url=?_U("/terms"), text=?_T("Terms of Service")}},
        #listitem{body=#link{url=?_U("/privacy"), text=?_T("Privacy Policy")}},
        #listitem{body=wf_tags:emit_tag(a, ?_T("Help & Support"),
          [{href, "https://kakaranet.uservoice.com/"},{target,"_blank"}])},
        #listitem{body=#link{url=?_U("/contact"), text=?_T("Contact")}},
        #listitem{body=[?_T("2011&mdash;2012 &copy; Kakaranet. All rights reserved."),"<br/>",
                        ?_T("Kakaranet is registered trademark of Paynet Inc."),"<br/>"]},
        #listitem{body=[#checkbox { id=replay_guiders, text=?_T("Replay Guiders"), postback=replay_guiders_changed,
                                    checked=(wf:cookie("replayguiders")=="yes") }]}
      ],
      #panel{class="page-content footer", body=[#list{class="navbar", body = LinkList},language()]}
  end.

footer_box() ->
  case wf_context:page_module() of
    view_table -> [];
    _ ->
      LinkList = [
%       #listitem{body=[?_T("About Us")] },
        #listitem{body=#link{url=?_U("/gifts"), text=?_T("Gifts")}},
%       #listitem{body=?_T("Pointing System")},
        #listitem{body=#link{url=?_U("/terms"), text=?_T("Terms of Service")}},
        #listitem{body=#link{url=?_U("/privacy"), text=?_T("Privacy Policy")}},
        #listitem{body=wf_tags:emit_tag(a, ?_T("Help & Support"),
          [{href, "https://kakaranet.uservoice.com/"},{target,"_blank"}])},
        #listitem{body=#link{url=?_U("/contact"), text=?_T("Contact")}},
        #listitem{body=[?_T("2011&mdash;2012 &copy; Kakaranet. All rights reserved."),"<br/>",
                        ?_T("Kakaranet is registered trademark of Paynet Inc."),"<br/>"]},
        #listitem{body=[#checkbox { id=replay_guiders, text=?_T("Replay Guiders"), postback=replay_guiders_changed,
                                    checked=(wf:cookie("replayguiders")=="yes") }]}
      ],


      ["<footer>",
        #list{class="navbar", body = LinkList},
        language(),
      "</footer>"]
  end.

uservoice() ->
  [].

event({error, Msg}) ->
    wf:wire(#alert{text=Msg});

event(login) ->
    login(login,password,login_hintbox);
event(register)->
    wf:session(fb_registration, undefined),
    wf:redirect(?_T("/login/register"));
event(logout) ->
  ?INFO("logout"),
  wf:session(fb_registration, undefined),
  wf:session(logged_with_fb, undefined),
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

event({more_entries, wall, LastId}) ->
  wall:more_entries(LastId);

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
  fb_utils:event(Other),
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
                         {ok, User} -> login:login_user(User);
              {error, not_verified} -> login:login_user(User);
            {error, user_not_found} -> display_error(MsgBox, "User not found");
                    {error, banned} -> display_error(MsgBox, "Account is banned.");
                   {error, unknown} -> display_error(MsgBox, "Your account is damaged. Contact with administrator.");
        {error, incorrect_password} -> display_error(MsgBox, "Bad password")
    end.

display_error(MsgBox, Message)->
    wf:update(MsgBox,?_T(Message)),
    wf:wire(MsgBox, #show{effect=slide, speed=300}).

show_if(remove_entry, Entry) ->
  CurrentUser = wf:user(),
  {Type, EntryOwner} = case wf:state(feed_owner) of
    {T, N} -> {T, N};
    _ -> {user, CurrentUser}
  end,
  case Type of
    _Any when Entry#entry.from == CurrentUser -> true;
    group ->
      case nsm_groups:user_is_owner(CurrentUser, EntryOwner) of
        true -> true;
        _ -> false
      end;
    user when CurrentUser == EntryOwner -> true;
    _ -> false
  end;
show_if(_, _Entry) -> false.

node_users() ->
  {Users,B} = lists:partition(fun({_,_,{A,user,Time}}) -> 
                  {_,X}=calendar:time_difference(Time, calendar:now_to_datetime(now())),
                  X < {0,10,0} end, [ X || X={_,_,{_,user,_}} <- qlc:e(gproc:table())]),
%  [ exit(Pid,kill) || {_,Pid,{A,user,Time}} <- B],
  Users.

online_users() ->
  OnlineUsers = nsm_queries:map_reduce(webutils,node_users,[]),
  sets:to_list(sets:from_list([User||{_,_,{User,user,Time}} <- OnlineUsers])).

counters()->
  case wf:user() of
       undefined ->  skip;
       LoggedUser -> wf:comet(fun() -> 
                   CometPid = self(), 
                   user_counter:register_user(CometPid),
                   gproc:reg({p,l,CometPid},{LoggedUser,user,calendar:now_to_datetime(now())}),
                   comet_update() end)
  end,
  OnlineCount = integer_to_list(user_counter:user_count()), 
  Games = [okey, tavla, king, batak, sorbi],

  [

  #panel{class="stat-bar", body=[
    "<dl class=\"dlist\">",
      "<dt>", ?_T("Online Gamers"),":", "</dt>",
      "<dd>",OnlineCount,"</dd>"
    "</dl>",
    #list{body=[counter_item(G) || G <- Games]}
  ]}

  ,
 #panel{class="list-top-photo-h page-content", body = [
          #span{style="font-size:14px; line-height:24px;font-weight:bold;", body=[?_T("Players"), ": ",
                    [ 
                        case site_utils:user_link(Who) of
                          undefined -> "";
                          "" -> "";
                          URL ->
                            #span{body=#link{url=URL, text=Who ++ " ", style = "font-weight:bold;"}}
                        end
                     || Who <- online_users() ]
                ]}]}

  ].

comet_update() ->
   receive X -> skip
%   after 30 -> skip
   end, comet_update().

counter_item(Game)->
  [H|Name] = atom_to_list(Game),
  #listitem{body=[
    "<dl>",
      "<dt>", string:to_upper(H), Name, "</dt>",
      "<dd>", user_count(Game), "</dd>",
    "</dl>"
  ]}.

api_event(fbAutoLogin, fb, Args)-> skip;
api_event(Name, fb, Args)-> fb_utils:api_event(Name, fb, Args);
api_event(Name, Tag, Args)-> skip.

user_count(GameH) ->
  A= nsm_queries:map_reduce(game_manager,counter,[case GameH of
      tavla -> game_tavla;
      okey -> game_okey; _ -> GameH
    end]),
  GameCounts = lists:foldl(fun(X, Sum) -> X + Sum end, 0, A),
  integer_to_list(GameCounts).

get_members(GId) ->
    Nav = [
        #span_b{class="links", body=[
          #link{text=?_T("All members"), url=?_U("/connections/group")++"/id/"++GId}
        ]}
    ],
    get_metalist(GId, ?_T("MEMBERS"), group, ?_T("Group have no members"), Nav).

get_metalist(Id, Title, Type, EmptyMsg, Nav) ->
    Data = case nsm_queries:cached_friends([Id,Type]) of
                {badrpc,_} -> nsm_users:retrieve_connections(Id,Type);
                undefined -> nsm_users:retrieve_connections(Id,Type);
                X -> X end, 
    ?INFO("metalist: ~p",[{Id,Type,Data}]),
    Friends = case Data of
                [] -> [EmptyMsg];
                Full -> lists:flatten([#listitem{body=[#image{image=get_avatar(Who), 
                                          style= case Paid of true -> "border:3px solid #ffb03b; padding:0px;"; _ -> "" end},
                               #link{text=RealName, style="font-size:12pt;",url=site_utils:user_link(Who)}]}||{Who,Paid,RealName}<-Data]) end,
    #panel{class="box", style="border:0", body=[
        #h3{text=Title, class="section-title" },
        #list{class="list-photo", body=[ Friends ]}, Nav ]}.

get_avatar(U) -> get_avatar(U,"tiny").
get_avatar(#user{avatar={_,_,_,Avatar}},"tiny") -> Avatar;
get_avatar(#user{avatar={_,_,Avatar,_}},"small") -> Avatar;
get_avatar(#user{avatar={_,Avatar,_,_}},"big") -> Avatar;
get_avatar(_,"tiny") -> "/images/no_avatar_tiny.jpg";
get_avatar(_,"small") -> "/images/no_avatar_small.jpg";
get_avatar(_,"big") -> "/images/no_avatar_big.jpg".

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

quick_nav() -> quick_nav(false).

quick_nav(FullSize) ->
  #list{class="quick-nav", body=[
    begin
      S = case Status of
        inactive -> #span{class="soon fullsize_"++atom_to_list(FullSize), text=?_T("Very soon...")};
        _ -> []
      end,
    #listitem{class=Status, body=
      #link{url=Url, postback=Postback, body=[
        #image{class="game-title", image=Title, width="77", height="77"},
        #image{class=current, image=Img1, width="160", height="88"},
        S,
        #span{class="lets-play", text=?_T("Let's Play!"), show_if=FullSize}
      ]}
    }
    end
    || {Status, Postback, Url, Img1, Title} <-
     [{active,{game, okey}, ?_U("/matchmaker/okey"), ?STATIC_ADDRESS++"/images/img-007.jpg", ?STATIC_ADDRESS++"/images/text-okey.png"},
      {active, {game, tavla}, ?_U("/matchmaker/tavla"), ?STATIC_ADDRESS++"/images/img-005.jpg", ?STATIC_ADDRESS++"/images/text-tavla.png"},
      {inactive, undefined, "#", ?STATIC_ADDRESS++"/images/img-003.jpg", ?STATIC_ADDRESS++"/images/text-king.png"},
      {inactive,undefined,  "#", ?STATIC_ADDRESS++"/images/img-001.jpg", ?STATIC_ADDRESS++"/images/text-batak.png"},
      {inactive, undefined, "#", ?STATIC_ADDRESS++"/images/img-009.jpg", ?STATIC_ADDRESS++"/images/text-sorbi.png"}]
  ]}.


page_module() ->
    wf:to_list(wf_context:page_module()).

page_script_path() -> ?STATIC_ADDRESS++ "/js/k" ++ page_module()++".min.js".

secondary_script() -> 
  case wf_context:page_module() of
    wall -> ?STATIC_ADDRESS ++ "/js/k"++ page_module() ++ ".sec.min.js";
    _ -> []
  end.

page_css_path() -> ?STATIC_ADDRESS++ "/css/k" ++ page_module()++".min.css".

page_script() ->
  case wf_context:page_module() of
    index -> ["$(document).ready(slideShow(\""++ site_utils:detect_language() ++"\"));"];
    _ -> []
  end.

user_voice()->
  case wf_context:page_module() of
    index -> [];
    _Mod ->
      ["<script type=\"text/javascript\">",
        "var uvOptions = {};",
        "(function() {",
          "var uv = document.createElement('script');",
          "uv.type = 'text/javascript';",
          "uv.async = true;",
          "uv.src = ('https:' == document.location.protocol ? 'https://' : 'http://') + 'widget.uservoice.com/2nRvnf6ACekkhJwy8PZlA.js';",
          "var s = document.getElementsByTagName('script')[0];",
          "s.parentNode.insertBefore(uv, s);",
        "})();",
      "</script>"]
  end.

print_head() ->
  Scripts = get_head(),
  [case Type of
      script -> "\t<script src=\""++Element++"\" type=\"text/javascript\" charset=\"utf-8\"></script>\n";
      raw -> "\t"++Element++"\n";
      Other -> ?PRINT({"Error", Other})
    end || {Type, Element} <- Scripts].

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

post_user_system_message(Description) ->    % this should be rewised completely
    User = wf:user(),
%    Destinations = [{User, user}],
    Destinations = [{"kakaranet", group}],
    ID = utils:uuid_ex(),
%    Route = [feed, user, User, entry, ID, add_system],
    Route = [feed, group, "kakaranet", entry, ID, add_system],
    nsx_msg:notify(Route, [User, Destinations, Description, []]),
    ID.

guiders_ok(Cookie) ->
  case wf:cookie("replayguiders") of
    "yes" -> true;
    _ ->
      case wf:user() of 
        undefined -> false;
        User ->
          {_, NowSecs, _} = erlang:now(),
          UserSecsOrUndefined = (webutils:user_info())#user.register_date,
          UserSecs = case UserSecsOrUndefined of
            {_, Number ,_} -> Number;
            _ -> NowSecs
          end,
          case NowSecs-UserSecs<86400 of % first 24 hours only (for those, who disable or lost cookies)
            false -> false;
            true ->
              case wf:cookie(Cookie ++ User) of
                "yes" -> false;
                _ -> wf:cookie(Cookie ++ User, "yes", "/", 24*60), true
              end
          end
      end
  end.
