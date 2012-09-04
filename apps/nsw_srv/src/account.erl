%% -*- mode: nitrogen -*-
-module (account).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_srv/include/user.hrl").
%-include_lib("zealot/include/config.hrl").
-include_lib("nsm_srv/include/invite.hrl").

-include("elements/records.hrl").
-include("setup.hrl").
-include("gettext.hrl").

main() ->
    case wf:user() /= undefined of
        true  -> main_authorized();
        false -> wf:redirect_to_login(?_U("/login"))
    end.

main_authorized() -> #template { file=code:priv_dir(nsw_srv)++"/templates/bare.html" }.

title() -> webutils:title(?MODULE).


body() ->
    Body = case wf:q('__submodule__') of
	       "invite" ->
		   invite();
	       _ ->
		   welcome()
	   end,
    #container_12 {
      body=[#grid_3 {alpha=true, class="account_menu border-form", body=menu() },
            #grid_9 { id=view_box, omega=true,
                      class="account_view_box border-form", body=Body }
           ]}.



logo() ->
    [
     #image{image="/images/logo.png", style="float: left; margin-top: 25px;"}
    ].

menu() ->
    [#p{body=#link{text=?_T("Profile"), postback={show, profile}}},
     #p{body=#link{text=?_T("Gifts"), postback={show, gifts}}},
     #p{body=#link{text=?_T("Account"), postback={show, account}}},
     #p{body=#link{text=?_T("Stats"), postback={show, stats}}},
     #p{body=#link{text=?_T("Invite"), postback={show, invite}}}].


%%Kunthar
%% Some texts disabled. ?T() added to some texts
welcome() ->
    [#br{},#br{},#br{},#br{},
     #br{},#br{},#br{},#br{},
     #h1{text=?_T("Hello there...")},
     #br{},#br{},#br{},#br{},
     #br{},#br{},#br{},#br{},
     %%#h1{text="Welcome text..."},
     #br{},#br{},#br{},#br{},
     #br{},#br{},#br{},#br{}].

stats() ->
    [#br{},#br{},#br{},#br{},
     #br{},#br{},#br{},#br{},
     #h1{text=?_T("Stats...")},
     #br{},#br{},#br{},#br{},
     #br{},#br{},#br{},#br{},
     %%#h1{text="Stats..."},
     #br{},#br{},#br{},#br{},
     #br{},#br{},#br{},#br{}].

account() ->
    [#br{},#br{},#br{},#br{},
     #br{},#br{},#br{},#br{},
     #h1{text=?_T("Your account dashboard. You can find all details about your account here.")},
     #br{},#br{},#br{},#br{},
     #br{},#br{},#br{},#br{},
     %%#h1{text="Account..."},
     #br{},#br{},#br{},#br{},
     #br{},#br{},#br{},#br{}].

gifts() ->
    [#br{},#br{},#br{},#br{},
     #br{},#br{},#br{},#br{},
     #h1{text=?_T("Gifts...")},
     #br{},#br{},#br{},#br{},
     #br{},#br{},#br{},#br{},
     %%#h1{text="Gifts..."},
     #br{},#br{},#br{},#br{},
     #br{},#br{},#br{},#br{}].





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% PROFILE %%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

profile() ->
    User = webutils:user_info(),
    #panel{body=[profile_basic_box(User),
                 profile_advanced_box(User),
                 #flash{id=profile_info},
                 save_cancel(User)]}.


profile_basic_box(User) ->
    #panel{class="border-form",
           body=[#panel{body=[#panel{body=avatar_update_box(User)},
                              basic_params(User)], class="account_profile_box"}
                ]
          }.

profile_advanced_box(User) ->
    #panel{class="border-form",
           body=[#panel{body=advanced_params(User),
                        style="text-align: left;",
                        class="account_profile_box"}]}.


advanced_params(User0) ->
    User = record_to_web(User0),

    CityList = webutils:city_list(),
    City = webutils:list_to_options(CityList, User#user.location),

    SexOptions = [?_T("Female"),
                  ?_T("Male")],
    Sex = webutils:list_to_options(SexOptions, User#user.sex),

    EduOptions = [?_T("Elementary School"),
                  ?_T("High School"),
                  ?_T("B.Sc."),
                  ?_T("M.Sc."),
                  ?_T("Phd")],
    Edu = webutils:list_to_options(EduOptions, User#user.education),

    {Year0, Month0, Day0} =
        case User#user.age of
            {_, _, _} = D ->
                D;
            _ ->
                {undefined, undefined, undefined}
        end,

    {Y0, _,_} = erlang:date(),
    ViewYear = Y0-18,

    Day = webutils:create_option_with_number({1,31}, Day0),
    Month = webutils:create_option_with_number({1,12}, Month0),
    Year = webutils:create_option_with_number({ViewYear,1900}, Year0),

    RevokeFacebook =
        case User#user.facebook_id of
            [] ->
                false;
            _ ->
                true
        end,
    UserPoint = rpc:call(?APPSERVER_NODE,users,get_user_point,[User#user.username]),

    [#table{class="account_profile_table",
            rows=[#tablerow{cells=[#tablecell{text=?_T("Birth date: ")},
                                   #tablecell{body=[#dropdown{id=profile_advanced_day,
                                                              class="nice_dropdown",
                                                              options=Day},
                                                    #dropdown{id=profile_advanced_month,
                                                              class="nice_dropdown",
                                                              options=Month},
                                                    #dropdown{id=profile_advanced_year,
                                                              class="nice_dropdown",
                                                              options=Year}]},

                                   #tablecell{text=?_T("City: ")},
                                   #tablecell{body=#dropdown{options=City,
                                                             id=profile_advanced_city,
                                                             class="nice_dropdown"}}]},

                  #tablerow{cells=[#tablecell{text=?_T("Sex: ")},
                                   #tablecell{body=#dropdown{options=Sex,
                                                             id=profile_advanced_sex,
                                                             class="nice_dropdown"}},

                                   #tablecell{text=?_T("Education: ")},
                                   #tablecell{body=#dropdown{options=Edu,
                                                             id=profile_advanced_edu,
                                                             class="nice_dropdown"}}]}
                 ]},
     #br{},#br{},
     #panel{id=revoke_facebook,
            show_if=RevokeFacebook,
            body=#button{class="nice_button",
                         text=?_T("Revoke Access from Facebook"),
                         postback={revoke_facebook, User0}}
           },

        #panel{body=[
            #span{text=?_TS("You have $point$ kakapoints", [{point, UserPoint}])},
            #br{},
            #span{text="You are cheater? ;-) Change your kakapoins (for tests!): "},
            #textbox{id=kakapoint, class=nice_textbox, text=wf:to_list(UserPoint)}
            ]}

           ].



basic_params(User0) ->
    User = record_to_web(User0),
    Body =
        #table{class="account_profile_table",
               rows=[#tablerow{cells=[#tablecell{text=?_T("Name: ")},
                                      #tablecell{body=#textbox{text=User#user.name,
                                                               class="nice_textbox",
                                                               id=profile_basic_name}}]},

                     #tablerow{cells=[#tablecell{text=?_T("Surname: ")},
                                      #tablecell{body=#textbox{text=User#user.surname,
                                                               class="nice_textbox",
                                                               id=profile_basic_surname}}]},

                     #tablerow{cells=[#tablecell{text=?_T("Email: ")},
                                      #tablecell{body=#textbox{text=User#user.email,
                                                               class="nice_textbox",
                                                               id=profile_basic_email}}]},

                     #tablerow{class="account_profile_br",
                               cells=[#tablecell{text=?_T("Nickname: ")},
                                      #tablecell{body=#label{text=User#user.username,
                                                             style="text-align: left;"}}]},

                     #tablerow{cells=[#tablecell{text=?_T("New password: ")},
                                      #tablecell{body=#password{id=password1,
                                                                class="nice_textbox"}}]},
                     #tablerow{cells=[#tablecell{text=?_T("Confirm: ")},
                                      #tablecell{body=#password{id=password2,
                                                                class="nice_textbox"}}]}
                    ]},


    Body.


%%%% update avatar %%%%
avatar_update_box(User) ->
    Avatar = avatar:get_avatar(User, big),
    #table{class="account_avatar_table",
	   rows=[
		 #tablerow{cells=[#tablecell{colspan=2, style="text-align:center;",
					     body=[#panel { id=avatarholder, body=[#image{image=Avatar,
                                                                                          class="avatar"}]}]
					    }]},
		 #tablerow{cells=[#tablecell{text=?_T("Change Avatar"), style="white-space:nowrap;" },
				  #tablecell{
					      body=[#upload { tag=upload_avatar, show_button=false }]
					    }]}
		]}.

start_upload_event(_Tag) ->
    wf:replace(avatarholder,
               #panel{id=avatarholder,
                      body=#image{image="/images/spinner.gif"}}).

finish_upload_event(_Tag, undefined, _, _) ->
    wf:flash(profile_info, ?_T("Please select a file"));

finish_upload_event(_Tag, OrigFile, LocalFile, _Node) ->
    User = webutils:user_info(),
    case avatar:process_uploaded_avatar(User#user.username, OrigFile, LocalFile) of
	{error, Error} -> wf:flash(profile_info, ?_TS("Error: $error$ ", [{error, Error}]));
	{ok, Avatar} ->
            wf:state(new_avatar, Avatar),
            wf:replace(avatarholder,
                       #panel { id=avatarholder,
                                body=#image{image=avatar:get_avatar(Avatar, big)} })
    end.

%%%% end update avatar %%%%


save_cancel(_User) ->
    #panel{body=[#button{id=save_button, text=?_T("Save"),
                         class="nice_button", postback=profile_save},
                 #button{id=cancel_button, text=?_T("Cancel"),
                         postback=profile_cancel,
                         class="nice_button"}]}.


%%%%%%%%%%%%%% END PROFILE %%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%% INVITE %%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

invite() ->
    User = webutils:user_info(),
    #panel{body=[generate_code_box(User),
                 send_code_box(User)]}.


generate_code_box(User) ->
    #panel{class="border-form",
           body=[%% #panel{body=table_code_view(Data)},
                 %% #br{},#br{},
                 #panel{body=?_T("Invite code URL: ")},
                 #panel{id=url_invite_panel,
                        body=#textbox{class="nice_textbox account_url_textbox"}},
                 #button{text=?_T("Generate link"),
                         class="nice_button account_generate_button",
                         postback={generate_invite, User}}]}.

send_code_box(User) ->
    #panel{class="border-form",
           body=[#flash{},
                 #panel{body=?_T("Send to mail: ")++"*"},
                 #panel{body=#textbox{id=mail_invite, class="nice_textbox account_url_textbox"}},
                 #panel{body=?_T("Enter his/her name: ")++"*"},
                 #panel{body=#textbox{id=name_invite, class="nice_textbox account_url_textbox"}},
                 #panel{body=[?_T("Question: ")++"*", #label{id=captcha_query}]},
                 #panel{body=#textbox{id=captcha_result, class="nice_textbox account_url_textbox"}},
                 #panel{body=?_T("Enter optional message for this user: ")},
                 #panel{body=#textarea{id=text_invite, class="nice_textbox account_url_textbox", style="text-align: left; height: 50px;"}},

                 #button{text=?_T("Send"),
                         class="nice_button account_generate_button",
                         postback={send_invite, User}}]}.



table_code_view(Data) ->
    #table{class="account_view_code",
           rows=[#tablerow{class="account_view_code_header",
                           cells=[#tableheader{text=?_T("Invite URL")},
                                  #tableheader{text=?_T("Send to")},
                                  #tableheader{text=?_T("Expire date")},
                                  #tableheader{text=?_T("Used by")} ]},
                 #bind{data=Data, map=view_map(),
                       body=[#tablerow{cells=[#tablecell{id=inviteUrl},
                                              #tablecell{id=inviteMail},
                                              #tablecell{id=expired},
                                              #tablecell{id=usedBy}]} ]}
                 ]}.

view_map() ->
    [ inviteUrl@text,
      inviteMail@text,
      expired@text,
      usedBy@text].

%%%%%%%%%%%%%% END INVITE %%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%% EVENT %%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



event({show, profile}) ->
    wf:update(view_box, profile()),
    wf:wire(save_button, profile_basic_name,
            #validate{on=blue, validators=[#is_required{text=?_T("Is required.")}]}),

    wf:wire(save_button, profile_basic_surname,
            #validate{on=blue,
                      validators=[#is_required{text=?_T("Is required.")}]}),
    wf:wire(save_button, profile_basic_email,
            #validate{on=blue,
                      validators=[#is_required{text=?_T("Is required.")},
                                  #is_email{text=?_T("Enter a valid email.")}
                                 ]});


event({show, gifts}) ->
    wf:update(view_box, gifts());
event({show, account}) ->
    wf:update(view_box, account());
event({show, stats}) ->
    wf:update(view_box, stats());
event({show, invite}) ->
    wf:update(view_box, invite()),
    captcha:generate(invite),
    event(update_captcha);
event(logout) ->
    wf:logout(),
    wf:redirect_to_login(?_U("/login"));



event({generate_invite, User}) ->
    {ok, InviteCode} = rpc:call(?APPSERVER_NODE,invite,generate_code,[User]),
    Url = site_utils:create_url_invite(InviteCode),
    NewTextBox = #textbox{class="nice_textbox account_url_textbox", text=Url},
    wf:update(url_invite_panel, NewTextBox);

event({send_invite, User}) ->
    case captcha:check(invite, wf:q(captcha_result)) of
	true ->
	    Mail = wf:q(mail_invite),
	    UserName = wf:q(name_invite),
	    Text = wf:q(text_invite),
	    case rpc:call(?APPSERVER_NODE,invite,send_invite_email,[User, Mail, UserName, Text]) of
		{error, wrong_email} ->
		    wf:flash(?_T("Wrong e-mail"));
		{error, wrong_username} ->
		    wf:flash(?_T("Wrong username."));
		{ok, _} ->
		    wf:flash(?_T("Invite sent!"))
	    end;
	false ->
	    wf:flash(?_T("Bad captcha!"))
    end;


event(profile_save) ->
    User = webutils:user_info(),
    NewName = wf:q(profile_basic_name),
    NewSurname = wf:q(profile_basic_surname),
    NewEmail = wf:q(profile_basic_email),
    Passwd1 = wf:q(password1),
    Passwd2 = wf:q(password2),

    Sex = site_utils:element_value(profile_advanced_sex),
    City = site_utils:element_value(profile_advanced_city),
    Edu = site_utils:element_value(profile_advanced_edu),

    Y = site_utils:element_value(profile_advanced_year, integer),
    M = site_utils:element_value(profile_advanced_month, integer),
    D = site_utils:element_value(profile_advanced_day, integer),

    BDate = {Y,M,D},

    User1 =  User#user{name = NewName,
                       surname = NewSurname,
                       email = NewEmail,
                       sex = Sex,
                       location = City,
                       education = Edu,
                       age = BDate},

    User2 =
        case wf:state(new_avatar) /= undefined of
            true -> User1#user{avatar = wf:state(new_avatar)};
            false -> User1
        end,

    Req =
        case Passwd1 of
            "" -> {ok, User2};
            P when P == Passwd2 ->
                case length(P) of
                    L when L < 6 ->
                        {error, ?_T("Password is to short. Six letters needed.")};
                    _ -> {ok, User2#user{password = P}}
                end;
            _ -> {error, ?_T("Passwords does not match.")}
        end,

    case Req of
        {ok, NewUser} ->
            %%FIX: FOR ONLY TEST! REMOVED THIS CODE!!!
            Points = wf:to_integer(wf:q(kakapoint)),
            rpc:call(?APPSERVER_NODE,users,set_user_point,[NewUser#user.username, Points]),
            wf:session(user_info, NewUser),
            rpc:call(?APPSERVER_NODE,zealot_db,put,[NewUser]),
            wf:flash(profile_info, ?_T("Saved!"));
        {error, ErrInfo} ->
            wf:flash(profile_info, ErrInfo)
    end;

event({profile, cancel}) ->
    wf:redirect(?_U("/account"));

event({revoke_facebook, User}) ->
    case User#user.password of
        undefined ->
            wf:flash(profile_info, ?_T("Sorry, but you can't revoke access to FB if you haven't defined your password! Otherwise you wouldn't be able to log in!"));
        _ ->
            case wf:session(fb_access_token) of
                undefined ->
                    BackUri = create_back_uri(),
                    wf:redirect(erlfb:create_auth_uri(BackUri));
                Token ->
                    revoke_facebook_request(Token)
            end
    end;

event(update_captcha) ->
    wf:update(captcha_query, captcha:format(invite));

event(Other) ->
    webutils:event(Other).



%%%%%%%%%%%%%% END EVNET %%%%%%%%%%%%%%%




%%%%%% internals

record_to_web(Rec) when is_tuple(Rec) ->
    [Atom | List0] = tuple_to_list(Rec),
    List = [ f(X) || X <- List0 ],
    list_to_tuple([Atom | List]).

f(undefined) ->
    "";
f(Else) ->
    Else.


create_back_uri() ->
    lists:concat([?HTTP_ADDRESS, ?_U("/account"), "?fb_revoke"]).

revoke_facebook_request(Token) ->
    case erlfb:revoke_access(Token) of
        {ok, "true"} ->
            wf:flash(profile_info, ?_T("Revoke success!")),
            User = webutils:user_info(),
            wf:update(revoke_facebook, #span{}),
            NewUser = User#user{facebook_id = undefined},
            rpc:call(?APPSERVER_NODE,zealot_db,put,[NewUser]),
            wf:session(user_info,NewUser);
        _ ->
            wf:flash(profile_info, ?_T("Revoke failed! Please try again or notify the administrator"))
    end.


