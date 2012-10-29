%% -*- mode: nitrogen -*-
-module (element_register).
-compile(export_all).

-include("common.hrl").

-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("setup.hrl").
-include_lib("nsm_db/include/user.hrl").
-include_lib("nsm_db/include/invite.hrl").

reflect() -> record_info(fields, register).

render_element(#register{}) ->
    inside_register_popup().

%% Page Elements
%%

inside_register_popup() ->
    PanelBody = #panel{class=[register_panel, clearfix], body=
							    #template { file=code:priv_dir(nsw_srv)++"/templates/register.html" }
							   },
    Render = #panel{id=register_panel, body=login:login_panel_wrapper(register, PanelBody)},

    %% try to get invite and put it to state
    case invite:get_invite() of
        {ok, Invite} ->
            wf:state(invite, Invite);
        _ ->
            ok
    end,

    wf:wire(reg_username, reg_username,
            #validate{on=blur,
                      validators=[#is_required{text=?_T("Please fill this field")},
                                  #custom{text=?_T("This name is exist"),
                                          function=fun custom_validator/2 }]}),

    wf:wire(reg_email, reg_email,
            #validate{on=blur,
                      validators=[#is_required{text=?_T("Please fill this field")},
                                  #is_email{text=?_T("Email is invalid")}]}),

    wf:wire(reg_passwd, reg_passwd,
            #validate{on=blur,
                      validators=[#is_required{text=?_T("Please fill this field")},
                                  #min_length{text=?_T("Password is to short. Six letters needed."),
                                              length=6}]}),
    wf:wire(reg_con_passwd, reg_con_passwd,
            #validate{on=blur,
                      validators=[#confirm_password{password=reg_passwd,
                                                    text=?_T("Please fill this field")}]}),

    wf:wire(wf:f("objs('birthday_desc').not('.qtip_added').addClass('qtip_added')"
		 ".qtip({content:{text:'~s'}, position:{at:'~s', my:'~s'}}).click(function() {$(this).qtip('show');});",
		 [wf:js_escape([?_T("Why are we asking for your birthday?"), "<br />",
				?_T("You must be at least 18 years of age to use the site according to law.")]),
		  "bottom left",
		  "top center"
		 ])),

    case wf:session(facebook) of
        undefined ->
            ok;
        UserInfo ->
            wf:update(fb_info, fb_info(UserInfo))
    end,

    Render.

title()->
    #h1{text=?_T("Sign Up")}.

register_hintbox()->
    #label {class="login_hintBox", id=register_hintbox, style="color:red"}.

field(username)->
    [#label{text=?_T("Username")},
     #panel{class="text text-focus", body=#textbox{class=ftxt, id=reg_username, next=reg_email}}];
field(email)->
    Mail = case wf:state(invite) of
        #invite_code{recipient = EMail} when EMail /= undefined->
            EMail;
        _ ->
            ""
    end,

    [#label{text=?_T("E-mail")},
     #panel{class="text", body=#textbox{class=ftxt, id=reg_email, next=reg_passwd, text=Mail}}];
field(passwd)->
    [#label{text=?_T("Password")},
     #panel{class="text", body=#password{class=ftxt, id=reg_passwd, next=reg_con_passwd}}];
field(con_passwd)->
    [#label{text=?_T("Password (again)")},
     #panel{class="text offset-1", body=#password{class=ftxt, id=reg_con_passwd, next=aa}}];
field(birth)->
    [#label{text=?_T("Birth Date")},
     #panel{class="ar",id=birthday_box, body=[birthday_box()]}].

register_button()->
    #button{id=aa, class="btn-submit", text=?_T("Register"),
	    actions=[
            webutils:serialize_event(show_please_wait, undefined, ?MODULE),
            webutils:serialize_event(register, undefined, ?MODULE)
        ]
    }.

facebook_button()->
    #link{class="login-facebook",postback=login_facebook, body=[#image{image="/images/login/login-facebook.png"}]}.

login_form()->
    [
     #panel{id=fb_info},
     #label{class="login_hintBox", id=reg_login_hintbox, style="color:red"},
     #label{class = "login_hintBox", id = login_hintbox, style = "color:red"},
     #label{text = ?_T("Username")},
     #panel{class = "text text-3",
	    body = [#textbox{id=reg_login, next=reg_password}]},
     "<label>",
     #link{class = "ar", text = ?_T("I forgot my password!"),
	   postback = show_forget},
     "</label>",
     #panel{class = "text text-3",
	    body = #password{id=reg_password, next=postlogin}},
     #panel{class = "row chk-row",
	    body =
		[#button{id = postlogin, class = "btn-submit",
			 text = ?_T("Login"), actions=webutils:serialize_event(reg_login, undefined, ?MODULE)},
		 #checkbox{class = "chk",
			   text = ?_T("Keep me logged in"), checked = true}]},

     #panel{class="center", body=facebook_button()}
    ].
%%FIX: create our own screenshot instead of currently used (if this
%% images stays at all
register_form_facebook() ->
    [#label{text=?_T("You'll see a window shown here after clicking the continue login:")},
     #br{},
     #image{image="/images/login/fb_perm.png"},
     #br{},
     #label{text=?_T("You should click 'Allow' to continue!")},
     #br{},
     #link{class="fb_continue", postback=register_by_facebook, text=?_T("Continue login >>")}
     #link{class="fb_continue", url="/", text=?_T("Cancel")}
    ].


terms_and_privacy() ->
    Terms = "<a target=\"_blank\" href=\""++?_U("/terms")++"\">"++?_T("Terms of Service")++"</a>",
    Privacy = "<a target=\"_blank\" href=\""++?_U("/privacy")++"\">"++?_T("Privacy Policy")++"</a>",

    ?_TS("By clicking on 'Register' above, you are agreeing to the $terms$ and the $privacy$.",
       [{terms, Terms}, {privacy, Privacy}]).

why_birthday() ->
    #label{id=birthday_desc, text=?_T("Why asking for birthday?")}.


%% TODO: custom validation isn't working
custom_validator(_Tag, User) ->
    nsm_users:if_exist(User).



birthday_box() ->
    birthday_box(undefined, undefined, undefined).

-spec birthday_box(undefined|string(), undefined|string(), undefined|string()) -> [record(dropdown)].
birthday_box(SDay, SMonth, SYear) ->
    {Y0, _,_} = erlang:date(),
    ViewYear = Y0-18,

    MaxDay = utils:days_in_month(SMonth, SYear),

    Day = webutils:create_option_with_number({1,MaxDay}, SDay, ?_T("Day")),
    Month = webutils:create_option_with_number({1,12}, SMonth, ?_T("Month")),
    Year = webutils:create_option_with_number({ViewYear,1900}, SYear, ?_T("Year")),

    [ #panel{class=sel, body= X#dropdown{class="cs-1", style="width:80px;"}} || X <-
    [
     #dropdown{id=reg_month, postback={birthday_changed},
	       class="dropdown_register",
	       options=Month},
     #dropdown{id=reg_day, postback={birthday_changed},
	       class="dropdown_register",
	       options=Day},
     #dropdown{id=reg_year, postback={birthday_changed},
	       class="dropdown_register",
	       options=Year}
    ]].


js_text_focus_script(Selector) ->
    "jQuery(\""++Selector++"\").parent().filter(\".text\").find(\"input\").focus(function(){$(\".text-focus\").removeClass(\"text-focus\");$(this).parent().addClass(\"text-focus\");})".

%%%%%%%% END register %%%%%%%%%%

finish_register(Invite) ->
    try
	finish_register_(Invite)
    catch
	throw:{msg, Msg} ->
            ?PRINT(Msg),
	    wf:update(register_hintbox, Msg)
    end.

finish_register_(Invite) ->
    {Name, Surname, FbId} =
        case wf:session(facebook) of
            undefined ->
                {wf:q(reg_name), wf:q(reg_surname), undefined};
            Data ->
                N = proplists:get_value(<<"first_name">>, Data),
                S = proplists:get_value(<<"last_name">>, Data),
                F =  wf:to_list(proplists:get_value(<<"id">>, Data)),
                {N,S,F}
        end,
    Day = site_utils:element_value(reg_day),
    Month = site_utils:element_value(reg_month),
    Year = site_utils:element_value(reg_year),
    BirthDay = case site_utils:check_date_correct({Year, Month, Day}) of
		   {ok, Date} -> Date;
		   {error, _} -> throw({msg, ?_T("Date of birth is incorrect.")})
	       end,

    Mail = wf:q(reg_email),

    {Status, VerificationCode} =
    case Invite of
        %% if user registered from inventation email -
        %% mark his email as verified
        #invite_code{recipient = Mail} ->
            {ok, undefined};
        _ ->
            {not_verified, site_utils:generate_code()}
    end,

    RegData = #user{username = wf:q(reg_username),
        password = wf:q(reg_passwd),
        email = Mail,
        name = Name,
        surname = Surname,
        facebook_id = FbId,
        verification_code = VerificationCode,
        age = BirthDay,
        register_date = erlang:now(),
        sex = wf:q(reg_sex),
        status = Status
    },
    User = wf:q(reg_username),
    Password = wf:q(reg_passwd),
    CPassword = wf:q(reg_con_passwd),

    case nsm_users:check_register_data(RegData) of
	ok -> ok;
	{error, username_too_short} ->
	    throw({msg, ?_T("Username is too short.")});
	{error, wrong_username} ->
	    throw({msg, ?_T("Username is in wrong format.")});
	{error, wrong_email} ->
	    throw({msg, ?_T("Email is in wrong format.")});
	{error, password_to_short} ->
	    throw({msg, ?_T("Password is too short.")});
	{error, user_too_young} ->
	    throw({msg, ?_T("You must be at least 18 to use this site.")})
    end,

    case Password of
	CPassword when Password /= "" ->
	    case zealot_auth:register(RegData) of
		{ok, register} ->
                    case  Invite of
                        #invite_code{code = Code} ->
                            nsx_util_notification:notify(["system", "use_invite"], {Code, User});
                        %% we skip inventation code when register without code
                        _ ->
                            ok
                    end,
                    {Subject, PlpainText} = mail_construction:welcome(User, Password, Mail),
                    nsx_util_notification:notify_email(Subject, PlpainText, Mail),

                    case Status of
                        %% if email isn't verified, send verification message
                        not_verified ->
                            {VSubject, VPlpainText} = mail_construction:verification(Mail, VerificationCode),
                            nsx_util_notification:notify_email(VSubject, VPlpainText, Mail);
                        _ ->
                            ok
                    end,

                    event({register_success, User});
                {error, email_taken} ->
		    wf:update(register_hintbox,
			      ?_T("This e-mail already taken by other user."));
		{error, facebook_taken} ->
		    wf:update(register_hintbox,
			      ?_T("This facebook account already used by some user."));
		{error, user_exist} ->
		    wf:update(register_hintbox,
			      ?_TS("User <b><i>'$username$'</i></b> exist!",
				   [{username, User}]));
		    %% TODO: add error for taken facebook account
		Other -> ?ERROR("unexpected error: ~p", [Other]),
                         wf:update(register_hintbox,
			      ?_T("Unexpected application error. Please, try later."))
	    end;
	_ ->
	    wf:update(register_hintbox, ?_T("Passwords should match!"))
    end.

set_form_values_from_FB() ->
    FBID = wf:q(facebook_userid),
    FbToken = wf:q(facebook_access_token),

    FBInfo =
	case erlfb:get_user_info(FbToken) of
	    {ok, {struct, Response}} ->
		Id = wf:to_list(proplists:get_value(<<"id">>, Response)),
		case wf:to_list(Id) of
		    FBID ->
			wf:session(facebook, Response),
			Response;
		    _ -> error
		end;
	    _ -> error
	end,

    case FBInfo of
	error -> ignore;
	_ ->
	    set_form_values_from_FB(FBInfo)
    end.

set_form_values_from_FB(FBInfo) ->
    Username = wf:to_list(proplists:get_value(<<"username">>, FBInfo)),
    Email    = wf:to_list(proplists:get_value(<<"email">>, FBInfo)),
    BirthDay = wf:to_list(proplists:get_value(<<"birthday">>, FBInfo)),
    wf:set(reg_email, Email),
    wf:set(reg_username, Username),
    case string:tokens(BirthDay, "/") of
	[M, D, Y] ->
	    wf:set(reg_day, D),
	    wf:set(reg_month, M),
	    wf:set(reg_year, Y);
	_ -> ignore
    end,
    ok.


fb_info(UserInfo) ->
    Id = wf:to_list(proplists:get_value(<<"id">>, UserInfo)),
    Picture = lists:concat(["https://graph.facebook.com/",Id,"/picture"]),
    Name = proplists:get_value(<<"first_name">>, UserInfo),
    Surname = proplists:get_value(<<"last_name">>, UserInfo),
    fb_info(Picture, Name, Surname).


fb_info(Picture, Name, Surname) ->
    [#image{image=Picture},
     #label{text=Name},
     #label{text=Surname}
    ].

event({birthday_changed}) ->
    SDay=wf:q(reg_day),
    SMonth=wf:q(reg_month),
    SYear=wf:q(reg_year),
    wf:update(birthday_box, birthday_box(SDay, SMonth, SYear));

event({register_success, User}) ->
    timer:sleep(200),   % rarely, but sometimes it simply can't find new user with login:login_user, so I put a delay here
    nsx_util_notification:notify(["subscription", "user", User, "add_to_group"], {"kakaranet", member}),
    nsx_util_notification:notify(["subscription", "user", User, "add_to_group"], {"yeniler", member}),
    timer:sleep(300),    % and this for group subscription
    login:login_user(User);

event(show_please_wait) ->
    wf:update(register_hintbox, "<span style='color:#44AA44'>" ++ ?_T("Please wait...") ++ "</span>");

event(register) ->
    InviteCodeRec = wf:state(invite),
    case InviteCodeRec of
        #invite_code{code = InviteCode} ->
            CheckCode = invite:check_code(InviteCode),
            case CheckCode of
                {ok, Invite} ->
                    ?PRINT("With CODE"),
                    finish_register(Invite);
                %% finish register without code, user will be independent
                error ->
                    ?PRINT("Without CODE"),
                    finish_register(undefined)
            end;
        _ ->
            ?PRINT("Without CODE2"),
            finish_register(undefined)
    end;

event(reg_login) ->
    webutils:login(reg_login,reg_password,reg_login_hintbox);

event({error, Msg}) ->
    wf:wire(#alert{text=Msg});

event(Other) ->
    webutils:event(Other).
