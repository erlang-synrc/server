%%----------------------------------------------------------------------
%% @author Vladimir Baranov <baranoff.vladimir@gmail.com>
%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%% @doc
%%     FIXME: add description to module login
%% @end
%%-------------------------------------------------------------------
-module(login).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_db/include/user.hrl").
-include_lib("nsm_db/include/invite.hrl").
-include("setup.hrl").
-include("elements/records.hrl").
-include("common.hrl").

-compile(export_all).

main() ->
    #template { file = code:priv_dir(nsw_srv)++"/templates/bare.html" }.

body()->
    Request = wf_context:request_bridge(),
    wf:session(ip, Request:peer_ip()),
    Content = case wf:q('__submodule__') of
        "register" ->
            login_register_panel_content();
        "verify" ->
            login_verification_account_panel_content();
        "forget" ->
            case wf:q(token) of
                undefined ->
                    wf:redirect("/access_denied");
                Token ->
                    forget_password(Token)
            end;
        _ ->
            login_panel_content()
    end,

    #panel{id=login_panel_holder, class="lightbox", body=Content }.


lightboxes() ->
    Lightboxes =  [splash_lightbox, simple_lightbox],
    wf:wire(page, #event{type=keydown, keycode=27, actions= [ #hide{target=X} || X <- Lightboxes ]}),
    [
        show_splash(),
        #lightbox{id=simple_lightbox, body=#panel{id=simple_panel, body=[]}, style="display: none; position: relative;"}
    ].


login_panel_content() ->
    wf:wire(element_register:js_text_focus_script(".wfid_login_panel input")),
    wf:wire("objs('login').focus();"),

    RightBody = ["<h3>",
        #span{class = "large", text = ?_T("Hey!")},
        #span{class = "small", text = ?_T("Why don't you ")},
        #span{class = "mdl", text = ?_T("join us :)")},
        "</h3>",
        #p{body =
            ?_T("Let's rock this place "
                "together with you")},
        #link{class = btn, text = ?_T("Join Now!"),
            url=?_U("/login/register")}],
    Title = #span{class = "head",
        text = ?_T("Member Login")},
    LeftCol = #panel{class = "col-3",
        body = #panel{class = "form-3", body = login_form()}},
    RightCol = #panel{class = "col-4",
        body = #panel{class = "box", body = RightBody}},
    Content = [
        Title,
        #panel{id=login_panel, class = "col-holder col-holder-2", body = [LeftCol, RightCol]}
    ],
    login_panel_wrapper(regular, Content).

login_register_panel_content() ->
    case wf:user() of
        undefined ->
            wf:wire(element_register:js_text_focus_script(".wfid_register_panel input")),
            wf:wire("objs('change_password').focus();"),
            [   #register{id="register_panel"},
                "<!-- Google Code for order Conversion Page -->
                <script type='text/javascript'>
                /* <![CDATA[ */
                var google_conversion_id = 1008605414;
                var google_conversion_language = 'tr';
                var google_conversion_format = '2';
                var google_conversion_color = 'ffffff';
                var google_conversion_label = 'zEe1CPKo1AMQ5rH44AM';
                var google_conversion_value = 0;
                /* ]]> */
                </script>
                <script type='text/javascript' src='http://www.googleadservices.com/pagead/conversion.js'>
                </script>
                <noscript>
                <div style='display:inline;'>
                <img height='1' width='1' style='border-style:none;' alt='' src='http://www.googleadservices.com/pagead/conversion/1008605414/?value=0&amp;label=zEe1CPKo1AMQ5rH44AM&amp;guid=ON&amp;script=0'/>
                </div>
                </noscript>"
            ];
        _ ->
            wf:redirect_from_login(?_U("/matchmaker/okey")),
            []
    end.


login_register_success_panel_content(Mail) ->
    Body = #panel{body=[
        #h2{class="head", text=?_T("Register success")},
        #panel{style="padding:0 10px 10px", body=[
            #label{text=?_TS("Welcome email with confirmation link has been sent to your "
                "address ($usermail$). To complete registration, "
                "follow instructions in the email.",
                [{usermail, Mail}])
            },
            #button{class="btn-ok", postback=hide_login, text=?_T("Ok")}
        ]}
    ]},
    login_panel_wrapper(register_success, Body).


login_verification_account_panel_content() ->
    Body = fun(Text) ->
        Content = [#h1{class="head", text=?_T("Verification account")},
	    #panel{style="padding:0 10px 10px", body=[
                #label{text=Text},
                #button{class="fb_continue", postback=hide_login, text=?_T("OK")}
        ]}],
        login_panel_wrapper(register_success, Content)
    end,
    Code = wf:q(code),
    case verify:verify_account(Code) of
        {error, _} ->
            Body(?_T("Bad verification code!"));
        {ok, _} ->
            Body(?_T("Your account has been activated. Now you can sign. Enjoy!"))
    end.

login_change_password_content() ->
    wf:wire(element_register:js_text_focus_script(".wfid_change_password_panel input")),
    wf:wire("objs('change_password').focus();"),

    Title = #span{class = "head",
        text = ?_T("Change password")},
    LeftCol = #panel{class = "col-3",
        body = #panel{class = "form-3", body =
            [#label{class = "login_hintBox", id = change_password_info,
                style = "color:red"},
                #label{text = ?_T("New password *:")},
                #panel{class = "text text-focus", body = [
                    #password{id=change_password, next = captcha_result}
                ]},
                #label{text = ?_T("Repeat password *:")},
                #panel{class = "text text-focus", body = [
                    #password{id=change_repeat_password, next = change_repeat_password}
                ]},
                #cool_button{id=change_password_next, text=?_T("Continue"), postback=change_password},
                #cool_button{postback=change_password_cancel, text=?_T("Cancel")}
        ]}
    },

    %% add validators
    wf:wire(change_passwd, change_password,
        #validate{on=blur,
            validators=[#is_required{text=?_T("Please fill this field")},
                #min_length{text=?_T("Password is to short. Six letters needed."),
                    length=6}]}),
    wf:wire(change_repeat_password, change_repeat_password,
        #validate{on=blur,
            validators=[#confirm_password{password=reg_passwd,
                text=?_T("Please fill this field")}]}),

    Content = [
        Title,
        #panel{id=change_password_panel, class = "col-holder col-holder-2", body = [LeftCol]}
    ],

    login_panel_wrapper(change, Content).


login_forget_panel_content() ->
    Title = #span{class = "head",
        text = ?_T("Forgot Password")},
    LeftCol = #panel{class = "col-3",
        body = #panel{class = "form-3", body =
            [#label{class = "login_hintBox", id = forget_info,
                style = "color:red"},
                #label{text = ?_T("Your email *:")},
                #panel{class = "text text-focus", body = [
                    #textbox{id=forget_email, next = captcha_result}
                ]},
                #label{id=captcha_query},
                #panel{class = "text", body = [
                    #textbox{id=captcha_result, next = forget_next}
                ]},
                #cool_button{id=forget_next, text=?_T("Continue"), postback=forget_start},
                #cool_button{postback=forget_cancel, text=?_T("Cancel")}
        ]}
    },

    Content = [
        Title,
        #panel{id=forget_panel, class = "col-holder col-holder-2", body = [LeftCol]}
    ],
    login_panel_wrapper(forget, Content).

login_panel_wrapper(_Type, Content) ->
    #panel{class="popup-2 popup-3", body =#panel{class = in,
        body = #panel{class = frame, body = Content }}}.

login_form() ->
    [#label{class = "login_hintBox", id = login_hintbox,
        style = "color:red"},
        #label{text = ?_T("Username")},
        #panel{class = "text text-focus", body = [
            #textbox{id = login, next = password}
        ]},
        #panel{class = text,
            body = #password{id = password, next = postlogin}},
        "<label>",
        #link{class = "ar", text = ?_T("I forgot my password!"),
            postback = show_forget},
        "</label>",
        #panel{class = "row chk-row", body = [#button{id = postlogin, class = "btn-submit",
                text = ?_T("Login"), postback = login},
                #checkbox{class = "chk", text = ?_T("Keep me logged in"), checked = true}]},
        #panel{class = "center fb-login-panel", body = fb_utils:login_btn("Login with Facebook")}].

splash_lightbox() ->
    ColLeft = #panel{class = "col-l",
        body = #panel{class = "heading", body = [
            #h2{text = ?_T("Kapali beta asamasindaki simetize hos geldiniz.")}
        ]
    }},

    ColRight = #panel{class = "col-r",
        body = [
            #p{body = ?_T("Wellcome to our open public site.")},
            #p{body = ?_T("What s beta anyway?")},
            #p{body = ?_T(
                "Our site is 'not' completely functional at this stage "
                "We glad if you report our faults "
                "and also advise to us for new  "
                "future requests. We believe it will be cool to meet you")},
            #list{class = "list-2", body = [
                #listitem{body = ?_T(
                    "All functions are "
                    "not completed yet.")},
                #listitem{body = ?_T("We can have some problems like logouts, age changes etc. :)")}]},
            #p{body = ?_T(
                "Since the site is half made "
                "you can say that you are in construction site.")},
            #p{body = ?_T(
                "Thank you very much for your kind support.")},
            #list{class = "list-3",
                body = [
                    #listitem{body = #link{url="/terms",    text=?_T("Usage Terms")}},
                    #listitem{body = #link{url ="/privacy", text=?_T("Privacy Policy")}}]},
            #panel{class = "form-1", body = #panel{class = "ar",
                body =[
                    #checkbox{id = splash_never_show, class = "chk", text = ?_T("Don't show this again")},
                    #button{class = "btn-submit", text = ?_T("Close"), postback = close_splash}
                ]
            }}
    ]},

    PanelBody = #panel{class = cols, body = [ColLeft, ColRight]},
    LightboxBody = webutils:lightbox_panel_template(splash_lightbox, PanelBody),

    #lightbox{id = splash_lightbox, class = "splash-lightbox", style = "display: block; position: absolute;",
        body = LightboxBody}.


%% @doc Shows splash screen if user didn't check "not show" option and
%% we don't have "?hide_splash" parameter.
show_splash() ->
    Show = case wf_context:page_module() of
	index ->
	    (wf:session(hide_splash) == undefined) andalso (wf:cookie("splash_never_show") =/= "true")
	    andalso (wf:q("hide_splash") == undefined);
	_other -> false
    end,
    if
        Show ->
            splash_lightbox();
        true ->
            []
    end.

forget_password(Token) ->
    ?PRINT(Token),
    case forget:check_token(Token) of
        {ok, UId} ->
            {ok, User} = nsm_users:get_user(UId),
            wf:session(user_for_change_password, User),
            login_change_password_content();

        {error, bad_token} ->
            ?WARNING("IP: ~p, forget_password: bad token", [wf:session(ip)]),
            ?_T("error");

        {error, token_expired} ->
            ?WARNING("IP: ~p, forget_password: token expired", [wf:session(ip)]),
            [?_T("Token expired")]
    end.

%% EVENTS

event(login) ->
    webutils:login(login,password,login_hintbox);

event(hide_login) ->
    wf:redirect("/");

event(cancel_login) ->
    wf:redirect("/");

event(forget_cancel) ->
    wf:redirect("/login");

event(hide_simpe_lightbox) ->
    wf:wire(simple_lightbox, #hide{});

event(close_splash) ->
    case wf:q(splash_never_show) of
        "on" ->
            io:format("Setting cookie~n"),
            wf:cookie("splash_never_show", "true", "/", 100*24*3600);  %% 100 days
        _ ->
            ok
    end,
    wf:session(hide_splash, true),
    wf:wire(splash_lightbox, #hide{});

event(show_forget) ->
    case nsm_acl:check_access({ip, wf:session(ip)}, {feature, forget_password}) of
        deny ->
            wf:redirect("./access_denied");
        _ ->
            captcha:generate(forget),
            wf:update(login_panel_holder, login_forget_panel_content()),
            wf:wire(element_register:js_text_focus_script(".wfid_forget_panel input")),
            wf:wire("objs('forget_email').focus();"),

            event(update_captcha)
    end;

event(update_captcha) ->
    wf:update(captcha_query, [?_T("Question: "), captcha:format(forget)]);

event(forget_start) ->
    case captcha:check(forget, wf:q(captcha_result)) of
        true ->
            event(forget);
        false ->
            ?WARNING("IP: ~p, forget_password: bad_captcha", [wf:session(ip)]),
            wf:update(forget_info, ?_T("Bad captcha!"))
    end;

event(forget) ->
    Email = wf:q(forget_email),
    ForgetStatus =
        case nsm_users:get_user({email, Email}) of
            {ok, #user{} = User} ->
                forget:init_forget(User);
            {error, bad_email} ->
                {error, bad_email};
            _ ->
                {error, user_not_found}
        end,

    case ForgetStatus of
        {error, bad_email} ->
            ?WARNING("IP: ~p, forget_password: incorrect_email", [wf:session(ip)]),
            wf:update(forget_info, ?_T("Email address is incorrect!"));

        {error, user_not_found} ->
            ?WARNING("IP: ~p, forget_password: user not found", [wf:session(ip)]),
            wf:update(forget_info, ?_T("User not found!!"));

        ok ->
            wf:update(forget_info, ?_T("Instructions has been sent to your email!"))
    end;

event(change_password) ->
    User = wf:session(user_for_change_password),
    Password = wf:q(change_password),
    RepeatPassword = wf:q(change_repeat_password),
    case Password of
        RepeatPassword ->
            ChangeData = User#user{password=utils:sha(Password)},
%            ok = rpc:call(?APSERVER_NODE,nsm_db,put,[ChangeData]),
            nsx_util_notification:notify(["db", "user", User#user.username, "put"], ChangeData),
            wf:update(change_password_info, ?_T("Change password - success!")),
            redirect("/", 2000);

        _ ->
            wf:update(change_password_info, ?_T("Password different!"))
    end;

event({change_language,SL}) -> webutils:event({change_language, SL}).

api_event(Name, Tag, Data)-> fb_utils:api_event(Name, Tag, Data).

%% TODO:
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

redirect(Url, Delay) ->
    wf:wire(#event{type=timer, delay = Delay, actions=#script{script="window.location=\""++Url++"\";"}}).


login_user(UserName) ->
    {ok, User} = nsm_users:get_user(UserName),
%    rpc:call(?APSERVER_NODE,nsm_users,update_after_login,[UserName]),
    nsx_util_notification:notify(["login", "user", UserName, "update_after_login"], []),
    wf:session(user_info, User), 
    wf:user(UserName),
    wf:cookie("lang", site_utils:detect_language(), "/", 100*24*60), %% 100 days
    wf:config_default(session_timeout, 120),    % setting nitrogen session to 2 hours
    wf:redirect_from_login(?_U("/dashboard")).
