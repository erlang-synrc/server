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
    PanelBody = #panel{class=[register_panel, clearfix], body=#template{ file=code:priv_dir(nsw_srv)++"/templates/register.html"}},
    Render = #panel{id=register_panel, body=login:login_panel_wrapper(register, PanelBody)},

    %% try to get invite and put it to state
    case invite:get_invite() of
        {ok, Invite} -> wf:state(invite, Invite);
        _ -> ok
    end,

    wf:wire(reg_username, reg_username,
	#validate{on=blur, validators=[
	    #is_required{text=?_T("Please fill this field")},
	    #custom{text=?_T("This name is exist"),
	    function=fun custom_validator/2}]}),

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
    Render.

title()-> #panel{text=?_T("Sign Up"), class="dlg-title"}.

register_hintbox()->
  case wf:q(msg) of
    undefined -> #label {class="login_hintBox", id=register_hintbox, style="color:red"};
    Msg -> #label {class="login_hintBox", id=register_hintbox, style="color:red", body=[site_utils:base64_decode_from_url(Msg)]}
  end.

field(username)-> 
    [#label{text=?_T("Username")},
     #panel{class="text text-focus", body=#textbox{class=ftxt, id=reg_username, next=reg_email}}];
field(email)->
    Mail = case wf:state(invite) of
        #invite_code{recipient = EMail} when EMail /= undefined-> EMail;
        _ -> ""
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
    #button{id=aa, class="btn-submit", text=?_T("Register"), actions=[
	webutils:serialize_event(show_please_wait, undefined, ?MODULE),
	webutils:serialize_event(register, undefined, ?MODULE)]}.

facebook()->
    case wf:session(fb_registration) of
	undefined -> [];
	[{error, E}] -> 
	    ErrorMsg = io_lib:format("Facebook error: ~p", [E]),
	    wf:redirect( ?_U("/index/message/") ++ site_utils:base64_encode_to_url(ErrorMsg));
	RegArgs ->
	    wf:info("Registration Element with fb ~p~n", [RegArgs]),
	    UserName = proplists:get_value(username, RegArgs),
	    [M,D,Y] = string:tokens(proplists:get_value(birthday, RegArgs, "1/1/1970"), "/"),
	    wf:set(reg_email, proplists:get_value(email, RegArgs)),
	    wf:set(reg_username, ling:replace(UserName,".","_")),
	    wf:set(reg_month, string:tokens(M,"0")),
	    wf:set(reg_day, D),
	    wf:set(reg_year, Y),
	    #panel{id=fb_info, class="fb-info-block", body=[
		#image{image="https://graph.facebook.com/" ++ UserName ++ "/picture"},
		proplists:get_value(username, RegArgs)]}
    end.

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
    case wf:session(fb_registration) of
      undefined ->
       {wf:q(reg_name), wf:q(reg_surname), undefined};
      Data ->
        N = proplists:get_value(first_name, Data),
        S = proplists:get_value(last_name, Data),
        F =  wf:to_list(proplists:get_value(id, Data)),
        {N,S,F}
    end,
  ?INFO("Registration: facebook data(session): ~p~n Name: ~p Surname: ~p FbId: ~p~n",
    [wf:session(fb_registration), Name, Surname, FbId]),
  Day = site_utils:element_value(reg_day),
  Month = site_utils:element_value(reg_month),
  Year = site_utils:element_value(reg_year),
  BirthDay =  case site_utils:check_date_correct({Year, Month, Day}) of
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

  User = ling:replace(wf:q(reg_username), ".", "_"),
  ?INFO("Username ~p replaced with ~p~n", [wf:q(reg_username), User]),
  Password = wf:q(reg_passwd),
  CPassword = wf:q(reg_con_passwd),

  RegData = #user{
    username = User,
    password = Password,
    email = Mail,
    name = Name,
    surname = Surname,
    facebook_id = FbId,
    team = nsm_db:create_team("team"),
    verification_code = VerificationCode,
    age = BirthDay,
    register_date = erlang:now(),
    sex = wf:q(reg_sex),
    status = Status
  },

  case nsm_users:check_register_data(RegData) of
    ok -> ok;
    {error, username_too_short} ->  throw({msg, ?_T("Username is too short.")});
    {error, wrong_username} ->      throw({msg, ?_T("Username is in wrong format.")});
    {error, wrong_email} ->         throw({msg, ?_T("Email is in wrong format.")});
    {error, password_to_short} ->   throw({msg, ?_T("Password is too short.")});
    {error, user_too_young} ->      throw({msg, ?_T("You must be at least 18 to use this site.")})
  end,

  case Password of
    CPassword when Password /= "" ->
      case nsm_users:register(RegData) of
        {ok, _RegisteredName} ->
          case  Invite of
            #invite_code{code = Code} ->
              nsx_msg:notify(["system", "use_invite"], {Code, User});
              %% we skip inventation code when register without code
            _ -> ok
          end,
          {Subject, PlpainText} = mail_construction:welcome(User, Password, Mail),
          nsx_msg:notify_email(Subject, PlpainText, Mail),

          case Status of
            %% if email isn't verified, send verification message
            not_verified ->
              {VSubject, VPlpainText} = mail_construction:verification(Mail, VerificationCode),
              nsx_msg:notify_email(VSubject, VPlpainText, Mail);
            _ -> ok
          end,

          event({register_success, User});
        {error, email_taken} -> wf:update(register_hintbox, ?_T("This e-mail already taken by other user."));
        {error, user_exist} ->  wf:update(register_hintbox, ?_TS("User <b><i>'$username$'</i></b> exist!", [{username, User}]));
        Other ->
          ?ERROR("unexpected error: ~p", [Other]),
          wf:update(register_hintbox, ?_T("Unexpected application error. Please, try later."))
      end;
    _ ->
      wf:update(register_hintbox, ?_T("Passwords should match!"))
  end.

event({birthday_changed}) ->
    SDay=wf:q(reg_day),
    SMonth=wf:q(reg_month),
    SYear=wf:q(reg_year),
    wf:update(birthday_box, birthday_box(SDay, SMonth, SYear));

event({register_success, User}) ->
    wf:session(fb_registration, undefined),
    timer:sleep(200),   % rarely, but sometimes it simply can't find new user with login:login_user, so I put a delay here
    nsx_msg:notify(["subscription", "user", User, "add_to_group"], {"kakaranet", User, member}),
    nsx_msg:notify(["subscription", "user", User, "add_to_group"], {"yeniler", User, member}),
    timer:sleep(300),    % and this for group subscription
    login:login_user(User,register);

event(show_please_wait) ->
    wf:update(register_hintbox, ["<span style='color:#44AA44'>" ++ ?_T("Please wait...") ++ "</span>", 
        #panel{class="view_media_other_attachment", style="float:none", body=#panel{class=loading_spiner}}]);

event(register) ->
    case wf:state(invite) of
        #invite_code{code = InviteCode} ->
            case invite:check_code(InviteCode) of
                {ok, Invite} -> finish_register(Invite);
                error -> finish_register(undefined)
            end;
        _ -> finish_register(undefined)
    end;

event({error, Msg}) ->
    wf:wire(#alert{text=Msg});

event(Other) ->
    webutils:event(Other).
