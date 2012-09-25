%% -*- mode: nitrogen -*-
-module (kakaadmin).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_db/include/user.hrl").
-include_lib("nsm_db/include/config.hrl").
-include_lib("nsm_db/include/invite.hrl").

-include("elements/records.hrl").

-include("setup.hrl").
-include("common.hrl").

-define(USER_LIST_PAGE_SIZE, 10).

main() ->
    case wf:user() of
	undefined ->
	    wf:redirect_to_login("/");
	_User  ->
	    main_authorized()
    end.

main_authorized() ->
    webutils:add_script("/nitrogen/jquery.paginatetable.js"),
    webutils:add_script("/nitrogen/bert.js"),
    webutils:add_script("/nitrogen/base64.js"),
    %% add extjs scripts
    webutils:add_script("/nitrogen/js/admin-lib.js"),

    webutils:add_script("/nitrogen/extjs/ext/ux/BufferView.js"),
    webutils:add_script("/nitrogen/extjs/ext/ux/RowEditor.js"),
    webutils:add_script("/nitrogen/extjs/ext-all.js"),
    webutils:add_script("/nitrogen/extjs/ext-base.js"),


    %% add extjs styles, common and themes
    add_stylesheet("/css/admin.css"),
    add_stylesheet("/nitrogen/extjs/ext/ux/css/RowEditor.css"),
    add_stylesheet("/css/xtheme-gray.css"),
    add_stylesheet("/css/ext-all.css"),

    #template { file=code:priv_dir(nsw_srv)++"/templates/bare.html" }.

title() -> webutils:title(?MODULE).

body() ->
    UserName = wf:user(),
    {ok, User} = rpc:call(?APPSERVER_NODE,nsm_users,get_user,[UserName]),
    case rpc:call(?APPSERVER_NODE,nsm_acl,check_access,[User, {feature, admin}]) of
	allow -> body_authorized();
	_ -> ?_T("You don't have access to do that.")
    end.

body_authorized() ->
    CurrentSelection = get_current_section(),

    Col = #panel{class=col, body=section_body(CurrentSelection)},
    Links = [
	     {users,		?_U("/kakaadmin"),			?_T("Users")},
	     {games,		?_U("/kakaadmin/games"),		?_T("Games")},
	     {tournaments,	?_U("/kakaadmin/tournaments"),	?_T("Tournaments")},
	     {system,		?_U("/kakaadmin/system"),		?_T("System Parameters")},
	     {stats,		?_U("/kakaadmin/stats"),		?_T("Statistics")},
	     {others,		?_U("/kakaadmin/others"),		?_T("Others")},
	     {reports,		?_U("/kakaadmin/reports"),		?_T("Reports")},
	     {affiliates,		?_U("/kakaadmin/affiliates"),   ?_T("Affiliates")}
	    ],
    Menu = #list{body=[#listitem{class=case CurrentSelection of S -> active; _ -> "" end,
				 body=#link{text=T, url=U}} || {S, U,T} <- Links]},

    SubmenuLinks = submenu(CurrentSelection),

    SubMenu = #list{body=[#listitem{body=#link{text=Text, postback=Postback}}
			   || {Text, Postback} <- SubmenuLinks]},

    #section{class="admin-block white-block", body=[#panel{class="admin-menu", body=Menu},
						    #grid_clear{},
						    #panel{class="admin-submenu round-block",
							   show_if=SubmenuLinks=/=[],
							   body=[SubMenu, #grid_clear{}]},
						    #panel{id=view_box, class="admin-content", body=Col}]}.

get_current_section() ->
    case wf:q('__submodule__') of
	undefined -> users;
	Page -> list_to_existing_atom(Page)
    end.

submenu(users) ->
    users_submenu();
submenu(system) ->
    config_submenu();
submenu(others) ->
	other_submenu();
submenu(affiliates) ->
    affiliates_submenu();
submenu(_) -> [].


section_body(users) ->
    invite();
section_body(games) ->
    ?_T("Not implemented");
section_body(tournaments) ->
    ?_T("Not implemented");
section_body(system) ->
    config_new();
section_body(stats) ->
    ?_T("Not implemented");
section_body(others) ->
	%% create placeholder for others elements
	["<div id=\"others-placeholder\"></div>"];
section_body(reports) ->
    ?_T("Not implemented");
section_body(affiliates) ->
    affiliates_body();
section_body(_) ->
    ?_T("Not implemented").



logo() ->
    [
     #image{image="/images/logo.png", style="float: left; margin-top: 25px;"}
    ].

menu() ->
    [#p{class="admin_menu_item", body=#link{text=?_T("Users"), postback={show, users}}},
     #p{class="admin_menu_item", body=#link{text=?_T("Games"), postback={show, games}}},
     #p{class="admin_menu_item", body=#link{text=?_T("Tournaments"), postback={show, turnaments}}},
     #p{class="admin_menu_item", body=#link{text=?_T("System parameters"), postback={show, config_list}}},
     #p{class="admin_menu_item", body=#link{text=?_T("Statistics"), postback={show, stats}}},
     #p{class="admin_menu_item", body=#link{text=?_T("Others"), postback={show, other}}},
     #p{class="admin_menu_item", body=#link{text=?_T("Reports"), postback={show, raports}}}].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%% PARAMS (CONFIG) %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

config_submenu() ->
    [{?_T("Add new value"), {show, config_new}},
     {?_T("List all values"), {show, config_list}}].

config() ->
    #panel{body=[#flash{},
		 #grid_clear{},
		 #panel{class="border-form kakaconfig-editor",
			body=[#panel{body=config_list(), style="padding: 10px;", class="div_table"}]}
		]}.

config_new() ->
    #panel{body=[#flash{},
		 #grid_clear{},
		 #panel{class="border-form",
			body=[#panel{body=config_new_box(), style="padding: 10px;", class="div_table"}]}
		]}.

config_list() ->
    get_tree_box([]). %% root
get_tree_box(Branch) ->
    Childs = get_tree(Branch),
    [ begin
	  Id = wf:temp_id(),
	  Show_link = #link{actions=#event{type=click, target=Id, actions=#show{}}},
	  EditPostback = {edit_config, Id, Branch++[ChildElem]},
	  DefaultPostback = case (Type =/= val) of %% load child if there's any
				true -> {load_config, Id, Branch++[ChildElem]}; %% get child
				false -> EditPostback %% edit value
			    end,
	  EditLink = case (Type =/= tree) of %% user can edit only val and both
			 true -> Show_link#link{text="(edit)",postback=EditPostback};
			 false -> ""
		     end,
	  #panel{ style="margin-left:5px;",
		  body=[
			Show_link#link{text=ChildElem, postback=DefaultPostback},
			"&nbsp;",#link{text="(hide)", actions=#event{type=click, target=Id, actions=#hide{}}},
			"&nbsp;",EditLink,#panel{id=Id}
		       ]}
      end || {ChildElem, Type} <- Childs]. %% Type can be val|tree|both

-spec get_tree(list()) -> [{list(),val}|{list(),tree}].
%% Get child elements in brach Branch. Elements are marked as val or tree (or both).
get_tree(Branch) ->
    FullTree = rpc:call(?APPSERVER_NODE,nsm_db,all,[config]),
    Tree = get_tree(FullTree, Branch, []),
    FiltredTree = filter_tree(Tree, []),
    lists:keysort(1, FiltredTree).


filter_tree([], Acc) ->
    Acc;
filter_tree([{Elem,Type}|Tail], Acc) ->
    case proplists:get_value(Elem, Acc) of
	undefined -> %% new value
	    filter_tree(Tail, [{Elem,Type}|Acc]);
	Type -> %% same type as current element. So no need to add
	    filter_tree(Tail, Acc);
	_OtherType -> %% is other type, so it's both
	    NewAcc = lists:keyreplace(Elem, 1, Acc, {Elem, both}),
	    filter_tree(Tail, NewAcc)
    end.

get_tree([], _Branch, Acc) ->
    Acc;
get_tree([Element|FT], Branch, Acc) ->
    case lists:prefix(Branch, Element) of
	true ->
	    BL = length(Branch),
	    case length(Element)-BL of
		0 -> %% element is on same level as branch (it's not a child) --- ignore it
		    get_tree(FT, Branch, Acc);
		1 -> %% element is key of value
		    get_tree(FT, Branch, [{lists:nth(BL+1,Element), val}|Acc]);
		_ -> %% >= 2 this branch has tree as chind element
		    get_tree(FT, Branch, [{lists:nth(BL+1,Element), tree}|Acc])
	    end;
	false ->
	    get_tree(FT, Branch, Acc)
    end.

config_new_box() ->
    config_edit_box("").

config_edit_box(Key) ->
    {KeyString, DefaulValue, DefaulType} =
	case Key of
	    "" -> % new value
		{"", "", string};
	    _ ->
		KS = wf:f("~w", [Key]),
		NotSet = make_ref(),
		{ok, Val} = rpc:call(?APPSERVER_NODE,nsm_db,get,[config, Key, NotSet]),
		case Val of
		    NotSet ->
			{KS, "", string};
		    Num when is_integer(Num) ->
			{KS, wf:f("~b", [Num]), integer};
		    Num when is_number(Num) ->
			{KS, wf:f("~f", [Num]), float};
		    Atom when is_atom(Atom) ->
			{KS, atom_to_list(Atom), atom};
		    String when is_list(String) ->
			{KS, String, string}
		end
	end,
    [
     #panel{body=[#label{text=?_TS("Variable key in format: $format$:",[{format, "[kakaserver, okey, robot_move]"}])},
		  #textbox{text=KeyString, id=config_var_name}]},
     #panel{body=[#label{text=?_T("Varialbe type:")},
		  #dropdown{id=config_var_type, options=[
							 #option { text=?_T("string"), value=string },
							 #option { text=?_T("atom"),   value=atom },
							 #option { text=?_T("integer"), value=integer },
							 #option { text=?_T("float"), value=float }
							],
			   value = DefaulType}]},
     #panel{body=[#label{text=?_T("Variable value:")},
		  #textbox{text=DefaulValue, id=config_var_value}]},
     #button{text=?_T("Add/Update variable"), class="nice_button", postback=config_save_new},
     ""
    ].



%%%%%%%%% END PARAMS (CONFIG) %%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%% USERS  %%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

users_submenu() ->
    [{?_T("Create invitation code"), {show, invite}},
     {?_T("Users list"), {show, users_list}}].

%%%%%%%%%%%%%%% Initive  %%%%%%%%%%%%%%


invite() ->
    Data = invite:convert_data(rpc:call(?APPSERVER_NODE,invite,get_all_code,[])),

    Pager =
	#panel{class="paging paging-2", body=#panel{class="center", body=[
	    #list{body=#listitem{body=#link{class="prevPage", text="<"}}},
	    #list{class="pageNumbers", body=" "},
	    #list{body=#listitem{body=#link{class="nextPage", text=">"}}}
	]}, show_if=Data=/=[]},

    #panel{body=[
        #panel{id=invites_flash},
        #br{},
        generate_code_box(),
        #br{},
        #grid_clear{},
        #panel{body=[#h3{text=?_T("Invites")},
            #panel{body=table_code_view(Data), id="invite_table"}],
            class="round-block", show_if=Data=/=[]},
        Pager]}.

generate_code_box() ->
    #panel{class="border-form",
           body=[#panel{body=?_T("Invite code URL: ")},
                 #panel{id=url_invite_panel,
                        body=[#label{text="Create ", style="display: inline;"},
                              #label{id=count_invition, text="1", style="display: inline;"},
                              #label{text=" invitions",  style="display: inline;"},
                              #br{},
                              #slider{id=count_slider,
                                      target=count_invition,
                                      value=1,
                                      text=" ~s "}]},
                 #button{text=?_T("Generate link"),
                         class="account-generate-button",
                         postback=generate_invite},
            #button{text=?_T("Delete expired invites"),
                class="ar account-generate-button",
                postback=delete_old_invites}
    ]}.



table_code_view(Data) ->
    #table{header=#tablerow{class="account_view_code_header",
                           cells=[#tableheader{text=?_T("Invite URL"), class="first-row"},
                                  #tableheader{text=?_T("Sent to")},
                                  #tableheader{text=?_T("Expire date")},
                                  #tableheader{text=?_T("Used by")} ]},
	   rows=[#bind{transform=fun tf_mark_odd/2,
		       data=Data, map=view_map(),
                       body=[#tablerow{id=table_row,
				       cells=[#tablecell{id=inviteUrl, class="first-row"},
                                              #tablecell{id=inviteMail},
                                              #tablecell{id=expired},
                                              #tablecell{id=usedBy}]} ]}
                 ], actions=ui_paginate()}.



view_map() ->
    [ inviteUrl@text,
      inviteMail@text,
      expired@text,
      usedBy@text].

ui_paginate() ->
    "$('.round-block .table').paginateTable({ rowsPerPage: 10, pager: '.paging', maxPageNumbers:20 });".

%%%%%%%%%%%%%% END INVITE %%%%%%%%%%%%%%%

%%%%%%%%%%%%%%% Users list  %%%%%%%%%%%%%%

users_list() ->
    FirstCursor = cursor:new(rpc:call(?APPSERVER_NODE, nsm_db, all, [user]), 2),
    {ok, Result, Cursor} = cursor:page_next(?USER_LIST_PAGE_SIZE, FirstCursor),
    wf:state(user_list_cursor, Cursor),
    view_user_list(Result, 1, cursor:size(Cursor)).

view_user_list(UsersList0, CurrentPage, NumberOfTerms) ->
    UsersList = users_list_get_data(UsersList0),
    #panel{body=[
        #panel{class="admin-user-list round-block",
               body=[#h3{text=?_T("User List")}, users_list_table_view(UsersList)]},
        pager(CurrentPage, NumberOfTerms)
    ]}.

users_list_table_view(Data) ->
    #table{class="nice_table",
           rows=[#tablerow{class="nice_table_header",
                           cells=[#tableheader{class="first-row", text=?_T("Username")},
                                  #tableheader{text=?_T("Email")},
                                  #tableheader{text=?_T("Name Surname")},
                                  #tableheader{text=?_T("Register data")},
                                  #tableheader{text=?_T("Last login")},
                                  #tableheader{text=?_T("Account status")},
                                  #tableheader{text=?_T("Invitation person")},
                                  #tableheader{text=?_T("Invitation code")} ]},
                 #bind{transform=fun tf_mark_odd/2,
                       data=Data, map=[ users_list_nickname@body,
                                        users_list_email@text,
                                        users_list_name@text,
                                        users_list_register_date@text,
                                        users_list_last_login@text,
                                        users_list_account_status@text,
                                        users_list_invitation_person@text,
                                        users_list_invitation_code@text
                                      ],
                       body=[#tablerow{id=table_row,
				       cells=[#tablecell{id=users_list_nickname, class="first-row"},
                                              #tablecell{id=users_list_email},
                                              #tablecell{id=users_list_name},
                                              #tablecell{id=users_list_register_date},
                                              #tablecell{id=users_list_last_login},
                                              #tablecell{id=users_list_account_status},
                                              #tablecell{id=users_list_invitation_person},
                                              #tablecell{id=users_list_invitation_code}]} ]}
                 ]}.

users_list_get_data(UsersList) ->
    [ begin
        WebRecord = record_to_web(R),
        #user{username = Username,
            email = Email,
            name = Name,
            surname = Surname,
            status = Status,
            register_date = RegisterDate} = WebRecord,

        {InvitationCode, InvitationPerson} =
        case rpc:call(?APPSERVER_NODE, invite, get_code_per_created_user, [Username]) of
            []->
                {"-", "-"};
            [#invite_code{code = Code, issuer = Issuer}] ->
                {Code, Issuer}
        end,

        LastLogin =
        case rpc:call(?APPSERVER_NODE,nsm_users,user_status,[Username]) of
            {error, _} ->
                "-";
            {ok, #user_status{last_login = L0}} ->
                f(L0)
        end,

        MarkEmpty = fun("") -> "-";
            (Value) -> Value
            end,

        NameSurname =
        case {Name, Surname} of
            {"", ""} -> "-";
            {_, _}   -> [MarkEmpty(Name)," ",MarkEmpty(Surname)]
        end,

        [ #link{url=site_utils:user_link(Username), text=Username},
            MarkEmpty(Email),
            NameSurname,
            MarkEmpty(RegisterDate),
            LastLogin,
            atom_to_list(Status),
            InvitationPerson,
            InvitationCode ]
    end  || R <- UsersList ].

tf_mark_odd(DataRow, Acc) when Acc == []; Acc==odd ->
    {DataRow, even, []};

tf_mark_odd(DataRow, Acc) when Acc == even ->
    {DataRow, odd, {table_row@class, "even"}}.


%% FIX: need to be improved in case of many (> 20) pages.
pager(CurrentPage, NumberOfTerms) ->
    NumberOfFullPages = (NumberOfTerms div ?USER_LIST_PAGE_SIZE),
    NunberOfPartPages = case (NumberOfTerms rem ?USER_LIST_PAGE_SIZE) of 0 -> 0; _ -> 1 end,
    NumberOfPages = NumberOfFullPages + NunberOfPartPages,

       ["<div class='paging paging-2'><div class=\"center\">",
	#list{body=#listitem{body=#link{class="prevPage", text="<",
			     postback={user_list, prev, CurrentPage, NumberOfTerms}}}},
	#list{class="pageNumbers",body=
	 [ #listitem{body=#link{body=wf:to_list(X), postback={user_list, X, NumberOfTerms},
		     class=case CurrentPage of X -> "active"; _ -> "" end}}
				|| X <- lists:seq(1, NumberOfPages) ]},
	#list{body=#listitem{body=#link{class="nextPage", text=">",
			     postback={user_list, next, CurrentPage, NumberOfTerms}}}},
	"</div></div>"].


%%%%%%%%%%%%%% END USERS LIST %%%%%%%%%%%%%%%

other_submenu() ->
	[{?_T("Packages"), {show, packages_list}},
	 {?_T("Gifts"),    {show, gifts_list}},
	 {?_T("Payments"), {show, payments_list}}].

packages_list() ->
	#packages_grid{}.


gifts_list() ->
	"Gifts list".

payments_list() ->
	#purchases_grid{}.

%% Affiliates
affiliates_submenu() ->
	[{?_T("Affiliates"), {show, affiliates}},
	 {?_T("Contracts"),  {show, affiliates_contracts}}].

nitrojoin([], _) ->
    [];
nitrojoin([H|[]], _) ->
    [H];
nitrojoin([H|T], Separator) ->
    [H] ++ [Separator] ++ nitrojoin(T, Separator).


cut_till_last_slash(String) ->
    List = string:tokens(String, "/"),
    hd(lists:reverse(List)).

str_to_num(N) ->
    case string:to_float(N) of
        {error,no_float} -> list_to_integer(N);
        {F,_Rest} -> F
    end.

something_to_list(N) when is_atom(N) ->
    "?";
something_to_list(N) when is_integer(N) ->
    integer_to_list(N);
something_to_list(N) ->
    hd(io_lib:format("~.2f",[N])).

real_affiliates_list() ->
    rpc:call(?APPSERVER_NODE,nsm_affiliates,affiliates,[]).

is_able_to_look_details(User) ->
    rpc:call(?APPSERVER_NODE,nsm_affiliates,is_able_to_look_details,[User]).

affiliates_list() ->
    AL = lists:sort(real_affiliates_list()),
    ALA = lists:filter(fun is_able_to_look_details/1, AL),
    ALD = lists:filter(fun(U) -> is_able_to_look_details(U) == false end, AL),
    [
        #br{},
        ?_T("Affiliates that can see payment details: "),
        nitrojoin([#link{url=(?_U("/affiliates/of/") ++ UserName), text=UserName} || UserName <- ALA], ", "),
        #br{},
        #br{},
        ?_T("Affiliates that can't: "),
        nitrojoin([#link{url=(?_U("/affiliates/of/") ++ UserName), text=UserName} || UserName <- ALD], ", ")
    ].

affiliates_body() ->
    [
        #panel{id=affiliates_list, style="font-weight:bold; font-size:14px;", body=affiliates_list()},
        #br{},
        #br{},
        #panel{id=affiliate_textbox_holder, body=
            #textbox{id=affiliate_username, placeholder=?_T("username..."), style="width:240px; height:20px; float:left",
                actions=#event{type=drop, postback=affiliate_textbox_changed}
            }
        },
        " ",
        #button{id=add_affiliate, postback=add_affiliate, text=?_T("Add")},
        " ",
        #button{id=remove_affiliate, postback=remove_affiliate, text=?_T("Remove")},
        " ",
        #button{id=allow_details_affiliate, postback=allow_details_affiliate, text=?_T("Allow to see details")},
        " ",
        #button{id=disallow_details_affiliate, postback=disallow_details_affiliate, text=?_T("Disallow that")}
    ].

affiliates_contracts_body() ->    
    LD = element(1, calendar:local_time()),
    Day = webutils:create_option_with_number({1,31}, integer_to_list(element(3, LD)), undefined),
    Month = webutils:create_option_with_number({1,12}, integer_to_list(element(2, LD)), undefined),
    Year = webutils:create_option_with_number({2012,2112}, integer_to_list(element(1, LD)), undefined),
    TypeList = rpc:call(?APPSERVER_NODE,nsm_affiliates,get_contract_types,[]),
    [
        #panel{id=affiliates_contracts, style="font-size:14px;", body=[
            #h1{text=?_T("New contract"), style="font-size:16px;"},
            #br{},
            #span{text=?_T("Username")++": "},
            #textbox{id=contract_new_user, placeholder=?_T("username..."), style="width:120px; height:20px;"},
            #br{},
            #br{},
            #span{text=?_T("Contract type")++": "},
            #dropdown { id=new_contract_type, options=[
                #option{value=CTypeId, text=
                    Name
                    ++": "++integer_to_list(Duration)++" " ++ ?_T("days") ++ ", " 
                    ++ integer_to_list(Limit)++" " ++ ?_T("purchases")
                    ++ ", " ++something_to_list(Commission)++"%"}
                || {CTypeId, Name, Duration, Limit, Commission, Disabled} <- TypeList, Disabled == false
            ]},
            #br{},
            #br{},
            #span{text=?_T("Starting")++": "},
            #dropdown{options=Day, id=contrant_new_day},
            #dropdown{options=Month, id=contrant_new_month},
            #dropdown{options=Year, id=contrant_new_year},
            #br{},
            #br{},
            #button{postback=add_new_contract, text=?_T("Add new contract")},
            #br{},
            #br{},
            #br{},
            #br{},


            #h1{text=?_T("Add new contract type"), style="font-size:16px;"},    
            #br{},
            #span{text=?_T("Type name")++": "},
            #textbox{id=contract_type_name, style="width:120px; height:20px;"},
            #br{},
            #br{},
            #span{text=?_T("Duration")++" ("++?_T("days")++"): "},
            #textbox{id=contract_type_duration, style="width:40px; height:20px;"},
            #span{text=" ", style="padding-left:16px;"},
            #span{text=?_T("Purchase limit")++": "},
            #textbox{id=contract_type_limit, style="width:40px; height:20px;"},
            #span{text=" ", style="padding-left:16px;"},
            #span{text=?_T("Commission (%)")++": "},
            #textbox{id=contract_type_commission, style="width:40px; height:20px;"},
            #br{},
            #br{},
            #button{postback=add_new_contract_type, text=?_T("Create new type")},
            #br{},
            #br{},
            #br{},
            #br{},


            #h1{text=?_T("Disable old contract type"), style="font-size:16px;"},
            #br{},
            #span{text=?_T("Contract type")++": "},
            #dropdown { id=old_contract_type, options=[
                #option{value=CTypeId, text=
                    Name
                    ++": "++integer_to_list(Duration)++" " ++ ?_T("days") ++ ", " 
                    ++ integer_to_list(Limit)++" " ++ ?_T("purchases")
                    ++ ", " ++something_to_list(Commission)++"%"}
                || {CTypeId, Name, Duration, Limit, Commission, Disabled} <- TypeList, Disabled == false
            ]},
            #br{},
            #br{},
            #button{postback=disable_old_contract_type, text=?_T("Disable")},
            #br{}
        ]}
    ].

create_new_contract_from_form() ->
    UserId = wf:q(contract_new_user),
    Id = wf:q(new_contract_type),
    Day = list_to_integer(wf:q(contrant_new_day)),
    Month = list_to_integer(wf:q(contrant_new_month)),
    Year = list_to_integer(wf:q(contrant_new_year)),
    GregDays = calendar:date_to_gregorian_days(Year, Month, Day),    
    TypeList = rpc:call(?APPSERVER_NODE,nsm_affiliates,get_contract_types,[]),
    {_, Name, Duration, Limit, Commission, _} = hd(
        [{CTId, A1, A2, A3, A4, A5} || {CTId, A1, A2, A3, A4, A5} <- TypeList, CTId == Id]
    ),
    Res = rpc:call(?APPSERVER_NODE,nsm_affiliates,check_contract,[
        UserId, Name, 
        calendar:gregorian_days_to_date(GregDays), 
        calendar:gregorian_days_to_date(GregDays+Duration), 
        Limit, Commission
    ]),
    case Res of 
        {error, {contracts_conflict, _}} ->
            wf:wire(#alert{text=?_TS("User '$username$' already has contract for this time!", [{username, UserId}]) });
        {error, AnythingElse} ->
            wf:wire(#alert{text=?_TS("Error - $something$!", [{something, AnythingElse}]) });
        _ ->
            nsx_util_notification:notify(["system", "create_contract"], {
                UserId, Name, 
                calendar:gregorian_days_to_date(GregDays), 
                calendar:gregorian_days_to_date(GregDays+Duration), 
                Limit, Commission
            }),
            wf:wire(#alert{text=?_TS("User '$username$' has new contract: '$contract$'!", [{username, UserId}, {contract, Name}]) })
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%% EVENT %%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

event(add_new_contract) ->
    UserId = wf:q(contract_new_user),
    case rpc:call(?APPSERVER_NODE, nsm_users, get_user, [{username, UserId}]) of
        {ok, _} ->
            case rpc:call(?APPSERVER_NODE, nsm_affiliates, is_existing_affiliate, [UserId]) of
                true ->
                    create_new_contract_from_form();
                false ->
                    wf:wire(#confirm{text=?_TS("User is not an affiliate! Do you want to make '$username$' an affiliate now and give him a contract?", [{username, UserId}]), 
                        postback={make_affiliate_and_add_contract, UserId} })
            end;
        _ -> 
            wf:wire(#alert{text=?_TS("User '$username$' does not exist!", [{username, UserId}]) })
    end;

event({make_affiliate_and_add_contract, UserId}) ->
    case rpc:call(?APPSERVER_NODE, nsm_users, get_user, [{username, UserId}]) of
        {ok, _} ->
%            rpc:call(?APPSERVER_NODE,nsm_affiliates, create_affiliate,[UserId]);
            nsx_util_notification:notify(["affiliates", "user", UserId, "create_affiliate"], {});
        _ -> 
            wf:wire(#alert{text=?_TS("User '$username$' does not exist!", [{username, UserId}]) })
    end,
    create_new_contract_from_form();
    
event(add_new_contract_type) ->
    Name = wf:q(contract_type_name),    
    Duration = list_to_integer(wf:q(contract_type_duration)),
    Limit = list_to_integer(wf:q(contract_type_limit)),
    Commission = str_to_num(wf:q(contract_type_commission)),
    nsx_util_notification:notify(["system", "create_contract_type"], {Name, Duration, Limit, Commission}),
    wf:replace(affiliates_contracts, affiliates_contracts_body());

event(disable_old_contract_type) ->
    Id = wf:q(old_contract_type),
    nsx_util_notification:notify(["system", "disable_contract_type"], {Id}),
    wf:replace(affiliates_contracts, affiliates_contracts_body());

event(affiliate_textbox_changed) ->
    Text = cut_till_last_slash(wf:q(affiliate_username)),
    wf:update(affiliate_textbox_holder, 
        #textbox{id=affiliate_username, placeholder=?_T("username..."), style="width:240px; height:20px; float:left", text=Text,
            actions=#event{type=drop, postback=affiliate_textbox_changed}
        }
    );

event(add_affiliate) ->
    AffiliateUsername=wf:q(affiliate_username),
    case rpc:call(?APPSERVER_NODE, nsm_users, get_user, [{username, AffiliateUsername}]) of
        {ok, _} ->
            nsx_util_notification:notify(["affiliates", "user", AffiliateUsername, "create_affiliate"], {}),
            wf:update(affiliates_list, affiliates_list());
        _ -> 
            wf:wire(#alert{text=?_TS("User '$username$' does not exist!", [{username, AffiliateUsername}]) })
    end;

event(remove_affiliate) ->
    AffiliateUsername=wf:q(affiliate_username),
    case lists:member(AffiliateUsername, real_affiliates_list()) of
        true ->
            nsx_util_notification:notify(["affiliates", "user", AffiliateUsername, "delete_affiliate"], {}),
            wf:update(affiliates_list, affiliates_list());
        false ->
            wf:wire(#alert{text=?_TS("User '$username$' is not an affiliate!", [{username, AffiliateUsername}]) })
    end;

event(allow_details_affiliate) ->
    AffiliateUsername=wf:q(affiliate_username),
    case lists:member(AffiliateUsername, real_affiliates_list()) of
        true ->
            nsx_util_notification:notify(["affiliates", "user", AffiliateUsername, "enable_to_look_details"], {}),
            wf:update(affiliates_list, affiliates_list());
        false ->
            wf:wire(#alert{text=?_TS("User '$username$' is not an affiliate!", [{username, AffiliateUsername}]) })
    end;

event(disallow_details_affiliate) ->
    AffiliateUsername=wf:q(affiliate_username),
    case lists:member(AffiliateUsername, real_affiliates_list()) of
        true ->
            nsx_util_notification:notify(["affiliates", "user", AffiliateUsername, "disable_to_look_details"], {}),
            wf:update(affiliates_list, affiliates_list());
        false ->
            wf:wire(#alert{text=?_TS("User '$username$' is not an affiliate!", [{username, AffiliateUsername}]) })
    end;

event(Event) ->
	case wf:user() of
		undefined ->
			wf:redirect_to_login("/");
		UserName ->
			{ok, User} = rpc:call(?APPSERVER_NODE,nsm_users,get_user,[UserName]),
			case rpc:call(?APPSERVER_NODE,nsm_acl,check_access, [User, {feature, admin}]) of
				allow -> u_event(Event);
				_ -> wf:redirect("/")
			end
	end.

u_event(delete_old_invites) ->
    Invites = rpc:call(?APPSERVER_NODE,invite,get_all_code,[]),
    {Deleted, NotExpired} = lists:foldl(
        fun(#invite_code{created_user = CUser, create_date = CData0, code = Code} = I, {Counter, Acc}) ->
            CData = utils:now_to_seconds(CData0),
            ETime = ?INVITE_CODE_EXPIRED,
            NowTime  = utils:now_to_seconds(now()),
            Expired = NowTime - (CData + ETime),

            case {CUser, Expired > 0} of
                {undefined, true} ->
                    nsx_util_notification:notify(["system", "delete"], {invite_code, Code}),
                    {Counter + 1, Acc};
                _ ->
                    {Counter, [I|Acc]}
            end
        end, {0,[]}, Invites),
    flash(invites_flash, success, ?_TS("$number$ invites has been deleted!", [{number, Deleted}])),
    Converted = invite:convert_data(NotExpired),
    wf:update(invite_table, table_code_view(Converted));

u_event({user_list, Direction, PageN, N}) ->
    Cursor = wf:state(user_list_cursor),
    NewPageNumber =
	case Direction of
	    next -> PageN +1;
	    prev -> PageN -1
	end,

    IsOutOfRange = max(1, min(N, NewPageNumber)) =/= NewPageNumber,

    case IsOutOfRange of
	true -> ignore;
	false ->
	    {ok, Result, NewCursor} =
	    case Direction of
		next -> cursor:page_next(?USER_LIST_PAGE_SIZE, Cursor);
		prev -> cursor:page_prev(?USER_LIST_PAGE_SIZE, Cursor)
	    end,
	    wf:state(user_list_cursor, NewCursor),
	    wf:update(view_box, view_user_list(Result, NewPageNumber, N))
    end;

u_event({user_list, N, MaxPage}) ->
    Cursor = wf:state(user_list_cursor),
    {ok, Result, NewCursor} = cursor:get_page(N, ?USER_LIST_PAGE_SIZE, Cursor),
    wf:state(user_list_cursor, NewCursor),
    wf:update(view_box, view_user_list(Result, N, MaxPage));

u_event({show, Page}) ->
	wf:wire("objs('.admin-submenu .link').removeClass('active');"
				"objs('me').addClass('active');"),
	case Page of
		invite ->
			wf:update(view_box, invite());
		users_list ->
			wf:update(view_box, users_list());
		config_new ->
			wf:update(view_box, config_new());
		config_list ->
			wf:update(view_box, config());
		%% Others pages
		packages_list ->
			wf:update(view_box, packages_list());
		gifts_list ->
			wf:update(view_box, gifts_list());
		payments_list ->
			wf:update(view_box, payments_list());
        %% Affiliates
		affiliates ->
			wf:update(view_box, affiliates_body());
		affiliates_contracts ->
			wf:update(view_box, affiliates_contracts_body());
		_ ->
			wf:update(view_box, ?_T("A not implemented"))
	end;

u_event({load_config, Id, Branch}) ->
    wf:update(Id, get_tree_box(Branch));

u_event({edit_config, Id, Element}) ->
    wf:update(Id, config_edit_box(Element));

u_event(logout) ->
    wf:logout(),
    wf:redirect_to_login(?_U("/login"));

u_event(config_save_new) ->
    case str_to_key(wf:q(config_var_name)) of
	{ok, Key} ->
	    StrVal = wf:q(config_var_value),
	    Value = try case wf:q(config_var_type) of
			    "string" ->
				{ok, StrVal};
			    "atom" ->
				{ok, list_to_atom(StrVal)};
			    "integer" ->
				{ok, list_to_integer(StrVal)};
			    "float" ->
				{ok, list_to_float(StrVal)};
			    _ ->
				{msg, ?_T("Not supported")}
			end
		    catch
			error:badarg ->
			    {msg, ?_T("Bad argument. May be you use wrong type.")}
		    end,
	    case Value of
		{ok, NewValue} ->
            nsx_util_notification:notify(["system", "put"], #config{key = Key,value=NewValue}),
		    wf:flash(?_TS("Value of $key$ set to $value$",[{key,wf:f("~w",[Key])},{value,NewValue}])); %% "
		{msg, Msg} ->
		    wf:flash(Msg)
	    end;
	{error, _} ->
	    wf:flash(?_T("Error: Variable key is in wrong format."))
    end;

u_event(generate_invite) ->
    User = wf:user(),
    Count = wf:to_integer(wf:q(count_slider_value)),
    InvList = lists:map(fun(_) ->
                                {ok, InviteCode} = rpc:call(?APPSERVER_NODE,invite,generate_code,[User]),
                                InviteCode
                        end, lists:seq(1, Count) ),
    Subject = ?_T("List of invitation URLs"),
    Text = [ io_lib:fwrite("~s~n", [site_utils:create_url_invite(Code)]) || Code <- InvList ],
    Email = webutils:user_info(email),
    nsx_util_notification:notify_email(Subject, lists:flatten(Text), Email),

    Data = invite:convert_data(rpc:call(?APPSERVER_NODE,invite,get_all_code,[])),
    NewBox = table_code_view(Data),
    flash(invites_flash, info, ?_TS("Generate $count$ code and send to address '$email$'.",
		  [{count, Count}, {email, Email}])),
    wf:update(invite_table, NewBox);

u_event(Any) ->
    webutils:event(Any).



%%%%%%%%%%%%%% END EVNET %%%%%%%%%%%%%%%



-spec str_to_key(string()) -> {ok, list()} | {error, format}.
%% @doc
%% This function takes string in format "[key1, key2, key3, ...]"  and
%% returns {ok, List} where List is list containing this keys, if
%% format is wrong or keys are not valid atoms than function returns
%% {error, format}
str_to_key([$[|Str]) ->
    str_to_key(Str, [], []);
str_to_key(_) ->
    {error, format}.

str_to_key([$]], E_Acc, Acc) ->
    {ok, Acc++[list_to_atom(E_Acc)]};
str_to_key([$\ |Tail], E_Acc, Acc) ->
    str_to_key(Tail, E_Acc, Acc);
str_to_key([$,|Tail], E_Acc, Acc) ->
    str_to_key(Tail, "", Acc++[list_to_atom(E_Acc)]);
str_to_key([H|Tail], E_Acc, Acc) ->
    Alphanum = lists:seq($a, $z)++lists:seq($A, $Z)++lists:seq($0, $9)++[$_],
    case lists:member(H, Alphanum) of
	true  -> str_to_key(Tail, E_Acc++[H], Acc);
	false -> {error, format}
    end.



%%%%%% internals


create_message(#user{username = UId}, Url) ->
    create_message(UId, Url);
create_message(UId, Url) ->
    Subject = ?_TS("$user$ invites you! - Kakaranet.com", [{user, UId}]),
    Content = ?_TS("Hello.\nClick on the link:\n$link$", [{link, Url}]),

    {Subject, Content}.


record_to_web(Rec) when is_tuple(Rec) ->
    [Atom | List0] = tuple_to_list(Rec),
    List = [ f(X) || X <- List0 ],
    list_to_tuple([Atom | List]).

f({_A, _B, _C} = Time) ->
    L = calendar:now_to_local_time(Time),
    site_utils:local_time_to_text(L);
f(undefined) ->
    "";
f(Else) ->
    Else.


add_stylesheet(Path) ->
	webutils:add_raw("<link rel=\"stylesheet\" type=\"text/css\" href=\""++Path++"\" media=\"all\" />").


flash(Id, Type, Text) ->
    wf:update(Id, #notice{type=Type, delay=3000, body=Text}).
