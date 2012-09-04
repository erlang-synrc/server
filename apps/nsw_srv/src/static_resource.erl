%% @author Bryan Fink <bryan@basho.com>
%% @author Andy Gross <andy@basho.com>
%% @author Justin Sheehy <justin@basho.com>
%% @copyright 2008-2009 Basho Technologies, Inc.

-module(static_resource).
-export([init/1]).
-export([allowed_methods/2,
         resource_exists/2,
         last_modified/2,
         content_types_provided/2,
         content_types_accepted/2,
         delete_resource/2,
         post_is_create/2,
         create_path/2,
         provide_content/2,
         accept_content/2,
         generate_etag/2]).

-record(context, {root,response_body=undefined,metadata=[]}).

-include_lib("kernel/include/file.hrl").
-include_lib("webmachine/include/webmachine.hrl").

init(ConfigProps) ->
    {root, Root} = proplists:lookup(root, ConfigProps),
    {ok, #context{root=Root}}.
    
allowed_methods(ReqData, Context) ->
    {['HEAD', 'GET'%, 'PUT', 'DELETE', 'POST'
        ], ReqData, Context}.

file_path(Context, Name) ->
    RelName = case Name of
                  "/" ++ _ -> tl(Name);
                  _ -> Name
              end,
    filename:join([Context#context.root, RelName]).

file_exists(Context, Name) ->
    NamePath = file_path(Context, Name),
    case filelib:is_regular(NamePath) of 
        true ->
            {true, NamePath};
        false ->
            false
    end.

resource_exists(ReqData, Context) ->
    Path = wrq:disp_path(ReqData),
    case file_exists(Context, Path) of 
        {true, _} ->
            {true, ReqData, Context};
        _ ->
            case Path of
                "p" -> {true, ReqData, Context};
                _ -> {false, ReqData, Context}
            end
    end.

maybe_gzip_object(Context, ReqData, Path) ->
    case wrq:get_req_header("accept-encoding", ReqData) of
        AE when is_list(AE) ->
            case re:run(AE, "(?:^|\\W)gzip(?:\\W|$)") of
                {match,_} ->
                    case file_exists(Context, Path++".gz") of
                        {true, FullPath} ->
                            {ok, Value} = file:read_file(FullPath),
                            NewMeta = [ {'content-encoding', "gzip"},
                                        {'vary', "Accept-Encoding"}
                                      | Context#context.metadata],
                            {true, Context#context{response_body=Value,metadata=NewMeta}};
                        false ->
                            false
                    end;
                _ -> false
            end;
        _ -> false
    end.

maybe_fetch_object(Context, ReqData, Path) ->
    % if returns {true, NewContext} then NewContext has response_body
    case Context#context.response_body of
        undefined ->
            case maybe_gzip_object(Context, ReqData, Path) of
                {true, NewContext} -> {true, NewContext};
                false ->
                    case file_exists(Context, Path) of
                        {true, FullPath} ->
                            {ok, Value} = file:read_file(FullPath),
                            {true, Context#context{response_body=Value}};
                        false ->
                            {false, Context}
                    end
            end;
        _Body ->
            {true, Context}
    end.

content_types_provided(ReqData, Context) ->
    CT = webmachine_util:guess_mime(wrq:disp_path(ReqData)),
    {[{CT, provide_content}], ReqData,
     Context#context{metadata=[{'content-type', CT}|Context#context.metadata]}}.

content_types_accepted(ReqData, Context) ->
    CT = case wrq:get_req_header("content-type", ReqData) of
             undefined -> "application/octet-stream";
             X -> X
         end,
    {MT, _Params} = webmachine_util:media_type_to_detail(CT),
    {[{MT, accept_content}], ReqData,
     Context#context{metadata=[{'content-type', MT}|Context#context.metadata]}}.

accept_content(ReqData, Context) ->
    Path = wrq:disp_path(ReqData),
    FP = file_path(Context, Path),
    ok = filelib:ensure_dir(filename:dirname(FP)),
    ReqData1 = case file_exists(Context, Path) of
        {true, _} ->
            ReqData;
        _ ->
            LOC = "http://" ++
                   wrq:get_req_header("host", ReqData) ++
                   "/fs/" ++ Path,
            wrq:set_resp_header("Location", LOC, ReqData)
    end,
    Value = wrq:req_body(ReqData1),
    case file:write_file(FP, Value) of
        ok ->
            {true, wrq:set_resp_body(Value, ReqData1), Context};
        Err ->
            {{error, Err}, ReqData1, Context}
    end.

post_is_create(ReqData, Context) ->
    {true, ReqData, Context}.

create_path(ReqData, Context) ->
    case wrq:get_req_header("slug", ReqData) of
        undefined -> {undefined, ReqData, Context};
        Slug ->
            case file_exists(Context, Slug) of
                {true, _} -> {undefined, ReqData, Context};
                _ -> {Slug, ReqData, Context}
            end
    end.

delete_resource(ReqData, Context) ->
    case file:delete(file_path(
                       Context, wrq:disp_path(ReqData))) of
        ok -> {true, ReqData, Context};
        _ -> {false, ReqData, Context}
    end.

provide_content(ReqData, Context) ->
    case maybe_fetch_object(Context, ReqData, wrq:disp_path(ReqData)) of
        {true, NewContext} ->
            Body = NewContext#context.response_body,
            ReqData1 = wrq:set_resp_headers(NewContext#context.metadata, ReqData),
            {Body, ReqData1, NewContext};
        {false, NewContext} ->
            {error, ReqData, NewContext}
    end.

last_modified(ReqData, Context) ->
    {true, FullPath} = file_exists(Context,
                                   wrq:disp_path(ReqData)),
    LMod = filelib:last_modified(FullPath),
    {LMod, ReqData, Context#context{metadata=[{'last-modified',
                    httpd_util:rfc1123_date(LMod)}|Context#context.metadata]}}.

hash_body(Body) -> mochihex:to_hex(binary_to_list(crypto:sha(Body))).

generate_etag(ReqData, Context) ->
    case maybe_fetch_object(Context, ReqData, wrq:disp_path(ReqData)) of
        {true, BodyContext} ->
            ETag = hash_body(BodyContext#context.response_body),
            ReqData1 = wrq:set_resp_headers(BodyContext#context.metadata, ReqData),
            {ETag, ReqData1,
             BodyContext#context{metadata=[{etag,ETag}|
                                           BodyContext#context.metadata]}};
        _ ->
            {undefined, ReqData, Context}
    end.
