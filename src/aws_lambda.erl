-module(aws_lambda).

-export([get_function/2, invoke/3, create_function/6, delete_function/1,
         list_functions/1, update_code/3]).
%% Application api
-ignore_xref([get_function/2, invoke/3, create_function/6, delete_function/1,
              list_functions/1, update_code/3]).

-define(API_VERSION, "2015-03-31").
-define(CALLBACK, aws_lambda_api_callback).

get_function(Name, Opts) ->
    Path = [?API_VERSION, "functions", Name, "versions", "HEAD"],
    Headers = [{"Content-Type", "application/json"}],
    case aws_http:get(Path, [], Headers, ?CALLBACK, Opts) of
        {ok, {{200, _}, _, Msg}} -> {ok, Msg};
        {ok, {{404, _}, _, _}} -> {error, not_found};
        Resp -> erlang:error({aws_lambda, Resp})
    end.

invoke(Name, Payload, Opts) ->
    Path = [?API_VERSION, "functions", Name, "invocations"],
    Headers0 = [{"Content-Type", "application/json"}],
    OptHeaders = [{"X-Amz-Invocation-Type", lambda_invocation_type}],
    Headers = opt_props(Headers0, OptHeaders, Opts),
    case aws_http:post(Path, Headers, Payload, ?CALLBACK, Opts) of
        {ok, {{200, _}, _, Msg}} -> Msg;
        {ok, {{202, _}, _, <<>>}} -> ok; %% FIXME: is this correct?
        {ok, {{204, _}, _, <<>>}} -> ok; %% FIXME: is this correct?
        Resp -> erlang:error({aws_lambda, Resp})
    end.

create_function(Name, ZipBin, Handler, Role, Runtime, Opts) ->
    Payload0 = #{<<"Code">> => #{<<"ZipFile">> => base64:encode(ZipBin)},
                 <<"FunctionName">> => bin(Name),
                 <<"Handler">> => bin(Handler),
                 <<"Runtime">> => bin(Runtime),
                 <<"Role">> => bin(Role)},
    OptArgs = [{<<"Timeout">>, lambda_timeout},
               {<<"MemorySize">>, lambda_memory_size},
               {<<"Description">>, lambda_description}],
    Payload = playload_opt_add(Payload0, OptArgs, Opts),
    Path = [?API_VERSION, "functions"],
    Headers = [{"Content-Type", "application/json"}],
    case aws_http:post(Path, Headers, Payload, ?CALLBACK, Opts) of
        {ok, {{201, _}, _, Msg}} -> {ok, Msg};
        {ok, {{409, _}, _, _}} -> {error, already_exists};
        Resp -> erlang:error({aws_lambda, Resp})
    end.

update_code(Name, ZipBin, Opts) ->
    Payload = #{<<"ZipFile">> => base64:encode(ZipBin)},
    Path = [?API_VERSION, "functions", Name, "versions", "HEAD", "code"],
    case aws_http:put(Path, [], Payload, ?CALLBACK, Opts) of
        {ok, {{200, _}, _, Msg}} -> {ok, Msg};
        {ok, {{404, _}, _, _}} -> {error, not_found};
        Resp -> erlang:error({aws_lambda, Resp})
    end.

delete_function(Name) ->
    Path = [?API_VERSION, "functions", Name],
    case aws_http:delete(Path, [], ?CALLBACK, []) of
        {ok, {{204, _}, _, _}} -> ok;
        {ok, {{404, _}, _, _}} -> {error, not_found};
        Resp -> erlang:error({aws_lambda, Resp})
    end.

list_functions(Opts) ->
    Path = [?API_VERSION, "functions"],
    OptQuery = [{"Marker", lambda_marker},
                {"MaxItems", lambda_max_items}],
    Query = opt_props([], OptQuery, Opts),
    case aws_http:get(Path, Query, [], ?CALLBACK, []) of
        {ok, {{200, _}, _Headers, Body}} -> Body;
        Resp -> erlang:error({aws_lambda, Resp})
    end.

%% ---------------------------------------------------------------------------
%% Internal

bin(Bin) when is_binary(Bin) -> Bin;
bin(IoList) when is_list(IoList) -> iolist_to_binary(IoList).

opt_props(Headers, Vals, Opts) ->
    lists:foldl(
      fun({PayloadName, OptName}, HeadersAcc) ->
              case proplists:get_value(OptName, Opts) of
                  undefined -> HeadersAcc;
                  Val -> [{PayloadName, Val}|HeadersAcc]
              end
      end,
      Headers,
      Vals).

playload_opt_add(Payload, Vals, Opts) ->
    lists:foldl(
      fun({PayloadName, OptName}, PayloadAcc) ->
              case proplists:get_value(OptName, Opts) of
                  undefined -> PayloadAcc;
                  Val -> maps:put(PayloadName, Val, PayloadAcc)
              end
      end,
      Payload,
      Vals).
