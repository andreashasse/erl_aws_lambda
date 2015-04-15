-module(aws_lambda).

-export([get_function/3, invoke/4, create_function/7, delete_function/1]).
%% Application api
-ignore_xref([get_function/3, invoke/4, create_function/7, delete_function/1]).

-define(API_VERSION, "2015-03-31").
-define(CALLBACK, aws_lambda_api_callback).

get_function(Name, Headers0, Opts) ->
    Path = [?API_VERSION, "functions", Name, "versions", "HEAD"],
    Headers = [{"Content-Type", "application/json"}|Headers0],
    case aws_http:get(Path, Headers, ?CALLBACK, Opts) of
        {ok, {{200, _}, _, Msg}} -> Msg;
        Resp -> erlang:error({aws_lambda, Resp})
    end.

invoke(Name, Payload, Headers0, Opts) ->
    Path = [?API_VERSION, "functions", Name, "invocations"],
    Headers = [{"Content-Type", "application/json"}|Headers0],
    case aws_http:post(Path, Headers, Payload, ?CALLBACK, Opts) of
        {ok, {{200, _}, _, Msg}} -> Msg;
        {ok, {{202, _}, _, <<>>}} -> ok; %% FIXME: is this correct?
        {ok, {{204, _}, _, <<>>}} -> ok; %% FIXME: is this correct?
        Resp -> erlang:error({aws_lambda, Resp})
    end.

create_function(Name, ZipBin, Handler, Role, Runtime, Headers0, Opts) ->
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
    Headers = [{"Content-Type", "application/json"}|Headers0],
    case aws_http:post(Path, Headers, Payload, ?CALLBACK, Opts) of
        {ok, {{201, _}, _, _}} -> ok;
        {ok, {{409, _}, _, _}} -> {error, already_exists};
        Resp -> erlang:error({aws_lambda, Resp})
    end.

delete_function(Name) ->
    Path = [?API_VERSION, "functions", Name],
    case aws_http:delete(Path, [], ?CALLBACK, []) of
        {ok, {{204, _}, _, _}} -> ok;
        %% Removing non existing functions are considered ok
        {ok, {{404, _}, _, _}} -> ok;
        Resp -> erlang:error({aws_lambda, Resp})
    end.


%% ---------------------------------------------------------------------------
%% Internal

bin(Bin) when is_binary(Bin) -> Bin;
bin(IoList) when is_list(IoList) -> iolist_to_binary(IoList).

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
