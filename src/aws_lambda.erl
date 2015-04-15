-module(aws_lambda).

-export([get_function/3, invoke/4, create_function/7, delete_function/1]).
%% Application api
-ignore_xref([get_function/3, invoke/4, create_function/7, delete_function/1]).


-define(API_VERSION, "2015-03-31").
-define(SERVICE, "lambda").

get_function(Name, Headers0, Opts) ->
    Path = [?API_VERSION, "functions", Name, "versions", "HEAD"],
    Headers = [{"Content-Type", "application/json"}|Headers0],
    aws_http:get(Path, Headers, ?SERVICE, Opts).

invoke(Name, Payload, Headers0, Opts) ->
    Path = [?API_VERSION, "functions", Name, "invocations"],
    Headers = [{"Content-Type", "application/json"}|Headers0],
    aws_http:post(Path, Headers, Payload, ?SERVICE, Opts).

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
    aws_http:post(Path, Headers, Payload, ?SERVICE, Opts).

delete_function(Name) ->
    Path = [?API_VERSION, "functions", Name],
    aws_http:delete(Path, [], ?SERVICE, []).

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
