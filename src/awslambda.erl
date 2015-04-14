-module(awslambda).

-export([get_function/3, invoke/4]).

-include_lib("aws_auth_v4/include/aws_auth_v4.hrl").

-define(API_VERSION, "2015-03-31").
-define(SERVICE, "lambda").

get_function(Name, Headers0, Opts) ->
    Path = [?API_VERSION, "functions", Name, "versions", "HEAD"],
    Headers = [{"Content-Type", "application/json"}|Headers0],
    aws_http:get(Path, Headers, ?SERVICE, Opts).

%% Do you own json encode / decode
invoke(Name, Payload, Headers0, Opts) ->
    Path = [?API_VERSION, "functions", Name, "invocations"],
    Headers = [{"Content-Type", "application/json"}|Headers0],
    case aws_http:post(Path, Headers, Payload, ?SERVICE, Opts) of
        {ok, _, _, Msg} ->
            JsonOpts = proplists:get_value(json_opts, Opts, [return_maps]),
            {ok, jiffy:decode(Msg, JsonOpts)};
        Err -> Err
    end.
