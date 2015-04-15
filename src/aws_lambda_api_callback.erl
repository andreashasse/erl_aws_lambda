-module(aws_lambda_api_callback).

-export([service_name/0, should_retry/1]).

service_name() -> "lambda".

should_retry({ok, {{Code, _}, _Hdrs, _Body}}) ->
    lists:member(Code, [429, 500]).
