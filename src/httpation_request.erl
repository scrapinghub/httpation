%%% @doc
%%% HTTPation Request: An HTTP request ADT utility functions over it.
%%% @end

-module(httpation_request).
-vsn("0.1.0").
-author("Craig Everett <zxq9@zxq9.com>").

-export([new/0, new/1]).

-record(http_request,
        {}).

-export_type([data/0, partial/0, segment/0]).

-type data()    :: #http_request{}.
-type partial() :: {Next    :: segment(),
                    Buffer  :: iodata(),
                    Partial :: #http_request{}}.
-type segment() :: line
                 | header
                 | body
                 | chunk
                 | trailer.



%%% Interface functions

-spec new() -> data().
%% @doc
%% Create a new, blank http request structure.

new() -> #http_request{}.


-spec new(String) -> Result
    when String  :: string(),
         Result  :: {ok, data()}
                  | {partial, partial()}
                  | {error, Reason},
         Reason  :: not_a_string | segment().
%% @doc
%% Accept a string and return a fully or partially parsed request object, or
%% return an error if the request stream is invalid.

new(String) when is_list(String) ->
    ok = io:format("Received ~tp~n  ...and I'm doing nothing with it yet.", [String]),
    {ok, #http_request{}};
new(_) ->
    {error, not_a_string}.
