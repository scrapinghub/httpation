%%% @doc
%%% HTTPation Response: An HTTP response ADT and utility functions over it.
%%% @end

-module(httpation_response).
-vsn("0.1.0").
-author("Craig Everett <zxq9@zxq9.com>").

-export([new/0, new/1]).

-record(http_response,
        {}).

-export_type([data/0, partial/0, segment/0]).

-type data()    :: #http_response{}.
-type partial() :: {Next    :: segment(),
                    Buffer  :: iodata(),
                    Partial :: #http_response{}}.
-type segment() :: line
                 | header
                 | body
                 | chunk
                 | trailer.



%%% Interface functions

-spec new() -> data().
%% @doc
%% Create a new, blank http response structure.

new() -> #http_response{}.


-spec new(String) -> Result
    when String :: string(),
         Result :: {ok, data()}
                 | {partial, partial()}
                 | {error, Reason},
         Reason :: not_a_string
                 | atom().
%% @doc
%% Accept a string and return a fully or partially parsed request object, or
%% return an error if the request stream is invalid.

new(String) when is_list(String) ->
    ok = io:format("Received ~tp~n  ...and I'm doing nothing with it yet.", [String]),
    {ok, #http_response{}};
new(_) ->
    {error, not_a_string}.
