%%% @doc
%%% HTTPation: An HTTP utility library.
%%% @end

-module(httpation).
-vsn("0.1.0").

-export([uri/0,      uri/1,
         request/0,  request/1,
         response/0, response/1,
         exchange/0]).

-export_type([uri/0, request/0, response/0, exchange/0]).


-record(http_exchange,
        {uri        = httpation_uri:new(),
         request    = httpation_request:new(),
         response   = httpation_response:new()}).

-type uri()      :: urilator:uri().
-type request()  :: httpation_request:data().
-type response() :: httpation_response:data().
-type exchange() :: #http_exchange{}.


-spec uri() -> uri().
%% @doc
%% Create a new, blank uri structure.

uri() ->
    httpation_uri:new().


-spec uri(Stream) -> Result
    when Stream  :: unicode:chardata(),
         Result  :: {ok, uri()}
                  | {error, Reason},
         Reason  :: not_a_string | Segment,
         Segment :: atom().
%% @doc
%% Parse any utf8-encoded iolist into a uri structure.

uri(Stream) ->
    case unicode:characters_to_list(Stream) of
        {incomplete, _, _} -> error;
        {error, _, _}      -> error;
        String             -> httpation_uri:new(String)
    end.


-spec request() -> request().
%% @doc
%% Create a new, blank http request structure.

request() ->
    httpation_request:new().


-spec request(Stream) -> Result
    when Stream  :: string() | binary(),
         Result  :: {ok, request()}
                  | {partial, httpation_request:partial()}
                  | {error, Reason},
         Reason  :: bad_type | Segment,
         Segment :: atom().
%% @doc
%% Accept a binary or string and return a fully or partially parsed request object, or
%% return an error if the request stream is invalid. The input must already be a binary
%% or string representation of the data, as the inner functions operate over strings,
%% and before parsing the headers there is no way to know what the best representation
%% for the body of the request may be.

request(Stream) when is_binary(Stream) ->
    String = binary_to_list(Stream),
    httpation_request:new(String);
request(Stream) when is_list(Stream) ->
    httpation_request:new(Stream);
request(_) ->
    {error, bad_type}.


-spec response() -> response().
%% @doc
%% Create a new, blank http response structure.

response() ->
    httpation_response:new().


-spec response(Stream) -> Result
    when Stream  :: string() | binary(),
         Result  :: {ok, response()}
                  | {partial, httpation_response:partial()}
                  | {error, Reason},
         Reason  :: bad_type | Segment,
         Segment :: atom().
%% @doc
%% Accept a binary or string and return a fully or partially parsed response object, or
%% return an error if the request stream is invalid. The input must already be a binary
%% or string representation of the data, as the inner functions operate over strings,
%% and before parsing the headers there is no way to know what the best representation
%% for the body of the request may be.

response(Stream) when is_binary(Stream) ->
    String = binary_to_list(Stream),
    httpation_request:new(String);
response(Stream) when is_list(Stream) ->
    httpation_request:new(Stream);
response(_) ->
    {error, bad_type}.


-spec exchange() -> exchange().
%% @doc
%% Create a new, blank http response structure.

exchange() ->
    #http_exchange{}.
