%%% @doc
%%% HTTPation Request: An HTTP request ADT utility functions over it.
%%% @end

-module(httpation_request).
-vsn("0.2.0").
-author("Craig Everett <zxq9@zxq9.com>").
-author("Oleg Tarasenko <oltarasenko@gmail.com>").

-export([new/0,        new/1,
         method/1,     method/2,
         version/1,    version/2,
         headers/1,    headers/2,
         uri/1,        uri/2,
         chunk_size/1, chunk_size/2,
         body/1,       body/2,
         trailer/1,    trailer/2]).

-record(http_request,
        {uri        = none :: none | httpation:uri(),
         method     = none :: none | method(),
         version    = none :: none | httpation:version(),
         headers    = none :: none | [httpation:header()],
         chunk_size = none :: none | non_neg_integer(),
         body       = none :: none | unicode:chardata() | binary(),
         trailer    = none :: none | [httpation:header()]}).

-export_type([data/0, method/0, partial/0, segment/0]).

-type data()    :: #http_request{}.
-type method()  :: get
                 | post
                 | head
                 | put
                 | delete
                 | connect
                 | options
                 | trace.
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


-spec method(Request) -> Method
    when Request :: httpation:request(),
         Method  :: method().
    %% @doc
    %% Returns HTTP method of the given request

method(#http_request{method = Method}) ->
    Method.


-spec method(Method, Request) -> NewRequest
    when Method     :: method(),
         Request    :: httpation:request(),
         NewRequest :: httpation:request().
%% @doc
%% Set HTTP method of given request entity
method(Method, Request) ->
    Request#http_request{method = Method}.


-spec version(Request) -> Version
    when Request :: httpation:request(),
         Version  :: httpation:version().
    %% @doc
    %% Returns the version of the given request

version(#http_request{version = Version}) ->
    Version.


-spec version(Version, Request) -> NewRequest
    when Version    :: httpation:version(),
         Request    :: httpation:request(),
         NewRequest :: httpation:request().
%% @doc
%% Set version of the given request entity
version(Version, Request) ->
    Request#http_request{version = Version}.


-spec headers(Request) -> Headers
    when Request  :: httpation:request(),
         Headers  :: [httpation:header()].
    %% @doc
    %% Returns the headers of the given request

headers(#http_request{headers = Headers}) ->
    Headers.


-spec headers(Headers, Request) -> NewRequest
    when Headers    :: [httpation:header()],
         Request    :: httpation:request(),
         NewRequest :: httpation:request().
%% @doc
%% Set headers of the given request entity
headers(Headers, Request) ->
    Request#http_request{headers = Headers}.


-spec uri(Request) -> URI
    when Request  :: httpation:request(),
         URI      :: httpation:uri().
    %% @doc
    %% Returns the uri of the given request

uri(#http_request{uri = URI}) ->
    URI.


-spec uri(URI, Request) -> NewRequest
    when URI        :: httpation:uri(),
         Request    :: httpation:request(),
         NewRequest :: httpation:request().
%% @doc
%% Set uri of the given request entity
uri(URI, Request) ->
    Request#http_request{uri = URI}.


-spec chunk_size(Request) -> ChunkSize
    when Request   :: httpation:request(),
         ChunkSize :: non_neg_integer().
    %% @doc
    %% Returns the chunk size of the given request

chunk_size(#http_request{chunk_size = ChunkSize}) ->
    ChunkSize.


-spec chunk_size(ChunkSize, Request) -> NewRequest
    when ChunkSize  :: non_neg_integer(),
         Request    :: httpation:request(),
         NewRequest :: httpation:request().
%% @doc
%% Set chunk size of the given request entity
chunk_size(ChunkSize, Request) ->
    Request#http_request{chunk_size = ChunkSize}.


-spec body(Request) -> Body
    when Request ::  httpation:request(),
         Body    ::  unicode:chardata() | binary().
    %% @doc
    %% Returns the body of the given request

body(#http_request{body = Body}) ->
    Body.


-spec body(Body, Request) -> NewRequest
    when Body       :: unicode:chardata() | binary() ,
         Request    :: httpation:request(),
         NewRequest :: httpation:request().
%% @doc
%% Set body of the given request entity
body(Body, Request) ->
    Request#http_request{body = Body}.


-spec trailer(Request) -> Trailer
    when Request :: httpation:request(),
         Trailer :: [httpation:header()].
    %% @doc
    %% Returns the trailer of the given request

trailer(#http_request{trailer = Trailer}) ->
    Trailer.


-spec trailer(Trailer, Request) -> NewRequest
    when Trailer    :: [httpation:header()],
         Request    :: httpation:request(),
         NewRequest :: httpation:request().
%% @doc
%% Set trailer of the given request entity
trailer(Trailer, Request) ->
    Request#http_request{trailer = Trailer}.
