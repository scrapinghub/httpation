%%% @doc
%%% HTTPation Response: An HTTP response ADT and utility functions over it.
%%% @end

-module(httpation_response).
-vsn("0.2.0").
-author("Craig Everett <zxq9@zxq9.com>").
-author("Oleg Tarasenko <oltarasenko@gmail.com>").

-export([new/0,        new/1,
         version/1,    version/2,
         code/1,       code/2,
         phrase/1,     phrase/2,
         headers/1,    headers/2,
         chunk_size/1, chunk_size/2,
         body/1,       body/2,
         trailer/1,    trailer/2]).

-record(http_response,
        {version    = none :: none | httpation:version(),
         code       = none :: none | code(),
         phrase     = none :: none | string(),
         headers    = none :: none | [httpation:header()],
         chunk_size = none :: none | non_neg_integer(),
         body       = none :: none | unicode:chardata() | binary(),
         trailer    = none :: none | [httpation:header()]}).

-export_type([data/0, partial/0, segment/0]).

-type data()    :: #http_response{}.
-type code()    :: 100..999.
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


-spec version(Response) -> Version
    when Response :: httpation:response(),
         Version  :: httpation:version().
    %% @doc
    %% Returns the version of the given response

version(#http_response{version = Version}) ->
    Version.


-spec version(Response, Version) -> NewResponse
    when Response    :: httpation:response(),
         Version     :: httpation:version(),
         NewResponse :: httpation:response().
%% @doc
%% Set version of the given response entity
version(Response, Version) ->
    Response#http_response{version = Version}.


-spec code(Response) -> Code
    when Response :: httpation:response(),
         Code     :: code().
    %% @doc
    %% Returns the code of the given response

code(#http_response{code = Code}) ->
    Code.


-spec code(Response, Code) -> NewResponse
    when Response    :: httpation:response(),
         Code        :: code(),
         NewResponse :: httpation:response().
%% @doc
%% Set code of the given response entity
code(Response, Code) ->
    Response#http_response{code = Code}.


-spec phrase(Response) -> Phrase
    when Response :: httpation:response(),
         Phrase     :: string().
    %% @doc
    %% Returns the phrase of the given response

phrase(#http_response{phrase = Phrase}) ->
    Phrase.


-spec phrase(Response, Phrase) -> NewResponse
    when Response    :: httpation:response(),
         Phrase        :: string(),
         NewResponse :: httpation:response().
%% @doc
%% Set phrase of the given response entity
phrase(Response, Phrase) ->
    Response#http_response{phrase = Phrase}.


-spec headers(Response) -> Headers
    when Response :: httpation:response(),
         Headers  :: [httpation:header()].
    %% @doc
    %% Returns the headers of the given response

headers(#http_response{headers = Headers}) ->
    Headers.


-spec headers(Response, Headers) -> NewResponse
    when Response    :: httpation:response(),
         Headers     :: [httpation:header()],
         NewResponse :: httpation:response().
%% @doc
%% Set headers of the given response entity
headers(Response, Headers) ->
    Response#http_response{headers = Headers}.


-spec chunk_size(Response) -> ChunkSize
    when Response   :: httpation:response(),
         ChunkSize  :: non_neg_integer().
    %% @doc
    %% Returns the chunk_size of the given response

chunk_size(#http_response{chunk_size = ChunkSize}) ->
    ChunkSize.


-spec chunk_size(Response, ChunkSize) -> NewResponse
    when Response    :: httpation:response(),
         ChunkSize   :: non_neg_integer(),
         NewResponse :: httpation:response().
%% @doc
%% Set chunk_size of the given response entity
chunk_size(Response, ChunkSize) ->
    Response#http_response{chunk_size = ChunkSize}.


-spec body(Response) -> Body
    when Response   :: httpation:response(),
         Body       :: non_neg_integer().
    %% @doc
    %% Returns the body of the given response

body(#http_response{body = Body}) ->
    Body.


-spec body(Response, Body) -> NewResponse
    when Response    :: httpation:response(),
         Body        :: unicode:chardata() | binary(),
         NewResponse :: httpation:response().
%% @doc
%% Set body of the given response entity
body(Response, Body) ->
    Response#http_response{body = Body}.


-spec trailer(Response) -> Trailer
    when Response      :: httpation:response(),
         Trailer       :: non_neg_integer().
    %% @doc
    %% Returns the trailer of the given response

trailer(#http_response{trailer = Trailer}) ->
    Trailer.


-spec trailer(Response, Trailer) -> NewResponse
    when Response    :: httpation:response(),
         Trailer     :: unicode:chardata() | binary(),
         NewResponse :: httpation:response().
%% @doc
%% Set trailer of the given response entity
trailer(Response, Trailer) ->
    Response#http_response{trailer = Trailer}.
