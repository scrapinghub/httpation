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
    when Response :: data(),
         Version  :: httpation:version().
    %% @doc
    %% Returns the version of the given response

version(#http_response{version = Version}) ->
    Version.


-spec version(Version, Response) -> NewResponse
    when Version     :: httpation:version(),
         Response    :: data(),
         NewResponse :: data().
%% @doc
%% Set version of the given response entity
version(Version, Response) ->
    Response#http_response{version = Version}.


-spec code(Response) -> Code
    when Response :: data(),
         Code     :: code().
    %% @doc
    %% Returns the code of the given response

code(#http_response{code = Code}) ->
    Code.


-spec code(Code, Response) -> NewResponse
    when Code        :: code(),
         Response    :: data(),
         NewResponse :: data().
%% @doc
%% Set code of the given response entity

code(Code, Response) ->
    Response#http_response{code = Code}.


-spec phrase(Response) -> Phrase
    when Response :: data(),
         Phrase   :: string().
    %% @doc
    %% Returns the phrase of the given response

phrase(#http_response{phrase = Phrase}) ->
    Phrase.


-spec phrase(Phrase, Response) -> NewResponse
    when Phrase      :: string(),
         Response    :: data(),
         NewResponse :: data().
%% @doc
%% Set phrase of the given response entity
phrase(Phrase, Response) ->
    Response#http_response{phrase = Phrase}.


-spec headers(Response) -> Headers
    when Response :: data(),
         Headers  :: [httpation:header()].
    %% @doc
    %% Returns the headers of the given response

headers(#http_response{headers = Headers}) ->
    Headers.


-spec headers(Headers, Response) -> NewResponse
    when Headers     :: [httpation:header()],
         Response    :: data(),
         NewResponse :: data().
%% @doc
%% Set headers of the given response entity
headers(Headers, Response) ->
    Response#http_response{headers = Headers}.


-spec chunk_size(Response) -> ChunkSize
    when Response   :: data(),
         ChunkSize  :: non_neg_integer().
    %% @doc
    %% Returns the chunk_size of the given response

chunk_size(#http_response{chunk_size = ChunkSize}) ->
    ChunkSize.


-spec chunk_size(ChunkSize, Response) -> NewResponse
    when ChunkSize   :: non_neg_integer(),
         Response    :: data(),
         NewResponse :: data().
%% @doc
%% Set chunk_size of the given response entity

chunk_size(ChunkSize, Response) ->
    Response#http_response{chunk_size = ChunkSize}.


-spec body(Response) -> Body
    when Response   :: data(),
         Body       :: unicode:chardata() | binary().
    %% @doc
    %% Returns the body of the given response

body(#http_response{body = Body}) ->
    Body.


-spec body(Body, Response) -> NewResponse
    when Body        :: unicode:chardata() | binary(),
         Response    :: data(),
         NewResponse :: data().
%% @doc
%% Set body of the given response entity

body(Body, Response) ->
    Response#http_response{body = Body}.


-spec trailer(Response) -> Trailer
    when Response      :: data(),
         Trailer       :: [httpation:header()].
    %% @doc
    %% Returns the trailer of the given response

trailer(#http_response{trailer = Trailer}) ->
    Trailer.


-spec trailer(Trailer, Response) -> NewResponse
    when Trailer     :: unicode:chardata() | binary(),
         Response    :: data(),
         NewResponse :: data().
%% @doc
%% Set trailer of the given response entity
trailer(Trailer, Response) ->
    Response#http_response{trailer = Trailer}.
