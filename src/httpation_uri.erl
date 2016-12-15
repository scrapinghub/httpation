%%% @doc
%%% Urilator: A URI handling function library.
%%%
%%% An ADT utility for URIs. Includes import, export and validation functions.
%%%
%%% TODO:
%%%     * Host IPv4 parsing
%%%     * Host IPv6 parsing
%%%     * Punycode conversion ( https://tools.ietf.org/html/rfc5891#section-4.4 )
%%%     * URI-safe escapes    ( https://tools.ietf.org/html/rfc7320 )
%%%     * UTF-8 readable, non-escaped output.
%%%     * Canonicalize the record fields in accordance with above RFCs.
%%% @end

-module(httpation_uri).
-vsn("0.1.0").
-author("Craig Everett <zxq9@zxq9.com>").

-export([new/0, new/1,
         export/1,
         protocol/1, protocol/2,
         username/1, username/2,
         password/1, password/2,
         hostname/1, hostname/2,
         port/1, port/2,
         path/1, path/2,
         qs/1, qs/2, update_qs/3, drop_qs/2,
         construct_uri/2,
         check_custom_port/2,
         pack_uri/5]).

-export_type([uri/0, qs/0, segment/0]).

-record(uri,
        {protocol = ""      :: string(),
         username = ""      :: string(),
         password = ""      :: string(),
         hostname = []      :: [string()],
         port     = default :: default | inet:port_number(),
         path     = ["/"]   :: [string()],
         qs       = []      :: [qs()],
         fragment = ""      :: string(),
         original = ""      :: string()}).
%        utf8     = ""      :: string(),
%        punycode = ""      :: string(),
%        escaped  = ""      :: string()}).


-type uri()     :: #uri{}.
-type qs()      :: {Label :: string(), Value :: string()}.
-type segment() :: scheme
                 | user_or_hostname
                 | hostname_or_port
                 | hostname
                 | port
                 | path
                 | querystring
                 | fragment.



%%% Interface functions

-spec new() -> uri().
%% @doc
%% Create a new, blank URI structure.

new() ->
    #uri{}.


-spec new(URI) -> Result
    when URI     :: string(),
         Result  :: {ok, uri()}
                  | {error, Reason},
         Reason  :: not_a_string | segment().
%% @doc
%% Accept a URI as a string and return a urilator:uri() structure.
%% If the input string is not a valid URI `{error, FunName, Args}' is returned.

new(URI) when is_list(URI) ->
    String = string:to_lower(URI),
    consume_protocol("", String, #uri{original = URI});
new(_) ->
    {error, not_a_string}.


consume_protocol(Acc, "://" ++ Rest, URI) ->
    Protocol = lists:reverse(Acc),
    consume_username("", Rest, URI#uri{protocol = Protocol});
consume_protocol(Acc, [Letter | Rest], URI)
        when $a =< Letter, Letter =< $z;
             $0 =< Letter, Letter =< $9 ->
    consume_protocol([Letter | Acc], Rest, URI);
consume_protocol(_, _, _) ->
    {error, scheme}.


consume_username("", "", _) ->
    {error, user_or_hostname};
consume_username(Acc, "", URI) ->
    Hostname = string:tokens(lists:reverse(Acc), "."),
    {ok, URI#uri{hostname = Hostname}};
consume_username(Acc, [$/ | Rest], URI) ->
    Hostname = string:tokens(lists:reverse(Acc), "."),
    consume_path("", [], Rest, URI#uri{hostname = Hostname});
consume_username(Acc, [$: | Rest], URI) ->
    Username = lists:reverse(Acc),
    consume_password("", Rest, URI#uri{username = Username});
consume_username(Acc, [$@ | Rest], URI) ->
    Username = lists:reverse(Acc),
    consume_hostname("", [], Rest, URI#uri{username = Username});
consume_username(Acc, [Letter | Rest], URI)
        when $a =< Letter, Letter =< $z;
             $0 =< Letter, Letter =< $9;
             Letter == $-; Letter == $_;
             Letter == $=; Letter == $. ->
    consume_username([Letter | Acc], Rest, URI);
consume_username(_, _, _) ->
    {error, user_or_hostname}.


consume_password("", "", #uri{username = ""}) ->
    {error, password_or_port};
consume_password("", "", URI = #uri{username = Username}) ->
    Hostname = string:tokens(Username, "."),
    {ok, URI#uri{username = "", hostname = Hostname}};
consume_password("", [$/ | Rest], URI = #uri{username = Username}) ->
    Hostname = string:tokens(Username, "."),
    consume_path("", [], Rest, URI#uri{username = "", hostname = Hostname});
consume_password(Acc, "", URI = #uri{username = Username}) ->
    case string:to_integer(lists:reverse(Acc)) of
        {Port, ""} ->
            Hostname = string:tokens(Username, "."),
            {ok, URI#uri{username = "", hostname = Hostname, port = Port}};
        _ ->
            {error, port}
    end;
consume_password(Acc, [$/ | Rest], URI = #uri{username = Username}) ->
    case string:to_integer(lists:reverse(Acc)) of
        {Port, ""} ->
            Hostname = string:tokens(Username, "."),
            NewURI = URI#uri{username = "", hostname = Hostname, port = Port},
            consume_path("", [], Rest, NewURI);
        _ ->
            {error, port}
    end;
consume_password(Acc, [$@ | Rest], URI) ->
    Password = lists:reverse(Acc),
    consume_hostname("", [], Rest, URI#uri{password = Password});
consume_password(Acc, [Letter | Rest], URI)
        when $a =< Letter, Letter =< $z;
             $0 =< Letter, Letter =< $9;
             Letter == $-; Letter == $_ ->
    consume_password([Letter | Acc], Rest, URI);
consume_password(_, _, _) ->
    {error, port}.


consume_hostname(Acc, Parts, "", URI) ->
    Part = lists:reverse(Acc),
    NewParts = [Part | Parts],
    Hostname = lists:reverse(NewParts),
    {ok, URI#uri{hostname = Hostname, path = ["/"]}};
consume_hostname(Acc, Parts, [$. | Rest], URI) ->
    Part = lists:reverse(Acc),
    consume_hostname("", [Part | Parts], Rest, URI);
consume_hostname(Acc, Parts, [$: | Rest], URI) ->
    Part = lists:reverse(Acc),
    NewParts = [Part | Parts],
    Hostname = lists:reverse(NewParts),
    consume_port("", Rest, URI#uri{hostname = Hostname});
consume_hostname(Acc, Parts, [$/ | Rest], URI) ->
    Part = lists:reverse(Acc),
    NewParts = [Part | Parts],
    Hostname = lists:reverse(NewParts),
    consume_path("", [], Rest, URI#uri{hostname = Hostname});
consume_hostname(Acc, Parts, [Letter | Rest], URI)
        when $a =< Letter, Letter =< $z;
             $0 =< Letter, Letter =< $9;
             Letter == $-; Letter == $_ ->
    consume_hostname([Letter | Acc], Parts, Rest, URI);
consume_hostname(_, _, _, _) ->
    {error, hostname}.


consume_port(Acc, "", URI) ->
    Port = list_to_integer(lists:reverse(Acc)),
    {ok, URI#uri{port = Port}};
consume_port(Acc, [$/ | Rest], URI) ->
    Port = list_to_integer(lists:reverse(Acc)),
    consume_path("", [], Rest, URI#uri{port = Port});
consume_port(Acc, [$? | Rest], URI) ->
    Port = list_to_integer(lists:reverse(Acc)),
    consume_qs(Rest, URI#uri{port = Port});
consume_port(Acc, [$# | Rest], URI) ->
    Port = list_to_integer(lists:reverse(Acc)),
    consume_fragment("", Rest, URI#uri{port = Port});
consume_port(Acc, [Letter | Rest], URI)
        when $0 =< Letter, Letter =< $9 ->
    consume_port([Letter | Acc], Rest, URI);
consume_port(_, _, _) ->
    {error, port}.


consume_path(Acc, Parts, "", URI) ->
    Part = lists:reverse(Acc),
    NewParts = [Part | Parts],
    Path = lists:reverse(NewParts),
    {ok, URI#uri{path = Path}};
consume_path(Acc, Parts, [$/ | Rest], URI) ->
    Part = lists:reverse(Acc),
    consume_path("", [Part | Parts], Rest, URI);
consume_path(Acc, Parts, [$? | Rest], URI) ->
    Part = lists:reverse(Acc),
    NewParts = [Part | Parts],
    Path = lists:reverse(NewParts),
    consume_qs(Rest, URI#uri{path = Path});
consume_path(Acc, Parts, [$# | Rest], URI) ->
    Part = lists:reverse(Acc),
    NewParts = [Part | Parts],
    Path = lists:reverse(NewParts),
    consume_fragment("", Rest, URI#uri{path = Path});
consume_path(Acc, Parts, [Letter | Rest], URI)
        when $a =< Letter, Letter =< $z;
             $0 =< Letter, Letter =< $9;
             Letter == $-; Letter == $_;
             Letter == $.; Letter == $,;
             Letter == $%; Letter == $+ ->
    consume_path([Letter | Acc], Parts, Rest, URI);
consume_path(_, _, _, _) ->
    {error, path}.


consume_qs("", URI) ->
    {ok, URI};
consume_qs([$# | Rest], URI) ->
    consume_fragment("", Rest, URI);
consume_qs(Rest, URI) ->
    consume_qs("", [], Rest, URI).


consume_qs("", Parts, "", URI) ->
    QS = lists:reverse(Parts),
    {ok, URI#uri{qs = QS}};
consume_qs(Acc, Parts, "", URI) ->
    Q = lists:reverse(Acc),
    Part = {Q, ""},
    QS = lists:reverse([Part | Parts]),
    {ok, URI#uri{qs = QS}};
consume_qs("", Parts, [$# | Rest], URI) ->
    QS = lists:reverse(Parts),
    consume_fragment("", Rest, URI#uri{qs = QS});
consume_qs(Acc, Parts, [$# | Rest], URI) ->
    Q = lists:reverse(Acc),
    Part = {Q, ""},
    QS = lists:reverse([Part | Parts]),
    consume_fragment("", Rest, URI#uri{qs = QS});
consume_qs("", _, [$= | _], _) ->
    error;
consume_qs(Acc, Parts, [$= | Rest], URI) ->
    consume_qs("", lists:reverse(Acc), Parts, Rest, URI);
consume_qs(Acc, Parts, [Letter | Rest], URI)
        when $a =< Letter, Letter =< $z;
             $0 =< Letter, Letter =< $9;
             Letter == $-; Letter == $_;
             Letter == $.; Letter == $,;
             Letter == $%; Letter == $+;
             Letter == ${; Letter == $};
             Letter == $[; Letter == $];
             Letter == $:; Letter == $@;
             Letter == $/ ->
    consume_qs([Letter | Acc], Parts, Rest, URI);
consume_qs(_, _, _, _) ->
    {error, querystring}.


consume_qs(Acc, Q, Parts, "", URI) ->
    S = lists:reverse(Acc),
    QS = lists:reverse([{Q, S} | Parts]),
    {ok, URI#uri{qs = QS}};
consume_qs(Acc, Q, Parts, [$& | Rest], URI) ->
    S = lists:reverse(Acc),
    consume_qs("", [{Q, S} | Parts], Rest, URI);
consume_qs(Acc, Q, Parts, [$; | Rest], URI) ->
    S = lists:reverse(Acc),
    consume_qs("", [{Q, S} | Parts], Rest, URI);
consume_qs(Acc, Q, Parts, [$# | Rest], URI) ->
    S = lists:reverse(Acc),
    QS = lists:reverse([{Q, S} | Parts]),
    consume_fragment("", Rest, URI#uri{qs = QS});
consume_qs(Acc, Q, Parts, [Letter | Rest], URI) ->
    consume_qs([Letter | Acc], Q, Parts, Rest, URI).


consume_fragment(_, Fragment, URI) ->
    {ok, URI#uri{fragment = Fragment}}.
% TODO: Escaping. The version above simply doesn't escape anything.
%consume_fragment(Acc, "", URI) ->
%    Fragment = lists:reverse(Acc),
%    {ok, URI#uri{fragment = Fragment}};
%consume_fragment(Acc, [Letter | Rest], URI) ->
%    consume_fragment([Letter | Acc], Rest, URI).
%consume_fragment(_, _, _) ->
%    {error, fragment}.


-spec export(URI) -> String
    when URI    :: uri(),
         String :: string().
%% @doc
%% Accept a URI datatype and export a URI-safe encoded string.
%% FIXME: This currently returns an unsafe URI.

export(URI = #uri{protocol = Protocol, hostname = RawHostname}) ->
    Hostname = string:join(RawHostname, "."),
    cat_port([Protocol, "://", Hostname], URI).


cat_port(Acc, URI = #uri{port = default}) ->
    cat_path(Acc, URI);
cat_port(Acc, URI = #uri{port = Number}) ->
    Port = integer_to_list(Number),
    cat_path([Acc, ":", Port], URI).


cat_path(Acc, URI = #uri{path = []}) ->
    cat_qs([Acc, "/"], URI);
cat_path(Acc, URI = #uri{path = Parts}) ->
    Path = string:join(Parts, "/"),
    cat_qs([Acc, "/", Path], URI).


cat_qs(Acc, URI = #uri{qs = []}) ->
    cat_fragment(Acc, URI);
cat_qs(Acc, URI = #uri{qs = Parts}) ->
    QS = string:join([[Q, "=", S] || {Q, S} <- Parts], "&"),
    cat_fragment([Acc, "?", QS], URI).


cat_fragment(Acc, #uri{fragment = ""}) ->
    lists:flatten(Acc);
cat_fragment(Acc, #uri{fragment = Fragment}) ->
    lists:flatten([Acc, "#", Fragment]).


-spec protocol(URI) -> Protocol
    when URI      :: uri(),
         Protocol :: string().
%% @doc
%% Accept a URI datatype and return the protocol.

protocol(#uri{protocol = Protocol}) -> Protocol.


-spec protocol(String, URI) -> NewURI
    when String :: string(),
         URI    :: uri(),
         NewURI :: uri().
%% @doc
%% Accept a protocol label and a URI, and return the URI updated with the
%% new protocol.
%% FIXME: Will currently accept any invalid input -- should only accept [:alpha:]

protocol(Protocol, URI) ->
    URI#uri{protocol = Protocol}.


-spec username(URI) -> Username
    when URI      :: uri(),
         Username :: string().
%% @doc
%% Accept a URI datatype and return the username.

username(#uri{username = Username}) ->
    Username.


-spec username(String, URI) -> NewURI
    when String :: string(),
         URI    :: uri(),
         NewURI :: uri().
%% @doc
%% Accept a username and a URI, and return the URI updated with the new username.

username(String, URI) ->
    URI#uri{username = String}.


-spec password(URI) -> Password
    when URI      :: uri(),
         Password :: string().
%% @doc
%% Accept a URI datatype and return the password.

password(#uri{password = Password}) ->
    Password.


-spec password(String, URI) -> NewURI
    when String :: string(),
         URI    :: uri(),
         NewURI :: uri().
%% @doc
%% Accept a password and a URI, and return the URI updated with the new password.

password(String, URI) ->
    URI#uri{password = String}.


-spec hostname(URI) -> Hostname
    when URI      :: uri(),
         Hostname :: string().
%% @doc
%% Accept a URI datatype and return the hostname.

hostname(#uri{hostname = Parts}) ->
    string:join(Parts, ".").


-spec hostname(String, URI) -> NewURI
    when String :: string(),
         URI    :: uri(),
         NewURI :: uri().
%% @doc
%% Accept a hostname and a URI, and return the URI updated with the new hostname.
%% FIXME: Currently accepts invalid input and performs no IDN conversions.

hostname(String, URI) ->
    Hostname = string:tokens(String, "."),
    URI#uri{hostname = Hostname}.


-spec port(URI) -> Port
    when URI  :: uri(),
         Port :: default
               | inet:port_number().
%% @doc
%% Accept a URI datatype and return the port or `default' if it is undefined.

port(#uri{port = Port}) -> Port.


-spec port(Port, URI) -> NewURI
    when Port   :: default
                 | inet:port_number(),
         URI    :: uri(),
         NewURI :: uri().
%% @doc
%% Accept a port number and a URI, and return the URI updated with the
%% new port.

port(default, URI) ->
    URI#uri{port = default};
port(Port, URI)
        when 0 =< Port, Port =< 65535 ->
    URI#uri{port = Port}.


-spec path(URI) -> Path
    when URI  :: uri(),
         Path :: string().
%% @doc
%% Accept a URI datatype and return the path as a string.

path(#uri{path = Parts}) ->
    filename:join(Parts).


-spec path(Path, URI) -> NewURI
    when Path   :: string(),
         URI    :: uri(),
         NewURI :: uri().
%% @doc
%% Accept a path and a URI, and return the URI updated with the new path.
%% FIXME: This currently accepts invalid input and could return an invalid
%%        result in certain cases.

path(String, URI) ->
    URI#uri{path = string:tokens(String, "/")}.


-spec qs(URI) -> Queries
    when URI     :: uri(),
         Queries :: [qs()].
%% @doc
%% Accept a URI datatype and return its list of queries.

qs(#uri{qs= QS}) -> QS.


-spec qs(Queries, URI) -> NewURI
    when Queries :: [qs()],
         URI     :: uri(),
         NewURI  :: uri().
%% @doc
%% Accept a list of queries and a URI, and return the URI updated with the
%% new queries, replacing all of the original ones.

qs(QS, URI) ->
    URI#uri{qs = QS}.


-spec update_qs(Label, Value, URI) -> NewURI
    when Label  :: string(),
         Value  :: string(),
         URI    :: uri(),
         NewURI :: uri().
%% @doc
%% Accept a Label, Value and a URI, upsert the query indicated by the label,
%% and return the new URI.

update_qs(Label, Value, URI = #uri{qs = QS}) ->
    NewQS = lists:keystore(Label, 1, QS, {Label, Value}),
    URI#uri{qs = NewQS}.


-spec drop_qs(Label, URI) -> NewURI
    when Label  :: string(),
         URI    :: uri(),
         NewURI :: uri().
%% @doc
%% Accept a label and a URI, and drop the query indicated by the label, if it
%% exists.

drop_qs(Label, URI = #uri{qs = QS}) ->
    NewQS = lists:keydelete(Label, 1, QS),
    URI#uri{qs = NewQS}.


-spec construct_uri(UriString, Defaults) -> Result
    when UriString :: string(),
         Defaults  :: [qs()],
         Result    :: {ok, uri()}
                    | {error, bad_uri}.
%% @doc
%% Accept a URI string and a proplist of default query values, and return
%% a normalized URI string, incorporating the default query values.

construct_uri(UriString, Defaults) ->
    case new(UriString) of
        {ok, URI} ->
            QS = qs(URI),
            {ok, lists:foldl(fun update_listkey/2, Defaults, QS)};
        Error ->
            Error
    end.


-spec update_listkey({Key, Value}, AccList) -> NewAccList
    when Key        :: string(),
         Value      :: string(),
         AccList    :: list(),
         NewAccList :: list().
%% @private
%% A helper function for using a fold to override default header values with a list
%% list of current header value overrides. Called by the foldl in construct_uri/2.
         
update_listkey({Key, Value}, QueryList) ->
    lists:keystore(Key, 1, QueryList, {Key, Value}).


%-spec is_tld(Domain) -> Result
%    when Domain :: string(),
%         Result :: boolean().
%%% %@doc
%%% Check if the Domain is a Top Level Domain
%%% It uses the bd providen in
%%% http://mxr.mozilla.org/mozilla-central/source/netwerk/dns/effective_tld_names.dat?raw=1
%
%is_tld(Domain) when is_binary(Domain) -> is_tld(binary_to_list(Domain));
%is_tld(Domain) -> publicsuffix:is_tld(Domain).


-spec check_custom_port(Scheme, Port) -> Result
    when Scheme    :: http_uri:scheme(),
         Port      :: pos_integer(),
         Result    :: {Scheme, MaybePort},
         MaybePort :: Port | default.
%% @doc
%% Accepts a Scheme and Port argument, and returns the scheme plus a custom
%% port number, or the scheme and the atom 'default'.
%%
%% Output of this function is consumed by format_uri/5.
%% @see urilator:format_uri/5


check_custom_port(http,     80) -> {http,   default};
check_custom_port(https,   443) -> {https,  default};
check_custom_port(ftp,      21) -> {ftp,    default};
check_custom_port(ssh,      22) -> {ssh,    default};
check_custom_port(scp,      22) -> {scp,    default};
check_custom_port(sftp,     22) -> {sftp,   default};
check_custom_port(telnet,   23) -> {telnet, default};
check_custom_port(smtp,     25) -> {smtp,   default};
check_custom_port(dns,      53) -> {smtp,   default};
check_custom_port(tftp,     69) -> {tftp,   default};
check_custom_port(gopher,   70) -> {gopher, default};
check_custom_port(Scheme, Port) -> {Scheme, Port}.


-spec pack_uri(SchemePort, User, Host, Path, Query) -> Result
    when SchemePort :: {http_uri:scheme(), Port},
         Port       :: pos_integer()
                     | default,
         User       :: http_uri:user_info(),
         Host       :: http_uri:host(),
         Path       :: http_uri:path(),
         Query      :: [{Key :: string(), Value :: string()}],
         Result     :: iolist().
%% @doc
%% Build an iolist from typed URI elements.

pack_uri({Scheme, default}, "", Host, Path, "") ->
    io_lib:format("~p://~s/~s", [Scheme, Host, Path]);
pack_uri({Scheme, default}, "", Host, Path, Query) ->
    io_lib:format("~p://~s/~s?~s", [Scheme, Host, Path, Query]);
pack_uri({Scheme, default}, User, Host, Path, "") ->
    io_lib:format("~p://~s@~s/~s", [Scheme, User, Host, Path]);
pack_uri({Scheme, default}, User, Host, Path, Query) ->
    io_lib:format("~p://~s@~s/~s?~s", [Scheme, User, Host, Path, Query]);
pack_uri({Scheme, Port}, "", Host, Path, "") ->
    io_lib:format("~p://~s:~p/~s", [Scheme, Host, Port, Path]);
pack_uri({Scheme, Port}, "", Host, Path, Query) ->
    io_lib:format("~p://~s:~p/~s?~s", [Scheme, Host, Port, Path, Query]);
pack_uri({Scheme, Port}, User, Host, Path, "") ->
    io_lib:format("~p://~s@~s:~p/~s", [Scheme, User, Host, Port, Path]);
pack_uri({Scheme, Port}, User, Host, Path, Query) ->
    io_lib:format("~p://~s@~s:~p/~s?~s", [Scheme, User, Host, Port, Path, Query]).
