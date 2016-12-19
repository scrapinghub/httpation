-module (http_request_tests).

-vsn("0.2.0").
-author("Oleg Tarasenko <oltarasenko@gmail.com>").

-include_lib("eunit/include/eunit.hrl").

method_test() ->
    NewMethod = "POST",
    {ok, HttpReq} = httpation_request:new("HTTP/1.1 GET http://meta.ua"),
    NewReq = httpation_request:method(HttpReq, NewMethod),
    ?assertEqual(httpation_request:method(NewReq), NewMethod).

version_test() ->
    NewVersion = "Http 1.0",
    {ok, HttpReq} = httpation_request:new("HTTP/1.1 GET http://meta.ua"),
    NewReq = httpation_request:version(HttpReq, NewVersion),
    ?assertEqual(httpation_request:version(NewReq), NewVersion).


headers_test() ->
    NewHeaders = [{'Accept-Charset', "utf-8"}],
    {ok, HttpReq} = httpation_request:new("HTTP/1.1 GET http://meta.ua"),
    NewReq = httpation_request:headers(HttpReq, NewHeaders),
    ?assertEqual(httpation_request:headers(NewReq), NewHeaders).


uri_test() ->
    NewURI = "http://scrapinghub.com",
    {ok, HttpReq} = httpation_request:new("HTTP/1.1 GET http://meta.ua"),
    NewReq = httpation_request:uri(HttpReq, NewURI),
    ?assertEqual(httpation_request:uri(NewReq), NewURI).


chunk_size_test() ->
    ChunkSize = 30,
    {ok, HttpReq} = httpation_request:new("HTTP/1.1 GET http://meta.ua"),
    NewReq = httpation_request:chunk_size(HttpReq, ChunkSize),
    ?assertEqual(httpation_request:chunk_size(NewReq), ChunkSize).


body_test() ->
    NewBody = <<"Some body">>,
    {ok, HttpReq} = httpation_request:new("HTTP/1.1 GET http://meta.ua"),
    NewReq = httpation_request:body(HttpReq, NewBody),
    ?assertEqual(httpation_request:body(NewReq), NewBody).


trailer_test() ->
    NewTrailer = [{'Accept-Charset', "utf-8"}],
    {ok, HttpReq} = httpation_request:new("HTTP/1.1 GET http://meta.ua"),
    NewReq = httpation_request:trailer(HttpReq, NewTrailer),
    ?assertEqual(httpation_request:trailer(NewReq), NewTrailer).