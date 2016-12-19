-module (http_response_tests).
-vsn("0.2.0").
-author("Oleg Tarasenko <oltarasenko@gmail.com>").


-include_lib("eunit/include/eunit.hrl").


version_test() ->
    NewVersion = "HTTP/2.0",
    {ok, HttpReq} = httpation_response:new("HTTP/1.1 200"),
    NewReq = httpation_response:version(HttpReq, NewVersion),
    ?assertEqual(httpation_response:version(NewReq), NewVersion).


code_test() ->
    NewCode = 401,
    {ok, HttpReq} = httpation_response:new("HTTP/1.1 200"),
    NewReq = httpation_response:code(HttpReq, NewCode),
    ?assertEqual(httpation_response:code(NewReq), NewCode).


phrase_test() ->
    NewPhrase = "Some phrase",
    {ok, HttpReq} = httpation_response:new("HTTP/1.1 200"),
    NewReq = httpation_response:phrase(HttpReq, NewPhrase),
    ?assertEqual(httpation_response:phrase(NewReq), NewPhrase).


headers_test() ->
    NewHeaders = [{'Accept-Charset', "utf-8"}],
    {ok, HttpReq} = httpation_response:new("HTTP/1.1 200"),
    NewReq = httpation_response:headers(HttpReq, NewHeaders),
    ?assertEqual(httpation_response:headers(NewReq), NewHeaders).


chunksize_test() ->
    NewChank = 100,
    {ok, HttpReq} = httpation_response:new("HTTP/1.1 200"),
    NewReq = httpation_response:chunk_size(HttpReq, NewChank),
    ?assertEqual(httpation_response:chunk_size(NewReq), NewChank).


trailer_test() ->
    NewHeaders = [{'Accept-Charset', "utf-8"}],
    {ok, HttpReq} = httpation_response:new("HTTP/1.1 200"),
    NewReq = httpation_response:trailer(HttpReq, NewHeaders),
    ?assertEqual(httpation_response:trailer(NewReq), NewHeaders).