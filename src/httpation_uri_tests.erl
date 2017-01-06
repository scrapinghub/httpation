-module(httpation_uri_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%% API
httpation_uri_test_() ->
    [
        {"common url", fun() ->
            {ok, URI} = httpation_uri:new("https://example.net/secure/Dashboard.jspa"),

            {uri, Protocol, Username, Password, Netloc, _, Path, _, _, _} = URI,
            ?assertEqual("https", Protocol),
            ?assertEqual([], Username),
            ?assertEqual([], Password),
            ?assertEqual(["secure", "dashboard.jspa"], Path),
            ?assertEqual("example.net", Netloc)
        end},

        {"username and password", fun() ->
            {ok, URI} = httpation_uri:new("https://u:p@example.net/secure/Dashboard.jspa"),

            {uri, Protocol, Username, Password, Netloc, _, Path, _, _, _} = URI,
            ?assertEqual("https", Protocol),
            ?assertEqual("u", Username),
            ?assertEqual("p", Password),
            ?assertEqual(["secure", "dashboard.jspa"], Path),
            ?assertEqual("example.net", Netloc)
        end},

        {"username and empty password", fun() ->
            {ok, URI} = httpation_uri:new("https://u:@example.net"),

            {uri, _, Username, Password, _, _, _, _, _, _} = URI,
            ?assertEqual("u", Username),
            ?assertEqual([], Password)
        end},

        {"port", fun() ->
            {ok, URI} = httpation_uri:new("https://example.net:8080/secure/Dashboard.jspa"),

            {uri, Protocol, _, _, Netloc, Port, Path, _, _, _} = URI,
            ?assertEqual("https", Protocol),
            ?assertEqual(8080, Port),
            ?assertEqual(["secure", "dashboard.jspa"], Path),
            ?assertEqual("example.net", Netloc)
        end},

        {"no path", fun() ->
            {ok, URI} = httpation_uri:new("https://example.net"),

            {uri, _, _, _, Netloc, _, Path, _, _, _} = URI,
            ?assertEqual(["/"], Path),
            ?assertEqual("example.net", Netloc)
        end},
        
        {"no path with trailing slash", fun() ->
            {ok, URI} = httpation_uri:new("https://example.net/"),
            
            {uri, _, _, _, Netloc, _, Path, _, _, _} = URI,
            ?assertEqual(["/"], Path),
            ?assertEqual("example.net", Netloc)
        end},
        
        {"dashed netloc", fun() ->
            {ok, URI} = httpation_uri:new("https://example-develop.atlassian.net"),

            {uri, _, _, _, Netloc, _, _, _, _, _} = URI,
            ?assertEqual("example-develop.atlassian.net", Netloc)
        end},

        {"multiple / in path", fun() ->
            {ok, URI1} = httpation_uri:new("https://example-develop.atlassian.net//path"),
            {ok, URI2} = httpation_uri:new("https://example-develop.atlassian.net///path"),
            {ok, URI3} = httpation_uri:new("https://example-develop.atlassian.net////path"),
            {ok, URI4} = httpation_uri:new("https://example-develop.atlassian.net/////path"),
            {ok, URI5} = httpation_uri:new("https://example-develop.atlassian.net//////path"),

            {uri, _, _, _, _, _, Path1, _, _, _} = URI1,
            ?assertEqual(["path"], Path1),
            {uri, _, _, _, _, _, Path2, _, _, _} = URI2,
            ?assertEqual(["path"], Path2),
            {uri, _, _, _, _, _, Path3, _, _, _} = URI3,
            ?assertEqual(["path"], Path3),
            {uri, _, _, _, _, _, Path4, _, _, _} = URI4,
            ?assertEqual(["path"], Path4),
            {uri, _, _, _, _, _, Path5, _, _, _} = URI5,
            ?assertEqual(["path"], Path5)
        end},
 
        {"splitted path ", fun() ->
            {ok, URI1} = httpation_uri:new("https://example-develop.atlassian.net//path/path1"),
            {ok, URI2} = httpation_uri:new("https://example-develop.atlassian.net///path/path1/path2"),
            {ok, URI3} = httpation_uri:new("https://example-develop.atlassian.net////path/path1/path2/path3"),

            {uri, _, _, _, _, _, Path1, _, _, _} = URI1,
            {uri, _, _, _, _, _, Path2, _, _, _} = URI2,
            {uri, _, _, _, _, _, Path3, _, _, _} = URI3,
            ?assertEqual(["path", "path1"], Path1),
            ?assertEqual(["path", "path1", "path2"], Path2),
            ?assertEqual(["path", "path1", "path2", "path3"], Path3)
        end},

        %% wrong inputs
        {"extra :", fun() ->
            {error, password_or_port} = httpation_uri:new("https://a:b:c@example-develop.atlassian.net")
        end},
        {"invalid chars in username", fun() ->
            {error, user_or_hostname} = httpation_uri:new("https://%@$@#!:b@example.net")
        end},
        {"invalid chars in hostname", fun() ->
            {error, hostname} = httpation_uri:new("https://a:b@example].atlassian.net")
        end},
        {"invalid chars in username and hostname", fun() ->
            {error, user_or_hostname} = httpation_uri:new("https://%@$@#!:b@example].atlassian.net")
        end},
        {"invalid chars in username and hostname", fun() ->
            {error, user_or_hostname} = httpation_uri:new("https://%@$@#!:b@example].atlassian.net")
        end}
        
    ].

