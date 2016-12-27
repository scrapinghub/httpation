-module(httpation_uri_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%% API
httpation_uri_test_() ->
    [
        {"common url", fun() ->
            {ok, URI} = httpation_uri:new("https://scrapinghub.atlassian.net/secure/Dashboard.jspa"),
            
            {uri, Protocol, Username, Password, Netloc, _, Path, _, _, _} = URI,
            ?assertEqual("https", Protocol),
            ?assertEqual([], Username),
            ?assertEqual([], Password),
            ?assertEqual(["secure", "dashboard.jspa"], Path),
            ?assertEqual("scrapinghub.atlassian.net", Netloc)
        end},
        {"username and password", fun() ->
            {ok, URI} = httpation_uri:new("https://u:p@scrapinghub.atlassian.net/secure/Dashboard.jspa"),
            
            {uri, Protocol, Username, Password, Netloc, _, Path, _, _, _} = URI,
            ?assertEqual("https", Protocol),
            ?assertEqual("u", Username),
            ?assertEqual("p", Password),
            ?assertEqual(["secure", "dashboard.jspa"], Path),
            ?assertEqual("scrapinghub.atlassian.net", Netloc)
        end},
        {"username and empty password", fun() ->
            {ok, URI} = httpation_uri:new("https://u:@scrapinghub.atlassian.net"),
            
            {uri, Protocol, Username, Password, Netloc, Port, Path, _, _, _} = URI,
            ?assertEqual(["/"], Path),
            ?assertEqual("u", Username),
            ?assertEqual([], Password),
            ?assertEqual("scrapinghub.atlassian.net", Netloc)
        end},
        {"port", fun() ->
            {ok, URI} = httpation_uri:new("https://scrapinghub.atlassian.net:8080/secure/Dashboard.jspa"),
            
            {uri, Protocol, Username, Password, Netloc, Port, Path, _, _, _} = URI,
            ?assertEqual("https", Protocol),
            ?assertEqual(8080, Port),
            ?assertEqual(["secure", "dashboard.jspa"], Path),
            ?assertEqual("scrapinghub.atlassian.net", Netloc)
        end},
        {"no path", fun() ->
            {ok, URI} = httpation_uri:new("https://scrapinghub.atlassian.net"),
            
            {uri, Protocol, Username, Password, Netloc, Port, Path, _, _, _} = URI,
            ?assertEqual(["/"], Path),
            ?assertEqual("scrapinghub.atlassian.net", Netloc)
        end},
        {"dashed netloc", fun() ->
            {ok, URI} = httpation_uri:new("https://scrapinghub-develop.atlassian.net"),
            
            {uri, Protocol, Username, Password, Netloc, Port, Path, _, _, _} = URI,
            ?assertEqual(["/"], Path),
            ?assertEqual("scrapinghub-develop.atlassian.net", Netloc)
        end}
        
    ].

