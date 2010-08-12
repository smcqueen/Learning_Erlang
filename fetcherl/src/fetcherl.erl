-module(fetcherl).

-export([init/0, timeit/1, fetch_with_http_async/1,
         fetch_with_pmap/2, fetch_with_map/1, main/1]).

-define(UA,"Mozilla/5.0 (Erlang http:request)").

main([UrlFile, Which|Rest]) ->
    init(),
    Urls = read_img_urls(atom_to_list(UrlFile)),
    io:format("fetching with ~p~n", [Which]),
    case Which of
        async ->
            timeit(fun() -> fetch_with_http_async(Urls) end);
        pmap ->
            N = case Rest of
                [Count] ->
                    list_to_integer(atom_to_list(Count));
                [] -> 10
            end,
            io:format("using ~p concurrent processes~n", [N]),
            timeit(fun() -> fetch_with_pmap(Urls, N) end);
        map ->
            timeit(fun() -> fetch_with_map(Urls) end)
    end;
main(_Args) ->
    io:format("expecting URLFILE async|pmap [N]~n").

init() ->
    application:start(inets),
    ok.

fetch_with_map(Urls) ->
    lists:map(
      fun(Url) ->
              {ok, {{_, Code, _}, _H, _B}} =
                  http:request(head, {Url, [{"User-Agent", ?UA}]}, [], []),
              Code > 399
      end,
      Urls).
    
fetch_with_pmap(Urls, N) ->
    putil:pmap(
      fun(Url) ->
              {ok, {{_, Code, _}, _H, _B}} =
                  http:request(head, {Url, [{"User-Agent", ?UA}]}, [], []),
              Code > 399
      end,
      Urls, N).
    
fetch_with_http_async(Urls) ->
    ReqIds = lists:map(
               fun(Url) ->
                       {ok, ReqId} =
                           http:request(head, {Url, [{"User-Agent", ?UA}]}, [],
                                        [{sync, false}]),
                       ReqId
               end,
               Urls),
    gather(ReqIds).

gather([ReqId|Ids]) ->
    receive
        {http, {ReqId, {{_, Code, _}, _H, _B}}} ->
            [Code > 399|gather(Ids)]
    end;
gather([]) ->
    [].

read_img_urls(Where) ->
    {ok, In} = file:open(Where, read),
    load_urls(In, []).

load_urls(In, Acc) ->
    case io:get_line(In, '') of
        eof ->
            lists:reverse(Acc);
        Line ->
            load_urls(In, [string:strip(Line, right, $\n)|Acc])
    end.

timeit(F) -> 
    statistics(wall_clock),
    Ans = F(),
    {_, RTime} = statistics(wall_clock),
    io:format("time: ~.3f secs~n", [RTime / 1000.0]),
    Ans.
