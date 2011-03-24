-module(rss).
-export([parse/1]).
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("eunit/include/eunit.hrl").

hexstring(<<X:128/big-unsigned-integer>>) ->
    %io:format("~32.16.0b~n", [X]).
    lists:flatten(io_lib:format("~32.16.0b", [X])).

itemExists(E) ->
    Binary = term_to_binary(E),
    Md5 = erlang:md5(Binary),
    S = hexstring(Md5),
    R = get(S),
    case R of
        undefined ->
            %io:format("undefined:  ~s~n", [E]),
            put(S,S),
            false;
        _ ->
            %io:format("exists:i  ~s~n", [E]),
            true
    end.

%% parse(Buffer) Buffer contains XML
%% returns a list of extracted XML strings
parse(Buffer) ->
    % parses a single RSS string buffer
    error_logger:info_report("rss:parse"),
    {R,_} = xmerl_scan:string(Buffer),
    ItemList = lists:reverse(parseElementsList(R)),
    L = [X || X <- ItemList, not(itemExists(X))],
    L.

parseElementsList([H|T]) when H#xmlElement.name == title ->
    Export = xmerl:export_simple([H], xmerl_xml),
    XML = lists:flatten(Export),
    [XML | parseElementsList(T)];
parseElementsList([H|T]) when is_record(H, xmlElement) ->
    parseElementsList(H#xmlElement.content) ++ parseElementsList(T);                                                                  
parseElementsList(X) when is_record(X, xmlElement) ->
    parseElementsList(X#xmlElement.content);
parseElementsList([_|T]) ->
    parseElementsList(T);
parseElementsList([]) ->
    [].

rss0_test() ->
    {ok, Buffer} = file:read_file("digg-science-rss2.xml"),
    MList = parse(binary_to_list(Buffer)),
    MList.
    %[H | _] = MList,
    %MList.

rss1_test() ->
    {ok, Buffer} = file:read_file("digg-science-rss1.xml"),
    MList = parse(binary_to_list(Buffer)),
    io:format("length = ~p~n", [length(MList)] ),
    ?assertEqual( 40, length(MList) ),
    [H | _] = MList,
    Export = xmerl:export_simple([H], xmerl_xml),
    {ok,IOF} = file:open('out.xml',[write]),
    io:format(IOF,"~s~n", [lists:flatten(Export)]).

rss2_test() ->
    rss1_test(),
    {ok, Buffer} = file:read_file("digg-science-rss2.xml"),
    MList = parse(binary_to_list(Buffer)),
    % io:format("length = ~p~n", length(MList) ),
    % io:format("List = ~p~n", MList ),
    ?assertEqual( 40, length(MList) ).

rss3_test() ->
    {ok, Buffer} = file:read_file("slash.xml"),
    MList = parse(binary_to_list(Buffer)),
    % io:format("length = ~p~n", length(MList) ),
    ?assertEqual( 15, length(MList) ).


