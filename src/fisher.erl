%% Copyright 2011 Michael Vierling.  All rights reserved.

-module(fisher).
-export([getwords/1]).
-import(lists,[reverse/1,member/2]).
-import(dict).

-include_lib("eunit/include/eunit.hrl").

%% mult(L) returns the all items multiplied together

%-spec mult([number()]) -> number().

mult(L) -> mult(L, 1).
mult([H|T], Total) -> mult(T, Total * H);
mult([], Total) -> Total.


fisherProb(Classifier, Doc, Cat) ->
    FList = getwords( Doc ),
    FeatureProb = fun cProb/3,
    ProbCounts = [ probDocCat(Classifier, X, Cat, FeatureProb) || X <- FList ],
    P = mult( ProbCounts ),
    FScore = -2 * math:log( P ),
    ICS = inverseChiSquared( FScore, length(FList) * 2 ),
    ICS.

chiSquared( _M, I, Max, _Term, Sum ) when I >= Max ->
    Sum;
chiSquared( M, I, Max, Term, Sum ) ->
    io:format("Term: ~p ~n", [Term]),
    io:format("Sum: ~p ~n", [Sum]),
    T2 = Term * (M / I),
    S2 = Sum + T2,
    chiSquared( M, I + 1, Max, T2, S2 ). 

inverseChiSquared(Chi, DF) ->
    io:format("Chi: ~p ~n", [Chi]),
    io:format("DF: ~p ~n", [DF]),
    M = Chi / 2.0,
    Sum = Term = math:exp( -M ),
    R = chiSquared( M, 1, (DF div 2), Term, Sum ),
    min( R, 1.0 ).    

cProb(Classifier, Doc, Cat) ->
    ProbDocCat = featureProb(Classifier, Doc, Cat),
    case ProbDocCat of
        0 -> 0;
        _ ->
            CatList = catList(Classifier),
            FreqCounts = [ featureProb(Classifier, Doc, X) || X <- CatList],
            FreqSum = lists:sum( FreqCounts ),
            Prob = ProbDocCat / FreqSum,
            Prob
    end.

classify(Classifier, Doc, Threshold) ->
    CatList = catList(Classifier),
    PList = [ {bayesProb(Classifier, Doc, X), X} || X <- CatList],
    lists:reverse( lists:sort( PList ) ).

bayesProb(Classifier, Doc, Cat) ->
    CatProb = catTotal(Classifier, Cat) / docTotal(Classifier),
    FeatureProb = fun featureProb/3,
    DocProb = probDocCat(Classifier, Doc, Cat, FeatureProb),
    Prob = DocProb * CatProb,
    Prob.

% Find probability of Doc, given Category
probDocCat(Classifier, Doc, Cat, FeatureProb) ->
    WordList = getwords( Doc ),
    ProbCounts = [ weightedProb(Classifier, Word, Cat, FeatureProb, 1.0, 0.5) || Word <- WordList ],
    mult( ProbCounts ).

docTotal(Classifier) ->
    CatList = dict:to_list(Classifier),
    docTotal(CatList, 0).
docTotal([H|T], Sum) ->
    {_Key, {_Dict, CatCount}} = H,
    docTotal(T, Sum + CatCount);
docTotal([], Sum) -> Sum.

catTotal(Classifier, Cat) ->
    Result = dict:find(Cat, Classifier),
    case Result of
        {ok, {_Dict, CatCount}} -> CatCount;
        _ -> 0
    end. 

catList(Classifier) ->
    CatList = dict:to_list(Classifier),
    OutList = [ Key || {Key, _} <- CatList ],
    OutList.

featureProb(Classifier, Feature, Cat) ->
    Result = dict:find(Cat, Classifier),
    case Result of
        {ok, {Dict, CatCount}} ->
            FeatureResult = dict:find(Feature, Dict),
            case FeatureResult of
                {ok, FeatureCount} ->
                    Probability = FeatureCount / CatCount,
                    Probability;
                error -> 0
            end;
        error -> 0
    end.

%featureProb(Classifier, Feature, Cat) ->    
%    {ok, {Dict,CatCount}} = dict:find(Cat, Classifier),
%    {ok, FeatureCount} = dict:find(Feature, Dict),
%    Probability = FeatureCount / CatCount,
%    Probability.

featureSummer(X, Feature) ->
    %io:format("~w.~n", [X] ),
    {_Key, Value} = X,
    {Dict,_CatCount} = Value,
    Result = dict:find(Feature, Dict),
    case Result of
        {ok, FeatureCount} ->
            FeatureCount;
        error -> 0
    end.

featureTotal(Classifier, Feature) ->
    CatList = dict:to_list(Classifier),
    SumList = [ featureSummer(X, Feature) || X <- CatList],
    Sum = lists:sum(SumList),
    Sum.

weightedProb(Classifier, Feature, Cat, FeatureProb, Weight, AP) ->
    BaseProb = FeatureProb(Classifier, Feature, Cat),
    FeatureTotal = featureTotal(Classifier, Feature),
    BP = ((Weight * AP) + (FeatureTotal * BaseProb))/(Weight + FeatureTotal),
    BP.

trainCount([], Dict) ->
    Dict;
trainCount([H|T],Dict) ->
    case dict:find(H, Dict) of
        {ok, Value} ->
            Dict2 = dict:store(H, Value + 1, Dict),
            trainCount(T,Dict2);
        _ ->
            Dict2 = dict:store(H, 1, Dict),
            trainCount(T,Dict2)
    end.

train(Classifier,Cat,Input) ->
    case dict:find(Cat, Classifier) of
        {ok, Value} ->
            {Dict,Count} = Value,
            Dict2 = trainCat(Dict,Input),
            NewValue = {Dict2, Count + 1},
            Classifier2 = dict:store(Cat, NewValue, Classifier),
            Classifier2;
        _->
            NewDict = trainCat(dict:new(), Input),
            FirstValue = { NewDict, 1 },
            Classifier2 = dict:store(Cat, FirstValue, Classifier),
            Classifier2
    end.

trainCat(Dict,Input) ->
    L = getwords(Input),
    Dict2 = trainCount(L, Dict),
    Dict2.

ascii() ->
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ".

getwords(Doc) ->
    tokens(Doc, ascii()).

-spec tokens(string(), string()) -> [[char(),...]].

tokens(S, Seps) ->
    tokens1(S, Seps, []).

tokens1([C|S], Seps, Toks) ->
    case member(C, Seps) of
        false -> tokens1(S, Seps, Toks);
        true -> tokens2(S, Seps, Toks, [C])
    end;
tokens1([], _Seps, Toks) ->
    reverse(Toks).

tokens2([C|S], Seps, Toks, Cs) ->
    case member(C, Seps) of
        false -> tokens1(S, Seps, [reverse(Cs)|Toks]);
        true -> tokens2(S, Seps, Toks, [C|Cs])
    end;
tokens2([], _Seps, Toks, Cs) ->
    reverse([reverse(Cs)|Toks]).

quick_fox() ->
    "the quick brown fox jumps over the lazy dog".

quick_money() ->
    "make quick money in the online casino".

sample_train(Classifier) ->
    Classifier2 = train(Classifier,"good","Nobody owns the water."),
    Classifier3 = train(Classifier2,"good","the quick rabbit jumps fences"),
    Classifier4 = train(Classifier3,"bad","buy pharmaceuticals now"),
    Classifier5 = train(Classifier4,"bad","make quick money at the online casino"),
    Classifier6 = train(Classifier5,"good","the quick brown fox jumps"),
    Classifier6.

fisher1_test() ->
    Dict = dict:new(),
    One = "one",
    Two = {Dict,1},
    Dict2 = dict:store("one", One, Dict),
    Dict3 = dict:store("two", Two, Dict2),
    {ok, Value} = dict:find("two", Dict3),
    {_,Count} = Value,
    ?assertEqual( 1, Count ),
    Three = {Dict, Count + 1},
    Dict4 = dict:store("three", Three, Dict3),
    {ok, Value2} = dict:find("three", Dict4),
    {_,Count2} = Value2,
    ?assertEqual( 2, Count2 ),
    Value.

fisher2_test() ->
    %{ok, Buffer} = file:read_file("fisher.erl"),
    Buffer = <<"the quick brown fox jumps over the lazy dog">>,
    WList = getwords(binary_to_list(Buffer)),
    ?assertEqual( 9, length(WList) ).

fisher3_test() ->
    Good = dict:new(),
    Bad = dict:new(),
    Good2 = trainCat(Good, quick_fox()),
    Bad2 = trainCat(Bad, quick_money()),
    ?assertEqual( 8, dict:size(Good2) ),
    ?assertEqual( 7, dict:size(Bad2) ),
    {ok, QuickValue} = dict:find("quick", Good2),
    ?assertEqual( 1, QuickValue ),
    {ok, TheValue} = dict:find("the", Good2),
    ?assertEqual( 2, TheValue ),
    Good2.

fisher4_test() ->
    Classifier = dict:new(),
    Classifier2 = train(Classifier,"good", quick_fox()),
    Classifier3 = train(Classifier2,"bad", quick_money()),
    ?assertEqual( 2, dict:size(Classifier3) ),
    {ok, {Dict,Count}} = dict:find("good", Classifier3),
    ?assertEqual( 1, Count ),
    ?assertEqual( 8, dict:size(Dict) ),
    Classifier3.

fisher5_test() ->
    Classifier = dict:new(),
    Classifier2 = sample_train(Classifier),
    Classifier2,
    GoodX = featureProb(Classifier2, "money", "goodX"),
    ?assertEqual( 0, GoodX ),
    QuickX = featureProb(Classifier2, "quickX", "good" ),
    ?assertEqual( 0, QuickX ),
    NotFound = featureProb(Classifier2, "money", "good" ),
    ?assertEqual( 0, NotFound ),
    NotFound.

fisher6_test() ->
    Classifier = dict:new(),
    Classifier2 = sample_train(Classifier),
    Classifier2,
    R = featureProb(Classifier2, "quick", "good"),
    R.

fisher7_test() ->
    Classifier = dict:new(),
    Classifier2 = sample_train(Classifier),
    T1 = featureTotal(Classifier2, "money"),
    ?assertEqual( 1, T1 ),
    T2 = featureTotal(Classifier2, "water"),
    ?assertEqual( 1, T2 ),
    T3 = featureTotal(Classifier2, "the"),
    ?assertEqual( 4, T3 ),
    T4 = featureTotal(Classifier2, "quick"),
    ?assertEqual( 3, T4 ),
    T5 = featureTotal(Classifier2, "quckX"),
    ?assertEqual( 0, T5 ),
    T5.

fisher8_test() ->
    Classifier = dict:new(),
    Classifier2 = sample_train(Classifier),
    Classifier3 = sample_train(Classifier2),
    FeatureProb = fun featureProb/3,
    R = weightedProb(Classifier3, "money", "good", FeatureProb, 1.0, 0.5),
    ?assert( 0.166666666666 < R ),
    ?assert( 0.166666666667 > R ),
    R.

fisher9_test() ->
    Classifier1 = sample_train(dict:new()),
    T1 = docTotal( Classifier1 ),
    ?assertEqual( 5, T1 ),
    C1 = catTotal( Classifier1, "good" ),
    ?assertEqual( 3, C1 ),
    Classifier2 = sample_train(Classifier1),
    T2 = docTotal( Classifier2 ),
    ?assertEqual( 10, T2 ),
    C2 = catTotal( Classifier2, "good" ),
    ?assertEqual( 6, C2 ),
    C3 = catTotal( Classifier2, "bad" ),
    ?assertEqual( 4, C3 ).

fisher10_test() ->
    Classifier1 = sample_train(dict:new()),
    P1 = bayesProb( Classifier1, "quick rabbit", "good" ),
    ?assert( 0.156249 < P1 ),
    ?assert( 0.15625 > P1 ),
    P2 = bayesProb( Classifier1, "quick rabbit", "bad" ),
    ?assert( 0.04 < P2 ),
    ?assert( 0.06 > P2 ).

fisher11_test() ->
    Classifier1 = sample_train(dict:new()),
    _P1 = bayesProb( Classifier1, "quick rabbit", "good" ),
    _L1 = catList( Classifier1 ),
    C1 = classify( Classifier1, "quick rabbit", 3),
    C1.

fisher12_test() ->
    Classifier1 = sample_train(dict:new()),
    P1 = cProb( Classifier1, "quick", "good" ),
    ?assert( 0.57142857142857 < P1 ),
    ?assert( 0.571428571428572 > P1 ),
    P2 = cProb( Classifier1, "money", "bad" ),
    ?assertEqual( 1.0, P2 ),
    FeatureProb = fun cProb/3,
    P3 = probDocCat( Classifier1, "money", "bad", FeatureProb),
    P3.

fisher13_test() ->
    R = inverseChiSquared( 1.75, 4 ),
    ?assert( 0.781616 < R ),
    ?assert( 0.7816163 > R ),
    R.

fisher14_test() ->
    Classifier1 = sample_train(dict:new()),
    P1 = fisherProb( Classifier1, "quick rabbit", "good" ),
    ?assert( 0.7801398658895 < P1 ),
    ?assert( 0.7801398658896 > P1 ),
    P2 = fisherProb( Classifier1, "quick rabbit", "bad" ),
    ?assert( 0.3563359628333525 < P2 ),
    ?assert( 0.3563359628333526 > P2 ),
    P2. 
