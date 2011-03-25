-module(json).
-export([encode/1, decode/1, fuzz/0, fuzz/1]).
-on_load(init/0).

init() ->
    SoName = case code:priv_dir(json) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, json]);
                _ ->
                    filename:join([priv, json])
            end;
        Dir ->
            filename:join(Dir, json)
    end,
    erlang:load_nif(SoName, 0).

decode(IoList) ->
    case reverse_tokens(IoList) of
    {ok, ReverseTokens} ->
        [[EJson]] = make_ejson(ReverseTokens, [[]]),
        EJson;
    Error ->
        Error
    end.



encode(true) ->
    <<"true">>;
encode(false) ->
    <<"false">>;
encode(null) ->
    <<"null">>;
encode(I) when is_integer(I) ->
    integer_to_list(I);
encode(F) when is_float(F) ->
    encode_double(F);
encode(S) when is_binary(S); is_atom(S) ->
    encode_string(S);
encode({Props}) when is_list(Props) ->
    encode_proplist(Props);
encode(Array) when is_list(Array) ->
    encode_array(Array);
encode(Bad) ->
    exit({encode, {bad_term, Bad}}).

encode_array([]) ->
    <<"[]">>;
encode_array(L) ->
    F = fun (O, Acc) ->
                [$,, encode(O) | Acc]
        end,
    [$, | Acc1] = lists:foldl(F, "[", L),
    lists:reverse([$\] | Acc1]).

encode_proplist([]) ->
    <<"{}">>;
encode_proplist(Props) ->
    F = fun ({K, V}, Acc) ->
                KS = encode_string(K),
                VS = encode(V),
                [$,, VS, $:, KS | Acc]
        end,
    [$, | Acc1] = lists:foldl(F, "{", Props),
    lists:reverse([$\} | Acc1]).


encode_string(_) ->
    not_loaded(?LINE).


encode_double(_) ->
    not_loaded(?LINE).


make_ejson([], Stack) ->
    Stack;
make_ejson([0 | RevEvs], [ArrayValues, PrevValues | RestStack]) ->
    % 0 ArrayStart
    make_ejson(RevEvs, [[ArrayValues | PrevValues] | RestStack]);
make_ejson([1 | RevEvs], Stack) ->
    % 1 ArrayEnd
    make_ejson(RevEvs, [[] | Stack]);
make_ejson([2 | RevEvs], [ObjValues, PrevValues | RestStack]) ->
    % 2 ObjectStart
    make_ejson(RevEvs, [[{ObjValues} | PrevValues] | RestStack]);
make_ejson([3 | RevEvs], Stack) ->
    % 3 ObjectEnd
    make_ejson(RevEvs, [[] | Stack]);
make_ejson([{1, Value} | RevEvs], [Vals | RestStack] = _Stack) ->
    % {1 , IntegerString}
    make_ejson(RevEvs, [[list_to_integer(Value) | Vals] | RestStack]);
make_ejson([{2, Value} | RevEvs], [Vals | RestStack] = _Stack) ->
    % {2, FloatString}
    make_ejson(RevEvs, [[list_to_float(Value) | Vals] | RestStack]);
make_ejson([{3, String} | RevEvs], [[PrevValue|RestObject] | RestStack] = _Stack) ->
    % {3 , ObjectKey}
    make_ejson(RevEvs, [[{String, PrevValue}|RestObject] | RestStack]);
make_ejson([Value | RevEvs], [Vals | RestStack] = _Stack) ->
    make_ejson(RevEvs, [[Value | Vals] | RestStack]).

fuzz() ->
    json_fuzz:fuzz(fun json_fuzz:choose/4).
fuzz(Chooser) ->
    json_fuzz:fuzz(Chooser).

not_loaded(Line) ->
    exit({json_not_loaded, module, ?MODULE, line, Line}).
    
reverse_tokens(_) ->
    not_loaded(?LINE).
