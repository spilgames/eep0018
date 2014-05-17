-module(json).
-export([encode/1, decode/1, fuzz/0, fuzz/1]).
-on_load(init/0).


%% specs borrowed from https://github.com/hio/erlang-json
%% Do note that Dialyzer is unable to verify these specs
%% because key functions are implemented in a NIF which
%% is signalled to Dialyzer with nif_error; this has the
%% effect that Dialyzer accepts (but doesnt check) the
%% supplied specs.
-export_type([value/0]).

-type value() :: json_primary()
               | json_object_t([{key(), value()}])
               | json_array_t(value()) .
%% any type of json value.

-type json_string() :: unicode:unicode_binary().
%% json string value is represented erlang binary term.

-type json_number() :: integer() | float().
%% json number value is represented erlang integer or float term.
-type json_primary() :: json_string()
                      | json_number()
                      | boolean()
                      | null .
%% non-structured values.

-type key() :: json_string() | atom().
%% key of object value. key() is mostly binary(), but it may be in atom() depended on key_decode option.

-type json_object_t(Pairs) :: {Pairs}.
%% json object value is represented in erlang tuple which contains single proplist style value.

%%-type json_object() :: json_object_t([{key(),value()}]).
%% json object.

-type json_array_t(T) :: [T].
%% json array value is represented in erlang array term.

%%-type json_array() :: json_array_t(value()).
%% json array value is represented in erlang array term.

-type text() :: unicode:chardata().
%% Json encoded text represented in erlang chardata term.
%% @see binary_text()

-type binary_text() :: unicode:unicode_binary().
%% Json encoded text represented in erlang binary term.
%% @see text()

-type decode_error() :: term().
%% reason for decode() error.

-type encode_error() :: term().
%% reason for encode() error.
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

-spec decode(text()) -> {ok, value()} | {error, decode_error()}.
decode(IoList) ->
    case reverse_tokens(IoList) of
    {ok, ReverseTokens} ->
        [[EJson]] = make_ejson(ReverseTokens, [[]]),
        {ok, EJson};
    Error ->
        Error
    end.

-spec encode(value()) -> {ok, binary_text()} | {error, encode_error()}.
encode(EJson) ->
    try
        RevList = encode_rev(EJson),
        {ok, final_encode(lists:reverse(lists:flatten([RevList])))}
    catch throw:Error ->
        Error
    end.

% Encode the json into a reverse list that's almost an iolist
% everything in the list is the final output except for tuples with
% {0, Strings} and {1, Floats}, which are to be converted to strings
% inside the NIF.
encode_rev(true) ->
    <<"true">>;
encode_rev(false) ->
    <<"false">>;
encode_rev(null) ->
    <<"null">>;
encode_rev(I) when is_integer(I) ->
    list_to_binary(integer_to_list(I));
encode_rev(S) when is_binary(S) ->
    {0, S};
encode_rev(S) when is_atom(S) ->
    {0, list_to_binary(atom_to_list(S))};
encode_rev(F) when is_float(F) ->
    {1, F};
encode_rev({Props}) when is_list(Props) ->
    encode_proplist_rev(Props, [<<"{">>]);
encode_rev(Array) when is_list(Array) ->
    encode_array_rev(Array, [<<"[">>]);
encode_rev(Bad) ->
    throw({encode, {bad_term, Bad}}).


encode_array_rev([], Acc) ->
    [<<"]">> | Acc];
encode_array_rev([Val | Rest], [<<"[">>]) ->
    encode_array_rev(Rest, [encode_rev(Val), <<"[">>]);
encode_array_rev([Val | Rest], Acc) ->
    encode_array_rev(Rest, [encode_rev(Val), <<",">> | Acc]).


encode_proplist_rev([], Acc) ->
    [<<"}">> | Acc];
encode_proplist_rev([{Key,Val} | Rest], [<<"{">>]) ->
    encode_proplist_rev(
        Rest, [encode_rev(Val), <<":">>, {0, as_binary(Key)}, <<"{">>]);
encode_proplist_rev([{Key,Val} | Rest], Acc) ->
    encode_proplist_rev(
        Rest, [encode_rev(Val), <<":">>, {0, as_binary(Key)}, <<",">> | Acc]).

as_binary(B) when is_binary(B) ->
    B;
as_binary(B) when is_list(B) ->
    list_to_binary(B);
as_binary(A) when is_atom(A) ->
    list_to_binary(atom_to_list(A)).




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
make_ejson([{0, Value} | RevEvs], [Vals | RestStack] = _Stack) ->
    % {0, IntegerString}
    make_ejson(RevEvs, [[list_to_integer(binary_to_list(Value)) | Vals] | RestStack]);
make_ejson([{1, Value} | RevEvs], [Vals | RestStack] = _Stack) ->
    % {1, FloatString}
    make_ejson(RevEvs, [[list_to_float(binary_to_list(Value)) | Vals] | RestStack]);
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
    % nif_error ensures that Dialyzer doesn't report "no local return" errors
    erlang:nif_error({json_not_loaded, module, ?MODULE, line, Line}).

reverse_tokens(_) ->
    not_loaded(?LINE).

final_encode(_) ->
    not_loaded(?LINE).
