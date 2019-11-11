-module(parser_erlang).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("src/parser_erlang.yrl", 105).

token_value({_, Val}) -> Val;
token_value({_, _, Val}) -> Val.
token_line({_, Line}) -> Line;
token_line({_, Line, _}) -> Line.



program_line([], [Stm | _]) -> ast_line(Stm);
program_line([D|_], _Stms) -> ast_line(D).

ast_line({_, _, Line, _}) -> Line.

-file("/usr/lib64/erlang/lib/parsetools-2.1.8/include/yeccpre.hrl", 0).
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2018. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%

-type yecc_ret() :: {'error', _} | {'ok', _}.

-spec parse(Tokens :: list()) -> yecc_ret().
parse(Tokens) ->
    yeccpars0(Tokens, {no_func, no_line}, 0, [], []).

-spec parse_and_scan({function() | {atom(), atom()}, [_]}
                     | {atom(), atom(), [_]}) -> yecc_ret().
parse_and_scan({F, A}) ->
    yeccpars0([], {{F, A}, no_line}, 0, [], []);
parse_and_scan({M, F, A}) ->
    Arity = length(A),
    yeccpars0([], {{fun M:F/Arity, A}, no_line}, 0, [], []).

-spec format_error(any()) -> [char() | list()].
format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true ->
            Message;
        _ ->
            io_lib:write(Message)
    end.

%% To be used in grammar files to throw an error message to the parser
%% toplevel. Doesn't have to be exported!
-compile({nowarn_unused_function, return_error/2}).
-spec return_error(integer(), any()) -> no_return().
return_error(Line, Message) ->
    throw({error, {Line, ?MODULE, Message}}).

-define(CODE_VERSION, "1.4").

yeccpars0(Tokens, Tzr, State, States, Vstack) ->
    try yeccpars1(Tokens, Tzr, State, States, Vstack)
    catch 
        error: Error: Stacktrace ->
            try yecc_error_type(Error, Stacktrace) of
                Desc ->
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                 Stacktrace)
            catch _:_ -> erlang:raise(error, Error, Stacktrace)
            end;
        %% Probably thrown from return_error/2:
        throw: {error, {_Line, ?MODULE, _M}} = Error ->
            Error
    end.

yecc_error_type(function_clause, [{?MODULE,F,ArityOrArgs,_} | _]) ->
    case atom_to_list(F) of
        "yeccgoto_" ++ SymbolL ->
            {ok,[{atom,_,Symbol}],_} = erl_scan:string(SymbolL),
            State = case ArityOrArgs of
                        [S,_,_,_,_,_,_] -> S;
                        _ -> state_is_unknown
                    end,
            {Symbol, State, missing_in_goto_table}
    end.

yeccpars1([Token | Tokens], Tzr, State, States, Vstack) ->
    yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens, Tzr);
yeccpars1([], {{F, A},_Line}, State, States, Vstack) ->
    case apply(F, A) of
        {ok, Tokens, Endline} ->
            yeccpars1(Tokens, {{F, A}, Endline}, State, States, Vstack);
        {eof, Endline} ->
            yeccpars1([], {no_func, Endline}, State, States, Vstack);
        {error, Descriptor, _Endline} ->
            {error, Descriptor}
    end;
yeccpars1([], {no_func, no_line}, State, States, Vstack) ->
    Line = 999999,
    yeccpars2(State, '$end', States, Vstack, yecc_end(Line), [],
              {no_func, Line});
yeccpars1([], {no_func, Endline}, State, States, Vstack) ->
    yeccpars2(State, '$end', States, Vstack, yecc_end(Endline), [],
              {no_func, Endline}).

%% yeccpars1/7 is called from generated code.
%%
%% When using the {includefile, Includefile} option, make sure that
%% yeccpars1/7 can be found by parsing the file without following
%% include directives. yecc will otherwise assume that an old
%% yeccpre.hrl is included (one which defines yeccpars1/5).
yeccpars1(State1, State, States, Vstack, Token0, [Token | Tokens], Tzr) ->
    yeccpars2(State, element(1, Token), [State1 | States],
              [Token0 | Vstack], Token, Tokens, Tzr);
yeccpars1(State1, State, States, Vstack, Token0, [], {{_F,_A}, _Line}=Tzr) ->
    yeccpars1([], Tzr, State, [State1 | States], [Token0 | Vstack]);
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, no_line}) ->
    Line = yecctoken_end_location(Token0),
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line});
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, Line}) ->
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line}).

%% For internal use only.
yecc_end({Line,_Column}) ->
    {'$end', Line};
yecc_end(Line) ->
    {'$end', Line}.

yecctoken_end_location(Token) ->
    try erl_anno:end_location(element(2, Token)) of
        undefined -> yecctoken_location(Token);
        Loc -> Loc
    catch _:_ -> yecctoken_location(Token)
    end.

-compile({nowarn_unused_function, yeccerror/1}).
yeccerror(Token) ->
    Text = yecctoken_to_string(Token),
    Location = yecctoken_location(Token),
    {error, {Location, ?MODULE, ["syntax error before: ", Text]}}.

-compile({nowarn_unused_function, yecctoken_to_string/1}).
yecctoken_to_string(Token) ->
    try erl_scan:text(Token) of
        undefined -> yecctoken2string(Token);
        Txt -> Txt
    catch _:_ -> yecctoken2string(Token)
    end.

yecctoken_location(Token) ->
    try erl_scan:location(Token)
    catch _:_ -> element(2, Token)
    end.

-compile({nowarn_unused_function, yecctoken2string/1}).
yecctoken2string({atom, _, A}) -> io_lib:write_atom(A);
yecctoken2string({integer,_,N}) -> io_lib:write(N);
yecctoken2string({float,_,F}) -> io_lib:write(F);
yecctoken2string({char,_,C}) -> io_lib:write_char(C);
yecctoken2string({var,_,V}) -> io_lib:format("~s", [V]);
yecctoken2string({string,_,S}) -> io_lib:write_string(S);
yecctoken2string({reserved_symbol, _, A}) -> io_lib:write(A);
yecctoken2string({_Cat, _, Val}) -> io_lib:format("~tp", [Val]);
yecctoken2string({dot, _}) -> "'.'";
yecctoken2string({'$end', _}) -> [];
yecctoken2string({Other, _}) when is_atom(Other) ->
    io_lib:write_atom(Other);
yecctoken2string(Other) ->
    io_lib:format("~tp", [Other]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



-file("src/parser_erlang.erl", 189).

-dialyzer({nowarn_function, yeccpars2/7}).
yeccpars2(0=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(1=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_1(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(2=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(3=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_3(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(4=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(5=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(6=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(7=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(8=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(9=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(10=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(11=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(12=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_12(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(13=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(14=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(15=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(16=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_16(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(17=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(18=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(19=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(20=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(21=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(22=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_22(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(23=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(24=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_24(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(25=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(26=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(27=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(28=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_28(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(29=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(30=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_30(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(31=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_31(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(32=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(33=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(34=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(35=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(36=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(37=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(38=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(39=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(40=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(41=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(42=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(43=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(44=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(45=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(46=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_46(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(47=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(48=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(49=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(50=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(51=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(52=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(53=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(54=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(55=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(56=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(57=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_57(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(58=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_58(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(59=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(60=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(61=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_61(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(62=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_62(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(63=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_63(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(64=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_64(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(65=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_65(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(66=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(67=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_67(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(68=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(69=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_69(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(70=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_70(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(71=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_71(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(72=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(73=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_73(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(74=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_74(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(75=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(76=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_76(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(77=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(78=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_78(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(79=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(80=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_80(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(81=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(82=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(83=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_83(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(84=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_84(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(85=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(86=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(87=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_87(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(88=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(89=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(90=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(91=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(92=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_92(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(93=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_93(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(94=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_94(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(95=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(96=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(97=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(98=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_98(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(99=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_99(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(100=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_100(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(101=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(102=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_102(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(103=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_103(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(104=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_104(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(105=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_102(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(106=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_106(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(107=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_107(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(108=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(109=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_109(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(110=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(111=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_111(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(112=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(113=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_113(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(114=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_114(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.4",{missing_state_in_action_table, Other}}).

yeccpars2_0(S, function, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 4, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_0_(Stack),
 yeccpars2_2(2, Cat, [0 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_1/7}).
yeccpars2_1(_S, '$end', _Ss, Stack, _T, _Ts, _Tzr) ->
 {ok, hd(Stack)};
yeccpars2_1(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_2/7}).
yeccpars2_2(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'if', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, skip, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, while, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_3(S, function, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 4, Ss, Stack, T, Ts, Tzr);
yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_3_(Stack),
 yeccpars2_113(_S, Cat, [3 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_4/7}).
yeccpars2_4(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 5, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_5/7}).
yeccpars2_5(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 6, Ss, Stack, T, Ts, Tzr);
yeccpars2_5(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_6(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_6_(Stack),
 yeccpars2_7(7, Cat, [6 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_7/7}).
yeccpars2_7(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_8(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_8_(Stack),
 yeccgoto_non_empty_param_decls(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_param_decls(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_10(S, '::', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_10_(Stack),
 yeccgoto_param_decl(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_11/7}).
yeccpars2_11(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(S, bool, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(S, int, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_12_(Stack),
 yeccgoto_param_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_13: see yeccpars2_11

yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_14_(Stack),
 yeccgoto_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_15_(Stack),
 yeccgoto_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_16/7}).
yeccpars2_16(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_17: see yeccpars2_11

yeccpars2_18(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_18_(Stack),
 yeccgoto_non_empty_types(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_19/7}).
yeccpars2_19(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_19(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_20_(Stack),
 yeccgoto_type(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_21: see yeccpars2_11

yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_22_(Stack),
 yeccgoto_non_empty_types(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_23/7}).
yeccpars2_23(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_24_(Stack),
 yeccgoto_non_empty_param_decls(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_25/7}).
yeccpars2_25(S, ret, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_26/7}).
yeccpars2_26(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_26(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_27: see yeccpars2_23

-dialyzer({nowarn_function, yeccpars2_28/7}).
yeccpars2_28(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_28(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_29: see yeccpars2_2

-dialyzer({nowarn_function, yeccpars2_30/7}).
yeccpars2_30(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_30(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_31(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 110, Ss, Stack, T, Ts, Tzr);
yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_31_(Stack),
 yeccgoto_stm_seq(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_32/7}).
yeccpars2_32(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_33(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 88, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_33_(Stack),
 yeccpars2_2(86, Cat, [33 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_34/7}).
yeccpars2_34(S, ':=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_34(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_35(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_35(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_35/7}).
yeccpars2_cont_35(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_35(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_35(S, false, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_35(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_35(S, true, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_35(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_36_(Stack),
 yeccgoto_stm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_37: see yeccpars2_35

yeccpars2_38(S, do, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_38(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_38/7}).
yeccpars2_cont_38(S, '&&', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_38(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_38(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_38(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_38(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_38(S, '==', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_38(S, '?', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_38(S, '||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_38(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_39: see yeccpars2_35

-dialyzer({nowarn_function, yeccpars2_40/7}).
yeccpars2_40(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_40(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_41_(Stack),
 yeccgoto_exp(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_42_(Stack),
 yeccgoto_exp(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_43_(Stack),
 yeccgoto_exp(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_44_(Stack),
 yeccgoto_exp(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_45_(Stack),
 yeccgoto_exp(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_46(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_38(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_47: see yeccpars2_35

yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_48_(Stack),
 yeccgoto_exp(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_49: see yeccpars2_35

%% yeccpars2_50: see yeccpars2_35

%% yeccpars2_51: see yeccpars2_35

%% yeccpars2_52: see yeccpars2_35

%% yeccpars2_53: see yeccpars2_35

%% yeccpars2_54: see yeccpars2_35

%% yeccpars2_55: see yeccpars2_35

%% yeccpars2_56: see yeccpars2_35

yeccpars2_57(S, '&&', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, '==', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, '||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_57_(Stack),
 yeccgoto_exp(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_58(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_58(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_38(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_59: see yeccpars2_35

yeccpars2_60(S, '&&', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(S, '==', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(S, '||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = 'yeccpars2_60_\'$end\''(Stack),
 yeccgoto_exp(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_60(_S, ')', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = 'yeccpars2_60_\')\''(Stack),
 yeccgoto_exp(hd(Nss), ')', Nss, NewStack, T, Ts, Tzr);
yeccpars2_60(_S, ',', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = 'yeccpars2_60_\',\''(Stack),
 yeccgoto_exp(hd(Nss), ',', Nss, NewStack, T, Ts, Tzr);
yeccpars2_60(_S, ':', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = 'yeccpars2_60_\':\''(Stack),
 yeccgoto_exp(hd(Nss), ':', Nss, NewStack, T, Ts, Tzr);
yeccpars2_60(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = 'yeccpars2_60_\';\''(Stack),
 yeccgoto_exp(hd(Nss), ';', Nss, NewStack, T, Ts, Tzr);
yeccpars2_60(_S, do, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_60_do(Stack),
 yeccgoto_exp(hd(Nss), do, Nss, NewStack, T, Ts, Tzr);
yeccpars2_60(_S, else, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_60_else(Stack),
 yeccgoto_exp(hd(Nss), else, Nss, NewStack, T, Ts, Tzr);
yeccpars2_60(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = 'yeccpars2_60_\'end\''(Stack),
 yeccgoto_exp(hd(Nss), 'end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_60(_S, then, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_60_then(Stack),
 yeccgoto_exp(hd(Nss), then, Nss, NewStack, T, Ts, Tzr);
yeccpars2_60(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_61(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_61_\'$end\''(Stack),
 yeccgoto_exp(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_61(_S, '&&', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_61_\'&&\''(Stack),
 yeccgoto_exp(hd(Nss), '&&', Nss, NewStack, T, Ts, Tzr);
yeccpars2_61(_S, ')', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_61_\')\''(Stack),
 yeccgoto_exp(hd(Nss), ')', Nss, NewStack, T, Ts, Tzr);
yeccpars2_61(_S, ',', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_61_\',\''(Stack),
 yeccgoto_exp(hd(Nss), ',', Nss, NewStack, T, Ts, Tzr);
yeccpars2_61(_S, ':', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_61_\':\''(Stack),
 yeccgoto_exp(hd(Nss), ':', Nss, NewStack, T, Ts, Tzr);
yeccpars2_61(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_61_\';\''(Stack),
 yeccgoto_exp(hd(Nss), ';', Nss, NewStack, T, Ts, Tzr);
yeccpars2_61(_S, '?', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_61_\'?\''(Stack),
 yeccgoto_exp(hd(Nss), '?', Nss, NewStack, T, Ts, Tzr);
yeccpars2_61(_S, do, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_61_do(Stack),
 yeccgoto_exp(hd(Nss), do, Nss, NewStack, T, Ts, Tzr);
yeccpars2_61(_S, else, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_61_else(Stack),
 yeccgoto_exp(hd(Nss), else, Nss, NewStack, T, Ts, Tzr);
yeccpars2_61(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_61_\'end\''(Stack),
 yeccgoto_exp(hd(Nss), 'end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_61(_S, then, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_61_then(Stack),
 yeccgoto_exp(hd(Nss), then, Nss, NewStack, T, Ts, Tzr);
yeccpars2_61(_S, '||', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_61_\'||\''(Stack),
 yeccgoto_exp(hd(Nss), '||', Nss, NewStack, T, Ts, Tzr);
yeccpars2_61(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_62(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_62_\'$end\''(Stack),
 yeccgoto_exp(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_62(_S, '&&', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_62_\'&&\''(Stack),
 yeccgoto_exp(hd(Nss), '&&', Nss, NewStack, T, Ts, Tzr);
yeccpars2_62(_S, ')', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_62_\')\''(Stack),
 yeccgoto_exp(hd(Nss), ')', Nss, NewStack, T, Ts, Tzr);
yeccpars2_62(_S, ',', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_62_\',\''(Stack),
 yeccgoto_exp(hd(Nss), ',', Nss, NewStack, T, Ts, Tzr);
yeccpars2_62(_S, ':', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_62_\':\''(Stack),
 yeccgoto_exp(hd(Nss), ':', Nss, NewStack, T, Ts, Tzr);
yeccpars2_62(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_62_\';\''(Stack),
 yeccgoto_exp(hd(Nss), ';', Nss, NewStack, T, Ts, Tzr);
yeccpars2_62(_S, '?', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_62_\'?\''(Stack),
 yeccgoto_exp(hd(Nss), '?', Nss, NewStack, T, Ts, Tzr);
yeccpars2_62(_S, do, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_62_do(Stack),
 yeccgoto_exp(hd(Nss), do, Nss, NewStack, T, Ts, Tzr);
yeccpars2_62(_S, else, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_62_else(Stack),
 yeccgoto_exp(hd(Nss), else, Nss, NewStack, T, Ts, Tzr);
yeccpars2_62(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_62_\'end\''(Stack),
 yeccgoto_exp(hd(Nss), 'end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_62(_S, then, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_62_then(Stack),
 yeccgoto_exp(hd(Nss), then, Nss, NewStack, T, Ts, Tzr);
yeccpars2_62(_S, '||', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_62_\'||\''(Stack),
 yeccgoto_exp(hd(Nss), '||', Nss, NewStack, T, Ts, Tzr);
yeccpars2_62(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_63(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_63_(Stack),
 yeccgoto_exp(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_64/7}).
yeccpars2_64(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_65(S, '&&', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_65(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_65(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_65(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_65(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_65(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_65(S, '==', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_65(S, '?', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_65(S, '||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_65_(Stack),
 yeccgoto_non_empty_exps(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_66: see yeccpars2_35

yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_67_(Stack),
 yeccgoto_non_empty_exps(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_68_(Stack),
 yeccgoto_exp(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_69(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_69_(Stack),
 yeccgoto_exp(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_70_(Stack),
 yeccgoto_exp(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_71(S, '&&', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(S, '==', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_71_(Stack),
 yeccgoto_exp(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_72: see yeccpars2_2

yeccpars2_73(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_73_(Stack),
 yeccgoto_stm(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_74(S, then, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_74(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_38(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_75: see yeccpars2_2

-dialyzer({nowarn_function, yeccpars2_76/7}).
yeccpars2_76(S, else, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_77: see yeccpars2_2

yeccpars2_78(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_78_(Stack),
 yeccgoto_stm(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_79(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_35(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_80(S, '&&', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(S, '==', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(S, '?', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(S, '||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_80_(Stack),
 yeccgoto_stm(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_81(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_81_(Stack),
 yeccgoto_exp(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_82(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_82(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_82(S, false, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_82(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_82(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_82(S, true, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_82_(Stack),
 yeccpars2_84(84, Cat, [82 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_exps(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_84/7}).
yeccpars2_84(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_84(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_85_(Stack),
 yeccgoto_stm(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_86: see yeccpars2_2

yeccpars2_87(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 88, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_87_(Stack),
 yeccpars2_98(_S, Cat, [87 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_88/7}).
yeccpars2_88(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 89, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_89/7}).
yeccpars2_89(S, '::', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 90, Ss, Stack, T, Ts, Tzr);
yeccpars2_89(S, ':=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 91, Ss, Stack, T, Ts, Tzr);
yeccpars2_89(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_90: see yeccpars2_11

%% yeccpars2_91: see yeccpars2_35

yeccpars2_92(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_38(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_93(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_93_(Stack),
 yeccgoto_var_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_94/7}).
yeccpars2_94(S, ':=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 95, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_95: see yeccpars2_35

yeccpars2_96(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 97, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_38(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_97(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_97_(Stack),
 yeccgoto_var_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_98(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_98_(Stack),
 yeccgoto_var_decls(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_99/7}).
yeccpars2_99(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_100(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_100_(Stack),
 yeccgoto_stm(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_101/7}).
yeccpars2_101(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_102/7}).
yeccpars2_102(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 104, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_103/7}).
yeccpars2_103(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_104(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 105, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_104_(Stack),
 yeccgoto_non_empty_ids(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_105: see yeccpars2_102

yeccpars2_106(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_106_(Stack),
 yeccgoto_non_empty_ids(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_107/7}).
yeccpars2_107(S, ':=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 108, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_108: see yeccpars2_35

yeccpars2_109(S, '&&', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(S, '==', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(S, '?', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(S, '||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_109_(Stack),
 yeccgoto_stm(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_110(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(S, 'if', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(S, skip, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(S, while, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_110_(Stack),
 yeccgoto_stm_seq(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_111_(Stack),
 yeccgoto_stm_seq(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_112(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_112_(Stack),
 yeccgoto_fun_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_113_(Stack),
 yeccgoto_fun_decls(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_114_(Stack),
 yeccgoto_program(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_exp/7}).
yeccgoto_exp(35, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(74, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(37, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(38, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(39, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(47, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(71, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(49=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(50, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(69, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(51, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(65, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(52, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(53, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(62, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(54, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(55, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(58, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(56, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(57, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(59, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(66, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(65, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(79, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(80, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(82, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(65, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(91, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(92, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(95, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(108, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(109, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_exps/7}).
yeccgoto_exps(82, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(84, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_fun_decl/7}).
yeccgoto_fun_decl(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(3, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_decl(3, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(3, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_fun_decls/7}).
yeccgoto_fun_decls(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_decls(3=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_non_empty_exps/7}).
yeccgoto_non_empty_exps(51, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(64, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_non_empty_exps(66=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_non_empty_exps(82=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_non_empty_ids/7}).
yeccgoto_non_empty_ids(102, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_103(103, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_non_empty_ids(105=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_106(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_non_empty_param_decls/7}).
yeccgoto_non_empty_param_decls(6=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_non_empty_param_decls(23=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_non_empty_types/7}).
yeccgoto_non_empty_types(17, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_non_empty_types(21=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_param_decl/7}).
yeccgoto_param_decl(6, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_param_decl(23, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_param_decl(27, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_param_decls/7}).
yeccgoto_param_decls(6, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_program/7}).
yeccgoto_program(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_stm/7}).
yeccgoto_stm(2, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(31, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_stm(29, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(31, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_stm(72, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(31, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_stm(75, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(31, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_stm(77, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(31, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_stm(86, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(31, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_stm(110, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(31, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_stm_seq/7}).
yeccgoto_stm_seq(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_stm_seq(29, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(30, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_stm_seq(72=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_stm_seq(75, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_stm_seq(77=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_stm_seq(86, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_99(99, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_stm_seq(110=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type/7}).
yeccgoto_type(11=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(13, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(16, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(17, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(18, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(21, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(18, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(90, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(94, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_var_decl/7}).
yeccgoto_var_decl(33, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(87, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var_decl(87, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(87, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_var_decls/7}).
yeccgoto_var_decls(33, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(86, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var_decls(87=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_98(_S, Cat, Ss, Stack, T, Ts, Tzr).

-compile({inline,yeccpars2_0_/1}).
-file("src/parser_erlang.yrl", 53).
yeccpars2_0_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_3_/1}).
-file("src/parser_erlang.yrl", 53).
yeccpars2_3_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_6_/1}).
-file("src/parser_erlang.yrl", 73).
yeccpars2_6_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_8_/1}).
-file("src/parser_erlang.yrl", 75).
yeccpars2_8_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_10_/1}).
-file("src/parser_erlang.yrl", 80).
yeccpars2_10_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ { declaration , param_decl , token_line ( __1 ) , [ { variable , token_value ( __1 ) } ] } ]
  end | __Stack].

-compile({inline,yeccpars2_12_/1}).
-file("src/parser_erlang.yrl", 82).
yeccpars2_12_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ { declaration , param_decl , token_line ( __1 ) , [ { variable , token_value ( __1 ) } , { type , __3 } ] } ]
  end | __Stack].

-compile({inline,yeccpars2_14_/1}).
-file("src/parser_erlang.yrl", 94).
yeccpars2_14_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { type , bool , token_line ( __1 ) , [ ] }
  end | __Stack].

-compile({inline,yeccpars2_15_/1}).
-file("src/parser_erlang.yrl", 93).
yeccpars2_15_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { type , int , token_line ( __1 ) , [ ] }
  end | __Stack].

-compile({inline,yeccpars2_18_/1}).
-file("src/parser_erlang.yrl", 98).
yeccpars2_18_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_20_/1}).
-file("src/parser_erlang.yrl", 95).
yeccpars2_20_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { type , tuple , token_line ( __1 ) , [ __2 | __4 ] }
  end | __Stack].

-compile({inline,yeccpars2_22_/1}).
-file("src/parser_erlang.yrl", 99).
yeccpars2_22_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_24_/1}).
-file("src/parser_erlang.yrl", 76).
yeccpars2_24_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_31_/1}).
-file("src/parser_erlang.yrl", 59).
yeccpars2_31_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_33_/1}).
-file("src/parser_erlang.yrl", 63).
yeccpars2_33_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_36_/1}).
-file("src/parser_erlang.yrl", 35).
yeccpars2_36_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { stm , skip , token_line ( __1 ) , [ ] }
  end | __Stack].

-compile({inline,yeccpars2_41_/1}).
-file("src/parser_erlang.yrl", 19).
yeccpars2_41_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { exp , literal , token_line ( __1 ) , [ { boolean , false } ] }
  end | __Stack].

-compile({inline,yeccpars2_42_/1}).
-file("src/parser_erlang.yrl", 20).
yeccpars2_42_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { exp , variable , token_line ( __1 ) , [ { name , token_value ( __1 ) } ] }
  end | __Stack].

-compile({inline,yeccpars2_43_/1}).
-file("src/parser_erlang.yrl", 17).
yeccpars2_43_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { exp , literal , token_line ( __1 ) , [ { number , token_value ( __1 ) } ] }
  end | __Stack].

-compile({inline,yeccpars2_44_/1}).
-file("src/parser_erlang.yrl", 18).
yeccpars2_44_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { exp , literal , token_line ( __1 ) , [ { boolean , true } ] }
  end | __Stack].

-compile({inline,yeccpars2_45_/1}).
-file("src/parser_erlang.yrl", 21).
yeccpars2_45_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , literal , token_line ( __1 ) , [ { number , - token_value ( __2 ) } ] }
  end | __Stack].

-compile({inline,yeccpars2_48_/1}).
-file("src/parser_erlang.yrl", 31).
yeccpars2_48_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_57_/1}).
-file("src/parser_erlang.yrl", 28).
yeccpars2_57_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , 'or' , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_60_\'$end\''/1}).
-file("src/parser_erlang.yrl", 30).
'yeccpars2_60_\'$end\''(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , conditional , ast_line ( __1 ) , [ { condition , __1 } , { 'if' , __3 } , { else , __5 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_60_\')\''/1}).
-file("src/parser_erlang.yrl", 30).
'yeccpars2_60_\')\''(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , conditional , ast_line ( __1 ) , [ { condition , __1 } , { 'if' , __3 } , { else , __5 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_60_\',\''/1}).
-file("src/parser_erlang.yrl", 30).
'yeccpars2_60_\',\''(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , conditional , ast_line ( __1 ) , [ { condition , __1 } , { 'if' , __3 } , { else , __5 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_60_\':\''/1}).
-file("src/parser_erlang.yrl", 30).
'yeccpars2_60_\':\''(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , conditional , ast_line ( __1 ) , [ { condition , __1 } , { 'if' , __3 } , { else , __5 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_60_\';\''/1}).
-file("src/parser_erlang.yrl", 30).
'yeccpars2_60_\';\''(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , conditional , ast_line ( __1 ) , [ { condition , __1 } , { 'if' , __3 } , { else , __5 } ] }
  end | __Stack].

-compile({inline,yeccpars2_60_do/1}).
-file("src/parser_erlang.yrl", 30).
yeccpars2_60_do(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , conditional , ast_line ( __1 ) , [ { condition , __1 } , { 'if' , __3 } , { else , __5 } ] }
  end | __Stack].

-compile({inline,yeccpars2_60_else/1}).
-file("src/parser_erlang.yrl", 30).
yeccpars2_60_else(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , conditional , ast_line ( __1 ) , [ { condition , __1 } , { 'if' , __3 } , { else , __5 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_60_\'end\''/1}).
-file("src/parser_erlang.yrl", 30).
'yeccpars2_60_\'end\''(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , conditional , ast_line ( __1 ) , [ { condition , __1 } , { 'if' , __3 } , { else , __5 } ] }
  end | __Stack].

-compile({inline,yeccpars2_60_then/1}).
-file("src/parser_erlang.yrl", 30).
yeccpars2_60_then(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , conditional , ast_line ( __1 ) , [ { condition , __1 } , { 'if' , __3 } , { else , __5 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_61_\'$end\''/1}).
-file("src/parser_erlang.yrl", 26).
'yeccpars2_61_\'$end\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , eq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_61_\'&&\''/1}).
-file("src/parser_erlang.yrl", 26).
'yeccpars2_61_\'&&\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , eq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_61_\')\''/1}).
-file("src/parser_erlang.yrl", 26).
'yeccpars2_61_\')\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , eq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_61_\',\''/1}).
-file("src/parser_erlang.yrl", 26).
'yeccpars2_61_\',\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , eq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_61_\':\''/1}).
-file("src/parser_erlang.yrl", 26).
'yeccpars2_61_\':\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , eq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_61_\';\''/1}).
-file("src/parser_erlang.yrl", 26).
'yeccpars2_61_\';\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , eq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_61_\'?\''/1}).
-file("src/parser_erlang.yrl", 26).
'yeccpars2_61_\'?\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , eq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,yeccpars2_61_do/1}).
-file("src/parser_erlang.yrl", 26).
yeccpars2_61_do(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , eq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,yeccpars2_61_else/1}).
-file("src/parser_erlang.yrl", 26).
yeccpars2_61_else(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , eq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_61_\'end\''/1}).
-file("src/parser_erlang.yrl", 26).
'yeccpars2_61_\'end\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , eq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,yeccpars2_61_then/1}).
-file("src/parser_erlang.yrl", 26).
yeccpars2_61_then(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , eq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_61_\'||\''/1}).
-file("src/parser_erlang.yrl", 26).
'yeccpars2_61_\'||\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , eq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_62_\'$end\''/1}).
-file("src/parser_erlang.yrl", 25).
'yeccpars2_62_\'$end\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , leq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_62_\'&&\''/1}).
-file("src/parser_erlang.yrl", 25).
'yeccpars2_62_\'&&\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , leq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_62_\')\''/1}).
-file("src/parser_erlang.yrl", 25).
'yeccpars2_62_\')\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , leq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_62_\',\''/1}).
-file("src/parser_erlang.yrl", 25).
'yeccpars2_62_\',\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , leq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_62_\':\''/1}).
-file("src/parser_erlang.yrl", 25).
'yeccpars2_62_\':\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , leq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_62_\';\''/1}).
-file("src/parser_erlang.yrl", 25).
'yeccpars2_62_\';\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , leq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_62_\'?\''/1}).
-file("src/parser_erlang.yrl", 25).
'yeccpars2_62_\'?\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , leq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,yeccpars2_62_do/1}).
-file("src/parser_erlang.yrl", 25).
yeccpars2_62_do(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , leq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,yeccpars2_62_else/1}).
-file("src/parser_erlang.yrl", 25).
yeccpars2_62_else(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , leq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_62_\'end\''/1}).
-file("src/parser_erlang.yrl", 25).
'yeccpars2_62_\'end\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , leq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,yeccpars2_62_then/1}).
-file("src/parser_erlang.yrl", 25).
yeccpars2_62_then(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , leq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_62_\'||\''/1}).
-file("src/parser_erlang.yrl", 25).
'yeccpars2_62_\'||\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , leq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,yeccpars2_63_/1}).
-file("src/parser_erlang.yrl", 23).
yeccpars2_63_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , sub , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,yeccpars2_65_/1}).
-file("src/parser_erlang.yrl", 87).
yeccpars2_65_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_67_/1}).
-file("src/parser_erlang.yrl", 88).
yeccpars2_67_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_68_/1}).
-file("src/parser_erlang.yrl", 32).
yeccpars2_68_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , tuple , token_line ( __1 ) , [ { components , [ __2 | __4 ] } ] }
  end | __Stack].

-compile({inline,yeccpars2_69_/1}).
-file("src/parser_erlang.yrl", 22).
yeccpars2_69_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , add , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,yeccpars2_70_/1}).
-file("src/parser_erlang.yrl", 24).
yeccpars2_70_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , mul , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,yeccpars2_71_/1}).
-file("src/parser_erlang.yrl", 27).
yeccpars2_71_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , 'and' , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,yeccpars2_73_/1}).
-file("src/parser_erlang.yrl", 45).
yeccpars2_73_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { stm , while , token_line ( __1 ) , [ { condition , __2 } , { body , __4 } ] }
  end | __Stack].

-compile({inline,yeccpars2_78_/1}).
-file("src/parser_erlang.yrl", 43).
yeccpars2_78_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { stm , 'if' , token_line ( __1 ) , [ { condition , __2 } , { then , __4 } , { else , __6 } ] }
  end | __Stack].

-compile({inline,yeccpars2_80_/1}).
-file("src/parser_erlang.yrl", 37).
yeccpars2_80_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { stm , assignment , token_line ( __1 ) , [ { lhs , token_value ( __1 ) } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,yeccpars2_81_/1}).
-file("src/parser_erlang.yrl", 20).
yeccpars2_81_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { exp , variable , token_line ( __1 ) , [ { name , token_value ( __1 ) } ] }
  end | __Stack].

-compile({inline,yeccpars2_82_/1}).
-file("src/parser_erlang.yrl", 85).
yeccpars2_82_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_85_/1}).
-file("src/parser_erlang.yrl", 39).
yeccpars2_85_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { stm , fun_app , token_line ( __1 ) , [ { lhs , token_value ( __1 ) } , { fun_name , token_value ( __3 ) } , { args , __5 } ] }
  end | __Stack].

-compile({inline,yeccpars2_87_/1}).
-file("src/parser_erlang.yrl", 63).
yeccpars2_87_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_93_/1}).
-file("src/parser_erlang.yrl", 67).
yeccpars2_93_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { declaration , var_decl , token_line ( __1 ) , [ { lhs , token_value ( __2 ) } , { rhs , __4 } ] }
  end | __Stack].

-compile({inline,yeccpars2_97_/1}).
-file("src/parser_erlang.yrl", 69).
yeccpars2_97_(__Stack0) ->
 [__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { declaration , var_decl , token_line ( __1 ) , [ { lhs , token_value ( __2 ) } , { rhs , __6 } , { type , __4 } ] }
  end | __Stack].

-compile({inline,yeccpars2_98_/1}).
-file("src/parser_erlang.yrl", 64).
yeccpars2_98_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_100_/1}).
-file("src/parser_erlang.yrl", 47).
yeccpars2_100_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { stm , block , token_line ( __1 ) , [ { decls , __2 } , { body , __3 } ] }
  end | __Stack].

-compile({inline,yeccpars2_104_/1}).
-file("src/parser_erlang.yrl", 90).
yeccpars2_104_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ token_value ( __1 ) ]
  end | __Stack].

-compile({inline,yeccpars2_106_/1}).
-file("src/parser_erlang.yrl", 91).
yeccpars2_106_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ token_value ( __1 ) | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_109_/1}).
-file("src/parser_erlang.yrl", 41).
yeccpars2_109_(__Stack0) ->
 [__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { stm , tuple_assignment , token_line ( __1 ) , [ { lhs , [ token_value ( __2 ) | __4 ] } , { rhs , __7 } ] }
  end | __Stack].

-compile({inline,yeccpars2_110_/1}).
-file("src/parser_erlang.yrl", 60).
yeccpars2_110_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_111_/1}).
-file("src/parser_erlang.yrl", 61).
yeccpars2_111_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_112_/1}).
-file("src/parser_erlang.yrl", 57).
yeccpars2_112_(__Stack0) ->
 [__11,__10,__9,__8,__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { declaration , fun_decl , token_line ( __1 ) , [ { function_name , token_value ( __2 ) } , { params , __4 } , { returns , __8 } , { body , __10 } ] }
  end | __Stack].

-compile({inline,yeccpars2_113_/1}).
-file("src/parser_erlang.yrl", 54).
yeccpars2_113_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_114_/1}).
-file("src/parser_erlang.yrl", 51).
yeccpars2_114_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { program , program , program_line ( __1 , __2 ) , [ { functions , __1 } , { main_stm , __2 } ] }
  end | __Stack].


-file("src/parser_erlang.yrl", 118).
