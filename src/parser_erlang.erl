-module(parser_erlang).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("src/parser_erlang.yrl", 129).

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
 yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(15=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(16=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(17=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_17(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(18=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(19=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(20=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_20(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(21=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(22=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_22(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(23=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_23(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(24=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(25=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(26=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_26(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(27=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(28=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_28(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(29=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(30=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(31=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(32=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_32(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(33=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(34=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_34(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(35=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(36=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(37=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(38=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(39=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(40=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(41=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(42=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(43=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(44=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(45=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(46=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(47=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(48=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(49=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(50=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(51=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(52=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_52(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(53=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(54=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(55=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(56=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(57=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(58=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(59=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(60=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(61=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(62=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(63=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_63(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(64=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_64(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(65=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(66=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_66(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(67=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(68=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_68(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(69=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_69(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(70=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_70(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(71=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(72=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(73=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_73(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(74=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_74(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(75=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_75(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(76=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_76(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(77=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(78=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_78(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(79=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(80=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(81=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_81(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(82=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_82(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(83=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(84=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_84(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(85=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(86=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(87=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_87(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(88=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(89=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(90=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(91=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(92=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(93=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_93(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(94=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(95=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_95(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(96=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(97=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(98=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(99=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_99(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(100=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_100(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(101=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(102=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_102(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(103=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_103(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(104=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_104(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(105=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_105(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(106=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_106(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(107=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_107(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(108=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(109=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_109(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(110=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(111=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(112=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(113=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(114=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_114(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(115=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(116=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_116(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(117=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(118=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_118(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(119=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(120=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_120(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(121=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_121(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(122=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(123=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_123(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(124=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_124(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(125=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_125(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(126=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_126(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(127=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_124(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(128=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_128(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(129=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_129(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(130=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(131=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_131(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(132=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(133=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_133(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(134=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_134(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(135=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_135(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(136=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_136(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'if', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, ifnil, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, skip, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, while, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_3(S, function, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 4, Ss, Stack, T, Ts, Tzr);
yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_3_(Stack),
 yeccpars2_135(_S, Cat, [3 | Ss], NewStack, T, Ts, Tzr).

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
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_8(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
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
yeccpars2_11(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(S, bool, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(S, int, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_12_(Stack),
 yeccgoto_param_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_13: see yeccpars2_11

%% yeccpars2_14: see yeccpars2_11

yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_15_(Stack),
 yeccgoto_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_16_(Stack),
 yeccgoto_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_17/7}).
yeccpars2_17(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_18(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_18_(Stack),
 yeccgoto_type(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_19(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_19_(Stack),
 yeccgoto_type(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_20/7}).
yeccpars2_20(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_21: see yeccpars2_11

yeccpars2_22(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_22_(Stack),
 yeccgoto_non_empty_types(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_23/7}).
yeccpars2_23(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_24_(Stack),
 yeccgoto_type(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_25: see yeccpars2_11

yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_26_(Stack),
 yeccgoto_non_empty_types(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_27/7}).
yeccpars2_27(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_27(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_28_(Stack),
 yeccgoto_non_empty_param_decls(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_29/7}).
yeccpars2_29(S, ret, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_30/7}).
yeccpars2_30(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_30(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_31: see yeccpars2_27

-dialyzer({nowarn_function, yeccpars2_32/7}).
yeccpars2_32(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_33: see yeccpars2_2

-dialyzer({nowarn_function, yeccpars2_34/7}).
yeccpars2_34(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 134, Ss, Stack, T, Ts, Tzr);
yeccpars2_34(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_35(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 132, Ss, Stack, T, Ts, Tzr);
yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_35_(Stack),
 yeccgoto_stm_seq(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_36/7}).
yeccpars2_36(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_37(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 110, Ss, Stack, T, Ts, Tzr);
yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_37_(Stack),
 yeccpars2_2(108, Cat, [37 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_38/7}).
yeccpars2_38(S, ':=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_39(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_39(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_39/7}).
yeccpars2_cont_39(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_39(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_39(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_39(S, false, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_39(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_39(S, nil, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_39(S, true, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_39(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_40/7}).
yeccpars2_40(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 89, Ss, Stack, T, Ts, Tzr);
yeccpars2_40(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_41_(Stack),
 yeccgoto_stm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_42: see yeccpars2_39

yeccpars2_43(S, do, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_43(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_43/7}).
yeccpars2_cont_43(S, '&&', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_43(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_43(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_43(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_43(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_43(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_43(S, '==', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_43(S, '?', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_43(S, '||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_43(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_44: see yeccpars2_39

-dialyzer({nowarn_function, yeccpars2_45/7}).
yeccpars2_45(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_46: see yeccpars2_39

yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_47_(Stack),
 yeccgoto_exp(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_48_(Stack),
 yeccgoto_exp(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_49_(Stack),
 yeccgoto_exp(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_50_(Stack),
 yeccgoto_exp(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_51_(Stack),
 yeccgoto_exp(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_52(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_52(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_43(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_53: see yeccpars2_39

%% yeccpars2_54: see yeccpars2_39

%% yeccpars2_55: see yeccpars2_39

%% yeccpars2_56: see yeccpars2_39

-dialyzer({nowarn_function, yeccpars2_57/7}).
yeccpars2_57(S, hd, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, tl, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_58: see yeccpars2_39

%% yeccpars2_59: see yeccpars2_39

%% yeccpars2_60: see yeccpars2_39

%% yeccpars2_61: see yeccpars2_39

%% yeccpars2_62: see yeccpars2_39

yeccpars2_63(S, '&&', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(S, '==', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(S, '||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_63_(Stack),
 yeccgoto_exp(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_64(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_43(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_65_(Stack),
 yeccgoto_exp(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_66(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_43(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_67: see yeccpars2_39

yeccpars2_68(S, '&&', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(S, '==', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(S, '||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = 'yeccpars2_68_\'$end\''(Stack),
 yeccgoto_exp(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_68(_S, ')', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = 'yeccpars2_68_\')\''(Stack),
 yeccgoto_exp(hd(Nss), ')', Nss, NewStack, T, Ts, Tzr);
yeccpars2_68(_S, ',', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = 'yeccpars2_68_\',\''(Stack),
 yeccgoto_exp(hd(Nss), ',', Nss, NewStack, T, Ts, Tzr);
yeccpars2_68(_S, '.', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = 'yeccpars2_68_\'.\''(Stack),
 yeccgoto_exp(hd(Nss), '.', Nss, NewStack, T, Ts, Tzr);
yeccpars2_68(_S, ':', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = 'yeccpars2_68_\':\''(Stack),
 yeccgoto_exp(hd(Nss), ':', Nss, NewStack, T, Ts, Tzr);
yeccpars2_68(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = 'yeccpars2_68_\';\''(Stack),
 yeccgoto_exp(hd(Nss), ';', Nss, NewStack, T, Ts, Tzr);
yeccpars2_68(_S, ']', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = 'yeccpars2_68_\']\''(Stack),
 yeccgoto_exp(hd(Nss), ']', Nss, NewStack, T, Ts, Tzr);
yeccpars2_68(_S, do, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_68_do(Stack),
 yeccgoto_exp(hd(Nss), do, Nss, NewStack, T, Ts, Tzr);
yeccpars2_68(_S, else, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_68_else(Stack),
 yeccgoto_exp(hd(Nss), else, Nss, NewStack, T, Ts, Tzr);
yeccpars2_68(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = 'yeccpars2_68_\'end\''(Stack),
 yeccgoto_exp(hd(Nss), 'end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_68(_S, then, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_68_then(Stack),
 yeccgoto_exp(hd(Nss), then, Nss, NewStack, T, Ts, Tzr);
yeccpars2_68(_S, '|', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = 'yeccpars2_68_\'|\''(Stack),
 yeccgoto_exp(hd(Nss), '|', Nss, NewStack, T, Ts, Tzr);
yeccpars2_68(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_69(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_69(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_69(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_69(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_69_\'$end\''(Stack),
 yeccgoto_exp(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_69(_S, '&&', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_69_\'&&\''(Stack),
 yeccgoto_exp(hd(Nss), '&&', Nss, NewStack, T, Ts, Tzr);
yeccpars2_69(_S, ')', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_69_\')\''(Stack),
 yeccgoto_exp(hd(Nss), ')', Nss, NewStack, T, Ts, Tzr);
yeccpars2_69(_S, ',', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_69_\',\''(Stack),
 yeccgoto_exp(hd(Nss), ',', Nss, NewStack, T, Ts, Tzr);
yeccpars2_69(_S, '.', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_69_\'.\''(Stack),
 yeccgoto_exp(hd(Nss), '.', Nss, NewStack, T, Ts, Tzr);
yeccpars2_69(_S, ':', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_69_\':\''(Stack),
 yeccgoto_exp(hd(Nss), ':', Nss, NewStack, T, Ts, Tzr);
yeccpars2_69(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_69_\';\''(Stack),
 yeccgoto_exp(hd(Nss), ';', Nss, NewStack, T, Ts, Tzr);
yeccpars2_69(_S, '?', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_69_\'?\''(Stack),
 yeccgoto_exp(hd(Nss), '?', Nss, NewStack, T, Ts, Tzr);
yeccpars2_69(_S, ']', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_69_\']\''(Stack),
 yeccgoto_exp(hd(Nss), ']', Nss, NewStack, T, Ts, Tzr);
yeccpars2_69(_S, do, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_69_do(Stack),
 yeccgoto_exp(hd(Nss), do, Nss, NewStack, T, Ts, Tzr);
yeccpars2_69(_S, else, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_69_else(Stack),
 yeccgoto_exp(hd(Nss), else, Nss, NewStack, T, Ts, Tzr);
yeccpars2_69(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_69_\'end\''(Stack),
 yeccgoto_exp(hd(Nss), 'end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_69(_S, then, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_69_then(Stack),
 yeccgoto_exp(hd(Nss), then, Nss, NewStack, T, Ts, Tzr);
yeccpars2_69(_S, '|', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_69_\'|\''(Stack),
 yeccgoto_exp(hd(Nss), '|', Nss, NewStack, T, Ts, Tzr);
yeccpars2_69(_S, '||', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_69_\'||\''(Stack),
 yeccgoto_exp(hd(Nss), '||', Nss, NewStack, T, Ts, Tzr);
yeccpars2_69(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_70(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_70_\'$end\''(Stack),
 yeccgoto_exp(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_70(_S, '&&', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_70_\'&&\''(Stack),
 yeccgoto_exp(hd(Nss), '&&', Nss, NewStack, T, Ts, Tzr);
yeccpars2_70(_S, ')', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_70_\')\''(Stack),
 yeccgoto_exp(hd(Nss), ')', Nss, NewStack, T, Ts, Tzr);
yeccpars2_70(_S, ',', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_70_\',\''(Stack),
 yeccgoto_exp(hd(Nss), ',', Nss, NewStack, T, Ts, Tzr);
yeccpars2_70(_S, '.', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_70_\'.\''(Stack),
 yeccgoto_exp(hd(Nss), '.', Nss, NewStack, T, Ts, Tzr);
yeccpars2_70(_S, ':', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_70_\':\''(Stack),
 yeccgoto_exp(hd(Nss), ':', Nss, NewStack, T, Ts, Tzr);
yeccpars2_70(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_70_\';\''(Stack),
 yeccgoto_exp(hd(Nss), ';', Nss, NewStack, T, Ts, Tzr);
yeccpars2_70(_S, '?', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_70_\'?\''(Stack),
 yeccgoto_exp(hd(Nss), '?', Nss, NewStack, T, Ts, Tzr);
yeccpars2_70(_S, ']', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_70_\']\''(Stack),
 yeccgoto_exp(hd(Nss), ']', Nss, NewStack, T, Ts, Tzr);
yeccpars2_70(_S, do, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_70_do(Stack),
 yeccgoto_exp(hd(Nss), do, Nss, NewStack, T, Ts, Tzr);
yeccpars2_70(_S, else, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_70_else(Stack),
 yeccgoto_exp(hd(Nss), else, Nss, NewStack, T, Ts, Tzr);
yeccpars2_70(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_70_\'end\''(Stack),
 yeccgoto_exp(hd(Nss), 'end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_70(_S, then, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_70_then(Stack),
 yeccgoto_exp(hd(Nss), then, Nss, NewStack, T, Ts, Tzr);
yeccpars2_70(_S, '|', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_70_\'|\''(Stack),
 yeccgoto_exp(hd(Nss), '|', Nss, NewStack, T, Ts, Tzr);
yeccpars2_70(_S, '||', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_70_\'||\''(Stack),
 yeccgoto_exp(hd(Nss), '||', Nss, NewStack, T, Ts, Tzr);
yeccpars2_70(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_71_(Stack),
 yeccgoto_exp(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_72(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_72_(Stack),
 yeccgoto_exp(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_73(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_73(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_73_(Stack),
 yeccgoto_exp(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_74(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_74(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_74_(Stack),
 yeccgoto_exp(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_75_(Stack),
 yeccgoto_exp(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_76(S, '&&', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, '==', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_76_(Stack),
 yeccgoto_exp(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_77_(Stack),
 yeccgoto_exp(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_78(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_78(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_78(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_43(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_79(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_79_(Stack),
 yeccgoto_exp(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_80: see yeccpars2_39

-dialyzer({nowarn_function, yeccpars2_81/7}).
yeccpars2_81(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_82(S, '&&', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_82(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_82(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_82(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 83, Ss, Stack, T, Ts, Tzr);
yeccpars2_82(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_82(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_82(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_82(S, '==', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_82(S, '?', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_82(S, '||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_82_(Stack),
 yeccgoto_non_empty_exps(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_83: see yeccpars2_39

yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_84_(Stack),
 yeccgoto_non_empty_exps(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_85_(Stack),
 yeccgoto_exp(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_86: see yeccpars2_2

-dialyzer({nowarn_function, yeccpars2_87/7}).
yeccpars2_87(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 88, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_88_(Stack),
 yeccgoto_stm(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_89/7}).
yeccpars2_89(S, then, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 90, Ss, Stack, T, Ts, Tzr);
yeccpars2_89(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_90: see yeccpars2_2

-dialyzer({nowarn_function, yeccpars2_91/7}).
yeccpars2_91(S, else, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 92, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_92: see yeccpars2_2

-dialyzer({nowarn_function, yeccpars2_93/7}).
yeccpars2_93(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 94, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_94(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_94_(Stack),
 yeccgoto_stm(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_95(S, then, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 96, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_43(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_96: see yeccpars2_2

-dialyzer({nowarn_function, yeccpars2_97/7}).
yeccpars2_97(S, else, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_98: see yeccpars2_2

-dialyzer({nowarn_function, yeccpars2_99/7}).
yeccpars2_99(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_100(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_100_(Stack),
 yeccgoto_stm(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_101(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_39(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_102(S, '&&', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, '==', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, '?', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, '||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_102_(Stack),
 yeccgoto_stm(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_103(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 104, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_(Stack),
 yeccgoto_exp(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_104(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, false, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, nil, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, true, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_104_(Stack),
 yeccpars2_106(106, Cat, [104 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_105(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_exps(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_106/7}).
yeccpars2_106(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_107(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_107_(Stack),
 yeccgoto_stm(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_108: see yeccpars2_2

yeccpars2_109(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 110, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_109_(Stack),
 yeccpars2_120(_S, Cat, [109 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_110/7}).
yeccpars2_110(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 111, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_111/7}).
yeccpars2_111(S, '::', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_111(S, ':=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 113, Ss, Stack, T, Ts, Tzr);
yeccpars2_111(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_112: see yeccpars2_11

%% yeccpars2_113: see yeccpars2_39

yeccpars2_114(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 115, Ss, Stack, T, Ts, Tzr);
yeccpars2_114(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_43(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_115(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_115_(Stack),
 yeccgoto_var_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_116/7}).
yeccpars2_116(S, ':=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 117, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_117: see yeccpars2_39

yeccpars2_118(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 119, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_43(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_119_(Stack),
 yeccgoto_var_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_120_(Stack),
 yeccgoto_var_decls(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_121/7}).
yeccpars2_121(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 122, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_122_(Stack),
 yeccgoto_stm(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_123/7}).
yeccpars2_123(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 124, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_124/7}).
yeccpars2_124(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_124(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_125/7}).
yeccpars2_125(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 129, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_126(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 127, Ss, Stack, T, Ts, Tzr);
yeccpars2_126(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_126_(Stack),
 yeccgoto_non_empty_ids(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_127: see yeccpars2_124

yeccpars2_128(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_128_(Stack),
 yeccgoto_non_empty_ids(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_129/7}).
yeccpars2_129(S, ':=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 130, Ss, Stack, T, Ts, Tzr);
yeccpars2_129(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_130: see yeccpars2_39

yeccpars2_131(S, '&&', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, '==', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, '?', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, '||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_131_(Stack),
 yeccgoto_stm(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_132(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, 'if', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, ifnil, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, skip, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, while, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_132_(Stack),
 yeccgoto_stm_seq(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_133_(Stack),
 yeccgoto_stm_seq(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_134(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_134_(Stack),
 yeccgoto_fun_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_135(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_135_(Stack),
 yeccgoto_fun_decls(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_136(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_136_(Stack),
 yeccgoto_program(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_exp/7}).
yeccgoto_exp(39, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(95, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(42, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(43, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(44, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(46, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(52, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(53, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(55, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(74, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(56, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(73, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(58, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(70, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(59, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(69, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(60, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(66, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(61, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(64, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(62, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(67, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(68, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(80, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(82, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(83, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(82, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(101, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_102(102, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(104, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(82, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(113, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(114, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(117, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(118, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(130, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_131(131, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_exps/7}).
yeccgoto_exps(104, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_106(106, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_fun_decl/7}).
yeccgoto_fun_decl(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(3, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_decl(3, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(3, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_fun_decls/7}).
yeccgoto_fun_decls(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_decls(3=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_135(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_non_empty_exps/7}).
yeccgoto_non_empty_exps(80, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(81, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_non_empty_exps(83=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_non_empty_exps(104=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_105(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_non_empty_ids/7}).
yeccgoto_non_empty_ids(124, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_125(125, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_non_empty_ids(127=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_128(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_non_empty_param_decls/7}).
yeccgoto_non_empty_param_decls(6=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_non_empty_param_decls(27=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_non_empty_types/7}).
yeccgoto_non_empty_types(21, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(23, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_non_empty_types(25=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_param_decl/7}).
yeccgoto_param_decl(6, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_param_decl(27, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_param_decl(31, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_param_decls/7}).
yeccgoto_param_decls(6, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_program/7}).
yeccgoto_program(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_stm/7}).
yeccgoto_stm(2, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(35, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_stm(33, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(35, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_stm(86, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(35, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_stm(90, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(35, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_stm(92, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(35, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_stm(96, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(35, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_stm(98, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(35, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_stm(108, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(35, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_stm(132, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(35, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_stm_seq/7}).
yeccgoto_stm_seq(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_136(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_stm_seq(33, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_stm_seq(86, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(87, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_stm_seq(90, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(91, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_stm_seq(92, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_93(93, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_stm_seq(96, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(97, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_stm_seq(98, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_99(99, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_stm_seq(108, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_121(121, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_stm_seq(132=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type/7}).
yeccgoto_type(11=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(13, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(20, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(14, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(17, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(21, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(22, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(25, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(22, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(112, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(116, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_var_decl/7}).
yeccgoto_var_decl(37, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(109, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var_decl(109, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(109, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_var_decls/7}).
yeccgoto_var_decls(37, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(108, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var_decls(109=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr).

-compile({inline,yeccpars2_0_/1}).
-file("src/parser_erlang.yrl", 76).
yeccpars2_0_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_3_/1}).
-file("src/parser_erlang.yrl", 76).
yeccpars2_3_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_6_/1}).
-file("src/parser_erlang.yrl", 96).
yeccpars2_6_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_8_/1}).
-file("src/parser_erlang.yrl", 98).
yeccpars2_8_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_10_/1}).
-file("src/parser_erlang.yrl", 103).
yeccpars2_10_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ { declaration , param_decl , token_line ( __1 ) , [ { variable , token_value ( __1 ) } ] } ]
  end | __Stack].

-compile({inline,yeccpars2_12_/1}).
-file("src/parser_erlang.yrl", 105).
yeccpars2_12_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ { declaration , param_decl , token_line ( __1 ) , [ { variable , token_value ( __1 ) } , { type , __3 } ] } ]
  end | __Stack].

-compile({inline,yeccpars2_15_/1}).
-file("src/parser_erlang.yrl", 117).
yeccpars2_15_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { type , bool , token_line ( __1 ) , [ ] }
  end | __Stack].

-compile({inline,yeccpars2_16_/1}).
-file("src/parser_erlang.yrl", 116).
yeccpars2_16_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { type , int , token_line ( __1 ) , [ ] }
  end | __Stack].

-compile({inline,yeccpars2_18_/1}).
-file("src/parser_erlang.yrl", 119).
yeccpars2_18_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { type , list , token_line ( __1 ) , [ { elements , __2 } ] }
  end | __Stack].

-compile({inline,yeccpars2_19_/1}).
-file("src/parser_erlang.yrl", 120).
yeccpars2_19_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { type , non_empty_list , token_line ( __1 ) , [ { elements , __2 } ] }
  end | __Stack].

-compile({inline,yeccpars2_22_/1}).
-file("src/parser_erlang.yrl", 122).
yeccpars2_22_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_24_/1}).
-file("src/parser_erlang.yrl", 118).
yeccpars2_24_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { type , tuple , token_line ( __1 ) , [ { components , [ __2 | __4 ] } ] }
  end | __Stack].

-compile({inline,yeccpars2_26_/1}).
-file("src/parser_erlang.yrl", 123).
yeccpars2_26_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_28_/1}).
-file("src/parser_erlang.yrl", 99).
yeccpars2_28_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_35_/1}).
-file("src/parser_erlang.yrl", 82).
yeccpars2_35_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_37_/1}).
-file("src/parser_erlang.yrl", 86).
yeccpars2_37_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_41_/1}).
-file("src/parser_erlang.yrl", 56).
yeccpars2_41_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { stm , skip , token_line ( __1 ) , [ ] }
  end | __Stack].

-compile({inline,yeccpars2_47_/1}).
-file("src/parser_erlang.yrl", 35).
yeccpars2_47_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { exp , literal , token_line ( __1 ) , [ { boolean , false } ] }
  end | __Stack].

-compile({inline,yeccpars2_48_/1}).
-file("src/parser_erlang.yrl", 36).
yeccpars2_48_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { exp , variable , token_line ( __1 ) , [ { name , token_value ( __1 ) } ] }
  end | __Stack].

-compile({inline,yeccpars2_49_/1}).
-file("src/parser_erlang.yrl", 33).
yeccpars2_49_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { exp , literal , token_line ( __1 ) , [ { number , token_value ( __1 ) } ] }
  end | __Stack].

-compile({inline,yeccpars2_50_/1}).
-file("src/parser_erlang.yrl", 49).
yeccpars2_50_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { exp , nil , token_line ( __1 ) , [ ] }
  end | __Stack].

-compile({inline,yeccpars2_51_/1}).
-file("src/parser_erlang.yrl", 34).
yeccpars2_51_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { exp , literal , token_line ( __1 ) , [ { boolean , true } ] }
  end | __Stack].

-compile({inline,yeccpars2_63_/1}).
-file("src/parser_erlang.yrl", 44).
yeccpars2_63_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , 'or' , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,yeccpars2_65_/1}).
-file("src/parser_erlang.yrl", 53).
yeccpars2_65_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , cons , token_line ( __1 ) , [ { head , __2 } , { tail , __4 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_68_\'$end\''/1}).
-file("src/parser_erlang.yrl", 46).
'yeccpars2_68_\'$end\''(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , conditional , ast_line ( __1 ) , [ { condition , __1 } , { 'if' , __3 } , { else , __5 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_68_\')\''/1}).
-file("src/parser_erlang.yrl", 46).
'yeccpars2_68_\')\''(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , conditional , ast_line ( __1 ) , [ { condition , __1 } , { 'if' , __3 } , { else , __5 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_68_\',\''/1}).
-file("src/parser_erlang.yrl", 46).
'yeccpars2_68_\',\''(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , conditional , ast_line ( __1 ) , [ { condition , __1 } , { 'if' , __3 } , { else , __5 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_68_\'.\''/1}).
-file("src/parser_erlang.yrl", 46).
'yeccpars2_68_\'.\''(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , conditional , ast_line ( __1 ) , [ { condition , __1 } , { 'if' , __3 } , { else , __5 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_68_\':\''/1}).
-file("src/parser_erlang.yrl", 46).
'yeccpars2_68_\':\''(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , conditional , ast_line ( __1 ) , [ { condition , __1 } , { 'if' , __3 } , { else , __5 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_68_\';\''/1}).
-file("src/parser_erlang.yrl", 46).
'yeccpars2_68_\';\''(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , conditional , ast_line ( __1 ) , [ { condition , __1 } , { 'if' , __3 } , { else , __5 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_68_\']\''/1}).
-file("src/parser_erlang.yrl", 46).
'yeccpars2_68_\']\''(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , conditional , ast_line ( __1 ) , [ { condition , __1 } , { 'if' , __3 } , { else , __5 } ] }
  end | __Stack].

-compile({inline,yeccpars2_68_do/1}).
-file("src/parser_erlang.yrl", 46).
yeccpars2_68_do(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , conditional , ast_line ( __1 ) , [ { condition , __1 } , { 'if' , __3 } , { else , __5 } ] }
  end | __Stack].

-compile({inline,yeccpars2_68_else/1}).
-file("src/parser_erlang.yrl", 46).
yeccpars2_68_else(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , conditional , ast_line ( __1 ) , [ { condition , __1 } , { 'if' , __3 } , { else , __5 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_68_\'end\''/1}).
-file("src/parser_erlang.yrl", 46).
'yeccpars2_68_\'end\''(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , conditional , ast_line ( __1 ) , [ { condition , __1 } , { 'if' , __3 } , { else , __5 } ] }
  end | __Stack].

-compile({inline,yeccpars2_68_then/1}).
-file("src/parser_erlang.yrl", 46).
yeccpars2_68_then(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , conditional , ast_line ( __1 ) , [ { condition , __1 } , { 'if' , __3 } , { else , __5 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_68_\'|\''/1}).
-file("src/parser_erlang.yrl", 46).
'yeccpars2_68_\'|\''(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , conditional , ast_line ( __1 ) , [ { condition , __1 } , { 'if' , __3 } , { else , __5 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_69_\'$end\''/1}).
-file("src/parser_erlang.yrl", 42).
'yeccpars2_69_\'$end\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , eq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_69_\'&&\''/1}).
-file("src/parser_erlang.yrl", 42).
'yeccpars2_69_\'&&\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , eq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_69_\')\''/1}).
-file("src/parser_erlang.yrl", 42).
'yeccpars2_69_\')\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , eq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_69_\',\''/1}).
-file("src/parser_erlang.yrl", 42).
'yeccpars2_69_\',\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , eq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_69_\'.\''/1}).
-file("src/parser_erlang.yrl", 42).
'yeccpars2_69_\'.\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , eq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_69_\':\''/1}).
-file("src/parser_erlang.yrl", 42).
'yeccpars2_69_\':\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , eq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_69_\';\''/1}).
-file("src/parser_erlang.yrl", 42).
'yeccpars2_69_\';\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , eq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_69_\'?\''/1}).
-file("src/parser_erlang.yrl", 42).
'yeccpars2_69_\'?\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , eq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_69_\']\''/1}).
-file("src/parser_erlang.yrl", 42).
'yeccpars2_69_\']\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , eq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,yeccpars2_69_do/1}).
-file("src/parser_erlang.yrl", 42).
yeccpars2_69_do(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , eq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,yeccpars2_69_else/1}).
-file("src/parser_erlang.yrl", 42).
yeccpars2_69_else(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , eq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_69_\'end\''/1}).
-file("src/parser_erlang.yrl", 42).
'yeccpars2_69_\'end\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , eq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,yeccpars2_69_then/1}).
-file("src/parser_erlang.yrl", 42).
yeccpars2_69_then(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , eq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_69_\'|\''/1}).
-file("src/parser_erlang.yrl", 42).
'yeccpars2_69_\'|\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , eq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_69_\'||\''/1}).
-file("src/parser_erlang.yrl", 42).
'yeccpars2_69_\'||\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , eq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_70_\'$end\''/1}).
-file("src/parser_erlang.yrl", 41).
'yeccpars2_70_\'$end\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , leq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_70_\'&&\''/1}).
-file("src/parser_erlang.yrl", 41).
'yeccpars2_70_\'&&\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , leq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_70_\')\''/1}).
-file("src/parser_erlang.yrl", 41).
'yeccpars2_70_\')\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , leq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_70_\',\''/1}).
-file("src/parser_erlang.yrl", 41).
'yeccpars2_70_\',\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , leq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_70_\'.\''/1}).
-file("src/parser_erlang.yrl", 41).
'yeccpars2_70_\'.\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , leq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_70_\':\''/1}).
-file("src/parser_erlang.yrl", 41).
'yeccpars2_70_\':\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , leq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_70_\';\''/1}).
-file("src/parser_erlang.yrl", 41).
'yeccpars2_70_\';\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , leq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_70_\'?\''/1}).
-file("src/parser_erlang.yrl", 41).
'yeccpars2_70_\'?\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , leq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_70_\']\''/1}).
-file("src/parser_erlang.yrl", 41).
'yeccpars2_70_\']\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , leq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,yeccpars2_70_do/1}).
-file("src/parser_erlang.yrl", 41).
yeccpars2_70_do(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , leq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,yeccpars2_70_else/1}).
-file("src/parser_erlang.yrl", 41).
yeccpars2_70_else(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , leq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_70_\'end\''/1}).
-file("src/parser_erlang.yrl", 41).
'yeccpars2_70_\'end\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , leq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,yeccpars2_70_then/1}).
-file("src/parser_erlang.yrl", 41).
yeccpars2_70_then(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , leq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_70_\'|\''/1}).
-file("src/parser_erlang.yrl", 41).
'yeccpars2_70_\'|\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , leq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,'yeccpars2_70_\'||\''/1}).
-file("src/parser_erlang.yrl", 41).
'yeccpars2_70_\'||\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , leq , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,yeccpars2_71_/1}).
-file("src/parser_erlang.yrl", 50).
yeccpars2_71_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , hd , ast_line ( __1 ) , [ { lhs , __1 } ] }
  end | __Stack].

-compile({inline,yeccpars2_72_/1}).
-file("src/parser_erlang.yrl", 51).
yeccpars2_72_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , tl , ast_line ( __1 ) , [ { lhs , __1 } ] }
  end | __Stack].

-compile({inline,yeccpars2_73_/1}).
-file("src/parser_erlang.yrl", 39).
yeccpars2_73_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , sub , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,yeccpars2_74_/1}).
-file("src/parser_erlang.yrl", 38).
yeccpars2_74_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , add , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,yeccpars2_75_/1}).
-file("src/parser_erlang.yrl", 40).
yeccpars2_75_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , mul , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,yeccpars2_76_/1}).
-file("src/parser_erlang.yrl", 43).
yeccpars2_76_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , 'and' , ast_line ( __1 ) , [ { lhs , __1 } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,yeccpars2_77_/1}).
-file("src/parser_erlang.yrl", 37).
yeccpars2_77_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , literal , token_line ( __1 ) , [ { number , - token_value ( __2 ) } ] }
  end | __Stack].

-compile({inline,yeccpars2_79_/1}).
-file("src/parser_erlang.yrl", 47).
yeccpars2_79_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_82_/1}).
-file("src/parser_erlang.yrl", 110).
yeccpars2_82_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_84_/1}).
-file("src/parser_erlang.yrl", 111).
yeccpars2_84_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_85_/1}).
-file("src/parser_erlang.yrl", 48).
yeccpars2_85_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { exp , tuple , token_line ( __1 ) , [ { components , [ __2 | __4 ] } ] }
  end | __Stack].

-compile({inline,yeccpars2_88_/1}).
-file("src/parser_erlang.yrl", 66).
yeccpars2_88_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { stm , while , token_line ( __1 ) , [ { condition , __2 } , { body , __4 } ] }
  end | __Stack].

-compile({inline,yeccpars2_94_/1}).
-file("src/parser_erlang.yrl", 70).
yeccpars2_94_(__Stack0) ->
 [__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { stm , ifnil , token_line ( __1 ) , [ { variable , token_value ( __2 ) } , { then , __4 } , { else , __6 } ] }
  end | __Stack].

-compile({inline,yeccpars2_100_/1}).
-file("src/parser_erlang.yrl", 64).
yeccpars2_100_(__Stack0) ->
 [__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { stm , 'if' , token_line ( __1 ) , [ { condition , __2 } , { then , __4 } , { else , __6 } ] }
  end | __Stack].

-compile({inline,yeccpars2_102_/1}).
-file("src/parser_erlang.yrl", 58).
yeccpars2_102_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { stm , assignment , token_line ( __1 ) , [ { lhs , token_value ( __1 ) } , { rhs , __3 } ] }
  end | __Stack].

-compile({inline,yeccpars2_103_/1}).
-file("src/parser_erlang.yrl", 36).
yeccpars2_103_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { exp , variable , token_line ( __1 ) , [ { name , token_value ( __1 ) } ] }
  end | __Stack].

-compile({inline,yeccpars2_104_/1}).
-file("src/parser_erlang.yrl", 108).
yeccpars2_104_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_107_/1}).
-file("src/parser_erlang.yrl", 60).
yeccpars2_107_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { stm , fun_app , token_line ( __1 ) , [ { lhs , token_value ( __1 ) } , { fun_name , token_value ( __3 ) } , { args , __5 } ] }
  end | __Stack].

-compile({inline,yeccpars2_109_/1}).
-file("src/parser_erlang.yrl", 86).
yeccpars2_109_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_115_/1}).
-file("src/parser_erlang.yrl", 90).
yeccpars2_115_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { declaration , var_decl , token_line ( __1 ) , [ { lhs , token_value ( __2 ) } , { rhs , __4 } ] }
  end | __Stack].

-compile({inline,yeccpars2_119_/1}).
-file("src/parser_erlang.yrl", 92).
yeccpars2_119_(__Stack0) ->
 [__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { declaration , var_decl , token_line ( __1 ) , [ { lhs , token_value ( __2 ) } , { rhs , __6 } , { type , __4 } ] }
  end | __Stack].

-compile({inline,yeccpars2_120_/1}).
-file("src/parser_erlang.yrl", 87).
yeccpars2_120_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_122_/1}).
-file("src/parser_erlang.yrl", 68).
yeccpars2_122_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { stm , block , token_line ( __1 ) , [ { decls , __2 } , { body , __3 } ] }
  end | __Stack].

-compile({inline,yeccpars2_126_/1}).
-file("src/parser_erlang.yrl", 113).
yeccpars2_126_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ token_value ( __1 ) ]
  end | __Stack].

-compile({inline,yeccpars2_128_/1}).
-file("src/parser_erlang.yrl", 114).
yeccpars2_128_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ token_value ( __1 ) | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_131_/1}).
-file("src/parser_erlang.yrl", 62).
yeccpars2_131_(__Stack0) ->
 [__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { stm , tuple_assignment , token_line ( __1 ) , [ { lhs , [ token_value ( __2 ) | __4 ] } , { rhs , __7 } ] }
  end | __Stack].

-compile({inline,yeccpars2_132_/1}).
-file("src/parser_erlang.yrl", 83).
yeccpars2_132_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_133_/1}).
-file("src/parser_erlang.yrl", 84).
yeccpars2_133_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_134_/1}).
-file("src/parser_erlang.yrl", 80).
yeccpars2_134_(__Stack0) ->
 [__11,__10,__9,__8,__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { declaration , fun_decl , token_line ( __1 ) , [ { function_name , token_value ( __2 ) } , { params , __4 } , { returns , __8 } , { body , __10 } ] }
  end | __Stack].

-compile({inline,yeccpars2_135_/1}).
-file("src/parser_erlang.yrl", 77).
yeccpars2_135_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_136_/1}).
-file("src/parser_erlang.yrl", 74).
yeccpars2_136_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { program , program , program_line ( __1 , __2 ) , [ { functions , __1 } , { main_stm , __2 } ] }
  end | __Stack].


-file("src/parser_erlang.yrl", 142).
