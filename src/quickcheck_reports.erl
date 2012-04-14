%% -----------------------------------------------------------------------------
%% Copyright (c) 2002-2011 Tim Watson (watson.timothy@gmail.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -----------------------------------------------------------------------------
-module(quickcheck_reports).

-behaviour(gen_server).

-export([before_test/2]).
-export([write_report/2]).

-export([module/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    report,
    current_test,
    results = []
}).

write_report(Msg, Args) ->
    gen_server:cast(?MODULE, {Msg, Args}),
    ok.

before_test(Config, _) ->
    QcOpts = rebar_config:get_local(Config, qc_opts, []),
    case lists:keyfind(on_output, 1, QcOpts) of
        {on_output, {?MODULE, write_report, _}} ->
            ReportDir = rebar_config:get_local(Config, qc_report_dir, ".test"),
            FileName = rebar_config:get_local(Config, qc_report, "Y-m-d-H-i-s"),
            Report = filename:join([rebar_utils:get_cwd(),
                ReportDir, dh_date:format(FileName)]),
            rebar_utils:ensure_dir(Report),
            {ok, _} = gen_server:start({local, ?MODULE}, ?MODULE, [Report], []);
        _ ->
            ok
    end.

module(Mod, Opts) ->
    Results = proper:module(Mod, Opts),
    catch(gen_server:call(?MODULE, write_report)),
    Results.

%% TODO: implement some kind of listener API (a la eunit_listener) for proper,
%% and use that instead of this fragile checking of format strings.

init([Report]) ->
    {ok, #state{report=Report}}.

handle_call(write_report, _From, State=#state{report=Report, results=Results}) ->
    Writer = xml_writer:file_writer(Report, [write, append]),
    Results2 = lists:foldl(fun results_collector/2, [], Results),
    Writer2 = lists:foldl(fun report_writer/2, Writer, Results2),
    xml_writer:close(Writer2),
    rebar_log:log(debug, "Results: ~p~n", [Results]),
    rebar_log:log(debug, "Results: ~p~n", [Results2]),
    % build_report(Passed, Failed, Output, Writer);
    {stop, normal, State}.

handle_cast(Msg, State) ->
    case catch(do_handle_cast(Msg, State)) of
        {noreply, _}=Response ->
            Response;
        Other ->
            io:format("FAILED! ~p~n", [Other]),
            Other
    end.

do_handle_cast({"Testing ~w:~w/~b~n", [M,F,_]}=Msg,
            State=#state{current_test=Current, results=Results}) ->
    NewResults = case Current of
        undefined -> Results;
        _ -> [Current|Results]
    end,
    {noreply, State#state{current_test={M,F,[Msg], undefined, erlang:now()},
                          results=NewResults}};
do_handle_cast({("OK: Passed" ++ _), _}=Msg,
            State=#state{current_test={_,_,_,_,Ts}=Current}) ->
    Current1 = setelement(4, write(Msg, Current), passed),
    Current2 = setelement(5, Current1, timer:now_diff(erlang:now(), Ts)),
    {noreply,
        State#state{current_test=Current2}};
do_handle_cast({("Failed: " ++ _), _}=Msg,
            State=#state{current_test={_,_,_,_,Ts}=Current}) ->
    Current1 = setelement(4, write(Msg, Current), failed),
    Current2 = setelement(5, Current1, timer:now_diff(erlang:now(), Ts)),
    {noreply, State#state{current_test=Current2}};
do_handle_cast({S,_}=Msg, State=#state{current_test=Current}) ->
    case lists:member(S, ["!", "x", ".", "~n"]) of
        true ->
            {noreply, State};
        false ->
            NewCurrent = write(Msg, Current),
            NewState = State#state{current_test=NewCurrent},
            {noreply, NewState}
    end.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

write(Msg, {_,_,_,_,_}=Current) ->
    setelement(3, Current, [Msg|element(3, Current)]).

report_writer({Mod, _, Fail, Time, TestCases}, Writer) ->
    Attrs = [{"tests", length(TestCases)},
             {"failures", integer_to_list(Fail)},
             {"errors", "0"},
             {"time", io_lib:format("~f", [to_seconds(Time)])},
             {"name", atom_to_list(Mod)}],
    xml_writer:with_element("testsuite", Writer,
        fun(Writer2) ->
            Writer3 = xml_writer:write_attributes(Attrs, Writer2),
            lists:foldl(fun report_writer/2, Writer3, TestCases)
        end
    );
report_writer({TestCase, Time, Output}, Writer) ->
    xml_writer:with_element("testcase", Writer,
        fun(Writer2) ->
            Name = atom_to_list(TestCase),
            Desc = string:join(lists:delete("prop",
                                            string:tokens(Name, "_")), " "),
            Attrs = [{"name", Name},
                     {"description", Desc},
                     {"time", io_lib:format("~f", [to_seconds(Time)])}],
            Writer3 = xml_writer:write_attributes(Attrs, Writer2),
            xml_writer:with_element("system-out", Writer3,
                fun(Writer4) ->
                    lists:foldl(fun({Msg, Args}, W) ->
                                    xml_writer:format(Msg, Args, W)
                                end, Writer4, Output)
                end
            )
        end
    ).
    %    <testsuite tests="2" failures="0" errors="0" skipped="0" time="0.000" name="module 'econfig_server'">
    %      <testcase time="0.004" name="econfig_tests:termconf_backend_test_/0_0" description="it should resolve external function calls in config files">
    %        <system-out>Writing /Users/t4/work/hyperthunk/econfig/.eunit/terms.config
    %
    %        </system-out>
    %      </testcase>
    %      <testcase time="0.001" name="econfig_tests:termconf_backend_test_/0_65" description="it should allow errors from function calls to propagate to the caller">
    %        <system-out>Writing /Users/t4/work/hyperthunk/econfig/.eunit/terms.config
    %
    %        </system-out>
    %      </testcase>
    %    </testsuite>

to_seconds(Time) ->
    Time / 1000000.

results_collector({M, F, Data, failed, Time},
                  [{M, Passed, Failed, AccTime, Output}|Acc]) ->
    [{M, Passed, Failed + 1, AccTime + Time, [{F, Time, Data}|Output]}|Acc];
results_collector({M, F, Data, passed, Time},
                  [{M, Passed, Failed, AccTime, Output}|Acc]) ->
    [{M, Passed + 1, Failed, AccTime + Time, [{F, Time, Data}|Output]}|Acc];
results_collector({M2, _, _, _, _}=E,
                  [{M1, _, _, _, _}|_]=Acc) when M1 /= M2 ->
    rebar_log:log(debug, "Switching from ~p to ~p~n", [M1, M2]),
    [initial_acc(E)|Acc];
results_collector(E, Acc) ->
    rebar_log:log(debug, "Processing ~p/~p~n", [E, Acc]),
    [initial_acc(E)|Acc].

initial_acc({M, F, Data, Status, Time}) ->
    case Status of
        passed ->
            {M, 1, 0, Time, [{F, Time, Data}]};
        failed ->
            {M, 0, 1, Time, [{F, Time, Data}]}
    end.
