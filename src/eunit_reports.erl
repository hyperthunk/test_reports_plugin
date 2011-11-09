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
-module(eunit_reports).
-export([before_test/2]).

before_test(Config, _File) ->
    EUnitOpts = rebar_config:get_local(Config, eunit_opts, []),
    rebar_log:log(debug, "[~p] processing eunit config in ~s~n",
                  [?MODULE, EUnitOpts, rebar_utils:get_cwd()]),
    rebar_log:log(debug, "[~p] processing ~p~n", [?MODULE, EUnitOpts]),
    [ sfprep(Opts) || {report, {eunit_surefire, Opts}} <- EUnitOpts ],
    ok.

sfprep(Opts) ->
    case lists:keyfind(dir, 1, Opts) of
        false ->
            ok;
        {dir, Dir} ->
            Path = filename:join([rebar_utils:get_cwd(), Dir, "foo.xml"]),
            rebar_utils:ensure_dir(Path)
    end.
