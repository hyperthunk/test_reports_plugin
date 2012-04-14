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
-module(test_reports_plugin).
-export([preprocess/2, postprocess/2]).

preprocess(Config, File) ->
    process(fun(Mod) -> Mod:before_test(Config, File) end).

postprocess(Config, File) ->
    process(fun(Mod) ->
                case erlang:function_exported(Mod, after_test, 2) of
                    true ->
                        Mod:after_test(Config, File);
                    false ->
                        ok
                end
            end).

process(Callback) ->
    BaseDir = rebar_config:get_global(base_dir, []),
    DepsDir = filename:join(BaseDir, rebar_config:get_global(deps_dir, "deps")),
%    rebar_log:log(debug, "Checking if ~s is a subdir of ~s~n", 
%                 [rebar_utils:get_cwd(), DepsDir]),
    case lists:prefix(DepsDir, rebar_utils:get_cwd()) of
        false ->
            Command = rebar_utils:command_info(current),
            Handler = list_to_atom(atom_to_list(Command) ++ "_reports"),
            case code:ensure_loaded(Handler) of
                {module, Mod} ->
                    Callback(Mod);
                Other ->
                    rebar_log:log(debug, "WTF: ~p~n", [Other]),
                    ok
            end;
        true ->
            ok
    end,
    {ok, []}.
