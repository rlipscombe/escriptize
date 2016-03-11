-module(escriptize).
-export([main/1]).

-include_lib("kernel/include/file.hrl").
-define(ENTRY_MODULE, escriptize_entry).

main([]) ->
    io:format("./escriptize output~n"),
    halt(1);

main([OutputPath] = _Args) ->
    Main = OutputPath,

    % We need specific paths in the ZIP file:
    % The main application needs, e.g.,
    %   {"hello/ebin/hello.beam", "ebin/hello.beam"}
    InputPaths0 = [{filename:join([Main, F]), F}
                   || F <- filelib:wildcard("ebin/*.beam")],

    % Then we need to add the deps, excluding ourselves, in case we're in the
    % deps directory.
    Deps = filelib:wildcard("*", "deps") -- ["escriptize"],
    InputPaths1 = lists:flatmap(
                   fun(Dep) ->
                           get_dep_entries(Dep)
                   end, Deps),

    % We need to inject the entry point; lift it from ourselves:
    {?ENTRY_MODULE, EntryBin, _} = code:get_object_code(?ENTRY_MODULE),
    EntryEntry = {atom_to_list(?ENTRY_MODULE) ++ ".beam", EntryBin},

    InputPaths = InputPaths0 ++ InputPaths1,
    ZipEntries = create_zip_entries(InputPaths),
    {ok, ZipBin} = create_zip([EntryEntry | ZipEntries]),

    EmuArgs = io_lib:format("-escript main ~s", [?ENTRY_MODULE]),
    Sections = [shebang,
                {emu_args, EmuArgs},
                {archive, ZipBin}],
    {ok, Bin} = escript:create('binary', Sections),
    ok = file:write_file(OutputPath, Bin),
    {ok, #file_info{mode = Mode}} = file:read_file_info(OutputPath),
    ok = file:change_mode(OutputPath, Mode bor 8#00111).

% Search deps/Dep/ebin and deps/Dep/priv for files to include.
% Transform them to zip entries, e.g.:
%   {"mochijson2/ebin/mochijson.beam", "deps/mochijson2/ebin/mochijson2.beam"}
get_dep_entries(Dep) ->
    DepDir = filename:join("deps", Dep),
    [{filename:join(Dep, F), filename:join(DepDir, F)}
     || F <- filelib:wildcard("ebin/*.beam", DepDir) ++ filelib:wildcard("priv/*", DepDir)].

create_zip_entries(InputPaths) ->
    lists:map(fun({Path, InputPath}) ->
                      {ok, Bin} = file:read_file(InputPath),
                      {Path, Bin}
              end, InputPaths).

create_zip(ZipEntries) ->
    {ok, {"mem", ZipBin}} = zip:create("mem", ZipEntries, [memory]),
    {ok, ZipBin}.
