-module(escriptize_entry).
-export([main/1]).

main(Args) ->
    % We need to unpack ourselves, mess with the code path, and then invoke the
    % original entry point.
    TempDir = get_temp_dir(),
    extract_self(TempDir),
    add_paths(TempDir),
    invoke_entry(Args).

extract_self(TempDir) ->
    ok = filelib:ensure_dir(TempDir ++ "/PLACEHOLDER"),

    Path = filename:dirname(code:which(?MODULE)),
    {ok, Bin} = file:read_file(Path),
    ZipBin = split_zip(Bin),
    {ok, _Files} = zip:extract(ZipBin, [{cwd, TempDir}]).

add_paths(TempDir) ->
    lists:foreach(
     fun(P) ->
             code:add_patha(filename:absname(filename:join(TempDir, P)))
     end, filelib:wildcard("*/ebin", TempDir)).

invoke_entry(Args) ->
    % escript:script_name/0 returns the (possibly fully-qualified) path to the
    % script that was invoked.
    Script = escript:script_name(),
    Mod = filename:basename(Script),
    erlang:apply(list_to_atom(Mod), main, [Args]).

get_temp_dir() ->
    Script = escript:script_name(),
    Mod = filename:basename(Script),
    Dir = filename:dirname(Script),
    filename:join(Dir, "." ++ Mod).

split_zip(Bin) ->
    Pos = find_zip(Bin),
    <<_Prefix:Pos/binary, ZipBin/binary>> = Bin,
    ZipBin.

find_zip(Bin) ->
    {Pos, _} = binary:match(Bin, <<"PK">>),
    Pos.
