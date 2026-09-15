
%% Defininition for creating command trees

%% A tree of #cmd{} records is used to define CLI menus and drive completion.
%% The final #cmd{} that has a data schema as it's children determines
%% the behaviour of completion within the data. This impacts list items
%% where a "set" command might want to prompt the user to create a new list entry

-record(cmd,
    {
       name :: string(),
       desc = "" :: string(),
       action :: undefined | fun() | {pipe, term()},
       children = fun() -> [] end :: fun(() -> list()) | fun((term()) -> list()) | list(),
       list_action = show :: show | set,
       %% Pipe catalog for `|` after this command and its descendants.
       %% `undefined` inherits from the parent; `[]` disables pipes.
       pipes = undefined :: undefined | list() | fun(() -> list()) | fun((term()) -> list()),
       %% Who may see and run this command. `ecli:permit/2` filters on it.
       %% `any` — every session; `read` — show; `write` — configure / set / commit.
       access = any :: any | read | write
    }).