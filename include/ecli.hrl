
%% Defininition for creating command trees

%% A tree of #cmd{} records is used to define CLI menus and drive completion.
%% The final #cmd{} that has a data schema as it's children determines
%% the behaviour of completion within the data. This impacts list items
%% where a "set" command might want to prompt the user to create a new list entry

-record(cmd,
    {
       name :: string(),
       desc = "" :: string(),
       action :: fun((term(), [atom()], term()) -> ok | {error, string()}),
       children = fun() -> [] end :: fun(() -> list()),
       list_action = show :: show | set
    }).