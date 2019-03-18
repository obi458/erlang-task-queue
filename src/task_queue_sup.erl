-module(task_queue_sup).

-behaviour(supervisor).

%% API
-export([
        start_link/1,
        child_spec/4
    ]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(Name) ->
    supervisor:start_link({local, Name}, ?MODULE, []).

child_spec(Id, Mod, Type, Args) ->
    {Id, {Mod, start_link, Args}, transient, 500, Type, [Mod]}.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    {ok, {{one_for_all, 0, 1}, [] }}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

