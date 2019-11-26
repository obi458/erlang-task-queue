-module(task_queue).

-export([
        behaviour_info/1,
        start/2,
        start/3,
        start_link/2,
        start_link/3,
        stop/1,
        in/2,
        in_r/2,
        unique_tasks/1,
        len/1,
        is_empty/1
    ]).

behaviour_info(callbacks) ->
    [
        {init, 1},
        {process_task, 2},
        {terminate, 2},
        {code_change, 3}
    ];

behaviour_info(_Other) ->
    undefined.

start(Module, Args) ->
    start(Module, Args, []).

start(Module, Args, Options) ->
    start_link(Module, Args, [{unlink, true} | Options]).

start_link(Module, Args) ->
    start_link(Module, Args, []).

start_link(Module, Args, Options) 
        when is_atom(Module), is_list(Options) ->
    RegName = regname(Module,Options),
    {ok, TaskQueueSup} = task_queue_sup:start_link(task_queue_sup_name(RegName)),
    {ok, TaskManager} =
        supervisor:start_child(
            TaskQueueSup,
            task_queue_sup:child_spec(
                manager_name(RegName), task_queue_manager, worker, [ Options ])),

    NewOptions = [
            {task_manager, TaskManager},
            {worker_module, Module},
            {worker_module_args, Args}
        | Options],

    {ok, _WorkersSup} =
        supervisor:start_child(
            TaskQueueSup,
            task_queue_sup:child_spec(
                workers_sup_name(RegName), task_queue_workers_sup, supervisor, [ NewOptions ])),

    case proplists:get_value(unlink, Options, false) of
        true -> erlang:unlink(TaskQueueSup);
        false -> ok
    end,

    erlang:register(regname(Module,Options),TaskManager),
    {ok, TaskQueueSup}.
    %{ok, TaskManager}.

stop(TaskQueue) ->
    true = erlang:exit(TaskQueue, shutdown).

in(Task, TaskQueue) ->
    gen_server:cast(TaskQueue, {in, Task}).

in_r(Task, TaskQueue) ->
    gen_server:cast(TaskQueue, {in_r, Task}).

unique_tasks(TaskQueue) ->
    gen_server:call(TaskQueue, unique_tasks).

len(TaskQueue) ->
    gen_server:call(TaskQueue, len).

is_empty(TaskQueue) ->
    len(TaskQueue) =:= 0.

manager_name(N) ->
  list_to_atom("task_manager_" ++ atom_to_list(N)).

workers_sup_name(N) ->
  list_to_atom("task_workers_sup_" ++ atom_to_list(N)).

task_queue_sup_name(N) ->
  list_to_atom("task_queue_sup_" ++ atom_to_list(N)).

regname(Mod,Options) ->
  case proplists:get_value(register,Options) of
    undefined -> Mod;
    Name -> Name
  end.



