-module(var_supervisor).
-behaviour(supervisor).
-export([start_link/1, init/1]).

start_link(InitValue) -> 
    supervisor:start_link( {local, varSupervisor}, ?MODULE, InitValue ).

% one_for_one, one_for_all, rest_for_one
init(InitValue) ->
    SupFlags = #{ strategy => one_for_one, intensity => 2, period => 3 },

    ChildA = #{ id => 'var_srv', start => {var_server, start_link, [InitValue]}, restart => permanent, shutdown => 2000, type => worker, modules => [var_server] },
    ChildSpecList = [ChildA],
    
    {ok, {SupFlags, ChildSpecList}}. 

%% SupervisorSpec = {SupFlags, ChildSpecList}