-module(rabbit_rss_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = []).

init([]) ->
    % We allow 3 restarts within a period of 10 seconds
    {ok, {{one_for_one, 3, 10},
          [{rabbit_rss_worker,
            {rabbit_rss_worker, start_link, []},
            permanent,
            10000,
            worker,
            [rabbit_rss_worker]}
          ]}}.
