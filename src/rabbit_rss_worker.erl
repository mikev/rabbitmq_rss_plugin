-module(rabbit_rss_worker).
-behaviour(gen_server).

-include_lib("xmerl/include/xmerl.hrl").

-export([start/0, start/2, stop/0, stop/1, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("amqp_client/include/amqp_client.hrl").

-record(state, {channel}).

rss_url() ->
    "http://earthquake.usgs.gov/earthquakes/catalogs/eqs1day-M0.xml".

test_retry_interval() ->
    % 20 seconds
    20000.

retry_interval() ->
    % 30 minutes
    1800000.

start() ->
  start_link(),
  ok.

start(normal, []) ->
  start_link().

stop() ->
  ok.

stop(_State) ->
  stop().

start_link() ->
  gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

%---------------------------
% Gen Server Implementation
% --------------------------

init([]) ->
  error_logger:info_report("worker:init"),
  application:start(inets),
  {ok, Connection} = amqp_connection:start(direct),
  {ok, Channel} = amqp_connection:open_channel(Connection),

  %% Declare a queue
  #'queue.declare_ok'{queue = Q} = amqp_channel:call(Channel, #'queue.declare'{queue = <<"test_queue">>}),

  timer:apply_after(retry_interval(), gen_server, call, [{global, ?MODULE}, fire]),
  {ok, #state{channel = Channel}}.

handle_call(Msg,_From,State = #state{channel = Channel}) ->
  case Msg of
    fire ->

      error_logger:info_report("rabbit_rss_worker:handle_call fire"),

      % Retrieve XML from RSS feed
      { ok, {_Status, _Headers, Body }} = httpc:request(rss_url()),

      % Parse the XML into a dedup'd list of individual items
      MList = rss:parse(Body),

      % Publish each XML item into an AMQP queue
      [amqp_publish( Channel, X ) || X <- MList],

      % Set our timer
      timer:apply_after(retry_interval(), gen_server, call, [{global, ?MODULE}, fire]),
      error_logger:info_report("rabbit_rss_worker:handle_call fire OK"),
      {reply, ok, State};
    _ ->
      error_logger:error_report("rabbit_rss_worker:handle_call fire NOT OK"),
      {reply, unknown_command, State}
  end.

amqp_publish( Channel, Message ) ->

      Export=xmerl:export_simple([Message],xmerl_xml),
      XMessage = lists:flatten(Export),
      error_logger:info_report(XMessage),

      Properties = #'P_basic'{content_type = <<"text/plain">>, delivery_mode = 1},
      BasicPublish = #'basic.publish'{exchange = <<>>,
                                      routing_key = <<"test_queue">>},
      Content = #amqp_msg{props = Properties, payload = XMessage},
      amqp_channel:call(Channel, BasicPublish, Content).

handle_cast(_,State) ->
    {noreply,State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_,#state{channel = Channel}) ->
    amqp_channel:call(Channel, #'channel.close'{}),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
