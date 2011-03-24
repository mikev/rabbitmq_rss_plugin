{application, rabbit_rss,
 [{description, "Embedded Rabbit RSS Reader"},
  {vsn, "0.01"},
  {modules, [
    rabbit_rss,
    rabbit_rss_sup,
    rabbit_rss_worker,
    rss
  ]},
  {registered, []},
  {mod, {rabbit_rss, []}},
  {env, []},
  {applications, [kernel, stdlib, rabbit, amqp_client]}]}.
