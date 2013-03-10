-define(emergency, emergency). % system is unusable
-define(alert,     alert). % action must be taken immediately
-define(critical,  critical). % critical conditions
-define(error,     error). % error conditions
-define(warning,   warning). % warning conditions
-define(notice,    notice). % normal but significant condition
-define(info,      info). % informational
-define(debug,     debug). % debug-level messages

-define(LOGMOD, lager).
-define(LOG(Format, Args, Level, Tags),
        lager:Level(Tags, "~p [~p:~p] "++Format, [self(), ?MODULE, ?LINE]++Args)).

-define(LOG_CLEAR(Format, Args, Level, Tags), lager:Level(Tags, Format, Args)).

-define(SAFE(X), (try
                      X
                  catch
                      Class:Reason ->
                          ?DBG("~s failed with ~p:~p in ~p", [??X, Class, Reason, erlang:get_stacktrace()])
                  end)).

-define(SAFE(X, A), (try
                         X
                     catch
                         Class:Reason ->
                             ?DBG("~s with params ~p failed with ~p:~p in ~p", [??X, A, Class, Reason, erlang:get_stacktrace()])
                     end)).

-define(DBG(Format, Args, Tag), ?LOG(Format, Args, ?debug, Tag)).
-define(DBG(Format, Args),      ?DBG(Format, Args, [])).
-define(DBG(Format),            ?DBG(Format, [])).

-define(INFO(Format, Args, Tag), ?LOG(Format, Args, ?info, Tag)).
-define(INFO(Format, Args),      ?INFO(Format, Args, [])).
-define(INFO(Format),            ?INFO(Format, [])).

-define(NOTICE(Format, Args, Tag), ?LOG(Format, Args, ?notice, Tag)).
-define(NOTICE(Format, Args),      ?NOTICE(Format, Args, [])).
-define(NOTICE(Format),            ?NOTICE(Format, [])).

-define(WARNING(Format, Args, Tag), ?LOG(Format, Args, ?warning, Tag)).
-define(WARNING(Format, Args),      ?WARNING(Format, Args, [])).
-define(WARNING(Format),            ?WARNING(Format, [])).

-define(ERROR(Format, Args, Tag), ?LOG(Format, Args, ?error, Tag)).
-define(ERROR(Format, Args),      ?ERROR(Format, Args, [])).
-define(ERROR(Format),            ?ERROR(Format, [])).

-define(CRITICAL(Format, Args, Tag), ?LOG(Format, Args, ?critical, Tag)).
-define(CRITICAL(Format, Args),      ?CRITICAL(Format, Args, [])).
-define(CRITICAL(Format),            ?CRITICAL(Format, [])).

-define(ALERT(Format, Args, Tag), ?LOG(Format, Args, ?alert, Tag)).
-define(ALERT(Format, Args),      ?ALERT(Format, Args, [])).
-define(ALERT(Format),            ?ALERT(Format, [])).

-define(EMERGENCY(Format, Args, Tag), ?LOG(Format, Args, ?emergency, Tag)).
-define(EMERGENCY(Format, Args),      ?EMERGENCY(Format, Args, [])).
-define(EMERGENCY(Format),            ?EMERGENCY(Format, [])).

-define(LOG_FILE(Class, Format, Args, Tag), ?LOG_CLEAR("log: ~s " ++ Format, [Class | Args], ?info, [clean | Tag])).
-define(LOG_FILE(Format, Args, Tag),        ?LOG_CLEAR(Format, Args, ?info, [raw | Tag])).
