%% @author Nick Crafford <nick@webdevengines.com>
%% @copyright 2011 Nick Crafford.

% Code yanked from: 
% http://www.planeterlang.org/en/planet/article/How_to_send_email_via_Gmail_using_Erlang/

-module(email_util).
-export([send_email/3,spawn_email/3]).

spawn_email(ToAddress, Subject, Body) ->
  spawn(
    fun() ->
      send_email(ToAddress, Subject, Body)
    end
  ).

send_email(ToAddress, Subject, Body) ->
  {ok, GmailUsername} = strategoserver_config:getVal(email_username),
  {ok, GmailPassword} = strategoserver_config:getVal(email_password),

  {{Year,Month,Day},{Hour,Min,Seconds}} =  erlang:localtime(),
  
  {Today,_} = erlang:universaltime(),
  DayOfWeek = calendar:day_of_the_week(Today),  
  DayName   = httpd_util:day(DayOfWeek),
  MonthName = httpd_util:month(Month),

  {ok, Socket} = ssl:connect("smtp.gmail.com", 465, [{active, false}], 5000),
  recv(Socket),
  send(Socket, "HELO localhost"),
  send(Socket, "AUTH LOGIN"),
  send(Socket, binary_to_list(base64:encode(GmailUsername))),
  send(Socket, binary_to_list(base64:encode(GmailPassword))),
  send(Socket, "MAIL FROM: <"++GmailUsername++">"),
  send(Socket, "RCPT TO:<"++ToAddress++">"),
  send(Socket, "DATA"),
  send_no_receive(Socket, "From: <"++GmailUsername++">"),
  send_no_receive(Socket, "To: <"++ToAddress++">"),
  send_no_receive(Socket, "Date: "++DayName++", "++integer_to_list(Day)++
                          " "++MonthName++" "++integer_to_list(Year)++
                          " "++integer_to_list(Hour)++
                          ":"++integer_to_list(Min)++
                          ":"++integer_to_list(Seconds)++" -0800"),
  send_no_receive(Socket, "Subject: " ++ Subject),
  send_no_receive(Socket, ""),
  send_no_receive(Socket, Body),
  send_no_receive(Socket, ""),
  send(Socket, "."),
  send(Socket, "QUIT"),
  ssl:close(Socket).

send_no_receive(Socket, Data) ->
  ssl:send(Socket, Data ++ "\r\n").

send(Socket, Data) ->
  ssl:send(Socket, Data ++ "\r\n"),
  recv(Socket).

recv(Socket) ->
  case ssl:recv(Socket, 0, 5000) of
  {ok, _} -> ok;
  {error, Reason} -> io:format("ERROR: ~p~n", [Reason])
  end.
