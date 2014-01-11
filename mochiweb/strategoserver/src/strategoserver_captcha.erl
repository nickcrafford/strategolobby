%% @author Nick Crafford <nick@webdevengines.com>
%% @copyright 2011 Nick Crafford.

-module(strategoserver_captcha).
-author("Nick Crafford <nickcrafford@gmail.com>").
-export([new/0,check/2,getCryptKey/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Create a new captcha image and key
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
new() ->
  FileName = lists:flatmap(fun(Item) -> integer_to_list(Item) end, tuple_to_list(now())),
  Code     = generate_rand(5),
  File     = io_lib:format("/tmp/~s.png",[FileName]),
  Cmd      = io_lib:format("convert -background 'none' -fill '#FFFFFF' -size 175 -gravity Center -wave 5x100 -swirl 50 -font DejaVu-Serif-Book -pointsize 36 label:~s -draw 'Bezier 10,40 50,35 100,35 150,35 200,50 250,35 350,35' ~s", [Code, File]),

  os:cmd(Cmd),

  {ok, BinPng} = file:read_file(File),
  file:delete(File),

  Sha = crypto:sha_mac(getCryptKey(), integer_to_list(lists:sum(Code)) ++ Code),
  CodeHex = mochihex:to_hex(Sha),
 
  {CodeHex, BinPng}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Check the passed code
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
check(CodeHex, Code) ->
  Sha = mochihex:to_bin(CodeHex),

  case crypto:sha_mac(getCryptKey(), integer_to_list(lists:sum(Code)) ++ Code) of
    Sha ->
      true;
    _ ->
      false
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Generate a random code
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generate_rand(Length) ->
  Now = now(),
  random:seed(element(1, Now), element(2, Now), element(3, Now)),
  lists:foldl(fun(_I, Acc) -> [do_rand(0) | Acc] end, [], lists:seq(1, Length)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Return a random char
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_rand(R) when R > 46, R < 58; R > 64, R < 91; R > 96 ->  
  R;
do_rand(_R) ->
  do_rand(48 + random:uniform(74)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Pull the key used to generate the code
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getCryptKey() ->
  {ok, Key}  = strategoserver_config:getVal(captcha_key),
  Key.
