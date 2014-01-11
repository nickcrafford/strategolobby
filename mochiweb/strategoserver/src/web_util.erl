%% @author Nick Crafford <nick@webdevengines.com>
%% @copyright 2011 Nick Crafford.

-module(web_util).
-author("Nick Crafford <nickcrafford@gmail.com>").
-export([trimSlashes/1, returnMsg/3, returnMsg/4, respondWithTemplate/6, renderFromTemplate/3]).
-define(TEMPLATE_PATH, "templates/").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Send the HTTP response with output from a ErlyDtl Template
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
respondWithTemplate(Req, TemplatePath, TemplateAtom, Title, Data, Code) ->
  NData    = Data ++ [{page_title, Title}],
  Out = renderFromTemplate(TemplatePath, TemplateAtom, NData),
  Req:respond({Code, [{"Content-Type", "text/html"}],Out}).

renderFromTemplate(TemplatePath, TemplateAtom, Data) ->
  erlydtl:compile(string:concat(?TEMPLATE_PATH, TemplatePath), TemplateAtom),
  {ok,Out} = TemplateAtom:render(Data),
  Out.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Strip the trailing and leading slashes for a URL path
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
trimSlashes(Path) ->
  Len = length(Path),
  if
    Len > 1 -> FirstRemoved = string:substr(Path, 2, Len),
               FRLen        = length(FirstRemoved),
               LastChar     = string:substr(FirstRemoved, FRLen, FRLen),
               case string:equal(LastChar,"/") of
                 true  -> string:substr(FirstRemoved,1, FRLen-1);
                 false -> FirstRemoved
               end;
    true -> ""
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Return a message ready for rendering via JSON
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
returnMsg(Status, Message, Type) ->
  returnMsg(Status, Message, Type, []).
returnMsg(Status, Message, Type, Data) ->
  NData = [{status,  Status },
           {message, Message},
           {type,    Type   }] ++ Data,
  {struct,  NData}.
