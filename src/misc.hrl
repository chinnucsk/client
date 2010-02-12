%%%-------------------------------------------------------------------
%%% File    : misc.hrl
%%% Author  : Olle Mattsson <olle@zubat>
%%% Description : 
%%%
%%% Created :  4 Feb 2010 by Olle Mattsson <olle@zubat>
%%%-------------------------------------------------------------------


-define(BOX_TITLE_HEIGHT, 15).

-record(form, {type,size,pos,title,flags}).

-record(texture, {id,
		  tid,
		  last_access,
		  size,
		  data}).
