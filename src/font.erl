%%%-------------------------------------------------------------------
%%% File    : font.erl
%%% Author  : Olle Mattsson <olle@zubat>
%%% Description : 
%%%
%%% Created : 30 Jan 2010 by Olle Mattsson <olle@zubat>
%%%-------------------------------------------------------------------
-module(font).

-export([load_font/1,draw/1,draw/2,bold/1,get_font/1,
	 glyph_info/2,glyph_width/1,get_font_size/1,
	 get_loaded_fonts/0, default_font/0]).


load_font(File) ->
    {ok, Bin} = file:read_file(File),
    {wings_font, _,{Font, _,W,H,Chars,Bitmaps}} = binary_to_term(Bin),
    F = ets:new(font, [ordered_set]),
    ets:insert(F, Chars),
    
    ets:insert(fonts, {Font, F, {W,H}, Bitmaps}),
    F.


draw([C|T]) ->
    char(C),
    draw(T);
draw([]) -> ok.

draw([C|T], Font) ->
    char(C, Font),
    draw(T, Font);
draw([], _Font) -> ok.

bold([C|T]) ->
    bold_char(C),
    bold(T);
bold([]) -> ok.


bold_char(C) ->
    Glyph = glyph_info(C, get_font(default_font())),
    draw_glyph(Glyph),
    Cw = glyph_width(Glyph),
    gl:bitmap(1, 1, 0, 0, -Cw+1, 0, <<0>>),
    draw_glyph(Glyph).

char(C) ->
    draw_glyph(glyph_info(C, get_font(default_font()))).

char(C, Font) ->
    draw_glyph(glyph_info(C, get_font(Font))).

draw_glyph({W,H,Xorig,Yorig,Xmove,B}) ->
    gl:bitmap(W, H, Xorig, Yorig, Xmove, 0, B).


glyph_info(C, {Table, {Width,Height}, Bitmaps}) ->
    case ets:lookup(Table, C) of
	[] when is_integer(C), C > 0 ->
	    %% Undefined character. Return a filled box.
	    NumBytes = ((Width+7) div 8) * Height,
	    B = <<(-1):NumBytes/unit:8>>,
	    {Width,Height,0,0,Width+1,B};
	[{_,W,H,Xorig,Yorig,Xmove,Offset}] ->
	    %% Valid character.
	    NumBytes = ((W+7) div 8)*H,
	    <<_:Offset/binary,B:NumBytes/binary,_/binary>> = Bitmaps,
	    {W,H,Xorig,Yorig,Xmove,B}
    end.

glyph_width({_,_,_,_,Xmove,_}) -> Xmove.


default_font() ->
    '7x14'.

get_font(Font) ->
    case ets:lookup(fonts, Font) of
	[] ->
	    throw({no_font, Font});
	[{Font, Table, Size, Bitmaps}] ->
	    {Table, Size, Bitmaps}
    end.

get_font_size(Font) ->
    {_,Size,_} = get_font(Font),
    Size.

get_loaded_fonts() ->
    Fonts = lists:append(ets:match(fonts, '$1')),
    [Font || {Font,_,_,_} <- Fonts].
    
