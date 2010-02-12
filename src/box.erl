%%%-------------------------------------------------------------------
%%% File    : box.erl
%%% Author  : Olle Mattsson <olle@zubat>
%%% Description : 
%%%
%%% Created :  4 Feb 2010 by Olle Mattsson <olle@zubat>
%%%-------------------------------------------------------------------
-module(box).

-include_lib("wx/include/gl.hrl").
-include("misc.hrl").

-define(TITLE_COLOR, {98,50,74}).
-define(BOX_TITLE_COLOR, {235,225,75}).
-define(BOX_COLOR, {75,60,210}).

-compile(export_all).

create_box(Text) ->
    create_box(Text, [{type, box},
		      {pos, {100,50}},
		      {size, {200,100}},
		      {flags, 0}]).

create_box(Title, Opts) ->
    parse_opts(Opts, #form{type = box, title = Title}).

parse_opts([{pos, Pos={X,Y}}|Opts], B) when is_integer(X),
					    is_integer(Y) ->
    parse_opts(Opts, B#form{pos = Pos});
parse_opts([{size, Size={W,H}}|Opts], B) when is_integer(W),
					      is_integer(H) ->
    parse_opts(Opts, B#form{size = Size});
parse_opts([{flags, Flags}|Opts], B) when is_integer(Flags) ->
    parse_opts(Opts, B#form{flags = Flags});
parse_opts([{type, Type}|Opts], B) when is_atom(Type) ->
    parse_opts(Opts, B#form{type = Type});
parse_opts([], B) ->  B;
parse_opts([Opt|_], _) ->
    throw({invalid_option, Opt}).




draw_form(#form{type = box,
		size = {W,H}, pos = {X,Y},
		title = Title, flags = _Flags}) ->
    gl:pushMatrix(),
    gl:loadIdentity(),

    gl:translatef(X,Y,0),
    gl:'begin'(?GL_QUADS),
    gl:color3ubv(?BOX_TITLE_COLOR),
    gl:vertex2f(0,0),
    gl:vertex2f(W,0),
    gl:vertex2f(W,?BOX_TITLE_HEIGHT),
    gl:vertex2f(0,?BOX_TITLE_HEIGHT),

    gl:color3ubv(?BOX_COLOR),
    gl:vertex2f(0,?BOX_TITLE_HEIGHT),
    gl:vertex2f(0,H),
    gl:vertex2f(W,H),
    gl:vertex2f(W,?BOX_TITLE_HEIGHT),
    gl:'end'(),

    draw_title(Title),
    border(W,H),
    


    gl:color3ubv({255,250,220}),
    gl:rasterPos2i(5, 25),
    font:draw("brodTexT", '6x11'),

    gl:popMatrix();
draw_form(#form{type = button,
		size = {W,H}, pos = {X,Y},
		title = Text, flags = _Flags}) ->
    gl:pushMatrix(),
    gl:loadIdentity(),

    gl:translatef(X,Y,0),
    gl:'begin'(?GL_QUADS),
    gl:color3ubv(?BOX_TITLE_COLOR),
    gl:vertex2f(0,0),
    gl:vertex2f(W,0),
    gl:vertex2f(W,?BOX_TITLE_HEIGHT),
    gl:vertex2f(0,?BOX_TITLE_HEIGHT),
    gl:'end'(),
    
    draw_title(Text),
    border(W,H),
    gl:popMatrix().




can_move(X,Y, #form{pos = {X2, Y2}, size = {W,_}}) ->
    if X > X2,
       X < X2+W,
       Y > Y2,
       Y < Y2+?BOX_TITLE_HEIGHT ->
	    true;
       true -> false
    end.


border(W, H) ->
    gl:'begin'(?GL_LINE_LOOP),
    gl:color3ubv({0,0,0}),
    gl:vertex2f(0,0),
    gl:vertex2f(W,0),
    gl:vertex2f(W,H),
    gl:vertex2f(0,H),
    gl:'end'().

draw_title(Title) ->
    gl:color3ubv(?TITLE_COLOR),
    gl:rasterPos2i(5, 12),
    font:draw(Title, '7x14').



is_set(Flag, Bit) ->
    B = Flag band (1 bsl (Bit-1)),
    B == (1 bsl (Bit-1)).


create_button(Text) ->
    create_button(Text, font:default_font()).

create_button(Text, Font) ->
    W = calc_width(Text, Font) + 10,
    {_,H} = font:get_font_size(Font),
    #form{type = button, title = Text, size = {W,H},
	  flags = 0, pos = {10,10}}.

calc_width(Text) ->
    calc_width(Text, font:default_font()).

calc_width(Text, F) ->
    Font = font:get_font(F),
    W = [font:glyph_width(font:glyph_info(Char, Font)) || Char <- Text],
    Fun = fun(X,Acc) ->
		  X+Acc
	  end,
    lists:foldl(Fun, 0, W).

yes_no_box() ->
    ok.

