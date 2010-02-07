%%%-------------------------------------------------------------------
%%% File    : graphics.erl
%%% Author  : Olle Mattsson <olle@zubat>
%%% Description : 
%%%
%%% Created : 10 Nov 2009 by Olle Mattsson <olle@zubat>
%%%-------------------------------------------------------------------
-module(gr).

-compile(export_all).

-include_lib("wx/include/wx.hrl").
-include_lib("wx/include/gl.hrl").
-include("tibia.hrl").



test() ->
    load_sprite_metadata("Tibia.dat"),
    load_sprite_data("Tibia.spr").

get_sprite(Num) ->
    [Spr] = ets:lookup(sprites, Num),
    Spr.

load_sprite_metadata(File) ->
    {ok, Data0} = file:read_file(File),
    io:format("Loading metadata ~p ..", [File]),
    <<DatVer:32/?UINT,
     ItemCount:16/?UINT,
     CreatureCount:16/?UINT,
     _EffectCount:16/?UINT,
     _DistanceCount:16/?UINT,Data/binary>> = Data0,
    
    MinClientID = 100,
    MaxClientID = ItemCount + CreatureCount,
    
    ets:new(sprite_id, [named_table,public,ordered_set, {keypos, #gameSprite.id}]),
    %%io:format("~p ~p ~p\n", [DatVer, 16#439d5a33, DatVer > 16#439d5a33]),

    if DatVer > 16#439d5a33 ->
	    load_metadata76(Data, MinClientID, MaxClientID),
	    io:format("Done.\n", []);
       true ->
	    ok
    end.

skip_bytes(Bytes, Data) ->
    <<_:Bytes/binary, Rest/binary>> = Data,
    Rest.
		


load_metadata76(Data, ID, MaxID) when ID =< MaxID ->
    {Data2,Sprite} = game_sprite76(Data, #gameSprite{}),
    <<Width:8/?UINT,Height:8/?UINT,Rest/binary>> = Data2,
    if Width > 1; Height > 1 ->
	    Rest2 = skip_bytes(1,Rest);
       true ->
	    Rest2 = Rest
    end,
    <<Frames,XDiv,YDiv,ZDiv,AnimationLen,Rest3/binary>> = Rest2,
    NumSprites = Width*Height*Frames*XDiv*YDiv*ZDiv*AnimationLen,
    {Rest4, SpriteList} = get_animation(Rest3, NumSprites),
    %%io:format("~p ~p ~p\n", [{Width,Height,Frames,XDiv,YDiv,ZDiv,AnimationLen}, NumSprites, ID]),
    ets:insert(sprite_id,Sprite#gameSprite{id = ID,
					   width = Width,
					   height = Height,
					   sprite_list = SpriteList,
					   frames = Frames,
					   xdiv = XDiv,
					   ydiv = YDiv,
					   zdiv = ZDiv,
					   animation_length = AnimationLen,
					   num_sprites = NumSprites}),
    load_metadata76(Rest4, ID +1, MaxID);
load_metadata76(Data, _,_) ->
    Data.

get_animation(Data, Frames) ->
    get_animation(Data, Frames, []).

get_animation(<<SpriteID:16/?UINT,Data/binary>>, Frames, Acc) when Frames > 0 ->
    get_animation(Data,Frames-1, [SpriteID|Acc]);
get_animation(Data, 0, Acc) ->
    {Data, lists:reverse(Acc)}.
    
    

game_sprite76(<<16#00,Data/binary>>, Item) ->
    game_sprite76(skip_bytes(2, Data), Item);
game_sprite76(<<16#09,Data/binary>>, Item) ->
    game_sprite76(skip_bytes(2, Data), Item);
game_sprite76(<<16#0A,Data/binary>>, Item) ->
    game_sprite76(skip_bytes(2, Data), Item);
game_sprite76(<<16#16,Data/binary>>, Item) ->
    game_sprite76(skip_bytes(4, Data), Item);
game_sprite76(<<16#19,Data/binary>>, Item) ->
    <<X:16/?UINT,Y:16/?UINT,Rest/binary>> = Data,
    Item2 = Item#gameSprite{drawoffset_x = X,
			    drawoffset_y = Y},
    game_sprite76(Rest, Item2);
game_sprite76(<<16#1A,Data/binary>>, Item) ->
    <<DrawHeight:16/?UINT,Rest/binary>> = Data,
    Item2 = Item#gameSprite{draw_height = DrawHeight},
    game_sprite76(Rest, Item2);
game_sprite76(<<16#1D,Data/binary>>, Item) ->
    <<MinimapColor:16/?UINT,Rest/binary>> = Data,
    Item2 = Item#gameSprite{minimap_color = MinimapColor},
    game_sprite76(Rest, Item2);
game_sprite76(<<16#1E,Data/binary>>, Item) ->
    game_sprite76(skip_bytes(2, Data), Item);
game_sprite76(<<16#FF,Data/binary>>, Item) ->
    {Data, Item};
game_sprite76(<<_,Data/binary>>, Item) ->
    game_sprite76(Data, Item);
game_sprite76(<<>>, Item) ->
    {<<>>, Item}.






load_sprite_data(File) ->
    {ok, Data} = file:read_file(File),
    io:format("Loading data ~p ..", [File]),
    ets:new(sprites, [named_table, public, ordered_set, {keypos, #sprite.id}]),
    <<_SpriteVer:32/?UINT,TotalPics:16/?UINT,Rest/binary>> = Data,
    put(totalPics, TotalPics),
    Index = read_sprite_index(Rest, TotalPics),
    read_sprites(Data, Index, 1),
    io:format("Done.\n", []).



read_sprite_index(Data, TotalPics) ->
    read_sprite_index(Data, TotalPics, []).

read_sprite_index(<<Index:32/?UINT,Data/binary>>, TotalPics, Acc) when TotalPics > 0 ->
    read_sprite_index(Data, TotalPics-1, [Index|Acc]);
read_sprite_index(_Data, 0, Acc) ->
    lists:reverse(Acc).

read_sprites(Data, [Index|Indexes], ID) when Index > 0 ->
    SkipBytes = Index+3,
    <<_:SkipBytes/binary,Size:16/?UINT,Sprite:Size/binary,_/binary>> = Data,
    Spr = #sprite{size = Size,
		  id = ID,
		  data = Sprite},
    ets:insert(sprites, Spr),
    read_sprites(Data, Indexes, ID+1);
read_sprites(Data, [_|Indexes], ID) ->
    read_sprites(Data, Indexes, ID+1);
read_sprites(_, [], _) ->
    ok.


get_rgb_data(#sprite{data = Data, size = Size}) ->
    {Acc, Pos} = get_bytes(Data, Size,{0,0}, rgb, <<>>),
    fill_trailing_pixels(rgb, Acc, Pos).

get_rgba_data(#sprite{data = Data, size = Size}) ->
    {Acc, Pos} = get_bytes(Data, Size,{0,0}, rgba, <<>>),
    fill_trailing_pixels(rgba, Acc, Pos).



fill_trailing_pixels(RGB,Acc, {X,Y}) when X<32, Y<32 ->
    case RGB of
	rgb ->
	    Acc2 = <<Acc/binary,16#FF,16#00,16#FF>>;
	rgba ->
	    Acc2 = <<Acc/binary,16#00,16#00,16#00,16#00>>,
	    Acc2
    end,
    if X+1 >= 32 ->
	    fill_trailing_pixels(RGB, Acc2, {0,Y+1});
       true ->
	    fill_trailing_pixels(RGB, Acc2, {X+1,Y})
    end;
fill_trailing_pixels(_RGB, Acc,_) ->
    Acc.



get_bytes(Data, Size, Pos, RGB, Acc) when Size > 0 ->
    <<ChunkSize:16/?UINT, Rest/binary>> = Data,
    io:format("Size: ~p  ~p  ~p\n", [byte_size(Data), Size, ChunkSize]),
    {Transparent, Pos2} = get_transparent_pixel(RGB, ChunkSize, Pos,<<>>),
    if Size - ChunkSize =< 0 ->
	    io:format("~p\n", [Pos2]),
	    {<<Acc/binary,Transparent/binary>>, Pos2};
       true ->
	    <<ChunkSize2:16/?UINT,Rest2/binary>> = Rest,
	    {Rest3, Pixels,Pos3} = get_pixel(Rest2, ChunkSize2, Pos2,RGB, <<>>),
	    get_bytes(Rest3,Size - 4 - ChunkSize2*3,Pos3, RGB,
		      <<Acc/binary,Transparent/binary,Pixels/binary>>)
    end;
get_bytes(_,_,Pos,_RGB,Acc) ->
    io:format("~p\n", [Pos]),
    {Acc,Pos}.

get_pixel(Data, ChunkSize, {X,Y}, RGB, Acc) when ChunkSize > 0 ->
    <<RGBData:3/binary,Rest/binary>> = Data,
    case RGB of
	rgb ->
	    Acc2 = <<Acc/binary,RGBData/binary>>;
	rgba ->
	    Acc2 = <<Acc/binary,RGBData/binary,16#FF>>,
	    Acc2
    end,
    if X+1 >= 32 ->
	    get_pixel(Rest,ChunkSize -1, {0, Y+1}, RGB, Acc2);
       true ->
	    get_pixel(Rest,ChunkSize -1, {X+1, Y}, RGB, Acc2)
    end;
get_pixel(Data, _,Pos,_RGB,Acc) ->
    {Data, Acc, Pos}.



get_transparent_pixel(RGB, ChunkSize, {X, Y}, Acc) when ChunkSize > 0 ->
    case RGB of
	rgb ->
	    Acc2 = <<Acc/binary,16#FF,16#00,16#FF>>;
	rgba ->
	    Acc2 = <<Acc/binary,16#00,16#00,16#00,16#00>>,
	    Acc2
    end,

    if X+1 >= 32 ->
	    get_transparent_pixel(RGB, ChunkSize -1, {0, Y+1}, Acc2);
       true ->
	    get_transparent_pixel(RGB, ChunkSize -1, {X+1, Y}, Acc2)
    end;
get_transparent_pixel(_RGB, 0,Pos,Acc) ->
    {Acc, Pos}.
  


get_image(_GS=#gameSprite{width = Width, height = Height, sprite_list = [Sprite|_SL]}) ->
    if Width =:= 1,
       Height =:= 1 ->
	    case ets:lookup(sprites, Sprite) of
		[] ->
		    false;
		[S] ->
		    RGB32x32 = get_rgb_data(S),
		    Alpha32x32 = gen_alpha(RGB32x32),
		    Img = wxImage:new(32,32,RGB32x32),
		    wxImage:setAlpha(Img, Alpha32x32),
		    Img
	    end;
       true ->
	    false
    end.

gen_alpha(Data) ->
    gen_alpha(Data, <<>>).

gen_alpha(<<255,0,255,Rest/binary>>, Acc) ->
    gen_alpha(Rest, <<Acc/binary,0>>);
gen_alpha(<<_,_,_,Rest/binary>>, Acc) ->
    gen_alpha(Rest, <<Acc/binary,255>>);
gen_alpha(<<>>, Acc) ->
    Acc.




create_gl_texture(S) ->
    RGBA = get_rgba_data(S),

    gl:bindTexture(?GL_TEXTURE_2D, S#sprite.id),
    gl:texImage2D(?GL_TEXTURE_2D, 0, ?GL_RGBA, 32, 32, 0, ?GL_RGBA, ?GL_UNSIGNED_BYTE, RGBA),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, 16#2802, 16#812F),
    gl:texParameteri(?GL_TEXTURE_2D, 16#2803, 16#812F),
    ets:insert(sprites,S#sprite{tid = true, last_access = erlang:now()}).

delete_gl_texture(#sprite{tid = false}) ->
    ok;
delete_gl_texture(S = #sprite{tid = true, id = Id}) ->
    gl:deleteTextures(Id),
    ets:insert(sprites, S#sprite{tid = false}).
    
