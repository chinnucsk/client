%%%-------------------------------------------------------------------
%%% File    : graphics.erl
%%% Author  : Olle Mattsson <olle@zubat>
%%% Description : 
%%%
%%% Created :  7 Feb 2010 by Olle Mattsson <olle@zubat>
%%%-------------------------------------------------------------------
-module(graphics).

-include_lib("wx/include/gl.hrl").
-include("tibia.hrl").

-export([get_rgba/1,gen_data/1, test_blit/0, create_gl_texture/1,
	 save/2, save_png/2, save_all/0, load_spr/1, blit_sprite/2]).

load_spr(File) ->
    {ok, Device} = file:open(File, [read,binary]),
    io:format("Loading data ~p..\n", [File]),
    ets:new(sprites, [named_table, public, ordered_set, {keypos, #sprite.id}]),

    {ok, <<Ver:32/?UINT, TotalPics:16/?UINT>>} = file:pread(Device, bof, 6),

    Index = read_sprite_index(Device, TotalPics),
    read_sprites(Device, Index, 1).


read_sprite_index(Device, TotalPics) ->
    read_sprite_index(Device, TotalPics, []).

read_sprite_index(_Data, 0, Acc) ->
    lists:reverse(Acc);
read_sprite_index(Device, TotalPics, Acc) ->
    {ok, <<Index:32/?UINT>>} = file:pread(Device, cur, 4),
    read_sprite_index(Device, TotalPics-1, [Index|Acc]).


read_sprites(Device, [Index|Indexes], ID) when Index > 0 ->
    {ok, <<Size:16/?UINT>>} = file:pread(Device, Index, 2),
     {ok, Data} = file:pread(Device, cur, Size),
    Spr = #sprite{size = Size,
		  id = ID,
		  data = Data},
    ets:insert(sprites, Spr),
    read_sprites(Device, Indexes, ID+1);
read_sprites(Device, [_|Indexes], ID) ->
    read_sprites(Device, Indexes, ID+1);
read_sprites(_, [], _) ->
    ok.





get_rgba(Data) ->
    element(1,get_rest(get_bytes(Data, byte_size(Data), {0,0}, <<>>))).


get_rest({Acc, Pos}) ->
    get_transparent_pixel(1024-(byte_size(Acc) div 4), Pos, Acc).

get_bytes(<<>>, 0, Pos, Acc) ->
    {Acc, Pos};
get_bytes(<<TSize:16/unsigned-integer-little>>, 2, Pos, Acc) ->
    {T, Pos2} = get_transparent_pixel(TSize, Pos, <<>>),
    {<<Acc/binary, T/binary>>, Pos2};
get_bytes(<<TSize:16/unsigned-integer-little,
	   PSize:16/unsigned-integer-little,
	   Data/binary>>, Size, Pos, Acc) when Size > 0 ->
    {T, Pos2} = get_transparent_pixel(TSize, Pos, <<>>),
    {Rest, P, Pos3} = get_pixel(Data, PSize, Pos2, <<>>),
    get_bytes(Rest, Size-4-(PSize*3), Pos3, <<Acc/binary,T/binary,P/binary>>).


get_transparent_pixel(0, Pos, Acc) ->
    {Acc, Pos};
get_transparent_pixel(Size, {32,Y}, Acc) ->
    get_transparent_pixel(Size, {0,Y+1}, Acc);
get_transparent_pixel(Size, {X,Y}, Acc) ->
    get_transparent_pixel(Size-1, {X+1,Y}, <<Acc/binary, 16#00:4/unit:8>>).


get_pixel(Data, ChunkSize, {32,Y}, Acc) ->
    get_pixel(Data, ChunkSize, {0,Y+1}, Acc);
get_pixel(Data, 0, Pos, Acc) ->
    {Data, Acc, Pos};
get_pixel(<<RGB:3/binary,Rest/binary>>, ChunkSize, {X, Y}, Acc) ->
    S = byte_size(Rest),
    get_pixel(Rest, ChunkSize-1, {X+1,Y}, <<Acc/binary, RGB/binary, 16#FF>>).


test_blit() ->
    glBlitTexture(10,10,1,255,255,255,255).

blit_sprite({X,Y}, #sprite{id = Id, tid = true}) ->
    glBlitTexture(X,Y,Id, 255,255,255,255);
blit_sprite({X,Y}, S=#sprite{id = Id}) ->
    create_gl_texture(S),
    glBlitTexture(X,Y,Id, 255,255,255,255).

glBlitTexture(SX, SY, TextureNumber, Red, Green, Blue, Alpha) ->
    gl:bindTexture(?GL_TEXTURE_2D, TextureNumber),
    gl:color4ub(Red, Green, Blue, Alpha),
    gl:'begin'(?GL_QUADS),
    gl:texCoord2f(0, 0), gl:vertex2f(SX,   SY),
    gl:texCoord2f(1, 0), gl:vertex2f(SX+32,SY),
    gl:texCoord2f(1, 1), gl:vertex2f(SX+32,SY+32),
    gl:texCoord2f(0, 1), gl:vertex2f(SX,   SY+32),
    gl:'end'(),
    gl:bindTexture(?GL_TEXTURE_2D, 0),
    true.

create_gl_texture(S=#sprite{data = Data, id = Id}) ->
    RGBA = get_rgba(Data),

    gl:bindTexture(?GL_TEXTURE_2D, Id),
    gl:texImage2D(?GL_TEXTURE_2D, 0, ?GL_RGBA, 32, 32, 0, ?GL_RGBA, ?GL_UNSIGNED_BYTE, RGBA),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, 16#2802, 16#812F),
    gl:texParameteri(?GL_TEXTURE_2D, 16#2803, 16#812F),
    ets:insert(sprites,S#sprite{tid = true, last_access = erlang:now()}).


gen_data(Img) ->
    Mask = <<(wxImage:getMaskRed(Img)),(wxImage:getMaskGreen(Img)),(wxImage:getMaskBlue(Img))>>,
    Data = wxImage:getData(Img),
    gen_data(Data, Mask, <<>>).

gen_data(<<>>, _Mask, Acc) ->
    Acc;
gen_data(Data, Mask, Acc) ->
    {Rest, Count} = transparent_count(Data, Mask, 0),
    {Rest2, Pixels} = pixel_count(Rest, Mask, <<>>),
    if byte_size(Pixels) == 0 ->
	    P = <<>>;
       true ->
	    P = <<(byte_size(Pixels) div 3):16/unsigned-integer-little, Pixels/binary>>,
	    P
    end,
    Acc2 = <<Acc/binary,
	    Count:16/unsigned-integer-little,
	    P/binary>>,

    gen_data(Rest2, Mask, Acc2).



transparent_count(<<Mask:3/binary,Rest/binary>>, Mask, C) ->
    transparent_count(Rest, Mask, C+1);
transparent_count(Rest, _Mask, C) ->
    {Rest, C}.

pixel_count(Rest = <<Mask:3/binary,_/binary>>, Mask, Acc) ->
    {Rest, Acc};
pixel_count(<<RGB:3/binary,Rest/binary>>, Mask, Acc) ->
    pixel_count(Rest, Mask, <<Acc/binary, RGB/binary>>);
pixel_count(<<>>, _Mask, Acc) ->
    {<<>>, Acc}.


-define(SPR_VERSION, 1).

save(Filename, Files) ->
    Data = gen_from_files(Files, []),
    Bin = <<?SPR_VERSION:32/unsigned-integer-little,
	   (merge_data(Data))/binary>>,
    file:write_file(Filename, Bin).

gen_from_files([File|Files], Acc) ->
    I = wxImage:new(File),
    Data = gen_data(I),
    wxImage:destroy(I),
    gen_from_files(Files, [<<(byte_size(Data)):16/unsigned-integer-little,
			    Data/binary>> | Acc]);
gen_from_files([], Acc) ->
    lists:reverse(Acc).

merge_data(Data) ->
    Pos = (length(Data) * 4) + 6,
    Index = gen_index(Data, Pos, <<>>),
    <<(length(Data)):16/unsigned-integer-little,
       Index/binary, (list_to_binary(Data))/binary>>.

gen_index([Data|Rest], Pos, Acc) ->
    gen_index(Rest, Pos+byte_size(Data), <<Acc/binary, Pos:32/unsigned-integer-little>>);
gen_index([], _, Acc) ->
    Acc.



save_all() ->
    ets:foldl(fun(S, Dir) ->
		      if S#sprite.id rem 1000 == 0 ->
			      io:format("~pK\n", [S#sprite.id div 1000]);
			 S#sprite.id rem 100 == 0 ->
			      io:format(".");
			 true ->
			      ignore
		      end,
		      save_png(integer_to_list(Dir),S),
		      S#sprite.id div 100
	      end,
	      0, sprites).

save_png(Dir, #sprite{id = Id, data = Data}) ->
    save_file(filename:join([Dir,integer_to_list(Id) ++ ".png"]), Data).

save_file(File, D) ->
    RGBA = get_rgba(D),
    Data = << <<R,G,B>> || <<R,G,B,_>> <= RGBA >>,
    Alpha = << <<A>> || <<_,_,_,A>> <= RGBA >>,
    I = wxImage:new(32,32,Data),
    wxImage:setAlpha(I, Alpha),
    Filename = filename:join(["png/", File]),
    filelib:ensure_dir(Filename),
    wxImage:saveFile(I, Filename),
    wxImage:destroy(I).

test_save(D) ->
    RGBA = get_rgba(D),
    Data = << <<R,G,B>> || <<R,G,B,_>> <= RGBA >>,
    Alpha = << <<A>> || <<_,_,_,A>> <= RGBA >>,
    I = wxImage:new(32,32,Data),
    wxImage:setAlpha(I, Alpha),
    wxImage:saveFile(I, "bar.png").

test_save2(Img) ->
    D = gen_data(Img),
    RGBA = get_rgba(D),
    Data = << <<R,G,B>> || <<R,G,B,_>> <= RGBA >>,
    Alpha = << <<A>> || <<_,_,_,A>> <= RGBA >>,

    I = wxImage:new(32,32,Data),
    wxImage:setAlpha(I, Alpha),
    wxImage:saveFile(I, "bar2.png").

    
