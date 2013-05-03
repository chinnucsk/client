%%%-------------------------------------------------------------------
%%% File    : main.erl
%%% Author  : Olle Mattsson <olle@zubat>
%%% Description : 
%%%
%%% Created : 26 Dec 2009 by Olle Mattsson <olle@zubat>
%%%-------------------------------------------------------------------
-module(main).

-behaviour(wx_object).

-include_lib("wx/include/wx.hrl").
-include_lib("wx/include/gl.hrl").
-include_lib("wx/include/glu.hrl").
-include("misc.hrl").


%% API
-export([start/0, start_link/0]).

%% gen_server callbacks
-export([init/1,
	 handle_event/2, handle_call/3, 
	 handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {gui, font, button, boxes,dir}).

start() ->
    wx_object:start(?MODULE, [], []).

start_link() ->
    wx_object:start_link(?MODULE, [], []).

init([]) ->
    wx:new(),

    graphics:load_spr("test.spr"),

    Gui = window:gl_frame("test", [{size, {800,600}}]),
    ets:new(fonts, [named_table,ordered_set]),
    font:load_font("6x11.wingsfont"),
    font:load_font("7x14.wingsfont"),
    gl:pixelStorei (?GL_UNPACK_ALIGNMENT, 1),
    %graphics:create_gl_texture(hd(ets:lookup(sprites, 1))),
    timer:send_interval(100, update),
    wxGLCanvas:connect(Gui#gui.canvas, left_down),
    wxGLCanvas:connect(Gui#gui.canvas, left_up),
    wxGLCanvas:connect(Gui#gui.canvas, size),
    wxGLCanvas:connect(Gui#gui.canvas, key_down),
    wxGLCanvas:connect(Gui#gui.canvas, key_up),
    {Gui#gui.frame, #state{gui = Gui,
			   button = box:create_button("Button"),
			   boxes = box:create_box("hello"),
			   dir = 1}}.

handle_event(#wx{event = #wxSize{type = size, size = {W,H}}}, State) ->
    window:gl_resize(W,H),
    {noreply, State};
handle_event(#wx{event = #wxMouse{type = left_down, x = X, y = Y}}, State) ->
    %%io:format("~p\n", [box:can_move(X,Y, State#state.boxes)]),
    case box:can_move(X, Y, State#state.boxes) of
	true ->
	    wxGLCanvas:connect((State#state.gui)#gui.canvas, motion);
	false ->
	    ignore
    end,
    {noreply, State};
handle_event(#wx{event = #wxMouse{type = left_up}}, State) ->
    wxGLCanvas:disconnect((State#state.gui)#gui.canvas, motion),
    {noreply, State};
handle_event(#wx{event = #wxMouse{type = motion, x = X, y = Y}},
	     State=#state{boxes = B}) ->
    self() ! update,
    {noreply, State#state{boxes = B#form{pos = {X, Y}}}};
handle_event(#wx{event = #wxKey{keyCode = Code,controlDown=true}}, State) when Code =< ?WXK_DOWN, Code >= ?WXK_LEFT->
    {noreply, State#state{dir = Code-?WXK_LEFT+1}};
handle_event(#wx{event = #wxKey{keyCode = Code, type = key_down}}, State)
  when ((Code >= 16#20) andalso (Code =< 16#7e)) ->
    io:format("~p\n", [Code]),
    {noreply, State#state{}};
handle_event(#wx{event = #wxKey{}}, State) ->
    {noreply, State};
handle_event(W=#wx{}, State) ->
    io:format("W: ~p\n", [W]),
    {noreply, State}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(update, State=#state{gui = Gui, dir = Dir}) ->
    gl:clear(?GL_COLOR_BUFFER_BIT),
    gl:loadIdentity(),
    gl:color3ubv({255,0,255}),
    gl:rasterPos2i(100, 100),
    font:draw("hello", lists:max(font:get_loaded_fonts())),
    gl:flush(),
    box:draw_form(State#state.boxes),
    box:draw_form(State#state.button),
    
    graphics:blit_sprite({50,50}, hd(ets:lookup(sprites, Dir))),
    wxGLCanvas:swapBuffers(Gui#gui.canvas),
    {noreply, State#state{}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------



