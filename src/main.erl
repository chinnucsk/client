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

-record(state, {frame, canvas, font, button, boxes,dir}).


start() ->
    wx_object:start(?MODULE, [], []).

start_link() ->
    wx_object:start_link(?MODULE, [], []).

init([]) ->
    wx:new(),

    graphics:load_spr("test.spr"),
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "test", [{size, {800,600}}]),
    
    GLAttrib = [{attribList, [?WX_GL_RGBA,?WX_GL_DOUBLEBUFFER,0]}],
    Canvas = wxGLCanvas:new(Frame, GLAttrib),

    wxFrame:show(Frame),
    wxGLCanvas:setCurrent(Canvas),

    init_gl(Canvas),

    ets:new(fonts, [named_table,ordered_set]),
    font:load_font("6x11.wingsfont"),
    font:load_font("7x14.wingsfont"),
    gl:pixelStorei (?GL_UNPACK_ALIGNMENT, 1),
    timer:send_interval(100, update),
    %%wxGLCanvas:connect(Canvas, left_down),
    wxGLCanvas:connect(Canvas, size),
    wxGLCanvas:connect(Canvas, key_down),
    wxGLCanvas:connect(Canvas, key_up),
    {Frame, #state{frame = Frame,
		   canvas = Canvas,
		   button = box:create_button("Button"),
		   boxes = box:create_box("hello"),
		   dir = 1}}.

handle_event(#wx{event = #wxSize{type = size, size = {W,H}}}, State) ->
    gl_resize(W,H),
    {noreply, State};
handle_event(#wx{event = #wxMouse{type = left_down, x = X, y = Y}}, State) ->
    %%io:format("~p\n", [box:can_move(X,Y, State#state.boxes)]),
    case box:can_move(X, Y, State#state.boxes) of
	true ->
	    wxGLCanvas:connect(State#state.canvas, left_up),
	    wxGLCanvas:connect(State#state.canvas, motion);
	false ->
	    ignore
    end,
    {noreply, State};
handle_event(#wx{event = #wxMouse{type = left_up}}, State) ->
    wxGLCanvas:disconnect(State#state.canvas, motion),
    wxGLCanvas:disconnect(State#state.canvas, left_up),
    {noreply, State};
handle_event(#wx{event = #wxMouse{type = motion, x = X, y = Y}},
	     State=#state{boxes = B}) ->
    self() ! update,
    {noreply, State#state{boxes = B#form{pos = {X, Y}}}};
handle_event(#wx{event = #wxKey{keyCode = Code, type = key_down}}, State) ->
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

handle_info(update, State=#state{dir = Dir}) ->
    gl:clear(?GL_COLOR_BUFFER_BIT),
    gl:loadIdentity(),
    gl:color3ubv({255,0,255}),
    gl:rasterPos2i(100, 100),
    font:draw("hello", lists:max(font:get_loaded_fonts())),
    gl:flush(),
    box:draw_form(State#state.boxes),
    box:draw_form(State#state.button),
    
    graphics:blit_sprite({50,50}, hd(ets:lookup(sprites, 2))),
    draw_console(State),
    wxGLCanvas:swapBuffers(State#state.canvas),
    {noreply, State#state{}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------


draw_console(State) ->
    {W,H} = wxGLCanvas:getSize(State#state.canvas),
    Box = box:create_box("Console", [{size, {W,100}},{pos, {0,H-100}}]),
    box:draw_form(Box).


init_gl(Canvas) ->
    {W,H} = wxWindow:getClientSize(Canvas),
    io:format("~p\n", [{W,H}]),
    gl:clearColor(0.1,0.8,0.1,1),
    gl:enable(?GL_TEXTURE_2D),
    gl:enable(?GL_COLOR_MATERIAL),
    gl:enable(?GL_BLEND),
    gl:disable(?GL_DEPTH_TEST),
    gl:blendFunc(?GL_SRC_ALPHA,?GL_ONE_MINUS_SRC_ALPHA),
 
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
 
    glu:ortho2D(0, W,H, 0),
 
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    
    gl:clear(?GL_COLOR_BUFFER_BIT).



gl_resize(W,H) ->
    gl:viewport(0, 0, W, H),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
 
    glu:ortho2D(0, W,H, 0),

    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    ok.

