%%%-------------------------------------------------------------------
%%% File    : scene.erl
%%% Author  : Olle Mattsson <olle@zubat>
%%% Description : 
%%%
%%% Created : 12 Feb 2010 by Olle Mattsson <olle@zubat>
%%%-------------------------------------------------------------------
-module(scene).

-include_lib("wx/include/wx.hrl").
-include_lib("wx/include/gl.hrl").
-include_lib("wx/include/glu.hrl").

-compile(export_all).

draw_scene(Canvas) ->
    {W0,H0} = wxGLCanvas:getSize(Canvas),
    Ratio = W0 / H0,

    X = W0 div 12,
    Y = 0,
    W = (W0/12)*10,
    H = H0 - 100,

    gl:translatef(X,Y,0),
    gl:'begin'(?GL_QUADS),
    gl:color3ubv({244,100,100}),
    gl:vertex2f(0,0),
    gl:vertex2f(W,0),
    gl:vertex2f(W,H),
    gl:vertex2f(0,H),

    gl:'end'(),

    box:border(W,H).
