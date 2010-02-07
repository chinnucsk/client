%%%-------------------------------------------------------------------
%%% File    : tibia.hrl
%%% Author  : Olle Mattsson <olle@zubat>
%%% Description : 
%%%
%%% Created : 15 May 2009 by Olle Mattsson <olle@zubat>
%%%-------------------------------------------------------------------


-define(UINT, unsigned-integer-little).

-define(NODE_START, 254).
-define(NODE_END, 255).
-define(ESCAPE_CHAR, 253).

-record(state, {key,
		account,
		player,
		server_pid}).

-record(key, {k1,k2,k3,k4}).

-record(node, {type,
	       data = <<>>,
	       children = []}).

-record(item, {id,
	       name,
	       article,
	       plural,
	       attributes}).

-record(map_item, {id,
		   attributes,
		   content}).

-record(item_type, {type,
		    flags,
		    server_id,
		    client_id,
		    ground_speed,
		    name,
		    sprite_hash,
		    minimap_color,
		    sub_param_7,
		    sub_param_8,
		    light_level,
		    light_color,
		    always_on_top_order}).

-record(coord, {x,y,z}).

-record(tile, {coord = #coord{},
	       type,
	       house_id,
	       items,
	       flags}).


-record(account, {name, password}).
-record(player, {account,
		 pos,
		 name,
		 id,
		 outfit,
		 direction,
		 health,
		 light,
		 skull,
		 shield}).

-record(outfit, {type,head,body,legs,feet,addons,corpse}).

-record(creature, {pos,
		   id,
		   name,
		   health,
		   direction,
		   outfit,
		   light,
		   speed,
		   skull,
		   shield}).

-record(monster, {name,
		  name_description,
		  race,
		  experience,
		  speed,
		  manacost,
		  health,
		  outfit,
		  flags,
		  elements,
		  immunities,
		  voices,
		  loot}).

-record(spawn, {center,radius,monsters}).
-record(spawn_monster, {monster,pos,spawn_time}).



-record(gameSprite, {id = 0,
		     draw_height = 0,
		     drawoffset_x = 0,
		     drawoffset_y = 0,
		     minimap_color = 0,
		     width = 0,
		     height = 0,
		     sprite_list = [],
		     frames = 0,
		     xdiv = 0,
		     ydiv = 0,
		     zdiv = 0,
		     animation_length = 0,
		     num_sprites = 0}).

-record(sprite, {id,
		 tid,
		 last_access,
		 size,
		 data}).
