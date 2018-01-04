%%--------------------------------------------------------------------
%% Type of grid
%%--------------------------------------------------------------------
-type grid_type() :: 'flat-topped' | 'point-topped'.

%%--------------------------------------------------------------------
%% Relative relationships between hex grids
%%--------------------------------------------------------------------
-type relative_orientation() ::
	front |
	left_front |
	right_front |
	left_side |
	right_side |
	back.

%%--------------------------------------------------------------------
%% Absolute relationships between hex grids
%%--------------------------------------------------------------------
-type absolute_orientation() ::
	flat_topped_orientation() | point_topped_orientation().

-type flat_topped_orientation() ::
	top |
	bottom |
	left_top |
	right_bottom |
	right_top |
	left_bottom.

-type point_topped_orientation() ::
	left |
	right |
	up_left |
	down_right |
	up_right |
	down_left.

%%--------------------------------------------------------------------
%% Hex Grid position spec is map with keys equal to
%%	relative_orientation
%%--------------------------------------------------------------------
-type position_spec() :: #{
    front => position(),
    left_front => position(),
    right_front => position(),
    left_side => position(),
    right_side  => position(),
    back => position()
}.

%%--------------------------------------------------------------------
%% Hex Grid cube coordinates
%%--------------------------------------------------------------------
-type position() ::
	{
		X :: integer(),
		Y :: integer(),
		Z :: integer()
	}.

-define(DFL_SPEC, [
	front, left_front, right_front,
	left_side, right_side, back
]).
-define(DFL_TYPE, 'flat-topped').
-define(DFL_ORIENTATION, top).
