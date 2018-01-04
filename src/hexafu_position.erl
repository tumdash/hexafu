-module(hexafu_position).
-author('tumdash<nikolay.kovalev@gmail.com>').

-include("hexafu.hrl").

%% API exports
-export([
	top/1, bottom/1,
	left_top/1, right_bottom/1,
	right_top/1, left_bottom/1
]).
-export([
	left/1, right/1,
	up_left/1, down_right/1,
	up_right/1, down_left/1
]).
-export([absolutize/2]).

-define(FLAT_TOP_EDGES, [
	{top, 0},
	{right_top, 1},
	{right_bottom, 2},
	{bottom, 3},
	{left_bottom, 4},
	{left_top, 5}
]).

-define(POINT_TOP_EDGES, [
	{up_right, 0},
	{right, 1},
	{down_right, 2},
	{down_left, 3},
	{left, 4},
	{up_left, 5}
]).

-define(RELATIVE, [
	{front, 0},
	{right_front, 1},
	{right_side, 2},
	{back, 3},
	{left_side, 4},
	{left_front, 5}
]).

%%--------------------------------------------------------------------
%% Neighbor fun returns given neighbor of hex in flat-topped grid
%%--------------------------------------------------------------------
-spec top(Hex :: position()) -> position().
-spec bottom(Hex :: position()) -> position().
-spec left_top(Hex :: position()) -> position().
-spec right_bottom(Hex :: position()) -> position().
-spec right_top(Hex :: position()) -> position().
-spec left_bottom(Hex :: position()) -> position().
%%--------------------------------------------------------------------
top({X, Y, Z}) ->
	{X, Y+1, Z-1}.

bottom({X, Y, Z}) ->
	{X, Y-1, Z+1}.

left_top({X, Y, Z}) ->
	{X-1, Y+1, Z}.

right_bottom({X, Y, Z}) ->
	{X+1, Y-1, Z}.

right_top({X, Y, Z}) ->
	{X+1, Y, Z-1}.

left_bottom({X, Y, Z}) ->
	{X-1, Y, Z+1}.

%%--------------------------------------------------------------------
%% Neighbor fun returns given neighbor of hex in point-topped grid
%%--------------------------------------------------------------------
-spec left(Hex :: position()) -> position().
-spec right(Hex :: position()) -> position().
-spec up_left(Hex :: position()) -> position().
-spec down_right(Hex :: position()) -> position().
-spec up_right(Hex :: position()) -> position().
-spec down_left(Hex :: position()) -> position().
%%--------------------------------------------------------------------
left({X, Y, Z}) ->
	{X-1, Y+1, Z}.

right({X, Y, Z}) ->
	{X+1, Y-1, Z}.

up_left({X, Y, Z}) ->
	{X, Y+1, Z-1}.

down_right({X, Y, Z}) ->
	{X, Y-1, Z+1}.

up_right({X, Y, Z}) ->
	{X+1, Y, Z-1}.

down_left({X, Y, Z}) ->
	{X-1, Y, Z+1}.

%%--------------------------------------------------------------------
%% Returns absolute hex by appling relative relationship
%%--------------------------------------------------------------------
-spec absolutize(
		Relative :: relative_orientation(),
		Absolute :: absolute_orientation()
) ->
	absolute_orientation() | {error, absolute} | {error, relative}.
%%--------------------------------------------------------------------
absolutize(Relative, Absolute) ->
	case proplists:get_value(Relative, ?RELATIVE) of
		undefined ->
			{error, relative};
		RelativePos ->
			case {
				proplists:get_value(Absolute, ?FLAT_TOP_EDGES),
				proplists:get_value(Absolute, ?POINT_TOP_EDGES)}
			of
				{undefined, AbsPos} when is_number(AbsPos) ->
					get_edge((RelativePos + AbsPos) rem 6,
							 ?POINT_TOP_EDGES);
				{AbsPos, undefined} when is_number(AbsPos) ->
					get_edge((RelativePos + AbsPos) rem 6,
							 ?FLAT_TOP_EDGES);
				_ ->
					{error, absolute}
			end
	end.

get_edge(Pos, Proplist) ->
	{Side, _} = lists:nth(Pos+1, Proplist),
    Side.
