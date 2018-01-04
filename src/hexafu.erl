-module(hexafu).
-author('tumdash<nikolay.kovalev@gmail.com>').

-include("hexafu.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API exports
-export([
	neighbors/1,
	neighbors/2,
	neighbors/3
]).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Returns neighbors of given hex
%%--------------------------------------------------------------------
-spec neighbors(Hex :: position()) -> position_spec().
%%--------------------------------------------------------------------
neighbors(Hex) ->
	neighbors(Hex, ?DFL_ORIENTATION, ?DFL_SPEC, ?DFL_TYPE).

%%--------------------------------------------------------------------
%% Returns neighbors of given hex with oriented hex
%%--------------------------------------------------------------------
-spec neighbors(
		Hex :: position(),
		Orientation :: absolute_orientation()
) -> position_spec().
%%--------------------------------------------------------------------
neighbors(Hex, Orientation) ->
	neighbors(Hex, Orientation, ?DFL_SPEC, ?DFL_TYPE).

%%--------------------------------------------------------------------
%% Returns neighbors of given hex according to orientation in given
%%	grid
%%--------------------------------------------------------------------
-spec neighbors(
		Hex :: position(),
		Orientation :: absolute_orientation(),
		Type :: grid_type()
) -> position_spec().
%%--------------------------------------------------------------------
neighbors(Hex, Orientation, Type) ->
	neighbors(Hex, Orientation, ?DFL_SPEC, Type).

%%====================================================================
%% Internal functions
%%====================================================================
neighbors(Hex, Orientation, Spec, _Type) ->
	lists:foldl(
		fun(S, MapIn) ->
			Fun = hexafu_position:absolutize(S, Orientation),
			maps:put(S, hexafu_position:Fun(Hex), MapIn)
		end,
		#{},
		Spec
	).

neighbors_flat_top_test() ->
	NullDfltNeighbors = hexafu:neighbors({0, 0, 0}),
	?assertEqual({0, 1, -1}, maps:get(front, NullDfltNeighbors)),
	?assertEqual({0, -1, 1}, maps:get(back, NullDfltNeighbors)),
	?assertEqual({1, -1, 0}, maps:get(right_side, NullDfltNeighbors)),
	?assertEqual({-1, 1, 0}, maps:get(left_front, NullDfltNeighbors)),
	?assertEqual({-1, 0, 1}, maps:get(left_side, NullDfltNeighbors)),
	?assertEqual({1, 0, -1}, maps:get(right_front, NullDfltNeighbors)),
	NullBtmNeighbors = hexafu:neighbors({0, 0, 0}, bottom, 'flat-topped'),
	?assertEqual(
		maps:get(front, NullBtmNeighbors),
		maps:get(back, NullDfltNeighbors)
	),
	?assertEqual(
		maps:get(back, NullBtmNeighbors),
		maps:get(front, NullDfltNeighbors)
	),
	?assertEqual(
		maps:get(left_front, NullBtmNeighbors),
		maps:get(right_side, NullDfltNeighbors)
	),
	?assertEqual(
		maps:get(right_side, NullBtmNeighbors),
		maps:get(left_front, NullDfltNeighbors)
	),
	?assertEqual(
		maps:get(right_front, NullBtmNeighbors),
		maps:get(left_side, NullDfltNeighbors)
	),
	?assertEqual(
		maps:get(left_side, NullBtmNeighbors),
		maps:get(right_front, NullDfltNeighbors)
	).

neighbors_point_top_test() ->
    RightNeighbors = hexafu:neighbors({0, 0, 0}, right, 'point-topped'),
    LeftNeighbors = hexafu:neighbors({0, 0, 0}, left, 'point-topped'),
	?assertEqual({1, -1, 0}, maps:get(front, RightNeighbors)),
	?assertEqual({1, -1, 0}, maps:get(back, LeftNeighbors)),
	?assertEqual({-1, 1, 0}, maps:get(back, RightNeighbors)),
	?assertEqual({-1, 1, 0}, maps:get(front, LeftNeighbors)),
	?assertEqual({1, 0, -1}, maps:get(left_front, RightNeighbors)),
	?assertEqual({1, 0, -1}, maps:get(right_side, LeftNeighbors)),
	?assertEqual({0, -1, 1}, maps:get(right_front, RightNeighbors)),
	?assertEqual({0, -1, 1}, maps:get(left_side, LeftNeighbors)),
	?assertEqual({0, 1, -1}, maps:get(left_side, RightNeighbors)),
	?assertEqual({0, 1, -1}, maps:get(right_front, LeftNeighbors)),
	?assertEqual({-1, 0, 1}, maps:get(right_side, RightNeighbors)),
	?assertEqual({-1, 0, 1}, maps:get(left_front, LeftNeighbors)).


