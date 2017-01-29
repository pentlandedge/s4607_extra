-module(convex_hull_tests).

-include_lib("eunit/include/eunit.hrl").

-export([point_list1/0]).

%% Define a test generator for the BCS character set functions.
convex_hull_test_() ->
    [is_left_checks(), take_right_checks(), quickhull_check1()].

is_left_checks() ->
    [?_assertEqual(true, convex_hull:is_left({0,0},{0,5},{-1,0})),
     ?_assertEqual(false, convex_hull:is_left({0,5},{0,0},{-1,0})),
     ?_assertEqual(false, convex_hull:is_left({0,0},{0,5},{1,3})),
     ?_assertEqual(true, convex_hull:is_left({0,0},{5,0},{1,3})),
     ?_assertEqual(true, convex_hull:is_left({1,1},{5,5},{-3,3}))].

take_right_checks() ->
    A = {4,-2},
    B = {-4,1},
    D = {-1,2},
    E = {-2,-2},
    RightOfAB = convex_hull:take_right_of_line(A, B, [D,E]),
    RightOfBA = convex_hull:take_right_of_line(B, A, [D,E]),
    [?_assertEqual([D], RightOfAB),
     ?_assertEqual([E], RightOfBA)].

quickhull_check1() ->
    Points = point_list1(),
    Hull = convex_hull:quickhull(Points),
    A = {4,-2},
    B = {-4,1},
    C = {3,1},
    D = {-1,2},
    E = {-2,-2},
    ExpectedHull = [B,E,A,C,D],
    [?_assertEqual(Hull, ExpectedHull)].

%% Returns a collection of points clustered around the origin. The hull is 
%% A, B, C, D, E. P and Q are not part of it.
point_list1() ->
    A = {4,-2},
    B = {-4,1},
    C = {3,1},
    D = {-1,2},
    E = {-2,-2},
    P = {2,0},
    Q = {-1,-1},
    [Q,C,P,A,E,D,B].

