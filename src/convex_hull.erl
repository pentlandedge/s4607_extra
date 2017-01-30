%% Implementation of algorithms for calculating the "Convex Hull" of a set of
%% points. This defines the perimeter of the set.
-module(convex_hull).

-export([
    quickhull/1, 
    quickhull2/1, 
    is_left/3, 
    take_right_of_line/3,
    inside_triangle/4, 
    remove_inside_triangle/4,
    divide_points/3, 
    distance_from_line/3,
    take_furthest_from_line/3]).

%% Caculates the convex hull using the "Quickhull" method. The points should
%% be supplied as a list of {X,Y} tuples.
%% Internally, we could do this with lists or possibly a digraph.
quickhull(Points) ->
    % Start by sorting the list on the X coord.
    Sorted = lists:keysort(1, Points),
    % Take the leftmost and rightmost points, A and B.
    [A|Rest] = Sorted,
    [B|Rest2] = lists:reverse(Rest),
    {S1, S2} = divide_points(Rest2, A, B),
    findhull(A, B, S2) ++ findhull(B, A, S1).

%% Alternate version of the quickhull algorithm that allows the user to 
%% supply a reference parameter. This is intended to allow the original
%% Lat, Lon mappings and similar to be retained, avoiding the need to 
%% perform the reverse coordinate frame conversion.
quickhull2(Points) ->
    % Start by sorting the list on the X coord.
    Sorted = lists:keysort(1, Points),
    % Take the leftmost and rightmost points, A and B.
    [A|Rest] = Sorted,
    [B|Rest2] = lists:reverse(Rest),
    {S1, S2} = divide_points(Rest2, A, B),
    findhull(A, B, S2) ++ findhull(B, A, S1).

%% Helper function for quickhull.
findhull(P, _Q, []) -> 
    [P];
findhull(P, Q, Sk) ->
    {Furthest, _, Rest} = take_furthest_from_line(P, Q, Sk),
    % Filter out points lying inside the triangle.
    OutsidePoints = remove_inside_triangle(Rest, P, Q, Furthest), 
    RightOfLine1 = take_right_of_line(P, Furthest, OutsidePoints),
    RightOfLine2 = take_right_of_line(Furthest, Q, OutsidePoints),
    findhull(P, Furthest, RightOfLine1) ++ findhull(Furthest, Q, RightOfLine2).

%% Use a cross product to figure out if point C lies to the left or right of
%% a line joining two points A and B.
%% Note: need to add a check for collinear points.
is_left({Ax,Ay}, {Bx,By}, {Cx,Cy}) ->
     CP = ((Bx - Ax)*(Cy - Ay) - (By - Ay)*(Cx - Ax)), 
     CP > 0. 

%% Function to select points to the right of the line from A to B.
take_right_of_line(A, B, Points) when is_list(Points) ->
    [P || P <- Points, is_left(A, B, P) == false].

%% Calculate whether a point P lies within the triangle defined by vertices
%% A,B and C. The algorithm checks to see that the point lies to the same 
%% side of each of the three sides of the triangle as a path is traced around
%% the perimeter.
inside_triangle(P, A, B, C) ->
    L1 = is_left(A, B, P),
    L2 = is_left(B, C, P),
    L3 = is_left(C, A, P),
    (L1 == L2) and (L2 == L3). 

%% Function to remove the points from the list that lie inside the triangle
%% defined by points A,B,C.
remove_inside_triangle(Points, A, B, C) when is_list(Points) ->
    [P || P <- Points, inside_triangle(P, A, B, C) == false].

%% Function to divide a list of points into two groups based on the line 
%% drawn between A and B.
divide_points(Points, A, B) ->
    F = fun(X, {L, R}) -> 
            case is_left(A, B, X) of
                true  -> {[X|L],R};
                false -> {L,[X|R]}
            end
        end,
    lists:foldl(F, {[],[]}, Points).

%% Function to calculate the distance of a point C from a line defined by two
%% other points A and B.
distance_from_line({Ax,Ay}, {Bx,By}, {Cx,Cy}) ->
    Xdiff = Bx - Ax,
    Ydiff = By - Ay, 
    Num = abs(Cx*Ydiff - Cy*Xdiff + Bx*Ay - By*Ax),
    Den = math:sqrt((Ydiff * Ydiff) + (Xdiff * Xdiff)),
    Num / Den.

%% Take the point from the list which is furthest from the line defined by 
%% points A and B.
take_furthest_from_line(A, B, [First|Rest]) ->
    F = fun(C, {Furthest, Distance, Others}) ->
           Dist = distance_from_line(A, B, C), 
           case Dist > Distance of 
               true -> 
                   {C, Dist, [Furthest|Others]};
               false -> 
                   {Furthest, Distance, [C|Others]}
           end
        end,
    Distance = distance_from_line(A, B, First),
    lists:foldl(F, {First, Distance, []}, Rest).

