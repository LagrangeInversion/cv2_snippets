% This module provides functions dealing with 
% creating a path for robot to cover some area
%

-module(motion).
-export([zigzag/1, zigzag/2]).

zigzag({X,Y}) -> zigzag({X,Y}, 20).
zigzag({X,Y}, Step) -> zigzag({X,Y}, Step, [{0,0}]).
    
zigzag({_,Y},_Step, [{_,Y1}|Itinerary]) when Y1 > Y -> lists:reverse(Itinerary);
zigzag({X,Y}, Step, [{0,Y1}|Itinerary]) -> zigzag({X,Y}, Step, [{X,Y1+Step},{X,Y1},{0,Y1}|Itinerary]);
zigzag({X,Y}, Step, [{X,Y1}|Itinerary]) -> zigzag({X,Y}, Step, [{0,Y1+Step},{0,Y1},{X,Y1}|Itinerary]).

