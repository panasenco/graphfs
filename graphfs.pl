:- module(graphfs, [echo_consumer/5, remove_consumer/5, insert_consumer/5, move_consumer/5,
    graph_consumer_state_newstate_newgraph/5]).
:- use_module(library(lists)).
:- use_module(library(reif)).

/* Graph consumers consume a graph one edge at a time, and return a list of new edges to replace the consumed ones.
 * Each consumer must have the interface something_consumer(Edge, State, NextState, KeepEdgeBool, InsertEdges).
 * Each consumer must terminate universally. That is:
 *      something_consumer(_,_,_,_,_), false.
 * must return false.
 * Each consumer must be able to consume two kinds of edges:
     * from_to_label(From, To, Label)
     * end
 */

/* The echo consumer keeps all edges and inserts nothing */
echo_consumer(_, null, null, true, []).

/* The remove consumer removes an edge from the graph */
remove_consumer(end, _, null, _, []).
remove_consumer(from_to_label(From, To, Label), from_to_label(DelFrom, DelTo, DelLabel),
    from_to_label(DelFrom, DelTo, DelLabel), KeepEdge, []) :-
    if_(((From = DelFrom), (To=DelTo), (Label=DelLabel)), (KeepEdge = false), (KeepEdge = true)).

/* Naive topological insert preserves topological ordering on insert, but doesn't guarantee it.
    * Guarantee: Naive topological insert will insert exactly once into any graph, even an unsorted or cyclic one.
    * Lemma: If the edges in the graph are in topological order, naive topological insert will always preserve the
             order.
    * Lemma: If any subset of edges in a graph is in topological order, naive topological insert will always preserve
             topological ordering of that subset.
    * Lemma: Naive topological insert preserves grouping by From node.
*/
insert_consumer(_, null, null, true, []).
insert_consumer(end, from_to_label(InsFrom, InsTo, InsLabel), null, _, [from_to_label(InsFrom, InsTo, InsLabel)]).
insert_consumer(from_to_label(From, _, _), from_to_label(InsFrom, InsTo, InsLabel), NextState, true, InsertEdges) :-
    if_((From = InsFrom; From = InsTo),
        (NextState = null, InsertEdges = [from_to_label(InsFrom, InsTo, InsLabel)]),
        (NextState = from_to_label(InsFrom, InsTo, InsLabel), InsertEdges = [])).

move_consumer(Edge, from_to_label(From, To, Label), [NextRemoveState, NextInsertState], KeepEdge, InsertEdges) :-
    move_consumer(Edge, [from_to_label(From, To, Label), from_to_label(From, To, Label)],
    [NextRemoveState, NextInsertState], KeepEdge, InsertEdges).
move_consumer(Edge, [RemoveState, InsertState], [NextRemoveState, NextInsertState], KeepEdge, InsertEdges) :-
    remove_consumer(Edge, RemoveState, NextRemoveState, KeepEdge, _),
    insert_consumer(Edge, InsertState, NextInsertState, _, InsertEdges).

/*
recursivemove_consumer(_, [], [], []).
recursivemove_consumer(end, [Move|Moves], [Move|Moves], []).
recursivemove_consumer(from_to_label(From, To, Label), [move(SourceNode, DestNode) | MoveTail], FinalMoves, Edges) :-
    if_((From = SourceNode),(NextMoves = [from_to_label(DestNode, To, Label)]),(NextMoves = [])),
    recursivemove_consumer(from_to_label(From, To, Label), MoveTail, NextMoveTail, NextEdges),
    append(NextMoves, NextMoveTail, FinalMoves),
    append(
*/

/* Allows an entire graph to be consumed by a consumer */
graph_consumer_state_newstate_newgraph([], Consumer, State, NewState, NewGraph) :-
    call(Consumer, end, State, NewState, _, NewGraph).
graph_consumer_state_newstate_newgraph([from_to_label(From, To, Label) | Tail], Consumer, State, NewState, NewGraph) :-
    call(Consumer, from_to_label(From, To, Label), State, NextState, KeepEdge, InsertEdges),
    if_(KeepEdge, Edge = [from_to_label(From, To, Label)], Edge = []),
    graph_consumer_state_newstate_newgraph(Tail, Consumer, NextState, NewState, TailGraph),
    append([InsertEdges, Edge, TailGraph], NewGraph).
