:- module(graphfs, [echo_consumer/4, delete_consumer/4, naivetopoinsert_consumer/4, edge_consume/5, consume/5,
    graph_clones_newgraph/3, list_insert_naivetopo/3]).
:- use_module(library(lists)).
:- use_module(library(reif)).

/* Graph consumers consume a graph one edge at a time, and return a list of new edges to replace the consumed ones.
 * Each consumer must have the interface predicate(Edge, State, NextState, Edges).
 * Each consumer must terminate universally. That is:
 *      predicate(_,_,_,_), false.
 * must return false.
 */

/* The echo consumer places each edge back into the graph */
echo_consumer(edge(Node1, Node2, Label), null, null, [edge(Node1, Node2, Label)]).

/* The delete consumer places no edges back into the graph */
delete_consumer(edge(_, _, _), null, null, []).


/* Naive topological insert preserves topological ordering on insert, but doesn't guarantee it.
    * Guarantee: Naive topological insert will succeed exactly once for any graph, even an unsorted or cyclic one.
    * Lemma: If the edges in the graph are in topological order, naive topological insert will preserve the order.
    * Lemma: If any subset of edges in a graph is in topological order, naive topological insert will preserve
             topological ordering of that subset.
    * Lemma: Naive topological insert preserves subject grouping.
*/
naivetopoinsert_consumer(edge(_, _, _), null, null, []).
naivetopoinsert_consumer(edge(Node1, _, Label), edge(InsertNode1, InsertNode2, InsertLabel), NextInsert, Edges) :-
    if_((Label = InsertLabel, (Node1 = InsertNode1; Node1 = InsertNode2)),
        (NextInsert = null, Edges = [edge(InsertNode1, InsertNode2, InsertLabel)]),
        (NextInsert = edge(InsertNode1, InsertNode2, InsertLabel), Edges = [])).

/* edge_consume/5 allows an edge to be consumed by multiple consumers */
edge_consume(edge(_, _, _), [], [], [], []).
edge_consume(edge(Node1, Node2, Label), [Consumer | ConsumerTail], [State | StateTail], [NextState | NextStateTail], Edges) :-
    call(Consumer, edge(Node1, Node2, Label), State, NextState, ConsumerEdges),
    edge_consume(edge(Node1, Node2, Label), ConsumerTail, StateTail, NextStateTail, ConsumerTailEdges),
    append(ConsumerEdges, ConsumerTailEdges, Edges).

/* consume/5 allows an entire graph to be consumed by multiple consumers */
consume([], _, States, States, []).
consume([edge(Node1, Node2, Label) | Tail], Consumers, States, FinalStates, NewGraph) :-
    edge_consume(edge(Node1, Node2, Label), Consumers, States, NextStates, Edges),
    consume(Tail, Consumers, NextStates, FinalStates, NewTail),
    append(Edges, NewTail, NewGraph).
