:- module(graphfs, [list_insert_naivetopo/3]).
:- use_module(library(reif)).


/* Naive topological insert preserves topological ordering on insert, but doesn't guarantee it.
    * Guarantee: Naive topological insert will succeed exactly once for any graph, even an unsorted or cyclic one.
    * Lemma: If the triples in the graph are in topological order, naive topological insert will preserve the order.
    * Lemma: If a subset of triples that share a predicate is in topological order, naive topological insert will
             preserve topological ordering of that subset.
    * Lemma: Naive topological insert is O(N).
    * Lemma: Naive topological insert preserves subject grouping.
*/
list_insert_naivetopo([], t(InsertSubject, InsertPredicate, InsertObject),
    [t(InsertSubject, InsertPredicate, InsertObject)]).
list_insert_naivetopo([t(Subject, Predicate, Object) | Tail], t(InsertSubject, InsertPredicate, InsertObject),
    [NaiveTopoHead | NaiveTopoTail]) :-
    if_(;(Subject = InsertSubject, Subject = InsertObject),
        (
            NaiveTopoHead = t(InsertSubject, InsertPredicate, InsertObject),
            NaiveTopoTail = [t(Subject, Predicate, Object) | Tail]
        ),
        (
            NaiveTopoHead = t(Subject, Predicate, Object),
            list_insert_naivetopo(Tail, t(InsertSubject, InsertPredicate, InsertObject), NaiveTopoTail)
        )
    ).
