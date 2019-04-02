# hsearch

hsearch provides core tools for Artificial Intelligence search algorithms.
mainly inspired by 'Artificial Intelligence: A Modern Approach'. 

## Background

Main elements of a search problem are:
- initial state
- many ways to act on the environment in order to bring us in a new set of states
- a strategy to choose correctly the next state
- a way to identify if we have reached the goal (a peculiar state or a specific condition)

A search algorithm which takes into account all these elements starts from the initial state, 
computes all next possible states, choose beetween them and check if the goal is reached and so on.
Doing this the algorithm creates a *search tree* (or graph) where each node contains a specific state, many nodes
could contain the same state if the state space is a graph (for instance if is allowed
to undo an action). The algorithm at each iteration appends nodes to the search tree, the whole of all not
yet expanded nodes constitutes the *fringe*. Through a *search strategy* the algorithm chooses which node
expand of those in the fringe, if there aren't nodes in the fringe the algorithm fails cause there is not a
solution in the state space, most often the state space is infine and a infinite loop happens if goal is not found.

## Library Core Interfaces

The library abstracts all previous described elements in order to provide a high level interface for search
problems:

- functions like `(Num c) => s -> [(s, c)]` are **next states generators** that, given the current state, generate 
  next states and associate them a step_cost 
- **policies**  are functions which embed the search strategy by defining a way to order elements in the fringe.
- **SNode** is a node of the search tree
- **SRoute** defines a spefific sequence of SNode in the Search Tree/Graph, it is not used directly
- functions like `s -> Bool` are used in order to check if the goal has been reached

## Usage

Main module is `AI.Search` which implements many generic search algorithms, such algorithms can be used with both tree
or graph search state spaces, the only difference is that in graph search algorithms keep track of closed nodes (states yet
visited and expanded) in  order to skip them if are met again. Graph search obviously can be very less memory efficient cause
forcing to mantain the whole search graph in memory (closed nodes + fringe) but allows to avoid cycles and converge faster when
states repeate. 

For instance:

~~~haskell
search :: (Num p, Ord i)
                  => Topology i s p                   -- ^ tree or graph search 
                  -> SearchPolicy s p i               -- ^ policy to be used
                  -> (s -> Bool)                      -- ^ function used to check if the goal is reached
                  -> (s -> [(s, p)])                  -- ^ generator of states
                  -> s                                -- ^ initial state
                  -> [s]                              -- ^ returns list of states
~~~

`search` function take:

- topology : a way to tell if we are interested in a graph or tree search, `AI.Search` provides 2 topologies, `tree`
  and `graph`, in graph search state must be instance of `Ord`.
- search policy : the policy to be used in order to build the search tree, for instance breadthFirstPolicy computes first
  all childs of a node and then childs of childs and so on. 
- (s -> Bool) : takes current state and check if the goal is reached
- (s -> [(s, p)] : takes current state and generates next states, Num p => p represents the step cost to reach that node
- s : is the initial state
