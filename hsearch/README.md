# hsearch

hsearch provides core tools for Artificial Intelligence search in a state space, 
mainly inspired by 'Artificial Intelligence: A Modern Approach'. 

## Background

In all search problem there is:

- initial state
- many ways to act on the environment in order to bring us in a new set of states
- a strategy to choose correctly the next state
- a way to identify if we have reached the goal (a peculiar state or a specific condition)

So a search algorithm which takes into account all these elements starts from the initial state, 
computes all next possible states, choose beetween them and check if the goal is reached and so on.
Doing this the algorithm creates a *search tree* where each node contains a specific state, many node
could contain the same state if the state space is a state graph (for instance if the state space allows
to undo an action). The algorithm at each iteration appends nodes to the search tree, the whole of all not
yet expanded nodes constitutes the *fringe*. Through a *search strategy* the algorithm chooses which node
expand of those in the fringe, if there aren't nodes in the fringe the algorithm fails cause there is not a
solution in the state space, most often the state space is infine and a loop happens.

## Library Core Interfaces

The library abstracts all previous described elements in order to provide a high level interface for search
problems:

- functions like `(Num c) => s -> [(s, c)]` are **next states generators** that, given the current state, generate 
  next states and associate them a step_cost 
- **Fringe** is a typeclass which defines `next` and `insert` functions in order to manage not yet expanded nodes
- **policies**  are functions which embed the search strategy by defining a way to order elements in the fringe.
- **SNode** is a node of the search tree
- functions like `s -> Bool` are used in order to check if the goal has been reached

## Simple use

In the module `AI.Search.Algorithms` there are user friendly search algorithms:

- breadthFirstSearch
- uniformCostSearch
- depthFirstSearch   (with iterative flavor)
- greedyBestFirst
- aStarSearch        (with iterative flavor)

Below an example of signature, `s` is the type of the state, `p` the type of the step_cost, its constraints
are `Num` or `Num` and `Ord` depending on whether this value is used to choose the next node from the fringe.

~~~haskell
depthFirstSearch :: (Eq s, Num p) => (s -> Bool)        -- ^ function used to check if the goal is reached
                                  -> (s -> [(s, p)])    -- ^ generator of states
                                  -> s                  -- ^ initial state
                                  -> [s]                -- ^ returns list of states
~~~

## Custom use

If simple use of algorithms is not enough, for instance if there is the need to change Fridge implementation or output a list
of states is not satisfactory then is the case of `AI.Search.Generics` which specifies more customizable search functions.
For instance searchUntilDepth enables to define a max depth in search but also the policy (common policies in `AI.Search.Policies`) and
when check for the goal.

~~~haskell

searchUntilDepth :: (Eq s, Ord i, Num p) => MaxDepth            -- ^ max depth to search
                                         -> (SNode s p -> i)    -- ^ policy to be used
                                         -> CheckTime           -- ^ when apply check goal
                                         -> (s -> Bool)         -- ^ function used to check if the goal is reached
                                         -> (s -> [(s, p)])     -- ^ generator of states
                                         -> s                   -- ^ initial state
                                         -> [s]                 -- ^ returns list of states
~~~

for maximum control can be used `genericSearch` which allows to change Fridge (2 implementations in `Data.AI.Search.Fridge`) and also
output. For instance if is useful to skip already expanded nodes (avoiding loops) with genericSearch is possibile:

~~~haskell
import qualified Data.AI.Search.Fridge.PrioritySetFringe as PS
-- equivalent to depthFirstSearch but skips yet expanded nodes cause different Fridge
depthFirstSearch' = genericSearch PS.empty state Forever depthFirstSearch Generation
~~~
