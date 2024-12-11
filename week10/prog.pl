/*
    Ex. 
        append(X, Y, Z) - want to define the append relation
        append([2, 3], [4, 5], X) provide input 2,3 & 4,5 does it provide the X output?
                                X is the result 
        We can go beyond that in logical programming 
        So, 
        logic programming is based on relations and not functions. 
        You do not have an input and output, all parameters are 
        treated uniformly. 

    Ex.
        append([2, 3], [4, 5], [2, 3, 4, 5])
        append(4, 5[2, 3], [], X)
        append(X, [4, 5], [2, 3, 4, 5])
        append(X, Y, [2, 3, 4, 5])
        
        results are relational and unique - multiple solutions 

*/

:- set_prolog_flag(verbose,silent).
:- set_prolog_flag(occurs_check,true).
:- style_check(-singleton).

/*  Functional programming : limited assignment
 *  Imperative programming : every variable can be changed
 *  Logic programming      : you can incremenet your knowledge
 *                           you can start with nothing and add more
 *                           information unless it is not contradictory
 *
*/

/* Logic programming is based on relations and not functions .
   You do not have input and output, all parameters are treated
   uniformly

   append([2,3],[4,5],[2,3,4,5])
   append(X,[4,5],[2,3,4,5])
   append(X,Y,[2,3,4,5])
*/

/* FACTS - axioms */

father(john,mary).
father(sam,john).
father(sam,kathy).



/* Queries
   father(sam,john).
   father(sam,X). - is there an X, such that Sam is the father of X? System will pattern match 
   father(X,mary).
   father(X,john),father(X,kathy). - How to interpreter the comma? It notates and so -> is there a a father of John AND father of kathy
   father(X,john),father(X,Y). 
   father(X,john),father(X,Y),Y \== john.
   father(X,Y),father(X,Z). 
   
   Ex. Suppose g1, g2, g3 
   	How will we find the solutions? 
   	? g1, 	g2, 	 g3 
   	  /\  	/\  	 /\
   	a1 a2 	b1 b2 	c1 c2
   	
   	we first satisfy g1, which we will use that solution to satisfy g2 and g3. We
   	we use solution a1 & b1 & c1. 
   	But suppose that g3 is not satisfiable? We do backtracking. We go back and do a different 
   	solution -- using b2 instead of b1, etc. 
   	
*/


/* Inference rules  */
/*
	:- means if 
	Is there such rule that X is the grandfather of Y is X is the father of Z and Z is the father of Y. 
*/

grandpa(X,Y) :- father(X,Z),father(Z,Y).

/*******************************************************************/

edge(a,b).
edge(b,c).
edge(c,d). /* There is a cycle here so the system will go in a loop */
edge(d,c). /* There is a cycle here so the system will go in a loop */
edge(c,a).

/*   edge(X,Y)       edge(X,Z) path(Z,Y)
     ----------      -------------------------
     path(X,Y)        path(X,Y)
     
     There is a direct edge from X to Y((edge(X, Y)), 
     then there exists a path from X to Y (path(X, Y))
     
     Is there is an edge from X to Z (edge(X, Z)) and a path from Z to Y (path(Z, Y)), then there exists a path from X to Y (path(X, Y)). 
     
     
     
*/

path(X,Y) :- edge(X,Y).
path(X,Y) :- edge(X,Z) , path(Z,Y).


/*  With length of a path (to avoid the cycle path)
    1 is represented as s(0)
    2 is represented as s(s(0))
*/
pathN(X,X,0). /* a path from X to X with length 0 */
pathN(X,Y,s(N)) :- edge(X,Z),pathN(Z,Y,N). 
/* s means sucessor of N  see notes */


/*     lists
       list patterns  x :: xs
                     [X | Xs]
*/

append([],X,X). /* base casem*/
append([H | X1], Y, [H | Z]) :- append(X1, Y, Z).
/* 
    H = head 
    appending Y to the list [H | X1] results in [H | Z]
    Z is the result if i append Y to X1 

*/

/*  append([a],[b,c,d],X).
    append([a,b], [c,d], [a,b,c,d]).
    append([a,b],[c,d],Z).
    append(X,[c,d],[a,b,c,d]).
    append([a,b],Y,[a,b,c,d]).
    append(X,Y,[a,b,c,d]).
    append(X,X,[b,c,b,c]).
    append([b,c],X,X).
*/


prefix(X,Z) :- append(X,Y,Z).
/* X is a prefix of Z is appending some list Y to X results in Z*/
suffix(X,Z) :- append(Y,X,Z).
/* X is a suffix of Z if appending some list Y to X results in Z*/


member(X,[X|_]).
member(X,[Y|Ys]) :- X \== Y, member(X,Ys).
/* if X = Y then true else member (X, XS) */

/* X = [1,2,3,4], member(a,X).
   member(a,X), X =[1,2,3,4].
   this goal goes on forever, you will backtrack forever until you reach 
   list [1, 2, 3, 4] but is not possible. 
*/


overlap(X,Y) :- member(M,X),member(M,Y).


/* overlap([1,2,c,d],[a,b,c,d]).
   overlap([1,X,3],[5,Y,8]).
*/



tree_member(K,node(K,_,_)).
tree_member(K,node(N,S,_)) :- K < N,tree_member(K,S).
tree_member(K,node(N,_,T)) :- K > N,tree_member(K,T).


/*  Consistent assignment
 L=[a,b | X].
 L=[a,b | X], X = [C,Y].
 L=[a,b|X], L =[a,c,d| Y].
*/


/*  Unification

X = 2+3.
X is 2+3.
5 = 2+3.
X is 2+3, X = 2 + 3.
X is 2+3,X=5.
2+3 = 2 + Y.
2+3 = 3 + Z.
X is 5, 6 is X+1.
X=5, 6 is X+1.
X=5, 6=X+1.
X=X+2.
X=2+X.
f(X,b)=f(a,Y).
arrow(int,S)=arrow(T,bool).

*/


/* Type inference for the simple language described below:

E ::= n | E1+E2| true | false | if E1 then E2 else E3 |
      x | E1 E2 | fn x => E | let x = E1 in E2
*/

/* Env |- n : int

type(Env,lit N,int).

     Env |- E1 : int  Env |- E2 : int
     --------------------------------
     Env |- E1 + E2 : int

       (x:int)      |- x: int
-------------------------------------
     (x:int) |- x + x : int


type(Env, plus(E1,E2),int) :- type(Env,E1,int), type(Env,E2,int).

     E |- true : bool         E |- false : bool

type(Env,true,bool).
type(Env,false,bool).

    Env |- E1: bool   Env |- E2 : T  Env |- E3 : T
    ----------------------------------------------
    Env |- if E1 then E2 else E3 : T


type(Env, if(E1,E2,E3),T) :- type(Env,E1,bool),type(Env,E2,T),type(Env,E3,T).

type([[x,bool]],if(true,lit(0),lit(9)),Z).

     lookup(Env,x)=T
     ----------------
     Env |- x : T

type(Env,var(X),T):- member([X, T],Env).

     Env |- E1 : S -> T     Env |- E2 : S
     -----------------------------------
     Env |- E1 E2 :  T

type(Env,apply(E1,E2),T) :- type(Env,E1,arrow(S,T)),type(Env,E2,S)).

    Env, x:S |- E : T
    -----------------------------
    Env |- lambda x.E : S -> T

    type(Env,lambda(X,E),arrow(S,T)) :- type([[X,S] | Env],E,T).

       Env |- E1 : S     Env, x: S |- E2 : T
       -----------------------------------------
       Env |- let x = E1 in E2 : T

type(Env,let(X,E1,E2),T) :- type(Env,E1,S), type([[X,S] |Env], E2,T).

*/


/* Type Inference */

type(Env, 0,            int).
type(Env, s(X),         int)        :- type(Env,X,int).
type(Env, true,         bool).
type(Env, false,        bool).
type(Env, if(E1,E2,E3), T)          :- type(Env,E1,bool),
                                       type(Env,E2,T),
                                       type(Env,E3,T).
type(Env, var(X),       T)          :- member([X, T],Env).
type(Env, ap(E1,E2), T)             :- type(Env,E1,arrow(S,T)),
                                       type(Env,E2,S).
type(Env, lambda(X,E),  arrow(S,T)) :- type([[X,S] | Env],E,T).
type(Env, let(X,E1,E2), T)          :- type(Env,E1,S),
                                       type([[X,S] |Env], E2,T).


/* type([],lambda(x,s(var(x))),Z)  arrow(int,int)  \x.x+1*/
/* type([],let(x,0,s(var(x))),Z). let x =0 in x+1 */
/* type([],ap(lambda(x,var(x)),0), Z).  (\x.x)0*/
/* type([],ap(lambda(x,true),0), Z). (\x.true)0 */
/* type([],lambda(x,ap(var(x),var(x))),Z).  lambda x. x x  S -> T = S occur check */
/* type([],if(lit(0),X,Y),T).  */


/* A simple evaluator */
eval(Env,lit(N),N).
eval(Env,true,true).
eval(Env,false,false).
eval(Env,lambda(X,E),closure(lambda(X,E),Env)).
eval(Env,plus(E1,E2),N)             :- eval(E1,N1), eval(E2,N2),
                                       N is N1+N2.
eval(Env,var(X),V)              :- member([X,V],Env).
eval(Env,if(E1,E2,E3), V)       :- eval(Env,E1,true), eval(Env,E2,V).
eval(Env,if(E1,E2,E3), V)       :- eval(Env,E1,false),
                                      eval(Env,E3,V).
eval(Env,ap(E1,E2),V)           :- eval(Env,E1,closure(lambda(X,E),Env1)),
                                   eval(Env,E2,V2),
                                   eval([[X,V2]|Env1],E,V).
eval(Env,let(X,E1,E2),V)        :- eval(Env,E1,V1),
                                   eval([[X,V1]|Env],E2,V).
/*
L=let(x,lit(3),let(f,lambda(z,var(x)),let(x,lit(9),ap(var(f),var(x))))),eval([],L,Z).
*/