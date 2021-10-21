

execution_loop(state(Acc, Pc, Mem, In, Out, Flag), Out1) :-

   one_instruction(state(Acc, Pc, Mem, In, Out, Flag),
   halted_state(_, _, _, _, Out1, _)).



execution_loop(state(Acc, Pc, Mem, In, Out, Flag), Out1) :-

   one_instruction(state(Acc, Pc, Mem, In, Out, Flag),
   state(NewAcc, NewPc, NewMem, In1, NewOut, NewFlag)),
   execution_loop(state(NewAcc, NewPc, NewMem, In1, NewOut, NewFlag), Out1).





% Add

one_instruction(state(Acc, Pc, Mem, In, Out, _),
 state(NewAcc, NewPc, Mem, In, Out, NewFlag)) :-

   scorpora(Pc, Mem, 1, Y1),
   !,
   pc_control(Pc, NewPc),
   nth0(Y1, Mem, Z1),
   Y2 is Acc + Z1,
   flag_control(Y2, NewAcc, NewFlag).



% Sub

one_instruction(state(Acc, Pc, Mem, In, Out, _),
 state(NewAcc, NewPc, Mem, In, Out, NewFlag)) :-

   scorpora(Pc, Mem, 2, Y1),
   !,
   pc_control(Pc, NewPc),
   nth0(Y1, Mem, Z1),
   Y2 is Acc - Z1,
   flag_control(Y2, NewAcc, NewFlag).



% Store

one_instruction(state(Acc, Pc, Mem, In, Out, Flag),
 state(Acc, NewPc, NewMem, In, Out, Flag)) :-

   scorpora(Pc, Mem, 3, Y1),
   !,
   change(Mem, Y1, Acc, NewMem),
   pc_control(Pc, NewPc).



% 4: Istruzione non valida

one_instruction(state(Acc, Pc, Mem, In, Out, Flag),
 state(Acc, Pc, Mem, In, Out, Flag)) :-
   scorpora(Pc, Mem, 4, _),
   !,
   write("Error, maybe a 42?"),
   fail.


% Load

one_instruction(state(_, Pc, Mem, In, Out, Flag),
 state(NewAcc, NewPc, Mem, In, Out, Flag)) :-

   scorpora(Pc, Mem, 5, Y1),
   !,
   nth0(Y1, Mem, Val),
   NewAcc is Val,
   pc_control(Pc, NewPc).


% Branch

one_instruction(state(Acc, Pc, Mem, In, Out, Flag),
 state(Acc, NewPc, Mem, In, Out, Flag)) :-

   scorpora(Pc, Mem, 6, Y1),
   !,
   NewPc is Y1.


% Branch zero

one_instruction(state(Acc, Pc, Mem, In, Out, Flag),
 state(Acc, NewPc, Mem, In, Out, Flag)) :-

   scorpora(Pc, Mem, 7, Y1),
   Flag = noflag,
   Acc is 0,
   !,
   NewPc is Y1.



one_instruction(state(Acc, Pc, Mem, In, Out, Flag),
 state(Acc, NewPc, Mem, In, Out, Flag)) :-

   scorpora(Pc, Mem, 7, _),
   !,
   pc_control(Pc, NewPc).



% Branch positive

one_instruction(state(Acc, Pc, Mem, In, Out, Flag),
 state(Acc, NewPc, Mem, In, Out, Flag)) :-

   scorpora(Pc, Mem, 8, Y1),
   Flag = noflag,
   !,
   NewPc is Y1.



one_instruction(state(Acc, Pc, Mem, In, Out, Flag),
 state(Acc, NewPc, Mem, In, Out, Flag)) :-

   scorpora(Pc, Mem, 8, _),
   !,
   pc_control(Pc, NewPc).



% Halt
one_instruction(state(Acc, Pc, Mem, In, Out, Flag),
 halted_state(Acc, NewPc, Mem, In, Out, Flag)) :-

   scorpora(Pc, Mem, 0, _),
   !,
   pc_control(Pc, NewPc).



% Input

one_instruction(state(_, Pc, Mem, In, Out, Flag),
 state(NewAcc, NewPc, Mem, In1, Out, Flag)) :-

   scorpora(Pc ,Mem, 9, 01),
   nth0(0, In, NewAcc, In1),
   pc_control(Pc, NewPc).



one_instruction(state(_, Pc, Mem, In, Out, Flag),
 state(_, _, Mem, _, Out, Flag)) :-

   scorpora(Pc ,Mem, 9, 01),
   !,
   length(In, 0),
   false.




% Output

one_instruction(state(Acc, Pc, Mem, In, Out, Flag),
 state(Acc, NewPc, Mem, In, Out1, Flag)) :-

   scorpora(Pc, Mem, 9, 02),
   !,
   append(Out, [Acc], Out1),
   pc_control(Pc, NewPc).




% Scorporamento

scorpora(X, Y, Z, Y1) :-
   nth0(X, Y, X1),
   modulo(X1, 100, Z, Y1).

modulo(X1, 100, H1, Y1) :-
   H1 is X1 div 100,
   Y1 is X1 mod 100.



% Controllo della flag

flag_control(X, Y, Z) :-
   X >= 1000,
   !,
   Y is X mod 1000,
   Z = flag.

flag_control(X, Y, Z) :-
   X < 0,
   !,
   Y is X mod 1000,
   Z = flag.

flag_control(X, Y, Z) :-
   X < 1000,
   X >= 0,
   Y is X,
   Z = noflag.


% Controllo program counter

pc_control(X, Y) :-
   Y is X + 1,
   Y < 100,
   !.

pc_control(_, Y) :-
   Y is 0.


% Modifica contenuto cella

change(X, Y, Z, X1) :-
   nth0(Y, X, _, Rest),
   nth0(Y, X1, Z, Rest).





% Lettura del file

file_to_list(File,List) :-

                        open(File, read, Stream),
                        read_string(Stream,_,String),
                        close(Stream),
                        string_upper(String, String1),
                        split_string(String1, "\n", "", List1),
                        delete_empty_lines(List1, 0, List).


% Elimina le linee vuote

delete_empty_lines(List1, Ind, List1) :-
                        length(List1, X),
                        X = Ind,
                        !.



delete_empty_lines(List1, Ind, List) :-
                        nth0(Ind, List1, String),
                        string_chars(String, FList),
                        all_empty(FList, 0, FlagEmpty),
                        FlagEmpty is 1,
                        !,
                        String1 = "",
                        change(List1, Ind, String1, List2),
                        Ind1 is Ind + 1,
                        delete_empty_lines(List2, Ind1, List).



delete_empty_lines(List1, Ind, List) :-
                        nth0(Ind, List1, String),
                        string_chars(String, FList),
                        all_empty(FList, 0, FlagEmpty),
                        FlagEmpty is 0,
                        Ind1 is Ind + 1,
                        delete_empty_lines(List1, Ind1, List).



% Controlliamo se la lista contiene solo char del tipo ' '

all_empty(FList, Ind, FlagEmpty) :-
                        nth0(Ind, FList, X),
                        X \= ' ',
                        !,
                        FlagEmpty is 0.



all_empty(FList, Ind, FlagEmpty) :-
                        nth0(Ind, FList, X),
                        X = ' ',
                        Ind1 is Ind + 1,
                        all_empty(FList, Ind1, FlagEmpty).



all_empty(FList, Ind, FlagEmpty) :-
                        length(FList, X),
                        Ind = X,
                        !,
                        FlagEmpty is 1.



% Elimina dall'indice n


delete_from_n([_|_], 0, []).



delete_from_n([X|Y], N, [X|Y]) :-
                        length([X|Y],S),
                        N >= S.



delete_from_n([X|Y], N, [X1|Y1]) :-
                        length([X|Y],S),
                        N < S,
                        nth0(N, [X|Y], _, [X2|Y2]),
                        delete_from_n([X2|Y2], N, [X1|Y1]).




% Eliminazione dei commenti nel file


elimina_commenti(List, N, List) :-
                         length(List,S),
                         N = S.



elimina_commenti(List, N, List1) :-
                         nth0(N, List, String),
                         string_chars(String, Charlist),
                         scommenta(Charlist, 0, 0, Newsflag, Newind),
                         Newsflag is 2,
                         Newind2 is Newind - 2,
                         delete_from_n(Charlist, Newind2, Charlist2),
                         string_chars(String2, Charlist2),
                         change(List, N, String2, List2),
                         S is N+1,
                         elimina_commenti(List2, S, List1).




elimina_commenti(List, N, List1) :-
                         nth0(N, List, String),
                         string_chars(String, Charlist),
                         scommenta(Charlist, 0, 0, Newsflag, _),
                         Newsflag is 0,
                         S is N+1,
                         elimina_commenti(List, S, List1).



% Rilevamento del carattere '/'


scommenta(List, Sflag, Ind, Newsflag, Newind) :-
                         length(List, S),
                         Ind = S,
                         Newind is Ind,
                         Newsflag is Sflag.


scommenta(List, Sflag, Ind, Newsflag, Newind) :-
                         nth0(Ind, List, '/'),
                         Ind1 is Ind+1,
                         Sflag1 is Sflag +1,
                         scommenta_sp(List, Sflag1, Ind1, Newsflag, Newind).


scommenta(List, Sflag, Ind, Newsflag, Newind) :-
                         nth0(Ind, List, F),
                         F \= '/',
                         Ind1 is Ind+1,
                         scommenta(List, Sflag, Ind1, Newsflag, Newind).



% Rileva il secondo '/'

scommenta_sp(List, _, Ind, _, _) :-
                         length(List, S),
                         Ind = S,
                         false.

scommenta_sp(List, _, Ind, Newsflag, Newind) :-
                         nth0(Ind, List, '/'),
                         Newind is Ind+1,
                         Newsflag is 2.


scommenta_sp(List, _, Ind, _, _) :-
                         nth0(Ind, List, F),
                         F \= '/',
                         false.




% Rimette gli spazi per ricostruire la stringa


re_insert_space([], [" "]).

re_insert_space([X], [X," "]).

re_insert_space([X|Y], [X, " "|Y1]) :-
                         re_insert_space(Y, Y1).


% Concatena le stringhe nella lista in una nuova stringa


concat(List, Ind, String0, String) :-
                          length(List, X),
                          Ind =< X,
                          !,
                          nth1(Ind, List, String1),
                          string_concat(String0, String1, String2),
                          Ind1 is Ind + 1,
                          concat(List, Ind1, String2, String).




concat(List, Ind, String0, String0) :-
                          length(List, X),
                          Ind > X.





% Rileva le label presenti nel file e le inserisce in una lista
% [Lable, valore_label]



rileva_label(List, Ind, List, LabelList, LabelList) :-
                          length(List, X),
                          X = Ind.


rileva_label(List, Ind, List1, LabelList, LabelList1) :-
                           nth0(Ind, List, String),
                           split_string(String, " ", " ", SList),
                           analizza(SList, Flaglabel, _),
                           Flaglabel is 0,
                           Ind1 is Ind + 1,
                           rileva_label(List, Ind1, List1, LabelList, LabelList1).



rileva_label(List, Ind, List1, LabelList, LabelList1) :-
                           nth0(Ind, List, String),
                           split_string(String, " ", " ", SList),
                           analizza(SList, Flaglabel, Label),
                           Flaglabel is 1,
                           save_canc_lab(SList, Label, Ind, LabelList, LabelList2, SList1),
                           re_insert_space(SList1, SList2),
                           concat(SList2, 1, "", String1),
                           change(List, Ind, String1, List2),
                           Ind1 is Ind + 1,
                           rileva_label(List2, Ind1, List1, LabelList2, LabelList1).



% Trova la label e imposta la Flaglabel


analizza(SList1, _, _) :-
                           length(SList1, Y),
                           Y > 3,
                           !,
                           false.


analizza(SList1, Flaglabel, _) :-
                           length(SList1, Y),
                           Y is 2,
                           Flaglabel is 0.



analizza(SList1, Flaglabel, _) :-
                           length(SList1, Y),
                           Y is 1,
                           !,
                           Flaglabel is 0.



analizza(SList1, Flaglabel, Label) :-
                           length(SList1, Y),
                           Y is 3,
                           nth0(0, SList1, Label),
                           Label \= "ADD",
                           Label \= "SUB",
                           Label \= "STA",
                           Label \= "LDA",
                           Label \= "BRA",
                           Label \= "BRZ",
                           Label \= "BRP",
                           Label \= "INP",
                           Label \= "OUT",
                           Label \= "HLT",
                           Label \= "DAT",
                           Flaglabel is 1.



analizza(SList1, Flaglabel, Label) :-
                           length(SList1, Y),
                           Y is 2,
                           nth0(0, SList1, Label),
                           Label \= "ADD",
                           Label \= "SUB",
                           Label \= "STA",
                           Label \= "LDA",
                           Label \= "BRA",
                           Label \= "BRZ",
                           Label \= "BRP",
                           Label \= "INP",
                           Label \= "OUT",
                           Label \= "HLT",
                           Label \= "DAT",
                           !,
                           Flaglabel is 1.


% Salva nella lista la label e il suo valore e poi elimina la label dal
% file


save_canc_lab(SList, Label, Ind, LabelList, LabelList2, SList1) :-
                           nth0(0, SList, _, SList1),
                           insert_in_list(LabelList, Label, Ind, LabelList2).


% Inserisce nella lista la label e il suo valore

insert_in_list([], Label, Ind, [Label, Ind]).

insert_in_list([X|Y], Label, Ind, [X|Y1]) :-
                            insert_in_list(Y, Label, Ind, Y1).




% Assegna alle label presenti nel file il loro valore



assegna_label(List1, Ind, _, List1) :-
                            length(List1, X),
                            Ind = X.



assegna_label(List1, Ind, LabelList, List3) :-
                            nth0(Ind, List1, String),
                            split_string(String, " ", " ", SList),
                            study_label(SList, LabelList, SList1),
                            re_insert_space(SList1, SList2),
                            concat(SList2, 1, "", String1),
                            change(List1, Ind, String1, List2),
                            Ind1 is Ind + 1,
                            assegna_label(List2, Ind1, LabelList, List3).



%Converte la label nel valore numerico corrispondente


study_label(SList, _, SList) :- length(SList, 1).



study_label(SList, _, SList) :-
                            nth0(1, SList, Element),
                            atom_number(Element, NewElement),
                            !,
                            NewElement >= 0.



study_label(SList, LabelList, SList1) :-
                            nth0(1, SList, Element),
                            confronta(Element, NewElement, LabelList, 0),
                            change(SList, 1, NewElement, SList1).



% Confrota la label trovata con quelle presenti in lista


confronta(Element, NewElement, LabelList, Ind) :-
                             nth0(Ind, LabelList, X),
                             X = Element,
                             !,
                             Ind1 is Ind +1,
                             nth0(Ind1, LabelList, NewElement).



confronta(Element, NewElement, LabelList, Ind) :-
                             nth0(Ind, LabelList, X),
                             X \= Element,
                             Ind1 is Ind + 2,
                             confronta(Element, NewElement, LabelList, Ind1).




confronta(Element, Element, LabelList, Ind) :-
                             length(LabelList, X),
                             Ind = X,
                             false.





% Converte la lista delle istruzioni nei valori corrispondenti


convert(List, Ind, List) :-
                             length(List, X),
                             X = Ind.



convert(List, Ind, List1) :-
                             nth0(Ind , List, String),
                             split_string(String, " ", " ", SList),
                             number(SList, Int),
                             Ind1 is Ind + 1,
                             change(List, Ind, Int, List2),
                             convert(List2, Ind1, List1).



% Converte l'istruzione nel valore corrispondente


number(SList, Int) :-
                             nth0(0, SList, X),
                             X = "ADD",
                             !,
                             nth0(1, SList, Y),
                             atom_number(Y, Num),
                             Num < 100,
                             Int is 100 + Num.




number(SList, Int) :-
                             nth0(0, SList, X),
                             X = "SUB",
                             !,
                             nth0(1, SList, Y),
                             atom_number(Y, Num),
                             Num < 100,
                             Int is 200 + Num.




number(SList, Int) :-
                             nth0(0, SList, X),
                             X = "STA",
                             !,
                             nth0(1, SList, Y),
                             atom_number(Y, Num),
                             Num < 100,
                             Int is 300 + Num.



number(SList, Int) :-
                             nth0(0, SList, X),
                             X = "LDA",
                             !,
                             nth0(1, SList, Y),
                             atom_number(Y, Num),
                             Num < 100,
                             Int is 500 + Num.



number(SList, Int) :-
                             nth0(0, SList, X),
                             X = "BRA",
                             !,
                             nth0(1, SList, Y),
                             atom_number(Y, Num),
                             Num < 100,
                             Int is 600 + Num.



number(SList, Int) :-
                             nth0(0, SList, X),
                             X = "BRZ",
                             !,
                             nth0(1, SList, Y),
                             atom_number(Y, Num),
                             Num < 100,
                             Int is 700 + Num.


number(SList, Int) :-
                             nth0(0, SList, X),
                             X = "BRZ",
                             !,
                             nth0(1, SList, Y),
                             atom_number(Y, Num),
                             Int is 700 + Num.



number(SList, Int) :-
                             nth0(0, SList, X),
                             X = "BRP",
                             !,
                             nth0(1, SList, Y),
                             atom_number(Y, Num),
                             Num < 100,
                             Int is 800 + Num.


number(SList, Int) :-
                             length(SList, 1),
                             nth0(0, SList, X),
                             X = "INP",
                             !,
                             Int is 901.



number(SList, Int) :-
                             length(SList, 1),
                             nth0(0, SList, X),
                             X = "OUT",
                             !,
                             Int is 902.


number(SList, Int) :-
                             length(SList, 1),
                             nth0(0, SList, X),
                             X = "HLT",
                             !,
                             Int is 0.



number(SList, Int) :-
                             length(SList, 2),
                             nth0(0, SList, X),
                             X = "DAT",
                             !,
                             nth0(1, SList, Y),
                             atom_number(Y, Int).



number(SList, Int) :-
                             nth0(0, SList, X),
                             X = "DAT",
                             Int is 0.




% Riempimento delle celle di memoria con 0



full_fill(Mem1, Mem1, Ind) :-
                             length(Mem1, X),
                             Ind = X.



full_fill(Mem1, Mem2, Ind) :-
                             change(Mem1, Ind, 0, Mem3),
                             Ind1 is Ind + 1,
                             full_fill(Mem3, Mem2, Ind1).



% Definisce la memoria



def_mem(Mem1, Mem1) :-
                            length(Mem1, 100),
                            !.



def_mem(Mem1, _) :-
                             length(Mem1, X),
                             X>100,
                             false.


def_mem(Mem1, Mem) :-
                             length(Mem1, X),
                             Y is 100 - X,
                             length(Mem2, Y),
                             full_fill(Mem2, Mem3, 0),
                             append(Mem1, Mem3, Mem).





lmc_load(Filename, Mem) :-
                             file_to_list(Filename, List),
                             elimina_commenti(List, 0, List1),
                             delete(List1, "", List2),
                             rileva_label(List2, 0, List3,[], LabelList1),
                             assegna_label(List3, 0, LabelList1, List4),
                             convert(List4, 0, List5),
                             def_mem(List5, Mem).




lmc_run(Filename, In, Out) :-
                     lmc_load(Filename, Mem),
                     execution_loop(state(0, 0, Mem, In, [], noflag ), Out).














