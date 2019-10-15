/* ai idea:
1. we have a list total include words starting from a to z and words 5~10 letters
2. We select list words having length like word input
3. We select list words starting with letter like word input
4. ai will choose random in the list
--future
5.  - After compare word ai and word input, we have a `result` include the index correctly placed letter, and index correctly letter
    - select from older list to a new list which have a word similar the `result` 
    - input new list to ai
    - repeat step 5
*/

/* humain idea:
1. Randomly selected in the file -> word 
2. Show the 1st letter and the length of the word
3. Input the guess word
4. Compare 2 words together 
5. Repeat step 3
6. Finish
*/
main:-  write('1. AI, 2. Humain'),nl,write('Votre choix: '), read(X),
        ((X=1, ai); (X=2, humain)).

% play game by ai
ai :-   write('Votre mot: '), read(Word), 
        atom_chars(Word,List_word),
        longueur(List_word,Long),
		open_file(List_word,Long,Result), 
		suggestion(List_word,Long),
		ai(Result,Word,1,Long).

ai(List,Word,N,End):-   choix(Guess,List), nl, write('Guess: '), write(Guess), nl,
                        list_index(Word,Guess,End, Result), write(Result), nl,
                        (
                            (Guess == Word, write('win'));
                            (
                                (   N \= End, 
                                    N1 is N+1, 
                                    remove_word(Guess, Result, List, New_list),
                                    list_new(Guess,New_list,Result,New_Result),
                                    ai(New_Result,Word,N1,End)
                                );
                                (
                                    N == End,
                                    nl, write('Loose'),
									nl, write('word: '),write(Word)
                                )
                            )
                        ).


% play game by humain
humain:-choix(Char_code,[97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122]),
		choix(Number,[5,6,7,8,9,10]),
		char_code(Ele,Char_code),
		atom_chars(Ele,Ele_word),
		open_file(Ele_word,Number,Result),
		choix(Word,Result),
		atom_chars(Word,List_word),longueur(List_word,Long),
		suggestion(List_word,Long),
		write('Votre Proposition: '), read(Guess), write(Guess), nl,
		humain(Word,Guess,1,Long).

humain(Word,Word,_,_):- nl,write('win').
humain(Word,Word,Long,Long):- nl, write('win').
humain(Word,Guess,Long,Long):-nl, Word \= Guess, list_index(Word,Guess, Long,Result), write(Result),nl, write('loose'),nl, write('word: '),write(Word).
humain(Word,Guess,N,Long):-Word \= Guess,
						list_index(Word,Guess, Long,Result),write(Result), 
						nl, write('Votre Proposition: '), read(X), write(X),nl, 
						N1 is N+1, humain(Word,X,N1,Long).

% Give a suggestion
% write 1st letter and nomber of length word
suggestion([H|_],Long) :- write(H), write_other_lettre(Long), nl.
write_other_lettre(1).
write_other_lettre(N):-write(*), write_other_lettre(N1), N is N1+1.

% read file and return the list in file
open_file([H|_],Long,X):-   lower_upper(H,H_up),name(H_up,H_code),name(Long,Long_code),name('.txt',Fextension),name('dicos-txt/',Repertoire),
						    concat(Repertoire,H_code,Name),concat(Name,Long_code,Name2), concat(Name2,Fextension,Name3),
						    name(Result,Name3),  open(Result,read,Str), lire_mots(Str,X),close(Str).

% read each words in the file
lire_mots(Stream,[]):-      at_end_of_stream(Stream).
lire_mots(Stream,[Mot|L]):- \+ at_end_of_stream(Stream),
							read(Stream,Mot),
							lire_mots(Stream,L).

% concat two listes		
concat([],L2,L2).
concat([X|Q],L2,[X|R]):-concat(Q,L2,R).

% the length of the word
longueur([],0).
longueur([_|Q],N1):- longueur(Q,N2), N1 is N2+1.

% choose random word
choix(Ele,Dictionnaire):-   randomize, 
						    longueur(Dictionnaire,Long),
						    random(0,Long,N),
						    element_at(Ele,Dictionnaire,N).

% find the word with position in the list
element_at(X,[X],0).
element_at(X,[X|_],0).
element_at(X,[_|Q],N1):- element_at(X,Q,N2), N1 is N2+1.

% compare 2 word 
list_index(Word,Guess,N,Result):-   atom_chars(Word,Ls1),
                                    atom_chars(Guess,Ls2),
                                    match_letter(Ls1,Ls2,Lists_same_value_index,0),
                                    correct_letter(Ls1,Ls2,Lists_same_value_not_index,0),
                                    remove_list(Lists_same_value_not_index,Lists_same_value_index,Lists_same_value_not_index_2),
                                    add_list_result(Lists_same_value_index,Lists_same_value_not_index_2,Result,0,N).

% filter from the older list to new list which has correct letter
list_new(Guess,List,Showed_result,X):-  exclamation_mark(Guess,List,Showed_result,New_list_match),
                                        question_mark(Guess,New_list_match,Showed_result,X).

% filter to new list correctly placed et correctly letter
exclamation_mark(Guess,List,Showed_result,New_list_match):- find_symbol('!',Showed_result,Lists_same_value_index),
                                                            find_list_match_letter(Guess,List,Lists_same_value_index,New_list_match).

% filter to new list correctly letter non placed
question_mark(Guess,List,Showed_result,X):- find_symbol('?',Showed_result,Lists_same_letter),
                                            atom_chars(Guess,List_word),
                                            find_all_element(List_word,Lists_same_letter,Result),
                                            find_list_same_letter(Result,List,X).

% remove the word constraint '*' from list
remove_word(Word,Showed_result,List,New_list):- find_symbol('*',Showed_result,List_not_correct),
                                                atom_chars(Word,List_word),
                                                find_all_element(List_word,List_not_correct,Result),
                                                find_list_same_letter(Result,List,X),
                                                remove_list(List,X,New_list).

% find list letter if same position and same letter
match_letter([],_,[],_).
match_letter(_,[],[],_).
match_letter([H|T],[H|T1],[Pos|T2],Pos):-   Pos_next is Pos+1, match_letter(T,T1,T2,Pos_next).
match_letter([H|T],[H1|T1],T2,Pos):-        H \= H1, Pos_next is Pos+1, match_letter(T,T1,T2,Pos_next).

% find list same letter
correct_letter([],_,[],_).
correct_letter(_,[],[],_).
correct_letter([H|T],[H1|T1],[Pos|Ts],Pos) :-   check_element([H|T],H1), Pos_next is Pos+1, correct_letter(T,T1,Ts,Pos_next).
correct_letter([H|T],[H1|T1],Ts,Pos) :-         \+check_element([H|T],H1), Pos_next is Pos+1, correct_letter(T,T1,Ts,Pos_next).

% check element dans liste
check_element([E|_],E).
check_element([H|T],E) :- E\= H, check_element(T,E).

% if 2 list had same value, remove it -> fid the liste only same letter not same positon
remove_list([],_,[]). 
remove_list([H|T],L2,Result):-      check_element(L2,H), !, remove_list(T,L2,Result).
remove_list([H|T],L2,[H|Result]) :- remove_list(T,L2,Result).

% add element -> find the positions correct letter and non correct letter
add_list_result(_,_,[],End,End).
add_list_result([Pos|T1],L2,['!'|Result],Pos,End) :-   !,Pos_next is Pos + 1, add_list_result(T1,L2,Result,Pos_next,End).
add_list_result(L1,[Pos|T2],['?'|Result],Pos,End) :-   !,Pos_next is Pos + 1, add_list_result(L1,T2,Result,Pos_next,End).
add_list_result(L1,L2,['*'|Result],Pos,End) :-        Pos_next is Pos + 1, add_list_result(L1,L2,Result,Pos_next,End).

% find from the older list to select conditions
compare_2_list(L1,L2,List) :-   atom_chars(L1,Ls1),
                                atom_chars(L2,Ls2),
                                match_letter(Ls1,Ls2,Result,0),
                                Result == List.

% find word correct placed and letter
find_list_match_letter(_,[],_,[]).
find_list_match_letter(Guess,[H|T1],Lists_same_value_index,T2):-        \+compare_2_list(Guess,H,Lists_same_value_index),!,
                                                                        find_list_match_letter(Guess,T1,Lists_same_value_index,T2).
find_list_match_letter(Guess,[H|T1],Lists_same_value_index,[H|T2]):-    find_list_match_letter(Guess,T1,Lists_same_value_index,T2).

% find symbol ! or ?
find_symbol(X,L1,L2):-  find_symbol(X,L1,L2,0).
find_symbol(_,[],[],_).
find_symbol(X,[H|T],[Pos|Ts],Pos):- H == X,!, Pos_next is Pos+1,find_symbol(X,T,Ts,Pos_next).
find_symbol(X,[_|T],Ts,Pos):-       Pos_next is Pos+1,find_symbol(X,T,Ts,Pos_next).

% find form older list to select word which has known letters
check_all_element_in_word(_,[]).
check_all_element_in_word(Word,[H|T]):- check_element(Word,H), check_all_element_in_word(Word,T).

% find all letter in word
find_all_element(_,[],[]).
find_all_element(Word,[H|T],[Ele|Result]) :- element_at(Ele,Word,H), find_all_element(Word,T,Result).

% filter from older to new list which has correct letter non placed
find_list_same_letter([],X,X).
find_list_same_letter(_,[],[]).
find_list_same_letter(Guess,[H|T1],[H|T2]) :-   atom_chars(H,Hs),
                                                check_all_element_in_word(Hs,Guess),!,
                                                find_list_same_letter(Guess,T1,T2).
find_list_same_letter(Guess,[_|T1],T2) :-   find_list_same_letter(Guess,T1,T2).