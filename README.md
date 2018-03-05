# Haskell-Re
Yet another regular expression engine implementation in Haskell

### Build
```bash
$ stack build
```

### Features
support:

`?` The question mark indicates zero or one occurrences of the preceding element. For example, colou?r matches both "color" and "colour".

`*` The asterisk indicates zero or more occurrences of the preceding element. For example, ab\*c matches "ac", "abc", "abbc", "abbbc", and so on.

`+` The plus sign indicates one or more occurrences of the preceding element. For example, ab+c matches "abc", "abbc", "abbbc", and so on, but not "ac".

`[]` A bracket expression. Matches a single character that is contained within the brackets. For example, [abc] matches "a", "b", or "c". [a-z] specifies a range which matches any lowercase letter from "a" to "z". These forms can be mixed: [abcx-z] matches "a", "b", "c", "x", "y", or "z", as does [a-cx-z].

`[^]` Matches a single character that is not contained within the brackets. For example, [^abc] matches any character other than "a", "b", or "c". [^a-z] matches any single character that is not a lowercase letter from "a" to "z". Likewise, literal characters and ranges can be mixed.

`()` Defines a marked subexpression. The string matched within the parentheses can be recalled later (see the next entry, \n). A marked subexpression is also called a block or capturing group.

### Automata

#### NFA-test:
```bash
*Regex.RegexParser> runRegexParser nfa "(m|f)ood"
[(states            :fromList [0,1,2,3,4,5,6,7,8]
input chars       :fromList "dfmo"
transitions       :fromList [Edge 1 'm' 2,Edge 3 'f' 4,Edge 5 'o' 6,Edge 6 'o' 7,Edge 7 'd' 8,Epsilon 0 1,Epsilon 0 3,Epsilon 2 5,Epsilon 4 5]
initial state     :0
acceptable states :fromList [8],"")]

*Regex.RegexParser> runRegexParser nfa "[a-bc]"
[(states            :fromList [0,1,2,3,4,5,6,7,8,9]
input chars       :fromList "abc"
transitions       :fromList [Edge 1 'a' 2,Edge 4 'b' 5,Edge 6 'c' 7,Epsilon 0 1,Epsilon 0 3,Epsilon 2 9,Epsilon 3 4,Epsilon 3 6,Epsilon 5 8,Epsilon 7 8,Epsilon 8 9]
initial state     :0
acceptable states :fromList [9],"")]

*Regex.RegexParser> runRegexParser nfa "[^a-zA-Z]"
[(states            :fromList [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37]
input chars       :fromList "0123456789"
transitions       :fromList [Edge 1 '0' 2,Edge 4 '1' 5,Edge 7 '2' 8,Edge 10 '3' 11,Edge 13 '4' 14,Edge 16 '5' 17,Edge 19 '6' 20,Edge 22 '7' 23,Edge 25 '8' 26,Edge 27 '9' 28,Epsilon 0 1,Epsilon 0 3,Epsilon 2 37,Epsilon 3 4,Epsilon 3 6,Epsilon 5 36,Epsilon 6 7,Epsilon 6 9,Epsilon 8 35,Epsilon 9 10,Epsilon 9 12,Epsilon 11 34,Epsilon 12 13,Epsilon 12 15,Epsilon 14 33,Epsilon 15 16,Epsilon 15 18,Epsilon 17 32,Epsilon 18 19,Epsilon 18 21,Epsilon 20 31,Epsilon 21 22,Epsilon 21 24,Epsilon 23 30,Epsilon 24 25,Epsilon 24 27,Epsilon 26 29,Epsilon 28 29,Epsilon 29 30,Epsilon 30 31,Epsilon 31 32,Epsilon 32 33,Epsilon 33 34,Epsilon 34 35,Epsilon 35 36,Epsilon 36 37]
initial state     :0
acceptable states :fromList [37],"")]
```

####DFA-test:
```bash
*Regex.RegexParser> runRegexParser dfa "(m|f)ood"
[(states            :fromList [fromList [0,1,3],fromList [2,5],fromList [4,5],fromList [6],fromList [7],fromList [8]]
input chars       :fromList "dfmo"
transitions       :fromList [Edge (fromList [0,1,3]) 'f' (fromList [4,5]),Edge (fromList [0,1,3]) 'm' (fromList [2,5]),Edge (fromList [2,5]) 'o' (fromList [6]),Edge (fromList [4,5]) 'o' (fromList [6]),Edge (fromList [6]) 'o' (fromList [7]),Edge (fromList [7]) 'd' (fromList [8])]
initial state     :fromList [0,1,3]
acceptable states :fromList [fromList [8]],"")]

*Regex.RegexParser> runRegexParser dfa "[a-bc]"
[(states            :fromList [fromList [0,1,3,4,6],fromList [2,9],fromList [5,8,9],fromList [7,8,9]]
input chars       :fromList "abc"
transitions       :fromList [Edge (fromList [0,1,3,4,6]) 'a' (fromList [2,9]),Edge (fromList [0,1,3,4,6]) 'b' (fromList [5,8,9]),Edge (fromList [0,1,3,4,6]) 'c' (fromList [7,8,9])]
initial state     :fromList [0,1,3,4,6]
acceptable states :fromList [fromList [2,9],fromList [5,8,9],fromList [7,8,9]],"")]

*Regex.RegexParser> runRegexParser dfa "[^a-zA-Z]"
[(states            :fromList [fromList [0,1,3,4,6,7,9,10,12,13,15,16,18,19,21,22,24,25,27],fromList [2,37],fromList [5,36,37],fromList [8,35,36,37],fromList [11,34,35,36,37],fromList [14,33,34,35,36,37],fromList [17,32,33,34,35,36,37],fromList [20,31,32,33,34,35,36,37],fromList [23,30,31,32,33,34,35,36,37],fromList [26,29,30,31,32,33,34,35,36,37],fromList [28,29,30,31,32,33,34,35,36,37]]
input chars       :fromList "0123456789"
transitions       :fromList [Edge (fromList [0,1,3,4,6,7,9,10,12,13,15,16,18,19,21,22,24,25,27]) '0' (fromList [2,37]),Edge (fromList [0,1,3,4,6,7,9,10,12,13,15,16,18,19,21,22,24,25,27]) '1' (fromList [5,36,37]),Edge (fromList [0,1,3,4,6,7,9,10,12,13,15,16,18,19,21,22,24,25,27]) '2' (fromList [8,35,36,37]),Edge (fromList [0,1,3,4,6,7,9,10,12,13,15,16,18,19,21,22,24,25,27]) '3' (fromList [11,34,35,36,37]),Edge (fromList [0,1,3,4,6,7,9,10,12,13,15,16,18,19,21,22,24,25,27]) '4' (fromList [14,33,34,35,36,37]),Edge (fromList [0,1,3,4,6,7,9,10,12,13,15,16,18,19,21,22,24,25,27]) '5' (fromList [17,32,33,34,35,36,37]),Edge (fromList [0,1,3,4,6,7,9,10,12,13,15,16,18,19,21,22,24,25,27]) '6' (fromList [20,31,32,33,34,35,36,37]),Edge (fromList [0,1,3,4,6,7,9,10,12,13,15,16,18,19,21,22,24,25,27]) '7' (fromList [23,30,31,32,33,34,35,36,37]),Edge (fromList [0,1,3,4,6,7,9,10,12,13,15,16,18,19,21,22,24,25,27]) '8' (fromList [26,29,30,31,32,33,34,35,36,37]),Edge (fromList [0,1,3,4,6,7,9,10,12,13,15,16,18,19,21,22,24,25,27]) '9' (fromList [28,29,30,31,32,33,34,35,36,37])]
initial state     :fromList [0,1,3,4,6,7,9,10,12,13,15,16,18,19,21,22,24,25,27]
acceptable states :fromList [fromList [2,37],fromList [5,36,37],fromList [8,35,36,37],fromList [11,34,35,36,37],fromList [14,33,34,35,36,37],fromList [17,32,33,34,35,36,37],fromList [20,31,32,33,34,35,36,37],fromList [23,30,31,32,33,34,35,36,37],fromList [26,29,30,31,32,33,34,35,36,37],fromList [28,29,30,31,32,33,34,35,36,37]],"")]
```

### TODO
#### Most important
improve the speed!

`{m, n}` Matches the preceding element at least m and not more than n times. For example, a{3,5} matches only "aaa", "aaaa", and "aaaaa". This is not found in a few older instances of regexes.

`$` Matches the ending position of the string or the position just before a string-ending newline. In line-based tools, it matches the ending position of any line.
