
-undef(DEBUG).

-define(DEBUG(Term), 
eventstream:log({?MODULE,self(),Term})).