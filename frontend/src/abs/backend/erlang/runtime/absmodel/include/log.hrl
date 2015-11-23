%%This file is licensed under the terms of the Modified BSD License.

-undef(DEBUG).

-define(DEBUG(Term),eventstream:log({?MODULE,self(),Term})).