bankerl
=======

A bank database simulation. A homework assignment from my [concurrent software class] [1]. (Project writeup does NOT belong to me, and this README is heavily based on the writeup.)

The Data
--------

### Bank

A list of 0 or more _Accounts_.

### Account

A tuple of three elements:

1. The account _owner_, an atom
2. The account _type_ (e.g. checking), an atom
3. The account _balance_, a non-negative integer

The Functions
-------------

### Return Types (Responses)

Each function returns a tuple of one of the following three forms.

* __{ok, Bank}__ for operations which initialize a new bank, i.e. init/0.
* __{ok}__ for operations that change the information in the Bank.
* __{ok, Response}__ for functions that query a _Bank_ - in these cases, the new and old bank lists are the same. The type of the _Response_ depends on the query itself.
* __{error, Diagnostic}__ for functions that fail; in such cases, it assumed the _Bank_ is unchanged, and _Diagnostic_ is a string describing the error.

### Query Functions

* __bank:size(Bank)__
	Returns the number of accounts in the _Bank_: {ok, Number}.
* __bank:accounts(Bank, Owner)__
	Returns __{ok, TypeList}__ where _TypeList_ is the list of account types associated with _Owner_.
* __bank:balance(Bank, Owner, Type)__
	* If the _Owner_ has an account of the given _Type_, returns __{ok, Balance}__.
	* If not, returns __{error, "No such account"}__.

### Operations

* __bank:open(Bank, Owner, Type)__
	* If the _Owner_ does not already have an account of the given _Type_ in the _Bank_, returns __{ok}__ and opens a new account with balance 0.
	* If the _Owner_ does already have an account of the given _Type_, returns __{error, "Duplicate account"}__.
* __bank:close(Bank, Owner, Type)__
	* If the _Owner_ has an account of the given _Type_ in the _Bank_, returns __{ok, ClosingBalance}__ where _ClosingBalance_ is the balance in the account; the account specified is now removed.
	* If not, returns __{error, "No such account"}__.
* __bank:deposit(Bank, Owner, Type, Amount)__
	* If the _Owner_ has an account of the given _Type_ in the _Bank_, and if the _Amount_ is positive, returns __{ok}__; the account balance is increased by _Amount_.
	* If the _Amount_ is not positive, returns __{error, "Negative amount"}__.
	* If the _Owner_ does not have an account of the given _Type_, returns __{error, "No such account"}__.
* __bank:withdraw(Bank, Owner, Type, Amount)__
	* If the _Owner_ has an account of the given _Type_ in the _Bank_, and if the _Amount_ is positive and does not exceed the account balance, returns __{ok}__; the account balance is decreased by _Amount_.
	* If the _Amount_ is not positive, returns __{error, "Negative amount"}__.
	* If the _Owner_ does not have an account of the given _Type_, returns __{error, "No such account"}__.
	* If the _Amount_ exceeds the account balance, returns __{error, "Overdrawn"}__.



[1]: http://www.se.rit.edu/~se441/spring_2013/Assignments/ErlangSequential3.html
