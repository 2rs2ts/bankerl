%% A banking module which stores accounts and can perform transactions.
%% Andrew Garrett
-module(bank).

%% A bank account under the ownership of an owner with name 'name', of type 'type',
%% and a balance 'balance'.
-record(account, {  name :: atom(),
                    type :: atom(),
                    balance = 0 :: non_neg_integer()  }).

%% maybe use an orddict

%% size/1
%% Return the number of accounts in the Bank: {ok, Size}
size(Bank) -> {ok, erlang:length(Bank)}.

%% accounts/2
%% Return the account types associated with a particular owner: {ok, TypeList}
accounts(Bank, Owner) ->
	FindType = fun(A) when A#account.name == Owner -> A#account.type end,
	{ok, lists:map(FindType, Bank)}.

%% balance/3
%% Return the balance in a particular owner's account of a particular type:
%% {ok, Balance}
% could use lists:keyfind
balance(Bank, Owner, Type) -> 
	FindBal = fun(A) when A#account.name == Owner, A#account.type == Type ->
		A#account.balance end,
	{Balance} = lists:map(FindBal, Bank),
	{ok, Balance}.


%% open/3
%% Create a new account of a specified type for a specified owner.
%% If the owner does not have an account of this type: {ok, NewBank}
%% If the owner already has an account of this type: {error, "Duplicate account"}
% need to find both owner and type
%open(Bank, Owner, Type) when lists:keyfind(Owner, #account.name, Bank) ->
%	{error, "Duplicate account"};
%open(Bank, Owner, Type) ->
%	NewAccount = #account(name = Owner, type = Type),
%	{ok, lists:keystore(Owner, #account.name, Bank, NewAccount}.	

open(Bank, Owner, Type) ->
	Find = fun(A) when A#account.name == Owner, A#account.type == Type -> A end,
	case lists:any(Find, Bank) of
		true -> {error, "Duplicate account"};
		false -> {ok, [#account(name = Owner, type = Type) | Bank]}
	end.


%% close/3
%% Close a specified owner's account of a specified type.
%% If the owner had an account of this type: {ok, {NewBank, ClosingBalance}}
%% If the owner did not have an account of this type: {error, "No such account"}
close(Bank, Owner, Type) ->
	Find = fun(A) when A#account.name == Owner, A#account.type == Type -> A end,
	{Account} = lists:map(Find, Bank},
	lists:delete(Account, Bank).


%% deposit/4
%% Deposit a specified amount of funds in a specified owner's account of a specified
%% type.
%% If the owner has an account of this type: {ok, NewBank}
%% If the owner does not have an account of this type: {error, "No such account"}
%% If the specified amount is not positive: {error, "Negative amount"}
deposit(_Bank, _Owner, _Type, Amount) when Amount < 0 ->
	{error, "Negative amount"};
deposit(Bank, Owner, Type, Amount) ->
% can't use keystore b/c appending, only nth value
	Find = fun(A) when A#account.name == Owner, A#account.type == Type -> A end,
	


%% withdraw/4
%% Withdraw a specified amount of funds from a specified owner's account of a
%% specified type.
%% If the owner has an account of this type and there are sufficient funds to
%% withdraw: {ok, NewBank}
%% If the owner has an account of this type but there are insufficient funds to
%% withdraw: {error, "Overdrawn"}
%% If the owner does not have an account of this type: {error, "No such account"}
%% If the specified amount is not positive: {error, "Negative amount"}
withdraw(_Bank, _Owner, _Type, Amount) when Amount < 0 ->
	{error, "Negative amount"};
withdraw(Bank, Owner, Type, Amount) ->
