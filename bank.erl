%% A banking module which stores accounts and can perform transactions.
%% Andrew Garrett
-module(bank).
-export([   size/1, accounts/2, balance/3,
            init/0, open/3, close/3, deposit/4, withdraw/4  ]).

%% A bank account under the ownership of an owner with owner 'owner', of type
%% 'type', and a balance 'balance'.
-record(account,
        { owner :: atom()
        , type :: atom()
        , balance = 0 :: non_neg_integer()
        }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Client-side functions
%%
%%  In these functions, Bank is the Pid of the Bank.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% init/0
%% Create a new Bank: {ok, Bank} where Bank is the process id of the Bank.
%% Must be called before any other operation.
init() ->
    BankMain = fun(BM, Bank) ->
        receive
            % {Pid, cmd, Key, Value} -> % Client cmds must pass self() as Pid
            %   {Status, Result} = operation(Bank, Key, Value),
            %   case {Status, Result} of
            %       {error, Reason} -> Pid ! {error, Result}, loop(Bank);
            %       {ok, NewBank} -> Pid ! {ok}, loop(NewBank)
            %   end;
            {Pid, size} ->
                Pid ! s_size(Bank),
                BM(BM, Bank);
            {Pid, accounts, Owner} ->
                Pid ! s_accounts(Bank, Owner),
                BM(BM, Bank);
            {Pid, balance, Owner, Type} ->
                Pid ! s_balance(Bank),
                BM(BM, Bank);
            {Pid, open, Owner, Type} ->
                case s_open(Bank, Owner, Type) of
                    {error, Reason} ->
                        Pid ! {error, Reason},
                        BM(BM, Bank);
                    {ok, NewBank} ->
                        Pid ! {ok},
                        BM(BM, NewBank)
                end;
            {Pid, close, Owner, Type} ->
                case s_close(Bank, Owner, Type) of
                    {error, Reason} ->
                        Pid ! {error, Reason},
                        BM(BM, Bank);
                    {ok, {NewBank, ClosingBalance}} ->
                        Pid ! {ok, ClosingBalance},
                        BM(BM, NewBank)
                end;
            {Pid, deposit, Owner, Type, Amount) ->
                case s_deposit(Bank, Owner, Type, Amount) of
                    {error, Reason} ->
                        Pid ! {error, Reason},
                        BM(BM, Bank);
                    {ok, NewBank} ->
                        Pid ! {ok},
                        BM(BM, NewBank)
                end;
            {Pid, withdraw, Owner, Type, Amount) ->
                case s_withdraw(Bank, Owner, Type, Amount) of
                    {error, Reason} ->
                        Pid ! {error, Reason},
                        BM(BM, Bank);
                    {ok, NewBank} ->
                        Pid ! {ok},
                        BM(BM, NewBank)
                end;
            Any -> io:format("Bank got some message ~p~n", [Any]), BM(BM)
        end.
    Bank = spawn(fun() -> BankMain(BankMain, [])),
    {ok, Bank}.

%% size/1
%% Return the number of accounts in the Bank: {ok, Size}
size(Bank) ->
    Bank ! {self(), size},
    receive
        Any -> Any
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Server-side functions
%%
%%  In these functions, Bank is the #account list.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% s_size/1
%% Return the number of accounts in the Bank: {ok, Size}
s_size(Bank) ->
    {ok, erlang:length(Bank)}.

%% s_accounts/2
%% Return the account types associated with a particular owner: {ok, TypeList}
s_accounts(Bank, Owner) ->
    TypeList = [#account.type || #account{} <- Bank, #account.owner == Owner],
    {ok, TypeList}.

%% s_balance/3
%% Return the balance in a particular owner's account of a particular type:
%% {ok, Balance}
s_balance(Bank, Owner, Type) ->
    case select_account(Bank, Owner, Type) of
        [] ->
            {error, "No such account"};
        [Account] ->
            {ok, Account#account.balance}
    end.

%% s_open/3
%% Create a new account of a specified type for a specified owner.
%% If the owner does not have an account of this type: {ok, NewBank}
%% If the owner already has an account of this type:
%% {error, "Duplicate account"}
s_open(Bank, Owner, Type) ->
    case select_account(Bank, Owner, Type) of
        [] ->
            {ok, [#account{owner = Owner, type = Type} | Bank]};
        [_Account] ->
            {error, "Duplicate account"}
    end.

%% s_close/3
%% Close a specified owner's account of a specified type.
%% If the owner had an account of this type: {ok, {NewBank, ClosingBalance}}
%% If the owner did not have an account of this type: {error, "No such account"}
s_close(Bank, Owner, Type) ->
    case select_account(Bank, Owner, Type) of
        [] ->
            {error, "No such account"};
        [Account] ->
            {ok, {lists:delete(Account, Bank), Account#account.balance}}
    end.

%% s_deposit/4
%% Deposit a specified amount of funds in a specified owner's account of a
%% specified type.
%% If the owner has an account of this type: {ok, NewBank}
%% If the owner does not have an account of this type:
%% {error, "No such account"}
%% If the specified amount is not positive: {error, "Negative amount"}
s_deposit(_Bank, _Owner, _Type, Amount) when Amount < 0 ->
	{error, "Negative amount"};
s_deposit(Bank, Owner, Type, Amount) ->
    case select_account(Bank, Owner, Type) of
        [] ->
            {error, "No such account"};
        [Account] ->
            DepositFun = fun
                (A) when A == Account ->
                    A#account{balance = A#account.balance + Amount};
                (A) -> A end,
            {ok, lists:map(DepositFun, Bank)}
    end.

%% s_withdraw/4
%% Withdraw a specified amount of funds from a specified owner's account of a
%% specified type.
%% If the owner has an account of this type and there are sufficient funds to
%% withdraw: {ok, NewBank}
%% If the owner has an account of this type but there are insufficient funds to
%% withdraw: {error, "Overdrawn"}
%% If the owner does not have an account of this type:
%% {error, "No such account"}
%% If the specified amount is not positive: {error, "Negative amount"}
s_withdraw(_Bank, _Owner, _Type, Amount) when Amount < 0 ->
	{error, "Negative amount"};
s_withdraw(Bank, Owner, Type, Amount) ->
    case select_account(Bank, Owner, Type) of
        [] ->
            {error, "No such account"};
        [Account] ->
            if  Account#account.balance - Amount < 0 ->
                    {error, "Overdrawn"};
                Account#account.balance - Amount >= 0 ->
                    WithdrawFun = fun
                        (A) when A == Account ->
                            A#account{balance = A#account.balance - Amount};
                        (A) -> A end,
                    {ok, lists:map(WithdrawFun, Bank)}
            end
    end.

%% select_account/3
%% Select the account of specified Owner and Type.
%% Uses lists:filter but because {Owner,Type} is the primary key, only one
%% account should ever be returned: the exception would be if you were to use an
%% anonymous variable. Result returned in a tuple to help pattern matching.
select_account(Bank, Owner, Type) ->
    Find =  fun(A) ->
                (A#account.owner == Owner) andalso (A#account.type == Type) end,
    lists:filter(Find, Bank).