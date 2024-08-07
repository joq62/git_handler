%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%% 
%%% @end
%%% Created : 18 Apr 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(git_handler). 
  
-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include 
%%
%%--------------------------------------------------------------------

-include("log.api").

-include("git_handler.hrl").
-include("git_handler.rd").


%% API

-export([
	 % pull,clone,delete,is_updated
	
	 all_filenames/1,
	 read_file/2,	
	 update_repo/1,
	 clone/2, 
	 delete/1,
	 is_repo_updated/1
       
	]).

-export([
	

	 


	]).

%% admin




-export([
	 start/0,
	 ping/0,
	 stop/0
	]).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).
		     
-record(state, {
		
	       }).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Reads the filenames in the RepoDir   
%% 
%% @end
%%--------------------------------------------------------------------
-spec all_filenames(RepoDir :: string()) -> 
	  {ok,FileNames::term()} | {error,Reason :: term()}.

all_filenames(RepoDir) ->
    gen_server:call(?SERVER,{all_filenames,RepoDir},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Reads the filenames in the RepoDir   
%% 
%% @end
%%--------------------------------------------------------------------
-spec read_file(RepoDir :: string(),FileName ::string()) -> 
	  {ok,Info::term()} | {error,Reason :: term()}.

read_file(RepoDir,FileName) ->
    gen_server:call(?SERVER,{read_file,RepoDir,FileName},infinity).

%%--------------------------------------------------------------------
%% @doc
%%    
%% 
%% @end
%%--------------------------------------------------------------------
-spec update_repo(RepoDir::string()) -> 
	  ok | {error, Reason :: term()}.
update_repo(RepoDir) ->
    gen_server:call(?SERVER,{update_repo,RepoDir},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Reads the filenames in the RepoDir   
%% 
%% @end
%%--------------------------------------------------------------------
-spec delete(RepoDir :: string()) -> 
	  ok | {error,Reason :: term()}.

delete(RepoDir) ->
    gen_server:call(?SERVER,{delete,RepoDir},infinity).

%%--------------------------------------------------------------------
%% @doc
%%    
%% 
%% @end
%%--------------------------------------------------------------------
-spec clone(RepoDir::string(),RepoGit::string()) -> 
	  ok | {error, Reason :: term()}.
clone(RepoDir,RepoGit) ->
    gen_server:call(?SERVER,{clone,RepoDir,RepoGit},infinity).

%%--------------------------------------------------------------------
%% @doc
%%    
%% 
%% @end
%%--------------------------------------------------------------------
-spec is_repo_updated(RepoDir :: string()) -> 
	  true | false | {error,Reason :: term()}.

% {error,["Inventory doesnt exists, need to clone"]} .
is_repo_updated(RepoDir) ->
    gen_server:call(?SERVER,{is_repo_updated,RepoDir},infinity).

%%--------------------------------------------------------------------
%% @doc
%%  
%% 
%% @end
%%--------------------------------------------------------------------
start()->
    application:start(?MODULE).

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec ping() -> pong | Error::term().
ping()-> 
    gen_server:call(?SERVER, {ping},infinity).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
	  {error, Error :: {already_started, pid()}} |
	  {error, Error :: term()} |
	  ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%stop()-> gen_server:cast(?SERVER, {stop}).
stop()-> gen_server:stop(?SERVER).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
	  {ok, State :: term(), Timeout :: timeout()} |
	  {ok, State :: term(), hibernate} |
	  {stop, Reason :: term()} |
	  ignore.

init([]) ->
    
    {ok, #state{ 
	    
	   },0}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
	  {reply, Reply :: term(), NewState :: term()} |
	  {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
	  {reply, Reply :: term(), NewState :: term(), hibernate} |
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
	  {stop, Reason :: term(), NewState :: term()}.




%%********************* Repo ************************************

handle_call({all_filenames,RepoDir}, _From, State) ->
    Result=try lib_git_handler:all_filenames(RepoDir) of
	       {ok,R}->
		    {ok,R};
	       {error,Reason}->
		   {error,Reason}
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      {ok,AllFileNames}->
		  {ok,AllFileNames};
	      ErrorEvent->
		% io:format("ErrorEvent ~p~n",[{ErrorEvent,?MODULE,?LINE}]),
		  ErrorEvent
	  end,
    {reply, Reply,State};

handle_call({read_file,RepoDir,FileName}, _From, State) ->
    Result=try lib_git_handler:read_file(RepoDir,FileName) of
	       {ok,R}->
		    {ok,R};
	       {error,Reason}->
		   {error,Reason}
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      {ok,Info}->
		  {ok,Info};
	      ErrorEvent->
		 ErrorEvent
	  end,
    {reply, Reply,State};

handle_call({update_repo,RepoDir}, _From, State) ->
    Result=try lib_git_handler:update_repo(RepoDir) of
	       ok->
		   ok;
	       {error,Reason}->
		   {error,Reason}
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      ok->
		  ok;
	      ErrorEvent->
		  ErrorEvent
	  end,
    {reply, Reply,State};

handle_call({clone,RepoDir,Git}, _From, State) ->
    Result=try lib_git_handler:clone(RepoDir,Git) of
	       ok->
		   ok;
	       {error,Reason}->
		   {error,Reason}
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      ok->
		  ok;
	      ErrorEvent->
		  ErrorEvent
	  end,
    {reply, Reply,State};


    
handle_call({is_repo_updated,RepoDir}, _From, State) ->
    Result=try lib_git_handler:is_repo_updated(RepoDir) of
	       {ok,R}->
		   {ok,R};
	       {error,Reason}->
		   {error,Reason}
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      {ok,IsUpdated}->
		  %io:format("IsUpdated ~p~n",[{IsUpdated,?MODULE,?LINE}]),
		   IsUpdated;
	      ErrorEvent->
		  ErrorEvent
	  end,
    {reply, Reply, State};

%%--------------------------------------------------------------------



handle_call({ping}, _From, State) ->
    Reply=pong,
    {reply, Reply, State};

handle_call(UnMatchedSignal, From, State) ->
    io:format("unmatched_signal ~p~n",[{UnMatchedSignal, From,?MODULE,?LINE}]),
    Reply = {error,[unmatched_signal,UnMatchedSignal, From]},
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast({stop}, State) ->
    
    {stop,normal,ok,State};

handle_cast(UnMatchedSignal, State) ->
    ?LOG_WARNING("Unmatched signal",[UnMatchedSignal]),
    io:format("unmatched_signal ~p~n",[{UnMatchedSignal,?MODULE,?LINE}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: normal | term(), NewState :: term()}.

handle_info(timeout, State) ->
    ?LOG_NOTICE("Server started ",[?MODULE]),
    {noreply, State};


handle_info(Info, State) ->
    ?LOG_WARNING("Unmatched signal",[Info]),
    io:format("unmatched_signal ~p~n",[{Info,?MODULE,?LINE}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
		  State :: term(),
		  Extra :: term()) -> {ok, NewState :: term()} |
	  {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
		    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
