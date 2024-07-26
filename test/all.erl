%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Created :
%%% Node end point  
%%% Creates and deletes Pods
%%% 
%%% API-kube: Interface 
%%% Pod consits beams from all services, app and app and sup erl.
%%% The setup of envs is
%%% -------------------------------------------------------------------
-module(all).      
 
-export([start/0]).

-define(TargetDir,"ctrl_dir").
-define(Vm,ctrl@c50).
-define(TarFile,"ctrl.tar.gz").
-define(App,"ctrl").
-define(TarSrc,"release"++"/"++?TarFile).
-define(StartCmd,"./"++?TargetDir++"/"++"bin"++"/"++?App).

-define(LogFileToRead,"./logs/test_git_handler_a/log.logs/test_logfile.1").

-define(AppVm,adder3@c50).
-define(AdderApp,adder3).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("log.api").
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    
    ok=setup(),
 %   ok=load_start_release(),
  %  ok=host_server_test(),
   % ok=deployment_server_test(),
  %  ok=application_server_test(),    
   % ok=controller_test(),

  %  ok=reconciliation_test(),

    timer:sleep(2000),
    io:format("Test OK !!! ~p~n",[?MODULE]),
    LogStr=os:cmd("cat "++?LogFileToRead),
    L1=string:lexemes(LogStr,"\n"),
    [io:format("~p~n",[Str])||Str<-L1],

    rpc:call(?Vm,init,stop,[],5000),
    timer:sleep(4000),
    init:stop(),
    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
reconciliation_test()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    %% Clean up before test 
    
    rpc:call(?Vm,application_server,stop_app,["adder3.application"],5000),
    rpc:call(?Vm,application_server,unload_app,["adder3.application"],5000),

    %%

    %% All possible application used to check if an applications is started/loaded that is not supposed to be started/loaded -> to stoped and unloaded
    %Check wanted applications
    {ok,AllApplicationFiles}=rpc:call(?Vm,application_server,all_filenames,[],5000),
    io:format("AllApplicationFiles ~p~n",[{AllApplicationFiles,?MODULE,?LINE}]),
    AllDeploymentFiles=[Filename||{Filename,_}<-rpc:call(?Vm,deployment_server,get_applications_to_deploy,[],5000)],
    io:format("AllDeploymentFiles ~p~n",[{AllDeploymentFiles,?MODULE,?LINE}]),
    
    ApplicationsToDeploy=[Filename||Filename<-AllDeploymentFiles,
				    false=:=rpc:call(?Vm,application_server,is_app_started,[Filename],5000)],
    io:format("ApplicationsToDeploy ~p~n",[{ApplicationsToDeploy,?MODULE,?LINE}]),
    ApplicationsToStop=[Filename||Filename<-AllApplicationFiles,
				  false=:=lists:member(Filename,AllDeploymentFiles),
				  true=:=rpc:call(?Vm,application_server,is_app_started,[Filename],5000)],
    io:format("ApplicationsToStop ~p~n",[{ApplicationsToStop,?MODULE,?LINE}]),
    
    Load_Start_Result=deploy(ApplicationsToDeploy,[]),
    io:format("Load_Start_Result ~p~n",[{Load_Start_Result,?MODULE,?LINE}]),
    %ok=rpc:call(?Vm,controller,load_start,["adder3.application"],3*5000),
    %% Needed to ensure that current application is connected to the new node 
    %% and rd is traded 
  %  pong=net_adm:ping(?Vm),
  %  ok=initial_trade_resources(),
  %  42=rd:call(adder3,add,[20,22],5000),

    ok.

deploy([],Acc)->
    Acc;
deploy([Filename|T],Acc)->
    Result=rpc:call(?Vm,controller,load_start,[Filename],3*5000),
    deploy(T,[{Result,Filename}|Acc]).
    
 
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
controller_test()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),



    %% Clean up before test 
    
    rpc:call(?Vm,application_server,stop_app,["adder3.application"],5000),
    rpc:call(?Vm,application_server,unload_app,["adder3.application"],5000),

    %% check read application configs

    {ok,[{value1,v11},{value2,12}]}=rpc:call(?Vm,controller,get_application_config,[app1],5000),
    {ok,[{value1,v21},{value2,22}]}=rpc:call(?Vm,controller,get_application_config,[app2],5000),

   {ok,[]}=rpc:call(?Vm,controller,get_application_config,[glurk],5000),



    %Load and start adder3
    {error,["Not started ","adder3.application"]}=rpc:call(?Vm,controller,stop_unload,["adder3.application"],5*5000),
    ok=rpc:call(?Vm,controller,load_start,["adder3.application"],5*5000),
    AppVm=adder3@c50,
    42=rpc:call(AppVm,adder3,add,[20,22],5000),
    
    {error,["Already loaded ","adder3.application"]}=rpc:call(?Vm,controller,load_start,["adder3.application"],5*5000),
    ok=rpc:call(?Vm,controller,stop_unload,["adder3.application"],3*5000),
    pang=net_adm:ping(AppVm),
    
    ok.
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
application_server_test()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    %% Clean up before test 
   rpc:call(?Vm,application_server,stop_app,["adder3.application"],5000),
   rpc:call(?Vm,application_server,unload_app,["adder3.application"],5000),

    pong=rpc:call(?Vm,application_server,ping,[],5000),
    {ok,AllFilenames}=rpc:call(?Vm,application_server,all_filenames,[],5000),
    [
     "adder3.application",
     "kvs.application",
     "phoscon.application"
    ]=lists:sort(AllFilenames),
    {ok,"Repo is up to date"}=rpc:call(?Vm,application_server, update,[],5000),

    %Load and start adder3

    {error,["Not loaded ","adder3.application"]}=rpc:call(?Vm,application_server,start_app,["adder3.application"],5000),
    {error,["Not started ","adder3.application"]}=rpc:call(?Vm,application_server,stop_app,["adder3.application"],5000),
    {error,["Not loaded ","adder3.application"]}=rpc:call(?Vm,application_server,unload_app,["adder3.application"],5000),
    
    pong=rpc:call(?Vm,application_server,ping,[],5000),

    ok=rpc:call(?Vm,application_server,load_app,["adder3.application"],5*5000),
    {error,["Not started ","adder3.application"]}=rpc:call(?Vm,application_server,stop_app,["adder3.application"],5000),

    ok=rpc:call(?Vm,application_server,start_app,["adder3.application"],5*5000),
    AppVm=adder3@c50,
    42=rpc:call(AppVm,adder3,add,[20,22],5000),
    
    {error,["Already loaded ","adder3.application"]}=rpc:call(?Vm,application_server,load_app,["adder3.application"],5000),
    {error,[" Application started , needs to be stopped ","adder3.application"]}=rpc:call(?Vm,application_server,unload_app,["adder3.application"],5000),

    ok=rpc:call(?Vm,application_server,stop_app,["adder3.application"],5000),
    pang=net_adm:ping(AppVm),
    {error,["Not started ","adder3.application"]}=rpc:call(?Vm,application_server,stop_app,["adder3.application"],5000),
    {error,["Already loaded ","adder3.application"]}=rpc:call(?Vm,application_server,load_app,["adder3.application"],5000),
    ok=rpc:call(?Vm,application_server,unload_app,["adder3.application"],5000),
    
    ok.
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
deployment_server_test()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    pong=rpc:call(?Vm,deployment_server,ping,[],5000),
    {ok,AllFilenames}=rpc:call(?Vm,deployment_server,all_filenames,[],5000),
    [
     "adder3.deployment",
      "kvs.deployment",
      "log2.deployment",
      "log2.deployment~",
      "phoscon_zigbee.deployment"
    ]=lists:sort(AllFilenames),
   
    [
     {"adder3.application","c50"},
     {"kvs.application","c50"},
     {"phoscon.application","c50"}
    ]=lists:sort(rpc:call(?Vm,deployment_server, get_applications_to_deploy,[],5000)),
   
    {ok,"Repo is up to date"}=rpc:call(?Vm,deployment_server, update,[],5000),
  
    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
host_server_test()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    pong=rpc:call(?Vm,host_server,ping,[],5000),
   {ok,AllFilenames}=rpc:call(?Vm,host_server,all_filenames,[],5000),
    ["c200.host","c201.host","c202.host","c230.host","c50.host"]=lists:sort(AllFilenames),
    ['ctrl@c200','ctrl@c201','ctrl@c202','ctrl@c230','ctrl@c50']=lists:sort(rpc:call(?Vm,host_server, get_host_nodes,[],5000)),
    
    [
     {app1,[{value1,v11},{value2,12}]},
     {app2,[{value1,v21},{value2,22}]},
     {conbee,[{conbee_addr,"172.17.0.2"},
	      {conbee_port,80},
	      {conbee_key,"Glurk"}]}
    ]=rpc:call(?Vm,host_server,get_application_config,[],5000),

   
    {ok,"Repo is up to date"}=rpc:call(?Vm,host_server, update,[],5000),
  
    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------


load_start_release()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    %% Delete ad_rel dir used for tar, stop Vm
    file:del_dir_r(?TargetDir),
    rpc:call(?Vm,init,stop,[],3000),
    timer:sleep(2000),
    
    %%
    ok=file:make_dir(?TargetDir),
    []=os:cmd("tar -zxf "++?TarSrc++" -C "++?TargetDir),
    
    %%
    []=os:cmd(?StartCmd++" "++"daemon"),
    timer:sleep(1000),
    pong=net_adm:ping(?Vm),
    pong=rpc:call(?Vm,rd,ping,[],5000),
    pong=rpc:call(?Vm,log,ping,[],5000),
    pong=rpc:call(?Vm,deployment_server,ping,[],2*5000),
    pong=rpc:call(?Vm,host_server,ping,[],5000),
    pong=rpc:call(?Vm,application_server,ping,[],3*5000),  
    pong=rpc:call(?Vm,git_handler,ping,[],5000),  
    pong=rpc:call(?Vm,controller,ping,[],10*5000),


    AllApps=rpc:call(?Vm,application,which_applications,[],6000),
    io:format("AllApps ~p~n",[{AllApps,?MODULE,?LINE,?FUNCTION_NAME}]),
    {ok,Cwd}=rpc:call(?Vm,file,get_cwd,[],6000),
    io:format("Cwd ~p~n",[{Cwd,?MODULE,?LINE,?FUNCTION_NAME}]),
    {ok,Filenames}=rpc:call(?Vm,file,list_dir,[Cwd],6000),
    io:format("Filenames ~p~n",[{Filenames,?MODULE,?LINE,?FUNCTION_NAME}]),
    AbsName=rpc:call(?Vm,code,where_is_file,["python.beam"],6000),
    io:format("AbsName ~p~n",[{AbsName,?MODULE,?LINE,?FUNCTION_NAME}]),
    
%    io:format("~p~n",[os:cmd("cat "++?LogFile)]),
    ok.
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------

setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),


    ok=application:start(log),
    file:make_dir(?MainLogDir),
    [NodeName,_HostName]=string:tokens(atom_to_list(node()),"@"),
    NodeNodeLogDir=filename:join(?MainLogDir,NodeName),
    ok=log:create_logger(NodeNodeLogDir,?LocalLogDir,?LogFile,?MaxNumFiles,?MaxNumBytes),

    ok=application:start(rd),

    ok=application:start(git_handler),
    pong=rpc:call(node(),git_handler,ping,[],3*5000),  
 %   ok=initial_trade_resources(),
    
    ok.


initial_trade_resources()->
    [rd:add_local_resource(ResourceType,Resource)||{ResourceType,Resource}<-[]],
    [rd:add_target_resource_type(TargetType)||TargetType<-[controller,adder3]],
    rd:trade_resources(),
    timer:sleep(3000),
    ok.
