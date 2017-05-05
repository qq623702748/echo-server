# echo-server
===============

## 此版本用于建立echo-server的架构

#### 架构描述

```erlang

                                server_app                            												=>      echo-server启动入口
                                    |
                                    V
        ===============================================server_super_sup======================================= 		=>      echo-server顶部监控树
        |                       |                           |						|					|
        V                       V                           V					    V					V
server_listener     server_accept_pool_sup      server_socket_sup		server_userlist		server_control	
                                |                       |							
                                V                       V							
				    server_accept_pool ...(n)       server_socket ...(n)

具体描述：
1、server_super_sup 采用one_for_one策略， 定义监听端口以及连接池大小
2、server_listener 监听指定的端口
3、server_accept_pool_sup 连接池的监控树，采用simple_one_for_one策略，每一个server_accept_pool均监听同一个listensocket
4、server_socket_sup 用户连接后socket监控树，采用simple_one_for_one策略，用于读取每个用户发送过来的tcp数据
```

#### 使用方式

- 加载所有模块之后，server_app:start(1,1).  %参数传任何值均可以。
- server_control 提供两个功能 
	1 查询在线用户，server_control:online_check() 
	2 踢掉指定的用户，如果用户不存在，则返回踢失败消息
	
#### echo-server 逻辑业务描述
- server_userlist : 用于存在在线用户的所有数据，以及ETS表和讨论组的数据
- server_control: 用于处理服务器在执行时期所运行的指令
- server_socket: 
        注意：socket中心跳包和登录包是不做拦截处理的，其他包均会拦截判断是否超出一分钟发送频率
        每个用户的tcp数据均在此文件读取，然后通过gen_server异步发送给server_userlist处理
- server_accept_pool: tcp连接池，连接池大小由server_app中定义（默认为50）
- server_app: 定义系统的监听端口以及连接池大小

#### echo-server 支持功能
- 用户上线提示，用户下线提示
- 用户密码登录，用户群发聊天
- 用户限制发送速率，用户心跳检测
- 用户私聊，
- 加入讨论组，离开讨论组，讨论组聊天（讨论组id:001/002)
- 服务器查询在线人数，服务器主动踢人