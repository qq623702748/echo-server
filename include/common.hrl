-define(PORT, 9527).	%服务器指定端口值
-define(ACCEPTPOOL, 2).%socket连接池大小
-define(SERVERUSERLISTPOOL, 2). %定义serverUserList连接池大小
-define(SERVERLOGINPOOL, 2). %定义serverlogin连接池大小
-define(SERVERMAPPER, server_mapper).
-define(SERVERLOGINMAPPER, server_login_mapper).
-define(HASHFACTOR,31).

-define(SERVER_BLACKBOARD, server_blackboard).

-define(WORLD_CHAT_BLACKBOARD, server_world_chat_blackboard).
-define(PRIVATE_CHAT_BLACKBOARD, server_private_chat_blackboard).
-define(GROUP_CHAT_BLACKBOARD, server_group_chat_blackboard).

-define(MSG_QUEUE_BLACKBOARD, server_msg_queue_blackboard).
-define(MSG_QUEUE_TIMEOUT, 5000). %消息队列查询时间间隔




-define(WORLD_CHAT_HANDLER, world_chat_handler).
-define(GROUP_CHAT_HANDLER, group_chat_handler).
-define(PRIVATE_CHAT_HANDLER, private_chat_handler).