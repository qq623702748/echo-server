# echo-server
===============

## �˰汾���ڽ���echo-server�ļܹ�

#### �ܹ�����

```erlang

                                server_app                            												=>      echo-server�������
                                    |
                                    V
        ===============================================server_super_sup======================================= 		=>      echo-server���������
        |                       |                           |						|					|
        V                       V                           V					    V					V
server_listener     server_accept_pool_sup      server_socket_sup		server_userlist		server_control	
                                |                       |							
                                V                       V							
				    server_accept_pool ...(n)       server_socket ...(n)

����������
1��server_super_sup ����one_for_one���ԣ� ��������˿��Լ����ӳش�С
2��server_listener ����ָ���Ķ˿�
3��server_accept_pool_sup ���ӳصļ����������simple_one_for_one���ԣ�ÿһ��server_accept_pool������ͬһ��listensocket
4��server_socket_sup �û����Ӻ�socket�����������simple_one_for_one���ԣ����ڶ�ȡÿ���û����͹�����tcp����
```

#### ʹ�÷�ʽ

- ��������ģ��֮��server_app:start(1,1).  %�������κ�ֵ�����ԡ�
- server_control �ṩ�������� 
	1 ��ѯ�����û���server_control:online_check() 
	2 �ߵ�ָ�����û�������û������ڣ��򷵻���ʧ����Ϣ
	
#### echo-server �߼�ҵ������
- server_userlist : ���ڴ��������û����������ݣ��Լ�ETS��������������
- server_control: ���ڴ����������ִ��ʱ�������е�ָ��
- server_socket: 
        ע�⣺socket���������͵�¼���ǲ������ش���ģ����������������ж��Ƿ񳬳�һ���ӷ���Ƶ��
        ÿ���û���tcp���ݾ��ڴ��ļ���ȡ��Ȼ��ͨ��gen_server�첽���͸�server_userlist����
- server_accept_pool: tcp���ӳأ����ӳش�С��server_app�ж��壨Ĭ��Ϊ50��
- server_app: ����ϵͳ�ļ����˿��Լ����ӳش�С

#### echo-server ֧�ֹ���
- �û�������ʾ���û�������ʾ
- �û������¼���û�Ⱥ������
- �û����Ʒ������ʣ��û��������
- �û�˽�ģ�
- ���������飬�뿪�����飬���������죨������id:001/002)
- ��������ѯ������������������������