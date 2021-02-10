# 诗悦选做作业
## Linux指令相关作业
**要求**：已学会Linux指令，并说明作用以及使用样例。

**提交**：根目录下`INIT_NODE.sh`文件，这是我毕业设计的一部分，作为初始化CentOS计算节点的环境配置自动化脚本，
主要功能为构建Docker及KVM虚拟化环境并配置计算节点的Agent，涉及到Linux较为常用的一些指令，故以此作为作业提交。

## Erlang相关题目
**要求**：在游戏中，最常见的就是排行榜，现在我们有一个等级排行榜，我们需要根据等级排序，现用列表来存储`List = [{"apple", 75}, {"orange", 80}|..]`，根据等级排序排行榜，并截取前100名。

**提交**：`top3`文件夹，为方便测试，修改了题目要求为截取前3名。

**要求**：在游戏中，AOE技能经常使用到。假设技能A是一个矩形AOE技能，四个顶点分布为`(X1,Y1), (X1, Y2), (X2, Y1), (X2, Y2)`，
BOSS的占地也是一个矩形，四个顶点为`(X3,Y3), (X3, Y4), (X4, Y3), (X4, Y4)`，请写出函数判断2个矩形是否相交。

**提交**：`hit_judgment`文件夹，根据题目理解，矩形相交的判断包含全包含的情况，并不局限于边相交的情况。

**要求**：在游戏中，有很多需要使用共享资源来处理的功能，这时候就需要单独管理进程来分配，并且按照请求有序执行资源分配，假设当前系统有X个资源，每个请求携带需要占用Y个资源，占用耗时Z秒。
使用`gen_server`实现该管理进程；
请求资源消息，分配资源，资源不足时，返回失败，耗时结束时释放资源；
取消资源消息，立马释放资源。

**提交**：`base_resource_serve`文件夹，直接执行`base_resource_server/src/main.erl`的`start/0`方法测试。

**要求**：为了更方便的查看资源使用情况，游戏中往往通过`ets`进行数据查询，并且将数据存储于`dets`中，方便重启服务器后能载入之前资源情况。
将当前使用资源情况存在`ets`和`dets`中，能实时查询每个请求的处理情况(开启时间，耗时，占用资源)；
管理进程从`dets`加载上次关闭时资源使用情况。

**提交**：`ets_resource_serve`文件夹，直接执行`ets_resource_server/src/main.erl`的`start/0`方法测试，`dets`储存于`ets_resource_server/dets/dets`文件。

**要求**：游戏中每一件事都要进行记录，方便技术和运营查看并分析数据，那么我们需要对该资源管理中心增加日志记录。
每个任务开启/完成/取消，需要写入文件`log.txt`中；
每条日志需要记录发生的时间戳。

**提交**：`log_resource_serve`文件夹，直接执行`log_resource_server/src/main.erl`的`start/0`方法测试，`dets`储存于`log_resource_server/dets/dets`文件,日志文件`log.txt`储存于`log_resource_server/logs/log.txt`文件。