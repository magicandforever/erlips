==========
Erlang实战
==========
----------------
- IP在线查询服务
----------------

:Author: litaocheng
:Mail: litaocheng@gmail.com
:Date: 2009.9.11

.. contents:: 目录
.. sectnum::

-----------------------------
Second-Level Title (optional)
-----------------------------
 
IP在线查询服务
 
 
GET geoip?ip=63.8.36.135
 
200 HTTP/1.1
 
{
 
  "countroy" : "china", 
 
           "city" : "beijing",  
 
          "long" : 43.22,
 
             "lat" : 120.22,
 
          "post" : "100000"
 
}
 
   
 
指标
 
支持峰值 100k/s 的查询
 
99%的请求在100ms内返回
 
99%高可用
 
支持不停机系统更新
 
系统易于扩展
 
开发迅速（1个星期?)
 
做好准备了么？
 
一点WEB开发经验（HTTP，JS 总得听过吧)
 
一点基本网络知识（IP不是"挨批")
 
一点Erlang知识（至少要在google上搜索过Erlang）
 
“三点”都准备奇了！
 
 
 
 
最重要的一点：
 
你对Erlang充满了好奇，看看它是如何漂亮的完成某些工作！
 
 
 
 
沿途的风景
 
Erlang简介
 
项目代码目录结构
 
OTP本面目
 
IP查询服务分析设计
 
码农上场，开始编码    
 
测试上场，码农喘口气
 
性能测试
 
部署信需求，新功能
 
 
 
 
 
 
 
 
 
 
 
 
 
 
开始之前的个人“苦恼”：
 
 
 
 
 
 
 
不想最后一页发现离题万里
 
不想让有经验的“熟”牛，感觉索然无趣
 
不想让对Erlang翘首企盼的新人，望而生畏
 
不想让slide寥寥几页
 
不想让slide页数多如牛毛...
 
 
 
我尽力以最简单的方式向您描述Erlang！
 
每篇Slide里面或有一些Note，可以进行扩展的阅读
 
 
 
 
Erlang简介
 
 
 
代码组织结构
 
 
 
关于OTP
 
 
 
IP查询服务分析设计
 
采用开源的方案,缩短开发工期，提高代码质量.
 
web server采用mochiweb
 
IP数据库采用 maxmind binary format data
 
egeoip， maxmind binary db的erlang client
 
采用Erlang的内建的通信机制，建立分布式架构
 
 
 
 
 
 
 
 
 
源代码(.erl .hrl)
 
%%%----------------------------------------------------------------------
%%%
%%% company @copyright 2009
%%%
%%% @author litao cheng <litaocheng@gmail.com>
%%% @version 0.1
%%% @doc handle the configure info(use ets)
%%%
%%%----------------------------------------------------------------------
 
 
Third-Level Title
=================

Fourth-Level Title
------------------

Fifth-Level Title
'''''''''''''''''

This is a typical paragraph.  A literal block follows.


::

    for a in [5,4,3,2,1]:   # this is program code, shown as-is
        print a
    print "it's..."
    # a literal block continues until the indentation ends

Bullet list items begin with one of "-", "*", or "+" (hyphen, asterisk, or plus sign), followed by whitespace and the list item body

Enumerated (numbered) list items are similar, but use an enumerator instead of a bullet. Enumerators are numbers (1, 2, 3, ...), letters (A, B, C, ...; uppercase or lowercase), or Roman numerals (i, ii, iii, iv, ...; uppercase or lowercase), formatted with a period suffix ("1.", "2."), parentheses ("(1)", "(2)"), or a right-parenthesis suffix ("1)", "2)"). For example:

Simple tables are easy and compact:

=====  =====  =======
  A      B    A and B
=====  =====  =======
False  False  False
True   False  False
False  True   False
True   True   True
=====  =====  =======

=====  =====  ======
   Inputs     Output
------------  ------
  A      B    A or B
=====  =====  ======
False  False  False
True   False  True
False  True   True
True   True   True
=====  =====  ======

Python_ is `my favourite
programming language`__.

.. _Python: http://www.python.org/

__ Python_ 

引用：
[CIT2002]_.
.. [CIT2002] A citation
   (as often used in journals). 

.. image:: images/ball1.gif

.. This text will not be shown
   (but, for instance, in HTML might be
   rendered as an HTML comment)

The |biohazard| symbol must be used on containers used to dispose of medical waste.

.. |biohazard| image:: biohazard.png 


*emphasized*
**strongly emphasized**
``Inline literals``
