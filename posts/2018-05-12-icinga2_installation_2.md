---
layout: article
categories: Technology
tags: icinga2 technology
title: Icinga2 安装手记 2 - 插件
---

## 准备工作

在安装插件之前，先用`icinga2 node wizard`命令配置master节点的证书CN。（配置内容保存在/etc/icinga2/constants.conf）

```
[root@icinga2-master1.localdomain /]# icinga2 node wizard

Welcome to the Icinga 2 Setup Wizard!

We will guide you through all required configuration details.

Please specify if this is a satellite/client setup ('n' installs a master setup) [Y/n]: n

Starting the Master setup routine...

Please specify the common name (CN) [icinga2-master1.localdomain]: icinga2-master1.localdomain
Reconfiguring Icinga...
Checking for existing certificates for common name 'master1'...
Generating master configuration for Icinga 2.

Please specify the API bind host/port (optional):
Bind Host []:
Bind Port []:

Done.

Now restart your Icinga 2 daemon to finish the installation!
```

## 安装Icinga 插件

### 1. Icinga Director
安装过程参考[安装文档](https://www.icinga.com/docs/director/latest/doc/02-Installation/)，共需两步

1.1 [下载地址](https://exchange.icinga.com/icinga/Director/releases)

1.2 将tar解压到/usr/share/icingaweb2/modules/director目录，并用Icinga web界面创建一个db resource


## 安装 Icinga2 client

首先从master机器用sftp拉取整个repo，创建对应的yum repo配置文件。

依次运行
```
yum install icinga2
yum install nagios-plugins-all
yum install vim-icinga2
icinga2 api setup
service icinga2 start
```

使用`icinga2 node wizard`配置client节点


![](/images/icinga2-client-node-wizard.png)

配置client节点上的zones.conf文件


在master节点上运行`icinga2 ca list`，查看client节点发送过来的证书请求，并使用`icinga2 ca sign XXX`对请求进行签名

使用`icinga2 deamon -C`验证配置文件的正确性

### check_memory.pl
requires Nagios::Plugin

## 配置icinga2 master到icinga2 client的ssh证书访问方式
```
ssh-keygen
ssh-copy-id -i ~/.ssh/id_rsa.pub root@icinga2-client1.localdomain
```

## influxdb + graphana
```
yum localinstall graphanaXXX.rpm
```


## 日志

主要存放在 /var/log/icinga2/icinga2.log
