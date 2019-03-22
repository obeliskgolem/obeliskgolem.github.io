---
layout: article
categories: Technology
tags: icinga2
title: Icinga2 安装手记
---

Icinga 2 是一个基于Nagios插件之上的一个监控框架。

<!--more-->

## 安装指令

For CentOS 7

查看操作系统版本：
`lsb_release -a`

安装Icinga仓库
`yum install epel-release`

安装Icinga2:
```
# yum install icinga2
# systemctl enable icinga2
# systemctl start icinga2
```

安装插件
`# yum install nagios-plugins-all`


## 运行Icinga

首先需要启动，centos使用systemd管理icinga2的服务
`# systemctl status icinga2`

通过systemd设置icinga2的自启动
```
#/etc/systemd/system/icinga2.service.d/override.conf

[Service]
Restart=always
RestartSec=1
StartLimitInterval=10
StartLimitBurst=3
```

重新加载systemctl配置
`systemctl daemon-reload && systemctl restart icinga2`

安装icinga2专用的SELinux
`yum install icinga2-selinux`

配置语法高亮
`yum install vim-icinga2`

## 安装 Icinga Web 2

安装mysql
```
# yum install mariadb-server mariadb
# systemctl enable mariadb
# systemctl start mariadb
# mysql_secure_installation
```

安装icinga2的IDM模块（Icinga Data Output）
`# yum install icinga2-ido-mysql`

在mysql中建立用户及表
```
mysql>  CREATE DATABASE icinga;
mysql>  GRANT SELECT, INSERT, UPDATE, DELETE, DROP, CREATE VIEW, INDEX, EXECUTE ON icinga.* TO 'icinga'@'localhost' IDENTIFIED BY 'icinga';
mysql> quit
```

导入数据库schema
`# mysql -u root -p icinga < /usr/share/icinga2-ido-mysql/schema/mysql.sql`

启用 IDO MySQL Module并重启icinga服务
`# icinga2 feature enable ido-mysql`
`# systemctl restart icinga2`

防火墙规则
```
# firewall-cmd --add-service=http
# firewall-cmd --permanent --add-service=http
```

启用Icinga的REST API
`# icinga2 api setup`

```
# vim /etc/icinga2/conf.d/api-users.conf

object ApiUser "icingaweb2" {
  password = "Wijsn8Z9eRs5E25d"
  permissions = [ "status/query", "actions/*", "objects/modify/*", "objects/query/*" ]
}
```

安装Icinga Web2
`yum install icingaweb2 icingacli`

安装SELinux for Icinga Web2
`yum install centos-release-scl`
`yum install icingaweb2-selinux`

安装Web Server
```
yum install httpd
systemctl start httpd.service
systemctl enable httpd.service
```

启用PHP-FPM
```
systemctl start rh-php71-php-fpm.service
systemctl enable rh-php71-php-fpm.service
```

在`/etc/opt/rh/rh-php71/php.ini`中设置`date.timezone = Asia/Shanghai`

安装ImageMagick
`# yum install ImageMagick`

打开页面 http://127.0.0.1/icingaweb2/setup 完成剩下的设置

***重启Icinga服务，大功告成！***

## 附：从本地仓库安装Icinga2

首先需要从互联网上下载所有Icinga2的包
根据`/etc/yum.repos.d/ICINGA-release.repo`定义的yum源进行下载。

定义centos的源，然后用repotrack将Icinga包及其依赖的包全部抓下来。并用这些包建立一个新的yum repo。

```
repotrack -r epel-6 -r base-6 -r updates-6 -r extras-6 -r centosplus-6 -r icinga-stable-release-el6 nagios-4.3.4-7.el6.x86_64 nagios-plugins-all-2.2.1-4git.el6.x86_64 icinga2-2.8.4-1.el6.icinga.x86_64.rpm

createrepo icinga-stable-release
```

```
sftp XXX@aaa.bbb.ccc.ddd:/home/icinga2

sftp> mkdir icinga_repo
sftp> put -r icinga_repo
```

## 参考文档

[Icinga2 官方安装教程](https://www.icinga.com/docs/icinga2/latest/doc/02-getting-started/)

[Icinga Web2 官方安装教程](https://www.icinga.com/docs/icingaweb2/latest/doc/02-Installation/)

[插件配置](https://www.icinga.com/docs/icinga2/latest/doc/13-addons/#addons)
