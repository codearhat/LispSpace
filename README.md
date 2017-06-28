# LispSpace
基于Spacemacs和SLIME，运行在Windows上的64位Common Lisp开发环境。

## 说明
我初学Common Lisp（以下简称CL）时对CL+EMACS+SLIME的配置很头痛，尝试多次才成功。为免重复劳动，就把配置好的开发环境打包作为套件，称为LispBase。

2017年6月学会了用Windows版（之前只会用cygwin版）Emacs配置Spacemacs，因此套件改名为LispSpace。
目标操作系统是64位的Win7/8.1/10。

欢迎同好使用。如有软件缺陷、版权问题、改进建议，请与我联系：codearhat@gmail.com

## 软件版本
* emacs-25.2.1
  http://www.gnu.org/software/emacs/
* spacemacs-0.200.9
  http://spacemacs.org/
* slime-2.19
  http://common-lisp.net/project/slime/
* sbcl-1.3.18
  SBCL开发活跃、性能好、32/64均支持。
  http://www.sbcl.org/
* ccl-1.11
  在Windows平台比较稳定，支持多种平台。
  http://ccl.clozure.com/
* abcl-1.5
  基于JVM的CL实现，启动较慢，运行速度还可以。
  https://common-lisp.net/project/armedbear/
* quicklisp
  CL事实上的包管理器。
  http://www.quicklisp.org/
* openssl-1.0.2L
  https://slproweb.com/products/Win32OpenSSL.html
* sqlite-3.19.3
  https://www.sqlite.org/
* mysql-client-6.0.2-winx64-vs2005
  https://dev.mysql.com/downloads/connector/c/
* nginx-1.13.1
  https://nginx.org/
* gnuplot-5.0.6
  http://gnuplot.sourceforge.net/

## 预装包（System）
我想用CL开发实际项目，因此预装了网络、数据库、加密、WEB、图形等软件包。
* caveman2（WEB开发）
* cl-base64
* cl-ftp
* cl-opengl
* cl-mysql
* clsql-odbc
* drakma（HTTP客户端、WEB爬虫）
* ftp
* ironclad（加密）
* md5
* plain-odbc
* postmodern（PostgreSQL客户端）
* sha3
* snmp
* telnetlib
* usocket
* vgplot（调用gnuplot绘图）

## .spacemacs设定
* 改用http下载ELPA(Emacs Lisp Package Archive)：`dotspacemacs-elpa-https nil`
* 改用Consolas字体：`dotspacemacs-default-font '("Consolas" :size 16 ...`
* 添加以下layer: dotspacemacs-configuration-layers
  * common-lisp
  * git
  * html
  * javascript
  * markdown
* 添加代理配置：dotspacemacs/user-init ()
* 配置CL程序：dotspacemacs/user-config ()

## 修改的热键：

## 资料
doc目录有些公开的CL资料。以下是些CL资料网站：
* [ANSI Common Lisp](http://acl.readthedocs.io/en/latest/zhCN/index.html)
* [Practical Common Lisp](http://www.gigamonkeys.com/book/)
* [HyperSpec](http://www.lispworks.com/documentation/HyperSpec/Front/index.htm)
* [The Common Lisp Cookbook](http://cl-cookbook.sourceforge.net/)
* https://common-lisp.net/
* http://quickdocs.org/

推荐书籍：
* Common Lisp Recipes
* Let Over Lambda - 50 Years of Lisp
* Paradigms of Artificial Intelligence Programming: Case Studies in Common Lisp
* Land of Lisp
