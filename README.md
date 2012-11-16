
KAKARANET DEPLOY
================

Contents
--------

* Infrastructure
* Building Project
* Configuring and Creating Releases
* Tuning
* Translations
* JavaScript Compressing
* Deploying to MASTER {SRV1,SRV2,SRV3}
* Profiling

Infrastructure
--------------

Project consist of set of erlang applications
that you can find in "apps" directory. They running
in context of tree erlang instances (three-layer cluster):

* Application Processes
* Game Processes
* Comet server

Each application has its own prefix to easily determine
to which layer it belongs:

    nsx - common applications
    nsm - application server
    nsg - game server
    nsw - comet server

All system relies on external erlang instances
that should be installed as part of deployment process externally:

* RabbitMQ enterprise message bus (separate cluster)
* Riak distributed hashtable (built-in in each node)

Building Project
----------------

### ERLANG

    $ sudo apt-get install build-essential libncurses5-dev openssl libssl-dev

Kakaranet project builds essentially with rebar and reltool.
You should use Erlang not less than R14. You should kerl erlang with
following ~/.kerlrc

    KERL_CONFIGURE_OPTIONS="--enable-threads --enable-smp-support \
              --enable-m64-build --without-javac --enable-kernel-poll"

And the kerl procedure is following:

    $ git clone git://github.com/spawngrid/kerl.git & cd kerl
    $ ./kerl build R15B02 r15b02
    $ ./kerl install r15b02 ~/apps/erlang-R15B02
    $ . ~/apps/erlang-R15B02/activate

Add folloowing lines to /etc/profile:

    PATH=$PATH:/home/kakauser/apps/erlang-R15B02/bin
    . /home/kakauser/apps/erlang-R15B02/activate

Create this dir and subdirs for later usage:

    $ mkdir -p /mnt/glusterfs/
    $ mkdir -p /mnt/glusterfs/apps
    $ mkdir -p /mnt/glusterfs/kakafiles

### RIAK

Install Riak version 1.2 from sources:

    $ cd /mnt/glusterfs/apps
    $ git clone https://github.com/basho/riak.git
    $ cd riak
    $ git checkout 8b32af5f362daa7018c10ecd509c1c3694cd8f3f
    $ make rel

Symlink required riak libraries to your erlang libs dir:

    $ ln -s /mnt/glusterfs/apps/riak/rel/riak/lib/ \
         {riak_sysmon,bear,riak_pipe,riak_kv,riak_core,poolboy, \
          luke,erlang_js,riak_pb,riak_control,webmachine,bitcask, \
          folsom,bear,riak_api,sext,eleveldb,protobuffs,ebloom,mochiweb}-* ~/apps/erlang-R15B02/lib/

Required files for db to be initialized properly:
Create folder for gifts:

    $ mkdir -p /home/kakauser/tmp
    $ mkdir -p /home/kakauser/tmp/kaka

And copy files from ~/kakauser/tmp/kaka  n srv1.kakaranet.com 
or srv2.kakaranet.com to this folder.

### RABBIT-MQ

For Ubuntu and Windows users please install prebuilt binaries from official site.
For Mac users please build RabbitMQ from sources:

    $ cd /mnt/glusterfs/apps/
    $ wget http://www.rabbitmq.com/releases/rabbitmq-server/v2.8.5/rabbitmq-server-2.8.5.tar.gz
    $ tar xvzf rabbitmq-server-2.8.5.tar.gz
    $ cd rabbitmq-server-2.8.5

Install additional lib before you go if you are building from sources:

    $ apt-get install xmlto
    $ make
    $ sudo make install TARGET_DIR=/usr/local/rabbitmq-server \
                        SBIN_DIR=/usr/local/bin MAN_DIR=/usr/local/man

Configure installed RabbitMQ instance:

    $ sudo rabbitmqctl add_user guest guest
    $ sudo rabbitmqctl add_vhost "/"
    $ sudo rabbitmqctl set_permissions -p "/" guest '.*' '.*' '.*'

Fast local start
----------------

Create this dir and subdirs for later usage:

    $ mkdir -p /mnt/glusterfs/
    $ mkdir -p /mnt/glusterfs/apps
    $ mkdir -p /mnt/glusterfs/kakafiles

Then just do local build

    $ ./rebar get-deps
    $ ./rebar compile
    $ ./release.sh
    $ ./release_sync.sh (for code hot-load)
    $ ./single_configure.sh
    $ ./start.sh

More deeply undestanding of local build
---------------------------------------

The erlang nodes are to build with reltool. There is three type of nodes
in "rels" directory for each cluster layer. Each node can be configured
at pre or post releasing stages. For example if we want to configure
application middleware and then release with run, we should issue:

    host:~/ns/rels/app/files$ ./configure (configure app.config)
    host:~/ns/rels/app$ ../../rebar -f generate (make release with reltool)
    host:~/ns/rels/app$ node/bin/ns_node console (run node in debug console)

All releases will be put in rels/*/node directories.
For local use all releases can be configured as above
but Web server needs to be configured at least with two parameters:

    host:~/ns/rels/web/files$ ./configure 192.168.1.108 app@local.com

You can tune each release with custom "config" erlang script.
E.g. if you want to change AMF port in Game server you can do following:

    host:~/ns/rels/game/node/etc$ ./config -file app.config nsg_srv game_srv_port -integer 9000

You can use it in "configure" shell scripts for automating releasing process.
Look into "config" script to see all available options and parameters.

Translations
------------

We are using Erlang gettext for localization. If developers implement some
localization aware functionality, the putting it to code as _T("Localizable String").
Then we need to perform two stages process of updating PO files.

Stage 1.

* Build Sources
* Move all strings from code to Default English PO file.
* Not release but just compile.
* Then you need to run translation.sh script:

    host:~/ns/apps/nsw_srv$ ./translation.sh

This will creates ~/ns/apps/nsw_srv/translations/lang/kakaweb/en/gettext.po the latest
syncronized default EN language.

Stage 2.

Update newly added strings from sources/default to local langugages.
You need to perform custom update.sh script for your language. E.g. for TR language:

    host:~/ns/apps/nsw_srv/priv/lang/custom/tr$ ./update.sh

This will updates turkish po file which you can edit with poedit and inplace commit.
Then system will build up on servers with buildbot and restarted with newly added string.

NOTE: before commit PO file, run update.sh again to delete
      additional empty lines between messages

Changes PO files in runtime.

If you need to update PO file on running system,
you should do following steps:

    $ mv gettext.po /rels/web/node/lib/nsw_srv-1/priv/lang/custom/tr/gettext.po
    $ /rels/web/node/bin/ns_node stop
    $ /rels/web/node/bin/ns_node stop

JavaScript Compressing
----------------------

Generation of combined/minified JS code requires juicer to be installed.
Get it from github:

    http://github.com/cjohansen/juicer

Get it using gem. You'd might want to start with libxml2:

    $ apt-get install libxslt-dev libxml2-dev

And then the gem itself:

    $ apt-get install ruby1.9.1-dev

And the juicer

    $ gem install juicer
    $ juicer install yui_compressor
    $ juicer install jslint  # currently not used, but for future


Deploying to MASTER {SRV1,SRV2,SRV3}
------------------------------------

Go to srv1/srv2/srv3:

    $ cd /home/kakauser/release/server

And perform steps described in Fast Local Start. 
Check if everything is OK and Enjoy!

Profiling
---------

To create profile of request to some URL, attach to webserver node console, and do:

    > rr(httpd).
    > fprof:start().
    > ets:insert(ac_tab, {{application_master,test}, group_leader()}).
    > application:set_env(test, nitrogen_handler_module, wf_context).
    > {ok, Socket} = gen_tcp:connect({127,0,0,1}, 7788, []).
    > fprof:apply(fun nitrogen:do/1, [#mod{socket=Socket, 
            method="GET",absolute_uri="http://localhost:7788/",
            request_uri="/",http_version="HTTP/1.0",request_line = 
            "GET / HTTP/1.0", parsed_header = [{"host", "localhost:7788"}],entity_body = "",
            connection = false}]).
    > gen_tcp:close(Socket). f().
    > fprof:profile().
    > fprof:analyse([{dest,""}]).

Result of analyse will be in ./rels/web/node/fprof.analysis, sorted by ACC.
How to read that output, see into fprof man page.

Feed Attachments
----------------

Apparantly we need graphicmagick (http://www.graphicsmagick.org/) to process uploaded images. 
Get it using apt-get:

    $ apt-get install graphicsmagick

Or read installation notes from official website.

-- 
maxim@synrc.com 
OM A HUM
