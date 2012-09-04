
KAKARANET PROJECT README
==========================

Contents
--------

   -- Infrastructure
   -- Building Project
   -- Configuring and Creating Releases
   -- Tuning
   -- Translations
   -- JavaScript Compressing
   -- Deploying to MASTER SRV3
   -- Profiling

Infrastructure
--------------

Project consist of set of erlang applications
that you can find in "apps" directory. They running
in context of tree erlang instances (three-layer cluster):

   -- Application server middleware
   -- Game server
   -- Web server

Each application has its own prefix to easily determine
to which layer it belongs:

      nsx - common applications
      nsm - application server
      nsg - game server
      nsw - web server

All system relies on external erlang instances
that should be installed as part of deployment process externally:

   -- RabbitMQ enterprise message bus
   -- Riak distributed hashtable
   -- CoachDB (Will be dropped soon. Kept for backward compatibility issues.)

Building Project
----------------

ERLANG

Kakaranet project builds essentially with rebar and reltool.

You should use Erlang R14.
If you have other version installed, use kerl:

      $ curl -O https://raw.github.com/spawngrid/kerl/master/kerl; chmod a+x kerl
      $ ./kerl build R14B04 r14
      $ ./kerl install r14 ~/erl-r14/
      $ . ~/erl-r14/activate

kerl has a problems with an existin PATH vars. 

If you have any problems with this approach, please read here:
http://wiki.basho.com/Installing-Erlang.html


Create this dir and subdirs for later usage:
mkdir -p /mnt/glusterfs/
mkdir -p /mnt/glusterfs/apps
mkdir -p /mnt/glusterfs/kakafiles


RIAK 

Install Riak version 1.2 to 
cd /mnt/glusterfs/apps
git clone https://github.com/basho/riak.git
cd riak
make rel

Symlink required riak libraries to your erlang libs dir:

ln -s /mnt/glusterfs/apps/riak/rel/riak/lib/{riak_sysmon,riak_pipe,riak_kv,riak_core,poolboy,luke,erlang_js,bitcask,folsom,riak_api,sext,eleveldb}-* ~/erl-r14/lib/

Note: If you see errors for gen_server2, try to remove rabbit from system at first. 

RABBIT-MQ

Download from here:
http://www.rabbitmq.com/download.html
Source .tar.gz
Current version is 2.8.5

cd /mnt/glusterfs/apps/
wget http://www.rabbitmq.com/releases/rabbitmq-server/v2.8.5/rabbitmq-server-2.8.5.tar.gz
tar xvzf rabbitmq-server-*.tar.gz
cd rabbitmq-server-*

install additional lib before you go:
apt-get install xmlto 

make
sudo make install TARGET_DIR=/usr/local/rabbitmq-server SBIN_DIR=/usr/local/bin MAN_DIR=/usr/local/man

:TODO
Additional settings for rabbit will be explained here.

Configuring and Creating Releases
---------------------------------

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

      host:~/ns/rels/web/files$ ./configure 192.168.1.108 7788 (specify listen address and port)

Initial local startup
---------------------

Compile & configure releases for local by running:

      host:~/ns$ ./local.sh
      host:~/ns$ ./start.sh
      host:~/ns$ ./rels/app/node/bin/ns_node attach
      (app@{hostname})1> zealot_db:init_db().
      ^D

Tuning
------

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

- Move all strings from code to Default English PO file.
- *Before doing that you should already build all source tree.*
- Not release but just compile.
- Then you need to run translation.sh script:

      host:~/ns/apps/nsw_srv$ ./translation.sh

- This will creates ~/ns/apps/nsw_srv/translations/lang/kakaweb/en/gettext.po the latest
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

      1. Put gettext.po for a given languages on /rels/web/node/lib/nsw_srv-1/priv/lang/custom/tr/gettext.po
      2. Stop web server /rels/web/node/bin/ns_node stop
      2. Start web server /rels/web/node/bin/ns_node stop

JavaScript Compressing
----------------------

Generation of combined/minified JS code requires juicer to be installed.
Get it from github:

      http://github.com/cjohansen/juicer

Get it using gem:

      $ gem install juicer
      $ juicer install yui_compressor
      $ juicer install jslint  # currently not used, but for future

Deploying to MASTER SRV3
------------------------

      $ go to srv3
      $ cd /home/kakauser/release/
      $ run command: ./deploy.sh
      $ check if everything is ok
      $ enjoy

Profiling
---------

To create profile of request to some URL, attach to webserver node console, and do:

      > rr(httpd).
      > fprof:start().
      > ets:insert(ac_tab, {{application_master,test}, group_leader()}).
      > application:set_env(test, nitrogen_handler_module, wf_context).
      > {ok, Socket} = gen_tcp:connect({127,0,0,1}, 7788, []).
      > fprof:apply(fun nitrogen:do/1, [#mod{socket=Socket, method="GET",absolute_uri="http://localhost:7788/",request_uri="/",http_version="HTTP/1.0",request_line = "GET / HTTP/1.0", parsed_header = [{"host", "localhost:7788"}],entity_body = "",connection = false}]).
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

Or read installation notes from official website: http://www.graphicsmagick.org/INSTALL-unix.html


OM A HUM
