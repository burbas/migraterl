# Build
In order to build Migraterl please use Rebar.

    ./rebar compile

# Usage

The following call will copy the `test` script to the destination node `'test2@localhost'`. `test:test()` will return
the name of the node it's running on.

     (test@localhost)1> test:test().
     test@localhost
     (test@localhost)2> application:start(migraterl).
     ok
     (test@localhost)3> net_adm:ping('test2@localhost').
     pong
     (test@localhost)4> migraterl:migrate_module(migraterl, 'test2@localhost', true).
     ok
     (test@localhost)5> test:test().
     test2@localhost

What happens is that the functions in the module `test` have been replaced by rpc-calls to the destination server. This might be usefull
for people who want to migrate some modules to different nodes, but not wanting to change the internal API.
