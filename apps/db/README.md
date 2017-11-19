db
=====

An OTP application

Build
-----

    * Theory
        Example if we have node-1@example, then we want to deploy our apps in
        another host, example node-2@example, below steps are needed:
        * node-1@example:
            * schema is created
            * mnesia application is started
            * table is created
            * data is added into that table

        * node-2@example:
            * mnesia application is started

        * node-1@example:
            * mnesia:change_config(extra_db_nodes, ['node-2@example'])
            * mnesia:change_table_copy_type(schema, 'node-2@example', disc_copies)

        * node-2@example:
            * mnesia:add_table_copy(Table, 'node-2@example', disc_copies)

    * How to test on one machine:
        * rebar3 tar
        * deploy apps by tar file in one linux shell, then we will have
          dek_demo node

        * change mnesia dir in config/sys.erlcfg (example "/tmp/data1/")
        * change node name in config/vm.args (example dek_demo1)
        * rebar3 tar
        * deloy apps by tar file in another linux shell, then we will have
          dek_demo1 node
        * from dek_demo1 node, try to connect to dek_demo node
          db_server:connect('dek_demo@example').