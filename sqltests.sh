./comparator -desc "sql16_non_repeatable_read_basic_violation_hint" -arch SQL -violates models/sql16.als -hint execution_hints/sql_non_repeatable_read_initial.als -events 5 -expect 1
./comparator -desc "sql16_non_repeatable_read_initial_violation_hint" -arch SQL -violates models/sql16.als -events 7 -expect 1
./comparator -desc "sql16_sz_order_long_fork_basic_hint" -arch SQL -violates models/sql16.als -hint execution_hints/sql_long_fork_weakest.als  -events 10 -expect 0
./comparator -desc "sql16_sz_order_long_fork_one_ser_hint" -arch SQL -violates models/sql16.als -hint execution_hints/sql_long_fork_one_ser.als  -events 10 -expect 0
./comparator -desc "sql16_sz_order_long_fork_strongest_hint" -arch SQL -violates models/sql16.als -hint execution_hints/sql_long_fork_strongest.als  -events 10 -expect 1
./comparator -desc "postgres_not_no_sz" -arch SQL -violates models/sql16.als -satisfies models/sql_postgres.als -events 7 -expect 0
./comparator -desc "postgres_not_sz_order" -arch SQL -violates models/sql16.als -satisfies models/sql_postgres.als -events 7 -expect 0
