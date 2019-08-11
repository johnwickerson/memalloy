./comparator -desc "sql16_non_repeatable_read_basic_violation_hint" -arch SQL -violates models/sql16_no_sz.als -hint execution_hints/sql_non_repeatable_read_initial.als -events 5 -expect 1
./comparator -desc "sql16_non_repeatable_read_initial_violation_hint" -arch SQL -violates models/sql16_no_sz.als -events 7 -expect 1
./comparator -desc "postgres_not_no_sz" -arch SQL -violates models/sql16_no_sz.als -satisfies models/sql_postgres.als -events 7 -expect 0
./comparator -desc "postgres_not_sz_order" -arch SQL -violates models/sql16_sz_order.als -satisfies models/sql_postgres.als -events 7 -expect 0
