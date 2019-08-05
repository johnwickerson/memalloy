./comparator -desc "postgres_not_no_sz" -arch SQL -violates models/sql16_no_sz.als -satisfies models/sql_postgres.als -events 7 -expect 0
./comparator -desc "postgres_not_sz_order" -arch SQL -violates models/sql16_sz_order.als -satisfies models/sql_postgres.als -events 7 -expect 0
