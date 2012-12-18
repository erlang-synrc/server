-module(nsm_mhits).
-author(' Serge Polkovnikov <serge.polkovnikov@gmail.com>').
-include("mhits.hrl").
-compile(export_all).

store(Word, IP, Date) -> nsx_msg:notify(["system","count_user"], #mhits{word = Word, ip = IP, date = Date, count = 1}).
stat_word_date(Word, Date) -> nsm_db:all_by_index(mhits, <<"mhits_word_date_bin">>, {Word, Date}).
stat_word(Word) -> nsm_db:all_by_index(mhits, <<"mhits_word_bin">>, Word).
stat_date(Date) -> nsm_db:all_by_index(mhits, <<"mhits_date_bin">>, Date).
stat_ip_date(IP, Date) -> nsm_db:all_by_index(mhits, <<"mhits_ip_date_bin">>, {IP, Date}).
stat_word_ip_date(Word,IP,Date) -> nsm_db:all_by_index(mhits, <<"mhits_word_ip_date_bin">>, {Word,IP, Date}).
clean_date(Date) -> nsm_db:delete_by_index(mhits, <<"mhits_date_bin">>, Date).
clean_all() -> nsm_db:delete_by_index(mhits, <<"bucket_bin">>, mhits).
