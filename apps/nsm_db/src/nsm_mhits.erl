%% Author: Sergei Polkovnikov <serge.polkovnikov@gamil.com>
%% Created: Dec 17, 2012
%% Description: TODO: Add description to nsm_mhits
-module(nsm_mhits).

%%
%% Include files
%%
-include("mhits.hrl").

%%
%% Exported Functions
%%
-export([store/3, stat_word_date/2, stat_word/1, stat_date/1, stat_ip_date/2, clean_date/1, clean_all/0]).

%%
%% API Functions
%%

store(Word, IP, Date) -> nsm_db:put(#mhits{word = Word, ip = IP, date = Date}).

stat_word_date(Word, Date) -> nsm_db:all_by_index(mhits, <<"mhits_word_date_bin">>, {Word, Date}).

stat_word(Word) -> nsm_db:all_by_index(mhits, <<"mhits_word_bin">>, Word).

stat_date(Date) -> nsm_db:all_by_index(mhits, <<"mhits_date_bin">>, Date).

stat_ip_date(IP, Date) -> nsm_db:all_by_index(mhits, <<"mhits_ip_date_bin">>, {IP, Date}).

clean_date(Date) -> nsm_db:delete_by_index(mhits, <<"mhits_date_bin">>, Date).

clean_all() -> nsm_db:delete_by_index(mhits, <<"bucket_bin">>, mhits).

%%
%% Local Functions
%%

