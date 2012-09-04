%%----------------------------------------------------------------------
%% @author Vladimir Baranov <baranoff.vladimir@gmail.com>
%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%% @doc
%%   Definitions for notifications
%% @end
%%--------------------------------------------------------------------

-record(notification, {object,
                       sub_object,
                       type,
                       action,
                       event_data
                      }).



