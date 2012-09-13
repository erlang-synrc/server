%%----------------------------------------------------------------------
%% @author Baranov Vladimir <vladimir.b.n.b@gmail.com>
%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%% @doc
%% Membership packages definitions file
%% @end
%%---------------------------------------------------------------------

-type payment_type():: credit_card | mobile | paypal | wire_transfer | facebook.
-type purchase_state() :: added | done | cancelled | pending | untracked.

-record(basket,
        {
         user_id                    ::any(),      % user id
         package_id                 ::any()}).    % FIXME: could be the trace. If item (package/membership)
                                                  % will change between user put buy button and order creation
                                                  % user can get a changed packet

-record(membership_package,
        {
         id                         ::any(),      % package id
         payment_type               ::payment_type(),
         no                         ::integer(),  % package number (need to display in pricelist)
         amount                     ::integer(),  % price
         deducted_for_gifts         ::integer(),  % kakush currency charge
         net_membership             ::integer(),  % net membership fee
         available_for_sale = false ::boolean(),  % not used yet
         quota::integer()                         % game quota
        }).

%% record for logging purchase states changes. For now log stored inside
%% purchase. an be moved to separate log table.
%% Info definitions by states:
%%   untracked - {real_state_name, real_state_info}
-record(state_change, {time     :: erlang:now(),
                       state    :: any(),
                       info     :: any()}).

-record(membership_purchase,
        {id                    :: any(),
         external_id           :: any(),     % id of the purchase in external payment system if any
         user_id               :: any(),
         state                 :: purchase_state(),
         membership_package    :: #membership_package{},
         start_time            :: erlang:now(),
         end_time              :: erlang:now(),
         state_log = []        :: [#state_change{}],
         info                  :: any()      % payment-specific info about purchase if any
        }).

%% Purchase states. MP = membership purchase

-define(MP_STATE_ADDED,     added).
-define(MP_STATE_DONE,      done).
-define(MP_STATE_CANCELLED, cancelled).
-define(MP_STATE_UNTRACKED, untracked).
-define(MP_STATE_PENDING,   pending).
-define(MP_STATE_FAILED,    failed).
%% temporary state, will be used to mark unexpected states received from PP
-define(MP_STATE_UNKNOWN,   unknown).

%% when purchase became confirmed we will made internal
%% transaction to move funds from system account to user
-define(MP_STATE_CONFIRMED, confirmed).
%% when we receive confirmation about purchase which is unknown for us
%% create empty purchase with this state and received information
-define(MP_STATE_UNEXPECTED, unexpected).
%% purchase dropped by admin
-define(MP_STATE_DISCARDED, discarded).

%% Payment specific info records

%% pi - from Purchase Info
-record(pi_credit_card, {
          cardholder_name,
          cardholder_surname,
          cardnumber_masked,
          retref_num,
          prov_date,
          auth_code
}).
