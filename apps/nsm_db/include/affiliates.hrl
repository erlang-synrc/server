-record(affiliates_rels,
        {
         user      :: any(),        % User Id.

         affiliate :: any(),        % An Id of the user which owns
                                        % the affiliate.

         depth     :: non_neg_integer() % A depth of the position in the
                                        % affiliate tree.
        }).


-record(affiliates_look_perms,
        {
         user_id      :: any(),
         enabled      :: boolean()
        }).

-record(affiliates_contract_types,
        {
         id              :: any(),
         name            :: string(),
         duration        :: integer(),
         purchases_limit :: all | integer(),
         commission      :: number(),
         disabled        :: boolean()
        }).

-record(affiliates_contracts,
        {
         id              :: nsm_affiliates:contract_id(),
         name            :: string(),
         start_date      :: calendar:date(),
         finish_date     :: calendar:date(),
         purchases_limit :: all | integer(),
         commission      :: number(),
         owner           :: any()
        }).

-record(affiliates_purchases,
        {
         user_id        :: any(),
         contract_id    :: integer(),
         purchases      :: list(),   %% [{PurchaseId, Time, PackageId, PackageNum, PaymentType, Price, Commission}]
         purchases_num  :: integer(),
         purchases_sum  :: integer(),%% Tl
         commission_sum :: integer() %% Tl * 100
        }).

