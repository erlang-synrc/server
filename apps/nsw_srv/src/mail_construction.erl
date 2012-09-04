%%----------------------------------------------------------------------
%% @author Pawel Flis <pawel_flis@silversoft.pl>
%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%% @doc
%% This module constructs an email message
%% 28/06/2011
%% @end
%%----------------------------------------------------------------------

-module(mail_construction).

-include("gettext.hrl").

-export([welcome/3, verification/2]).


welcome(User, Password, _Mail) ->
    Subject = ?_T("Welcome to Kakaranet"),

    PlainText = ?_TS("Thank you for using Kakaranet Social Zone. \n\n"

        "Please find your registration details below:\n\n"
        "User name: $username$\n"
        "Password: $password$\n\n"

        "If you did not register for Kakaranet, then someone probably\n"
        "mis-typed their email address. You can ignore this message,\n"
        "and we apologize for the inconvenience.\n\n"

        "Happy Social Gaming,\n"
        "The Kakaranet team\n",
        [{username, User}, {password, Password}]
    ),

    {Subject, PlainText}.


verification(Mail, VerificationCode) ->
    VerifyAddress = verify:create_url(VerificationCode),

    Subject = ?_T("Kakaranet membership verification"),

    PlainText = ?_TS("Thank you for using Kakaranet Social Zone. \n"
        "To complete the registration process,\n"
        "you need to confirm your email address ($email$)\n"
        "by clicking the link below: \n\n"

        "$verificationaddress$\n\n"

        "If you did not register for Kakaranet, then someone probably \n"
        "mis-typed their email address. You can ignore this message, \n"
        "and we apologize for the inconvenience.\n\n"

        "If you have any problems verifying your email address, \n"
        "you can get help from us and other Kakaranet users \n"
        "in the Kakaranet feedback group (http://kakaranet.uservoice.com)\n\n"

        "Happy Social Gaming,\n"
        "The Kakaranet team\n",
        [{email, Mail}, {verificationaddress, VerifyAddress}]),

    {Subject, PlainText}.

