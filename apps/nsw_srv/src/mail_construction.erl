-module(mail_construction).
-include("gettext.hrl").
-compile(export_all).

tournament(User, Mail, Date, Time, Gift, Tournament) ->
    Subject = ?_T("Kakaranet Okey Turnuva Duyuru"),
    PlainText = ?_TS("Değerli $username$!\n\n"

        "Bugün $date$ saat $time$ de başlayacak $gift$ ödüllü Kakaranet in "
        "$tour$ okey turnuvasını hatırlatmak için bu e postayı gönderdik "
        "$gift$ e ulaşmak için lütfen zamanında yerinizi alın.\n\n"

        "İyi oyunlar dileriz.\n\n"

        "Kakaranet ekibi",
        [{username, User}, {gift, Gift},{tour, Tournament},{time,Time},{date,Date}]
    ),

    {Subject, PlainText}.

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

