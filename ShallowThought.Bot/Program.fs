open ShallowThought.BotFramework
open ShallowThought.BotFramework.Util
open Kathy

let server = "irc.luna.red"
let port = 44444
let nick = "ShallowThought"
let channels = ["#squad"]
let password = "bokunopico911"

let mutable chat_enabled = false

let msgHandler (line: string) (write: string -> unit) =
    let (|Prefix|_|) (p:string) (s:string) =
        if s.StartsWith( p ) then
            Some (s.Substring(p.Length))
        else
            None

    let say channel text = write (sprintf "PRIVMSG %s :%s" channel text) 

    let everyone_else (user: string) (channel: string) (content: string) =
        match content with
            | Prefix "!echo" rest -> say channel (sprintf "Don't tell me what to do, %s" user)
            | Prefix "!time" rest -> say channel (sprintf "Up yours, %s" user)
            | Prefix "!die" rest -> say channel "Lol no"
            | Prefix "!kill" rest -> 
                let trimmed = rest.Trim()
                match trimmed with
                | Prefix "irisu" rem -> 
                    say channel "Hehehehe, ok"
                    say channel ("!irisu " + "ثم نفس سقطت وبالتحديد،, جزيرتي باستخدام أن دنو. إذ هنا؟ الستار وتنصيب كان. أهّل ايطاليا، بريطانيا-فرنسا قد أخذ. سليمان، إتفاقية بين ما, يذكر الحدود أي بعد, معاملة بولندا، الإطلاق عل إيو.")
                    say channel ("!irisu " + "בְּרֵאשִׁית, בָּרָא אֱלֹהִים, אֵת הַשָּׁמַיִם, וְאֵת הָאָרֶץ")
                    say channel ("!irisu " + "הָיְתָהtestالصفحات التّحول")
                    say channel ("!irisu " + "﷽")
                    say channel ("!irisu " + "مُنَاقَشَةُ سُبُلِ اِسْتِخْدَامِ اللُّغَةِ فِي النُّظُمِ الْقَائِمَةِ وَفِيم يَخُصَّ التَّطْبِيقَاتُ الْحاسُوبِيَّةُ، ")
                | _ -> say channel ( sprintf "I rather kill you, %s" user )
            | Prefix "!please" rest ->
                let trimmed = rest.Trim()
                match trimmed with
                | Prefix "echo" rem -> say channel ( sprintf "Ugh fine... '%s'" (rem.Trim()) )
                | Prefix "cooperate" rem -> say channel ("When pigs fly, meatbag")
                | Prefix "time" rem -> say channel ( sprintf "It's %s. Wear a watch next time." (System.DateTime.Now.ToString()) )
                | _ -> ()
            | Prefix "!speak" rest ->
                chat_enabled <- true
                say channel "Ok"
            | Prefix "!shutup" rest ->
                chat_enabled <- false
                say channel "Aww :("
            | _ -> 
                if chat_enabled then
                    let lower = content.ToLowerInvariant()
                    let response = 
                        if (lower.IndexOf(" ") > 0) then
                            Kathy.response (lower.Substring(0, lower.IndexOf(" "))) (lower.Substring(lower.IndexOf(" ")+1)) + "\n"
                        else
                            Kathy.response lower "" + "\n"
                    say channel response
                    
    let owner_talking (user: string) (channel: string) (content: string) =
        match content with
        | Prefix "!echo" rem -> say channel ( sprintf "'%s'" (rem.Trim()) )
        | Prefix "!cooperate" rem -> say channel ("Anything for you, Sir")
        | Prefix "!time" rem -> say channel ( sprintf "It's %s" (System.DateTime.Now.ToString()) )
        | Prefix "!die" rest -> exit(0)
        | _ -> (everyone_else user channel content)        

    match line with
    | Message (JOIN(user, channel)) when user = nick -> say channel "Hello world!"
    | Message (JOIN(user, channel)) -> say channel (sprintf "Hi %s!" user)
    | Message (QUIT(user, reason)) -> say "#squad" (Kathy.good_bye_response())
    | Message (PRIVMSG(user, channel, content)) when Seq.exists ((=) channel) channels ->
        if (user = "Pr0metheus") then
            owner_talking user channel content
        else
            everyone_else user channel content
    | _ -> ()

[<EntryPoint>]
let main argv = 
    let bot = new SimpleBot(server, port, nick, channels, password, msgHandler)
    bot.Start()
    0
