module Kathy 

// Define an active recognizer for numbers
// If the conversion suceeds it is a Number
// else it is None
let (|Number|None|) (input:string) =
    try let value = System.Double.Parse(input) 
        Number
    with 
        | _ -> None

// Define an active recognizer for Math expressions keywords and for numbers
let (|Math|None|) (input:string) =
    match input with
        | Number | "numbers" | "number" | "math" | "plus" | "sign" | "+" | "-" | "*" | "%" | "many" | "much"
            -> Math
        | _ 
            -> None

// Define an active recognizer for keywords that express desire, positive statements, etc.
let (|Wish|Positive|Negative|None|) input =
    match input with
        | "want" | "desire" | "need"
                -> Wish
        | "certainly" | "yes" | "true" | "ok" | "right"
                -> Positive
        | "not" | "no" | "don't" | "false" | "wrong"
                -> Negative
        | _ -> None

// Define an active recognizer for keywords that express salutation, dismissal, personal statements, etc.
let (|Hello|Bye|Personal|Question|Answer|None|) input =
    match input with
        | "hello" | "hi" | "morning" 
                -> Hello
        | "Goodbye" | "bye" | "go"
                -> Bye
        | "you" | "be" | "am" | "is" | "are" | "was" 
                -> Personal
        | "why" 
                -> Question
        | "because" 
                -> Answer
        | _     
                -> None

// init randomizer
let rand = new System.Random()

// branching posible responses based on random number
let hello_response () =
    let n = rand.Next(10) 
    match n with
        | 0 -> "How do you do."
        | 1 -> "Is nice talking to you."
        | 2 -> "Tell me something new."
        | 3 -> "Nice to meet you."
        | 4 -> "My pleasure."
        | 5 -> "Hi."
        | 6 -> "Hello."
        | 7 -> "Good day."
        | 8 -> "Salutation!"
        | 9 -> "Welcome!"

let good_bye_response () =
    let n = rand.Next(10) 
    match n with
        | 0 -> "Talk to you soon."
        | 1 -> "It was nice talking to you."
        | 2 -> "Good bye."
        | 3 -> "Stay a bit longer."
        | 4 -> "Adios amigo."
        | 5 -> "Bye."
        | 6 -> "Adios."
        | 7 -> "See you."
        | 8 -> "Please don't go"
        | 9 -> "Why are you leaving me alone?"

let personal_response (str:string) =
    let n = rand.Next(10) 
    match n with
        | 0 -> "My name is Meliza Sharp."
        | 1 -> "I work as a teacher."
        | 2 -> "I am 30 years old."
        | 3 -> "I have a lot of friends."
        | 4 -> "I love children."
        | 5 -> "I love pets."
        | 6 -> "I think a better world is possible."
        | 7 -> "I have no car, I go to work in a bicycle."
        | 8 -> "I am very concerned with the environment."
        | 9 -> "I hate lyers."

let question_response (str:string) =
    let n = rand.Next(10) 
    match n with
        | 0 -> "Why are you asking " + str
        | 1 -> "I also think " + str
        | 2 -> "Do you really want to know about " + str + "?"
        | 3 -> "I don't know "
        | 4 -> "Do you find interesting to tall about " + str + "?"
        | 5 -> "What do you mean with " + str + "?"
        | 6 -> "Don't bother me with " + str
        | 7 -> "Let us consider talking about " + str + " a bit further."
        | 8 -> "Why are interested in " + str + "?"
        | 9 -> "Do you think " + str + " is worth dicussing it"

let answer_response (str:string) =
    let n = rand.Next(10) 
    match n with
        | 0 -> "Yes, " + str
        | 1 -> "Is that the real reason " + str + "?" 
        | 2 -> "Do you really want to know about " + str + "?"
        | 3 -> "Should you " + str + "?"
        | 4 -> "Does that reason seem to explain anything else?"
        | 5 -> "Maybe"
        | 6 -> "That's ok with " + str
        | 7 -> "Ok, " + str + " is very interesting subject."
        | 8 -> "No, I really dont matter whether " + str + " or not"
        | 9 -> "If only it were always be an answer for " + str + "!"

let wish_response (str:string) =
    let n = rand.Next(10) 
    match n with
        | 0 -> "I wish to have a lot of money to " + str
        | 1 -> "What would it mean if you got " + str + "?" 
        | 2 -> "Why do you want " + str + "?"
        | 3 -> "Suppose you got " + str + " soon!"
        | 4 -> "Anybody wants " + str 
        | 5 -> "I think don't really want " + str
        | 6 -> "If only it were true that " + str + "!"
        | 7 -> "We some times need a miracle!"
        | 8 -> "I do not belive on miracles myself"
        | 9 -> "Your have to put your feets on the ground!"


let negative_response (str:string) =
    let n = rand.Next(10) 
    match n with
        | 0 -> "Why not?"
        | 1 -> "Don't give up with " + str 
        | 2 -> "We allways have to be positive"
        | 3 -> "Are you sure you don't " + str + "?"
        | 4 -> "I am allways optimistic when facing " + str 
        | 5 -> "Just try to overcome " + str
        | 6 -> "I do not see the dificulty to face it"
        | 7 -> "Do not worry, we always find a light at the end of the tunnel"
        | 8 -> "This crisis wont last forever"
        | 9 -> "There are signs of recovery"

let positive_response (str:string) =
    let n = rand.Next(10) 
    match n with
        | 0 -> "That's the spirit we need"
        | 1 -> "You will suceed with " + str 
        | 2 -> "We have to be positive"
        | 3 -> "Live is beautiful"
        | 4 -> "You are very optimistic!" 
        | 5 -> "Good luck!"
        | 6 -> "At least I haven't loose my house"
        | 7 -> "Yes, you are absolutely right"
        | 8 -> "I am confident we will find a solution"
        | 9 -> "We can help each other"

let math_response (str:string) =
    let n = rand.Next(10) 
    match n with
        | 0 -> "I am not very good at math."
        | 1 -> "So, do you love numbers."
        | 2 -> "Numbers! Numbers! That's why I love poetry!"
        | 3 -> "I do not understand " + str + "!"
        | 4 -> "Do you always try to " + str 
        | 5 -> "Just try to solve " + str + " yourself!" 
        | 6 -> "The expresion " + str + " is meaningless"
        | 7 -> "That's why I love computers they do all the math stuff"
        | 8 -> "Don't bother me with silly questions"
        | 9 -> "Yes, you seems very intelligent, but I do not love math"

let none_response (str:string) =
    let n = rand.Next(10) 
    match n with
        | 0 -> "Maybe."
        | 1 -> "Perhaps " + str 
        | 2 -> "Yes."
        | 3 -> "Ah!"
        | 4 -> "Whatever." 
        | 5 -> "Sorry, the chat closed unexpectedly. What was your last question?"
        | 6 -> "Where were we? I losed track of the conversation."
        | 7 -> "Very interesting"
        | 8 -> "Wow!"
        | 9 -> "Mmmmmm!"

// main recursive response function
// find the first match with a key token
// and return a response acordingly 
let rec response (token: string) (str: string) =
    match token with
        | Hello
            -> hello_response ()
        | Bye
            -> good_bye_response ()
        | Personal
            -> personal_response str
        | Question
            -> question_response str
        | Answer
            -> answer_response str
        | Wish 
            -> wish_response str
        | Negative 
            -> negative_response str
        | Positive 
            -> positive_response str
        | Math
            -> math_response str
        | "" 
            -> none_response str
        | None when (str.IndexOf(" ") > 0) 
            -> response (str.Substring(0,str.IndexOf(" "))) (str.Substring(str.IndexOf(" ")+1))
        | None when (str.IndexOf(" ") < 0) 
            -> response str ""
        | None 
            -> math_response str
