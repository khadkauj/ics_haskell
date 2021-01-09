module Email where

data Person = Employee { eFirstName :: String, eLastName :: String } | Student { sFirstName :: String, sLastName :: String }

data Role = Role { rName :: String }

class Email a where
    address :: a -> String -- <something@example.com>
    greeting :: a -> String -- proper greeting phrase
    message :: a -> String -> String -- proper Internet message

instance Email Person where
    address (Student f m) = "To: <" ++ f ++ "." ++  m ++ "@example.com>"
    address (Employee f m) = "To: <" ++ f ++ "." ++  m ++ "@example.com>"
    greeting (Student f m) = "Dear " ++ f ++ ","
    greeting (Employee f m) = "Dear " ++ f ++ ","
    message (Student f m) nw = "To: <" ++ f ++ "." ++ m ++ "@example.com>" ++ "\n\n" ++ "Dear " ++ f ++ "," ++ "\n\n" ++ nw
    message (Employee f m) nw = "To: <" ++ f ++ "." ++  m ++ "@example.com>" ++ "\n\n" ++ "Dear " ++ f ++ "," ++ "\n\n" ++ nw

instance Email Role where
    address (Role r) =  "To: <" ++ r ++ "@example.com>"
    greeting (Role r) = "Dear " ++ r ++ ","
    message (Role r) gw = "To: <" ++ r ++ "@example.com>" ++ "\n\n" ++ "Dear " ++ r ++ "," ++ "\n\n" ++ gw

s = Student { sFirstName = "Kim", sLastName = "Smart" }
e = Employee { eFirstName = "Michael", eLastName = "Faraday" }
r = Role { rName = "President" }

nw = "Have a nice weekend"
gw = "You are doing a great job, expect to receive a bonus payment."

main = do
    putStrLn(message s nw)
    putStrLn(message e nw)
    putStrLn(message r gw)