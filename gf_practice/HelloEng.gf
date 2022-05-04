concrete HelloEng of Hello = {

    lincat Greeting, Recipient = {s : Str} ;

    lin
      Hello recip = {s = "hello" ++ recip.s} ;
      GoodMorning recip = {s = "good morning" ++ recip.s} ;
      GoodDay recip = {s = "good day" ++ recip.s} ;
      World = {s = "world"} ;
      Mum = {s = "mum"} ;
      Friends = {s = "friends"} ;
      Everyone = {s = "everyone"} ;
      Dad = {s = "dad"} ;
      Doggie = {s = "doggie"} ;
      Class = {s = "class"} ;
      People = {s = "people"} ;
}
