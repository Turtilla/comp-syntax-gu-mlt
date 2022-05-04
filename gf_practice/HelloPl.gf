concrete HelloPl of Hello = {

    lincat Greeting, Recipient = {s : Str} ;

    lin
      Hello recip = {s = "hej" ++ recip.s} ;
      GoodMorning recip = {s = "dzień dobry" ++ recip.s} ;
      GoodDay recip = {s = "dzień dobry" ++ recip.s} ;
      World = {s = "świecie"} ;
      Mum = {s = "mamo"} ;
      Friends = {s = "przyjaciele"} ;
      Everyone = {s = "wszyscy"} ;
      Dad = {s = "tato"} ;
      Doggie = {s = "piesku"} ;
      Class = {s = "klaso"} ;
      People = {s = "ludzie"} ;
}