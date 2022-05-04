concrete HelloSwe of Hello = {

    lincat Greeting, Recipient = {s : Str} ;

    lin
      Hello recip = {s = "hejsan" ++ recip.s} ;
      GoodMorning recip = {s = "godmorgon" ++ recip.s} ;
      GoodDay recip = {s = "god dag" ++ recip.s} ;
      World = {s = "världen"} ;
      Mum = {s = "mamma"} ;
      Friends = {s = "vänner"} ;
      Everyone = {s = "alla"} ;
      Dad = {s = "pappa"} ;
      Doggie = {s = "vovve"} ;
      Class = {s = "klassen"} ;
      People = {s = "människor"} ;
}