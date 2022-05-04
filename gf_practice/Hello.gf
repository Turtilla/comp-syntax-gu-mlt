-- a "Hello World" grammar
abstract Hello = {
    flags startcat = Greeting ;

    cat Greeting ; Recipient ;

    fun
      Hello : Recipient -> Greeting ;
      GoodMorning : Recipient -> Greeting ;
      GoodDay : Recipient -> Greeting ;
      World, Mum, Friends, Everyone, Dad, Doggie, Class, People : Recipient ;
}