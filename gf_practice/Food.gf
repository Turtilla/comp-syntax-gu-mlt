abstract Food = {
  
      flags startcat = Utt ;
  
      cat
        Utt ; Phrase ; Item ; Kind ; Quality ;
  
      fun
        Utterance : Phrase -> Utt ;
        ButUtterance : Phrase -> Utt ;
        Is : Item -> Quality -> Phrase ;
        IsQ : Item -> Quality -> Phrase ;
        This, That : Kind -> Item ;
        QKind : Quality -> Kind -> Kind ;
        Wine, Cheese, Fish, Pasta, Pie, Cake, Casserole, Steak : Kind ;
        Very : Quality -> Quality ;
        Fresh, Warm, Italian, Expensive, Delicious, Boring, Yummy, Disgusting, Flavorful, Cheap, Affordable : Quality ;
    }