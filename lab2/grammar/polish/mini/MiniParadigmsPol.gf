resource MiniParadigmsEng = open

  MiniGrammarEng,
  MiniResEng
  
in {

oper
  mkN = overload {
    mkN :  Str -> Str -> Str -> Gender -> Noun   -- predictable nouns, e.g. jajo-jaja, kobieta-kobiety, byk-byki, król-królowie
      = \noun,plural,gen,g -> lin N (smartNoun noun plural gen g) ;
    mkN : Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Gender -> Noun  -- irregular nouns, e.g. człowiek-ludzie
      = \sgnom, sggen, sgdat, sgacc, sgins, sgloc, sgvoc, plnom, plgen, pldat, placc, plins, plloc, plvoc, g -> 
      lin N (mkNoun sgnom sggen sgdat sgacc sgins sgloc sgvoc plnom plgen pldat placc plins plloc plvoc g) ;
    } ;

--TODO mkPN
  mkPN : Str -> PN
    = \s -> lin PN {s = s} ;

  mkA : Str -> A
    = \masc -> lin A (smartAdj masc) ;

  mkV = overload {
    mkV : Str -> Conjugation -> Verb  -- predictable verb, e.g. czytać-czytam-czytasz-etc.
      = \inf, conj -> lin V (conjVerb inf conj) ;
    mkV : Str -> Str -> Conjugation -> Verb  -- verb with an unpredictable imperative, e.g. robić-robię-rób-etc.
      = \inf, imp, conj -> lin V (irregImpVerb inf imp conj) ;
    mkV : Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> V  -- irregular verb, e.g. iść-idę-idziesz-etc.
      = \inf, imp2sg, pressg1, pressg2, pressg3, prespl1, prespl2, prespl3, lpart, lpartmascsg, lpartmascpl -> 
      lin V (mkVerb inf imp2sg pressg1 pressg2 pressg3 prespl1 prespl2 prespl3 lpart lpartmascsg lpartmascpl) ;
    } ;

--These cannot just take strings since constructing a verb requires the conjugation
--something here is causing a warning when compiling: Warning: ignoring lock fields in resolving mkV2 (mkV "łamać" IX) for {s : VForm => Str} 
--using V; however, this does not impact the grammar working and we could not find a solution/explanation as to what causes it in the lab.
  mkV2 = overload {
    mkV2 : V -> Case -> Case -> V2            -- any verb with direct object, e.g. "drink"
      = \v,c   -> lin V2 (v ** {cp = [] ; rp = c ; rn = c}) ;
    mkV2 : V -> Str -> Case -> Case -> V2     -- any verb with preposition
      = \v,p,c -> lin V2 (v ** {cp = p ; r = c ; rn = c}) ;
    } ;

--TODO mkVS
  mkVS : V -> VS
    = \v -> lin VS v ;

  mkAdv : Str -> Adv
    = \s -> lin Adv {s = s} ;
  
  mkPrep : Str -> Case -> Prep
    = \s,c -> lin Prep {s = s ; c = c} ;

}