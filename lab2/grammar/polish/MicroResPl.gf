resource MicroResPl = open Prelude in {

param
  VForm = Inf | Pres NPAgreement ;
  Number = Sg | Pl ;
  Case = Nom | Gen | Dat | Acc | Ins | Loc | Voc ;
  Gender = MascAnim | Masc | Fem | Neut ;
  Person = First | Second | Third ;
  Conjugation = I | II | III | IV | Va | VIa | VIb | VIIa | VIIb | VIII | IX | Xa | Xb ; -- | Xc | XI not implemented because of irregularities

  NPAgreement = NPAgr Number Person ;
  GNAgreement = GNAgr Gender Number ;

oper
  --for nouns I need to store 2 numbers, 7 cases each, and the gender. Aside from the typical masculine, feminine, neuter, there is something that
  --I here called the masculine animate gender, but it mostly sets apart human beings from all the other masculine nouns. The major challenge here
  --was that aside from these categories, the final consonant of the root of the noun also influences the kinds of endings or sound changes that it
  --undergoes. In addition, sometimes there is a case-to-case ending variation that is not dictated by any features of the word, but is simply the
  --more widely accepted form, and there is no predicting it without knowing it beforehand. This is why this is not an exhaustive list. Due to there
  --often being animate and inanimate forms of the same masculine noun, I needed to introduce the plural as a variable that would help me distinguish
  --between those; it also helps tell apart historically soft p, b, m, w from their hard counterparts (which is not visible in the nominative singular)
  --for masculine nouns. From what I saw the official Polish grammar handles this by having tens, if not hundreds, of sets of endings, and it passes
  --nouns to the function together with a number to identify the set. I wanted to avoid it, but, consequently, my smart noun function is not ideal
  --and cannot account for all the forms, especially the ones that are in free variation or undergo some not fully predictable internal sound changes.
  --I also chose not to implement some patterns that I know only concern one or two words, as these seem to be irregular enough to be treated as such.
  --There is a smart function for each gender, since the sheer number of variations made it hard to otherwise troubleshoot errors (it is easier to
  --fix the error if you know that it is in the endings for masculine nouns, than for all the nouns). There is then a "master function" smartNoun,
  --which selects which of the four to use based on the given gender.
  Noun : Type = {s : Number => Case => Str ; g : Gender} ;

  mkNoun : Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Gender -> Noun 
        = \sgnom, sggen, sgdat, sgacc, sgins, sgloc, sgvoc, plnom, plgen, pldat, placc, plins, plloc, plvoc, g -> {
        s = table {
            Sg => table {
                Nom => sgnom ;
                Gen => sggen ;
                Dat => sgdat ;
                Acc => sgacc ;
                Ins => sgins ;
                Loc => sgloc ;
                Voc => sgvoc
            } ;
            Pl => table {
                Nom => plnom ;
                Gen => plgen ;
                Dat => pldat ;
                Acc => placc ;
                Ins => plins ;
                Loc => plloc ;
                Voc => plvoc 
            }
            } ;
            g = g
        } ;

  smartNounNeut : Str -> Str -> Gender -> Noun = \noun,plural,g -> case noun of {
              ---most neuter nouns that end with "o", like "jajo" or "mleko"; does not account for the changes to the root in some plutal genitives ("okno" - "okien") or for the relatively irregular "dziecko"
              jaj + "o" => case jaj of {
                mi + "ast" => mkNoun noun plural (jaj + "u") noun (jaj + "em") (mi + "eście") noun plural jaj (jaj + "om") plural (jaj + "ami") (jaj + "ach") plural g ;
                jezio + "r" => mkNoun noun plural (jaj + "u") noun (jaj + "em") (jezio + "rze") noun plural jaj (jaj + "om") plural (jaj + "ami") (jaj + "ach") plural g ;
                sl + "ow" => mkNoun noun plural (jaj + "u") noun (jaj + "iem") (jaj + "ie") noun plural (sl + "ów") (jaj + "om") plural (jaj + "ami") (jaj + "ach") plural g ;
                _ => mkNoun noun plural (jaj + "u") noun (jaj + "iem") (jaj + "u") noun plural jaj (jaj + "om") plural (jaj + "ami") (jaj + "ach") plural g 
              } ;
              --foreign neuter nouns that end with "um", where all the singular forms are the same, e.g. "muzeum", "memorandum".
              muze + "um" => mkNoun noun noun noun noun noun noun noun plural (muze + "ów") (muze + "om") plural (muze + "ami") (muze + "ach") plural g ;
              --neuter nouns ending with "ę" that mostly denote the young of some species (e.g. "cielę" - calf, "prosię" - piglet). Does not account for "imię".
              zwierz + "ę" => mkNoun noun (zwierz + "ęcia") (zwierz + "ęciu") noun (zwierz + "ęciem") (zwierz + "ęciu") noun plural (zwierz + "ąt") (zwierz + "ętom") plural (zwierz + "ętami") (zwierz + "ętach") plural g ;
              --neuter verb-derived nouns describing actions, e.g. "jedzenie" - food/eating, "siedzenie" - seat/sitting, "myślenie" - thinking.
              jedze + "nie" => mkNoun noun plural (jedze + "niu") noun (jedze + "niem") (jedze + "niu") noun plural (jedze + "ń") (jedze + "niom") plural (jedze + "niami") (jedze + "niach") plural g ;
              --neuter nouns ending in "e", with a case accounting for internal vowel mutation.
              pol + "e" => case pol of {
                p + "o" + l => mkNoun noun plural (pol + "u") (pol + "e") (pol + "em") (pol + "u") (pol + "e") plural (p + "ó" + l) (pol + "om") plural (pol + "ami") (pol + "ach") plural g ;
                zdje + "ci" => mkNoun noun plural (pol + "u") (pol + "e") (pol + "em") (pol + "u") (pol + "e") plural (zdje + "ć") (pol + "om") plural (pol + "ami") (pol + "ach") plural g ;
                pyta + "ni" => mkNoun noun plural (pol + "u") (pol + "e") (pol + "em") (pol + "u") (pol + "e") plural (pyta + "ń") (pol + "om") plural (pol + "ami") (pol + "ach") plural g ;
                _ => mkNoun noun plural (pol + "u") (pol + "e") (pol + "em") (pol + "u") (pol + "e") plural (pol + "y") (pol + "om") plural (pol + "ami") (pol + "ach") plural g 
              } ;
              --neuter nouns ending with "us".
              op + "us" => mkNoun noun (noun + "u") (noun + "owi") noun (noun + "em") (noun + "ie") (noun + "ie") plural (noun + "ów") (noun + "om") plural (noun + "ami") (noun + "ach") plural g 
  } ;

  smartNounFem : Str -> Str -> Gender -> Noun = \noun,plural,g -> case noun of { 
              --feminine nouns ending in "ew" - the plurals for "krew" (blood) are not really used and the ones here are modelled after "brew" (eyebrow), and don't fully fit the dictionary entry for "krew", but sound acceptable to me as a native speaker.
              kr + "ew" => mkNoun noun plural plural noun (kr + "wią") plural plural plural plural (kr + "wiom") plural (kr + "wiami") (kr + "wiach") plural g ;
              --feminine nouns ending with "a" with a velar stem.
              wal + "ka" => mkNoun noun plural (wal + "ce") (wal + "kę") (wal + "ką") (wal + "ce") (wal + "ko") plural (wal + "k") (wal + "kom") plural (wal + "kami") (wal + "kach") plural g ;
              dro + "ga" => mkNoun noun plural (dro + "dze") (dro + "gę") (dro + "gą") (dro + "dze") (dro + "go") plural (dro + "g") (dro + "gom") plural (dro + "gami") (dro + "gach") plural g ;
              ce + "cha" => mkNoun noun plural (ce + "sze") (ce + "chę") (ce + "chą") (ce + "sze") (ce + "cho") plural (ce + "ch") (ce + "chom") plural (ce + "chami") (ce + "chach") plural g ;         
              wata + "ha" => mkNoun noun plural (wata + "sze") (wata + "hę") (wata + "hą") (wata + "sze") (wata + "ho") plural (wata + "h") (wata + "hom") plural (wata + "hami") (wata + "hach") plural g ;
              --feminine nouns ending with "a" with a historically soft stem.
              pra + ("ca"|"dza"|"sza"|"ża"|"rza"|"cza"|"dża") => case noun of {prac + "a" => mkNoun noun (prac + "y") (prac + "y") (prac + "ę") (prac + "ą") (prac + "y") (prac + "o") plural prac (prac + "om") plural (prac + "ami") (prac + "ach") plural g } ;
              --feminine nouns ending with "a" with a soft stem; does not account for some animate nouns' vocatives in singular ending with "u" instead of "o"; I split them into two based on where the majority of the nouns have the "o" or the "u", but there are still exceptions.
              a + ("la"|"bia"|"mia"|"wia"|"pia"|"ia") => case noun of {asi + "a" => mkNoun noun asi asi (asi + "ę") (asi + "ą") asi (asi + "o") plural asi (asi + "om") plural (asi + "ami") (asi + "ach") plural g } ;
              a + ("sia"|"cia"|"zia"|"dzia"|"nia") => case noun of {asi + "a" => mkNoun noun asi asi (asi + "ę") (asi + "ą") asi (asi + "u") plural asi (asi + "om") plural (asi + "ami") (asi + "ach") plural g } ;
              fre + "ja" => mkNoun noun (fre + "i") (fre + "i") (fre + "ję") (fre + "ją") (fre + "i") (fre + "jo") plural (fre + "i") (fre + "jom") plural (fre + "jami") (fre + "jach") plural g ;
              --feminine nouns ending with "a" with a hard stem.
              la + ("ba"|"pa"|"fa"|"wa"|"ma"|"ła"|"ta"|"da"|"sa"|"za"|"na") => case noun of {lab + "a" => mkNoun noun plural (lab + "ie") (lab + "ę") (lab + "ą") (lab + "ie") (lab + "o") plural lab (lab + "om") plural (lab + "ami") (lab + "ach") plural g } ;
              kobie + "ta" => mkNoun noun plural (kobie + "cie") (kobie + "tę") (kobie + "tą") (kobie + "cie") (kobie + "to") plural (kobie + "t") (kobie + "tom") plural (kobie + "tami") (kobie + "tach") plural g ;
              szko + "ła" => mkNoun noun plural (szko + "le") (szko + "łę") (szko + "łą") (szko + "le") (szko + "ło") plural (szko + "ł") (szko + "łom") plural (szko + "łami") (szko + "łach") plural g ;
              ope + "ra" => mkNoun noun plural (ope + "rze") (ope + "rę") (ope + "rą") (ope + "rze") (ope + "ro") plural (ope + "r") (ope + "rom") plural (ope + "rami") (ope + "rach") plural g ;
              --feminine nouns ending with "a" with a vowel stem.
              id + "ea" => case noun of { ide + "a" => mkNoun noun (ide + "i") (ide + "i") (ide + "ę") (ide + "ą") (ide + "i") (ide + "o") plural (ide + "i") (ide + "om") plural (ide + "ami") (ide + "ach") plural g } ;
              stat + "ua" => case noun of { statu + "a" => mkNoun noun plural (statu + "i") (statu + "ę") (statu + "ą") (statu + "i") (statu + "o") plural (statu + "i") (statu + "om") plural (statu + "ami") (statu + "ach") plural g } ;
              --feminine nouns ending with "i".
              mistrzy + "ni" => mkNoun noun noun noun (mistrzy + "nię") (mistrzy + "nią") noun noun plural (mistrzy + "ń") (mistrzy + "niom") plural (mistrzy + "niami") (mistrzy + "niach") plural g ;
              --feminine nouns ending with a historically soft consonant.
              podro + ("c"|"dz"|"sz"|"ż"|"rz"|"cz"|"dż") => mkNoun noun (noun + "y") (noun + "y") noun (noun + "ą") (noun + "y") (noun + "y") plural (noun + "y") (noun + "om") plural (noun + "ami") (noun + "ach") plural g ;
              --feminine nouns ending with a soft consonant.
              milo + "ść" => mkNoun noun plural plural noun (milo + "ścią") plural plural plural plural (milo + "ściom") plural (milo + "ściami") (milo + "ściach") plural g ;  
              ja + "źń" => mkNoun noun (ja + "źni") (ja + "źni") noun (ja + "źnią") (ja + "źni") (ja + "źni") plural (ja + "źni") (ja + "źniom") plural (ja + "źniami") (ja + "źniach") plural g 
   } ;

  smartNounMasc : Str -> Str -> Gender -> Noun = \noun,plural,g -> case noun of {
              --velar stem masculine nouns; does not account for irregular variation of "a" and "u" in genitive singular.
              par + ("k"|"g") => mkNoun noun (noun + "a") (noun + "owi") noun (noun + "iem") (noun + "u") (noun + "u") plural (noun + "ów") (noun + "om") plural (noun + "ami") (noun + "ach") plural g ;
              da + ("h"|"ch") => mkNoun noun (noun + "u") (noun + "owi") noun (noun + "em") (noun + "u") (noun + "u") plural (noun + "ów") (noun + "om") plural (noun + "ami") (noun + "ach") plural g ;
              --historically soft stem masculine nouns.
              --add other variants
              zaj + "ąc" => mkNoun noun (noun + "a") (noun + "owi") (noun + "a") (noun + "em") (noun + "u") (noun + "u") plural (zaj + "ęcy") (noun + "om") (noun + "e") (noun + "ami") (noun + "ach") (noun + "e") g ;
              tasiem + "iec" => mkNoun noun (tasiem + "ca") (tasiem + "cowi") (tasiem + "ca") (tasiem + "cem") (tasiem + "cu") (tasiem + "cu") plural (tasiem + "ców") (tasiem + "com") plural (tasiem + "cami") (tasiem + "cach") plural g ;
              ko + ("c"|"dz") => mkNoun noun (noun + "a") (noun + "owi") noun (noun + "em") (noun + "u") (noun + "u") plural (noun + "ów") (noun + "om") plural (noun + "ami") (noun + "ach") plural g ;
              klu + ("cz"|"sz"|"ż"|"rz"|"dż") => mkNoun noun (noun + "a") (noun + "owi") noun (noun + "em") (noun + "u") (noun + "u") (noun + "e") (noun + "y") (noun + "om") plural (noun + "ami") (noun + "ach") plural g ;
              --soft stem masculine nouns.
              ki + ("j"|"l") => mkNoun noun (noun + "a") (noun + "owi") noun (noun + "em") (noun + "u") (noun + "u") plural (noun + "ów") (noun + "om") plural (noun + "ami") (noun + "ach") plural g ;
              ko + "ń" => mkNoun noun (ko + "nia") (ko + "niowi") (ko + "nia") (ko + "niem") (ko + "niu") (ko + "niu") plural (ko + "ni") (ko + "niom") plural (ko + "niami") (ko + "niach") plural g ;
              pa + "ź" => mkNoun noun (pa + "zia") (pa + "ziowi") (pa + "zia") (pa + "ziem") (pa + "ziu") (pa + "ziu") plural (pa + "zi") (pa + "ziom") plural (pa + "ziami") (pa + "ziach") plural g ;
              re + "dź" => mkNoun noun (re + "dzia") (re + "dziowi") (re + "dzia") (re + "dziem") (re + "dziu") (re + "dziu") plural (re + "dzi") (re + "dziom") plural (re + "dziami") (re + "dziach") plural g ;
              mi + "ś" => mkNoun noun (mi + "sia") (mi + "siowi") (mi + "sia") (mi + "siem") (mi + "siu") (mi + "siu") plural (mi + "si") (mi + "siom") plural (mi + "siami") (mi + "siach") plural g ;
              ki + "ć" => mkNoun noun (ki + "cia") (ki + "ciowi") (ki + "cia") (ki + "ciem") (ki + "ciu") (ki + "ciu") plural (ki + "ci") (ki + "ciom") plural (ki + "ciami") (ki + "ciach") plural g ;
              --some soft stem masculine nouns and hard stem masculine nouns (they are indistinguishable in Nom Sg), need a "case plural of" expression.
              kar + ("p"|"b"|"m"|"w") => case plural of {
                kar + ("pie"|"bie"|"mie"|"wie") => mkNoun noun (noun + "ia") (noun + "iowi") (noun + "ia") (noun + "iem") (noun + "iu") (noun + "iu") plural (noun + "i") (noun + "iom") plural (noun + "iami") (noun + "iach") plural g ;
                --hard stem masculine nouns.
                dom + "y" => mkNoun noun (noun + "u") (noun + "owi") noun (noun + "em") (noun + "u") (noun + "u") plural (noun + "ów") (noun + "om") plural (noun + "ami") (noun + "ach") plural g 
              } ;
              --does not account for "ó"->"o" shifts.
              rowe + "r" => mkNoun noun (noun + "a") (noun + "owi") noun (noun + "em") (noun + "ze") (noun + "ze") plural (noun + "ów") (noun + "om") plural (noun + "ami") (noun + "ach") plural g ;
              fia + "t" => mkNoun noun (noun + "a") (noun + "owi") noun (noun + "em") (fia + "cie") (fia + "cie") plural (noun + "ów") (noun + "om") plural (noun + "ami") (noun + "ach") plural g ;
              --this example showcases well the issues with local irregularities; the same word in the nominative means mud/river sand or a mule. These endings are for the animal, but the mud version gets a different genitive and accusative singular ("u" and no ending). There is no regularity to this, nor is there a way to predict it other than just knowing.
              mu + "ł" => mkNoun noun (noun + "a") (noun + "owi") (noun + "a") (noun + "em") (mu + "le") (mu + "le") plural (noun + "ów") (noun + "om") plural (noun + "ami") (noun + "ach") plural g ; 
              ga + "d" => mkNoun noun (noun + "a") (noun + "owi") (noun + "a") (noun + "em") (ga + "dzie") (ga + "dzie") plural (noun + "ów") (noun + "om") plural (noun + "ami") (noun + "ach") plural g ;
              no + ("n"|"z"|"s"|"f") => mkNoun noun (noun + "a") (noun + "owi") noun (noun + "em") (noun + "ie") (noun + "ie") plural (noun + "ów") (noun + "om") plural (noun + "ami") (noun + "ach") plural g 
              --due to their rarity or difficulty, masculine nouns with fleeting vowels (pies vs. psy), with the "ą" vowel (wąż vs. węży), and full irregulars have been excluded.
  } ;

  smartNounMascAnim : Str -> Str -> Gender -> Noun = \noun,plural,g -> case plural of {
              --masculine animate nouns ending with "owie" in Nom Pl.
              krol + "owie" => case noun of { 
                zie + "ć" => mkNoun noun (zie + "cia") (zie + "ciowi") (zie + "cia") (zie + "ciem") (zie + "ciu") (zie + "ciu") plural (zie + "ciów") (zie + "ciom") (zie + "ciów") (zie + "ciami") (zie + "ciach") plural g ;
                zie + "ś" => mkNoun noun (zie + "sia") (zie + "siowi") (zie + "sia") (zie + "siem") (zie + "siu") (zie + "siu") plural (zie + "siów") (zie + "siom") (zie + "siów") (zie + "siami") (zie + "siach") plural g ;
                zie + "ń" => mkNoun noun (zie + "nia") (zie + "niowi") (zie + "nia") (zie + "niem") (zie + "niu") (zie + "niu") plural (zie + "niów") (zie + "niom") (zie + "niów") (zie + "niami") (zie + "niach") plural g ;
                zie + "ź" => mkNoun noun (zie + "zia") (zie + "ziowi") (zie + "zia") (zie + "ziem") (zie + "ziu") (zie + "ziu") plural (zie + "ziów") (zie + "ziom") (zie + "ziów") (zie + "ziami") (zie + "ziach") plural g ;
                zie + "dź" => mkNoun noun (zie + "dzia") (zie + "dziowi") (zie + "dzia") (zie + "dziem") (zie + "dziu") (zie + "dziu") plural (zie + "dziów") (zie + "dziom") (zie + "dziów") (zie + "dziami") (zie + "dziach") plural g ;
                krol => mkNoun noun (noun + "a") (noun + "owi") (noun + "a") (noun + "em") (noun + "u") (noun + "u") (noun + "owie") (noun + "ów") (noun + "om") (noun + "ów") (noun + "ami") (noun + "ach") (noun + "owie") g 
               } ;
              --masculine animate nouns ending with "i" in Nom Pl.
              adwokac + "i" => case noun of {
                angli + "sta" => mkNoun noun (angli + "sty") (angli + "ście") (angli + "stę") (angli + "stą") (angli + "ście") (angli + "sto") (angli + "ści") (angli + "stów") (angli + "stom") (angli + "stów") (angli + "stami") (angli + "stach") (angli + "ści") g ;
                poe + "ta" => mkNoun noun (poe + "ty") (poe + "cie") (poe + "tę") (poe + "tą") (poe + "cie") (poe + "to") (poe + "ci") (poe + "tów") (poe + "tom") (poe + "tów") (poe + "tami") (poe + "tach") (poe + "ci") g ;
                meszczy + "zna" => mkNoun noun (meszczy + "zny") (meszczy + "źnie") (meszczy + "znę") (meszczy + "zną") (meszczy + "źnie") (meszczy + "zno") (meszczy + "źni") (meszczy + "zn") (meszczy + "znom") (meszczy + "zn") (meszczy + "znami") (meszczy + "znach") (meszczy + "źni") g ;
                adwoka + "t" => mkNoun noun (noun + "a") (noun + "owi") (noun + "a") (noun + "em") (adwoka + "cie") (adwoka + "cie") (adwoka + "ci") (noun + "ów") (noun + "om") (noun + "ów") (noun + "ami") (noun + "ach") (adwoka + "ci") g ;
                cze + "ch" => mkNoun noun (noun + "a") (noun + "owi") (noun + "a") (noun + "em") (noun + "u") (noun + "u") (cze + "si") (noun + "ów") (noun + "om") (noun + "ów") (noun + "ami") (noun + "ach") (cze + "si") g ;
                sasi + "ad" => mkNoun noun (noun + "a") (noun + "owi") (noun + "a") (noun + "em") (sasi + "edzie") (sasi + "edzie") (sasi + "edzi") (noun + "ów") (noun + "om") (noun + "ów") (noun + "ami") (noun + "ach") (sasi + "edzi") g ;
                szwe + "d" => mkNoun noun (noun + "a") (noun + "owi") (noun + "a") (noun + "em") (noun + "zie") (noun + "zie") (noun + "zi") (noun + "ów") (noun + "om") (noun + "ów") (noun + "ami") (noun + "ach") (noun + "zi") g ;
                weteran => mkNoun noun (noun + "a") (noun + "owi") (noun + "a") (noun + "em") (noun + "ie") (noun + "ie") (noun + "i") (noun + "ów") (noun + "om") (noun + "ów") (noun + "ami") (noun + "ach") (noun + "i") g  
              } ;
              --masculine animate nouns ending with "y" in Nom Pl.
              koledz + "y" => case noun of {
                kole + "ga" => mkNoun noun (kole + "gi") (kole + "dze") (kole + "gę") (kole + "gą") (kole + "dze") (kole + "go") plural (kole + "gów") (kole + "gom") (kole + "gów") (kole + "gami") (kole + "gach") plural g ;
                doradc + "a" => mkNoun noun plural plural (doradc + "ę") (doradc + "ą") plural (doradc + "o") plural (doradc + "ów") (doradc + "om") (doradc + "ów") (doradc + "ami") (doradc + "ach") plural g ; 
                anglik => mkNoun noun (noun + "a") (noun + "owi") (noun + "a") (noun + "iem") (noun + "u") (noun + "u") plural (noun + "ów") (noun + "om") (noun + "ów") (noun + "ami") (noun + "ach") plural g  --accounts for stem changes g/dz, k/c as they only occur in nom=voc
              } ;
              --masculine animate nouns ending with "e" in Nom Pl.
              gosci + "e" => case noun of {
                gos + "ć" => mkNoun noun (gos + "cia") (gos + "ciowi") (gos + "cia") (gos + "ciem") (gos + "ciu") (gos + "ciu") plural (gos + "ci") (gos + "ciom") (gos + "ci") (gos + "ciami") (gos + "iach") plural g ;
                cesarz => mkNoun noun (noun + "a") (noun + "owi") (noun + "a") (noun + "em") (noun + "u") (noun + "u") plural (noun + "y") (noun + "om") (noun + "y") (noun + "ami") (noun + "ach") plural g 
              } ;
              --masculine animate nouns ending with "anie" in Nom Pl.
              ameryk + "anie" => case noun of {
                ameryk + "anin" => mkNoun noun (noun + "a") (noun + "owi") (noun + "a") (noun + "em") (noun + "ie") (noun + "ie") (ameryk + "anie") (ameryk + "anów") (ameryk + "anom") (ameryk + "anów") (ameryk + "anami") (ameryk + "anach") (ameryk + "anie") g ;
                hiszp + "an" => mkNoun noun (noun + "a") (noun + "owi") (noun + "a") (noun + "em") (noun + "ie") (noun + "ie") (noun + "ie") (noun + "ów") (noun + "om") (noun + "ów") (noun + "ami") (noun + "ach") (noun + "ie") g 
              }
  } ;

  smartNoun : Str -> Str -> Gender -> Noun = \noun,plural,g -> case g of {
            --this was suggested by Arianna to easier troubleshoot problems with the functions for each gender, and to make it more modular
            Neut => smartNounNeut noun plural g ;
            Fem => smartNounFem noun plural g ;
            Masc => smartNounMasc noun plural g ;
            MascAnim => smartNounMascAnim noun plural g 
        } ;

  --for adjectives, since this grammar does not include different degrees (nor negated adjectives, which in Polish are spelled together with the
  --negation, e.g. "miły" (nice) vs. "niemiły" (not nice, rude)), I needed to store 4 genders, each with a plural - although the plurals for all
  --but masculine animate/human are the same. There is, in fact, only 11 different forms that a single adjective can have. However, they can differ
  --based on the final vowel in the masculine nominative singular, as well as the final consonant of the root. I decided to tell these apart in the 
  --smart adjective function, and in the make adjective one simply require the 11 forms to be entered. The smart adjective function only needs the
  --masculine singular nominative form as the rest is practically fully predictable from that. This is perhaps the most regular of the categories.
  Adjective : Type = {s : Gender => Number => Case => Str} ;

  mkAdj : (yform, egoform, emuform, ymform, ychform, ymiform, eform, aform, ejform, anform, plmasc : Str) -> Adjective
    = \yform,egoform,emuform,ymform,ychform,ymiform,eform,aform,ejform,anform,plmasc -> {
    s = table {
        MascAnim => table {
          Sg => table {
            Nom => yform ; --żółty, niebieski
            Gen => egoform ; --żółtego, niebieskiego
            Dat => emuform ; --żółtemu, niebieskiemu
            Acc => egoform ; 
            Ins => ymform ; --żółtym, niebieskim
            Loc => ymform ;
            Voc => yform 
          } ;
          Pl => table {
            Nom => plmasc ; --żółci, niebiescy
            Gen => ychform ; --żółtych, niebieskich
            Dat => ymform ;
            Acc => ychform ;
            Ins => ymiform ; --żółtymi, niebieskimi
            Loc => ychform ;
            Voc => plmasc
          }
        } ;
        Masc => table {
          Sg => table {
            Nom => yform ;
            Gen => egoform ;
            Dat => emuform ;
            Acc => yform ; 
            Ins => ymform ;
            Loc => ymform ;
            Voc => yform 
          } ;
          Pl => table {
            Nom => eform ; --żółte, niebieskie
            Gen => ychform ;
            Dat => ymform ;
            Acc => eform ;
            Ins => ymiform ;
            Loc => ychform ;
            Voc => eform
          }
        } ;
        Fem => table {
          Sg => table {
            Nom => aform ; --żółta, niebieska
            Gen => ejform ; --żółtej, niebieskiej
            Dat => ejform ;
            Acc => anform ; --żółtą, niebieską
            Ins => anform ;
            Loc => ejform ;
            Voc => aform
          } ;
          Pl => table {
            Nom => eform ;
            Gen => ychform ;
            Dat => ymform ;
            Acc => eform ;
            Ins => ymiform ;
            Loc => ychform ;
            Voc => eform
          }
        } ;
        Neut => table {
          Sg => table {
            Nom => eform ;
            Gen => egoform ;
            Dat => emuform ;
            Acc => eform ;
            Ins => ymform ;
            Loc => ymform ;
            Voc => eform
          } ;
          Pl => table {
            Nom => eform ;
            Gen => ychform ;
            Dat => ymform ;
            Acc => eform ;
            Ins => ymiform ;
            Loc => ychform ;
            Voc => eform
          }
        } 
      } 
    } ;
  
  
  smartAdj : Str -> Adjective = \masc -> case masc of {
     du  +  "ży" => mkAdj (du + "ży") (du + "żego") (du + "żemu") (du + "żym") (du + "żych") (du + "żymi") (du + "że") (du + "ża") (du + "żej") (du + "żą") (du + "zi") ;
     wes  +  "oły" => mkAdj (wes + "oły") (wes + "ołego") (wes + "ołemu") (wes + "ołym") (wes + "ołych") (wes + "ołymi") (wes + "ołe") (wes + "oła") (wes + "ołej") (wes + "ołą") (wes + "eli") ;
     mi  +  "ły" => mkAdj (mi + "ły") (mi + "łego") (mi + "łemu") (mi + "łym") (mi + "łych") (mi + "łymi") (mi + "łe") (mi + "ła") (mi + "łej") (mi + "łą") (mi + "li") ;
     mlo  +  "dy" => mkAdj (mlo + "dy") (mlo + "dego") (mlo + "demu") (mlo + "dym") (mlo + "dych") (mlo + "dymi") (mlo + "de") (mlo + "da") (mlo + "dej") (mlo + "dą") (mlo + "dzi") ;
     pro  +  "sty" => mkAdj (pro + "sty") (pro + "stego") (pro + "stemu") (pro + "stym") (pro + "stych") (pro + "stymi") (pro + "ste") (pro + "sta") (pro + "stej") (pro + "stą") (pro + "ści") ;
     pracowi  +  "ty" => mkAdj (pracowi + "ty") (pracowi + "tego") (pracowi + "temu") (pracowi + "tym") (pracowi + "tych") (pracowi + "tymi") (pracowi + "te") (pracowi + "ta") (pracowi + "tej") (pracowi + "tą") (pracowi + "ci") ;
     star  +  "szy" => mkAdj (star + "szy") (star + "szego") (star + "szemu") (star + "szym") (star + "szych") (star + "szymi") (star + "sze") (star + "sza") (star + "szej") (star + "szą") (star + "si") ;
     zmecz  +  "ony" => mkAdj (zmecz + "ony") (zmecz + "onego") (zmecz + "onemu") (zmecz + "onym") (zmecz + "onych") (zmecz + "onymi") (zmecz + "one") (zmecz + "ona") (zmecz + "onej") (zmecz + "oną") (zmecz + "eni") ;
     dob  +  "ry" => mkAdj (dob + "ry") (dob + "rego") (dob + "remu") (dob + "rym") (dob + "rych") (dob + "rymi") (dob + "re") (dob + "ra") (dob + "rej") (dob + "rą") (dob + "rzy") ;
     elegan  +  "cki" => mkAdj (elegan + "cki") (elegan + "ckiego") (elegan + "ckiemu") (elegan + "ckim") (elegan + "ckich") (elegan + "ckimi") (elegan + "ckie") (elegan + "cka") (elegan + "ckiej") (elegan + "cką") (elegan + "ccy") ;
     wyso  +  "ki" => mkAdj (wyso + "ki") (wyso + "kiego") (wyso + "kiemu") (wyso + "kim") (wyso + "kich") (wyso + "kimi") (wyso + "kie") (wyso + "ka") (wyso + "kiej") (wyso + "ką") (wyso + "cy")  ;
     ubo  +  "gi"  => mkAdj (ubo + "gi") (ubo + "giego") (ubo + "giemu") (ubo + "gim") (ubo + "gich") (ubo + "gimi") (ubo + "gie") (ubo + "ga") (ubo + "giej") (ubo + "gą") (ubo + "dzy") ;
     pachna  +  "cy"  => mkAdj (pachna + "cy") (pachna + "cego") (pachna + "cemu") (pachna + "cym") (pachna + "cych") (pachna + "cymi") (pachna + "ce") (pachna + "ca") (pachna + "cej") (pachna + "cą") (pachna + "cy") ;
     proro  +  "czy"  => mkAdj (proro + "czy") (proro + "czego") (proro + "czemu") (proro + "czym") (proro + "czych") (proro + "czymi") (proro + "cze") (proro + "cza") (proro + "czej") (proro + "czą") (proro + "czy") ;
     cu  +  "dzy"  => mkAdj (cu + "dzy") (cu + "dzego") (cu + "dzemu") (cu + "dzym") (cu + "dzych") (cu + "dzymi") (cu + "dze") (cu + "dza") (cu + "dzej") (cu + "dzą") (cu + "dzy") ;
     slab  +  "y" => mkAdj (slab + "y") (slab + "ego") (slab + "emu") (slab + "ym") (slab + "ych") (slab + "ymi") (slab + "e") (slab + "a") (slab + "ej") (slab + "ą")  (slab + "i") ;
     tan + "i" => mkAdj (tan + "i") (tan + "iego") (tan + "iemu") (tan + "im") (tan + "ich") (tan + "imi") (tan + "ie") (tan + "ia") (tan + "iej") (tan + "ią") (tan + "i") 
     } ;

  --for the verb, as far as only the present tense is concerned, only the infinitive and six present verb forms are needed. These are stored using
  --the NPAgr, since they need to agree with the subject in the number and person. In the past tense, in all the forms except for 2nd person plural,
  --the gender is also something that needs to be agreed for, but I disregarded it here as it is not currently needed. For the conjugate verb function,
  --which is the smart verb function to an extent, aside from the infinitive, one needs the number of the conjugation it takes, as these are not
  --predictable from just the spelling. This does not also fully account for changes in the root or conjugations where endings can vary.
  Verb : Type = {s : VForm => Str} ;

  mkVerb : (inf,pressg1,pressg2,pressg3,prespl1,prespl2,prespl3 : Str) -> Verb
    = \inf,pressg1,pressg2,pressg3,prespl1,prespl2,prespl3 -> {
    s = table {
      Inf => inf ; 
      Pres (NPAgr Sg First) => pressg1 ;
      Pres (NPAgr Sg Second) => pressg2 ;
      Pres (NPAgr Sg Third) => pressg3 ;
      Pres (NPAgr Pl First) => prespl1 ;
      Pres (NPAgr Pl Second) => prespl2 ;
      Pres (NPAgr Pl Third) => prespl3 
      }
    } ;

  -- determining the conjugation could be done with the infinitive and one of the inflected forms
  -- or should we have separate functions?
  conjVerb : Str -> Conjugation -> Verb = \inf, conj -> case conj of {
     I => case inf of { czyt  +  "ać" => mkVerb inf (czyt + "am") (czyt + "asz") (czyt + "a") (czyt + "amy") (czyt + "acie") (czyt + "ają")} ; -- 1st conjugation
     II => case inf of { umi  +  "eć" =>  mkVerb inf (umi + "em") (umi + "esz") (umi + "e") (umi + "emy") (umi + "ecie") (umi + "eją") } ;  -- 2nd conjugation
     III => case inf of { tani + "eć"  => mkVerb inf (tani + "eję") (tani + "ejesz") (tani + "eje") (tani + "ejemy") (tani + "ejcie") (tani + "eją") } ;  -- 3rd conjugation
     IV => case inf of { mal  +  "ować" => mkVerb inf (mal + "uję") (mal + "ujesz") (mal + "uje") (mal + "ujemy") (mal + "ujecie") (mal + "ują") } ;  -- 4th conjugation
     Va => case inf of { ciag  +  "nąć" => mkVerb inf (ciag + "nę") (ciag + "niesz") (ciag + "nie") (ciag + "niemy") (ciag + "niecie") (ciag + "ną") } ;  -- 5th conjugation, needs calling Va because it otherwise conflicts with V(erb)
     VIa => case inf of { rob  +  "ić" => mkVerb inf (rob + "ię") (rob + "isz") (rob + "i") (rob + "imy") (rob + "icie") (rob + "ią") } ;  -- 6th A conjugation
     VIb => case inf of { wierz  +  "yć" => mkVerb inf (wierz + "ę") (wierz + "ysz") (wierz + "y") (wierz + "ymy") (wierz + "ycie") (wierz + "ą") } ;  -- 6th B conjugation
     VIIa => case inf of { widz  +  ("ieć"|"eć") => mkVerb inf (widz + "ę") (widz + "isz") (widz + "i") (widz + "imy") (widz + "icie") (widz + "ą") } ;  -- 7th A conjugation
     VIIb => case inf of { lez  +  ("ieć"|"eć") => mkVerb inf (lez + "ę") (lez + "ysz") (lez + "y") (lez + "ymy") (lez + "ycie") (lez + "ą") } ;  -- 7th B conjugation
     VIII => case inf of { zysk  +  ("iwać"|"ywać") => mkVerb inf (zysk + "uję") (zysk + "ujesz") (zysk + "uje") (zysk + "ujemy") (zysk + "ujecie") (zysk + "ują") } ;  -- 8th conjugation, in pres same as 4th (merge?)
     IX => case inf of { la  +  "mać" => mkVerb inf (la + "mię") (la + "miesz") (la + "mie") (la + "miemy") (la + "miecie") (la + "mią") ;
                         ska  +  "kać" => mkVerb inf (ska + "czę") (ska + "czesz") (ska + "cze") (ska + "czemy") (ska + "czecie") (ska + "czą") } ;
     -- some verbs in the 9th conugation mutates the root https://pl.wiktionary.org/wiki/Aneks:J%C4%99zyk_polski_-_koniugacja_IX
     Xa => case inf of { pi  +  "ć" => mkVerb inf (pi + "ję") (pi + "jesz") (pi + "je") (pi + "jemy") (pi + "jecie") (pi + "ją") } ; -- 10th A conjugation
     Xb => case inf of { l  +  "ać" => mkVerb inf (l + "eję") (l + "ejesz") (l + "eje") (l + "ejemy") (l + "ejecie") (l + "eją") }  -- 10th B conjugation
     -- 10th C conjugation mutates the root too and has variation in endings https://pl.wiktionary.org/wiki/Aneks:J%C4%99zyk_polski_-_koniugacja_Xc
     -- 11th conjugation mutates the root as well https://pl.wiktionary.org/wiki/Aneks:J%C4%99zyk_polski_-_koniugacja_XI
     } ;  

  -- two-place verb with "case" as preposition; for transitive verbs, c=[]
  Verb2 : Type = Verb ** {cp : Str} ;

  be_Verb : Verb = mkVerb "być" "jestem" "jesteś" "jest" "jesteście" "jesteśmy" "są" ; 

}
